#include "backend/registers.hpp"
#include "backend/stack_frame.hpp"
#include "backend/symbols.hpp"
#include "backend/utils.hpp"
#include "cfg.hpp"
#include <algorithm>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Pass.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <memory>
#include <range/v3/all.hpp>
#include <string>
#include <unordered_map>

namespace sysy {
class AsmGenerator {
    std::unordered_map<std::string, StackFramePtr> stackFrames;
    AsmBuilder builder;

    llvm::CmpInst::Predicate currentCmpOp;

    void visit(std::unique_ptr<llvm::Module>& module)
    {
        for (auto& func : *module) {
            if (func.isDeclaration())
                continue;
            auto stackFrame = std::make_shared<StackFrame>(func, module->getDataLayout());
            stackFrames[func.getName().str()] = stackFrame;
        }
        for (auto& func : *module) {
            if (func.isDeclaration())
                continue;
            auto stackFrame = stackFrames[func.getName().str()];
            analyzeCallerSaveRegs(func, stackFrame);
            stackFrame->analyzeCalleeSaveRegCount(func);
        }
    }

    auto getCallerSavedRegCandidates(string_view calledFunc) -> std::vector<Register>
    {
        std::vector<Register> regCandidates { CALLER_SAVED_REGISTERS.begin(), CALLER_SAVED_REGISTERS.end() };
        if (calledFunc == "printf"sv) {
            regCandidates.push_back(Register::RBX);
        }
        return regCandidates;
    }

    void dumpExternFuncCall(const llvm::CallInst& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        auto callerName = stackFrame->getFuncName();
        auto calledOperand = inst.getCalledOperand();
        auto calledFuncName = calledOperand->getName().str();
        int argNum = inst.getNumOperands() - 1;
        int regIdx = 0;
        auto regCandidates = getCallerSavedRegCandidates(calledFuncName);
        auto savedRegs = regCandidates | ranges::views::filter([&](auto reg) {
            return shouldCallerSaveReg(stackFrame->getInstLoc(), inst, callerName, calledFuncName, reg, stackFrame);
        }) | ranges::to<std::vector<Register>>();
        for (auto reg : savedRegs) {
            auto regName = getRegisterName(reg);
            builder.addOp("mov", fmt::format("qword ptr [rsp + {}]", stackFrame->getCallerSavedRegLocByRsp(regIdx)),
                regName);
            regIdx++;
        }
        if (argNum > 6) {
            // TODO: handle more than 6 arguments
            return;
        }
        for (int i = 0; i < std::min(argNum, 6); i++) {
            auto targetReg = FUNCTION_PARAM_REGISTERS[i];
            auto& op = *inst.getArgOperand(i);
            auto name = op.getName().str();
            auto size = stackFrame->getDataLayout().getTypeAllocSize(op.getType());
            if (size == 4)
                targetReg = getLowerRegister(targetReg);
            string arg;
            if (op.getType()->isPointerTy() || op.getType()->isArrayTy()) {
                if (llvm::isa<llvm::GlobalVariable>(op) || !stackFrame->isOnReg(name)) {
                    arg = dumpOperandAddr(op, stackFrame, size);
                } else {
                    arg = fmt::format("[{}]", getRegisterName(stackFrame->getReg(name)));
                }
                builder.addOp("lea", targetReg, arg);
            } else {
                arg = dumpOperand(op, stackFrame, size);
                builder.addOp("mov", targetReg, arg);
            }
        }
        builder.addOp("xor", Register::RAX, Register::RAX);
        builder.addOp("call", fmt::format("{}@PLT", calledFuncName));
        regIdx = 0;
        for (auto reg : savedRegs) {
            auto regName = getRegisterName(reg);
            builder.addOp("mov", regName,
                fmt::format("qword ptr [rsp + {}]", stackFrame->getCallerSavedRegLocByRsp(regIdx)));
            regIdx++;
        }
    }

    void dumpCallInst(const llvm::CallInst& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        // print debug info
        auto callerName = stackFrame->getFuncName();
        auto calledOperand = inst.getCalledOperand();
        auto calledFuncName = calledOperand->getName().str();
        bool isExternal = inst.getCalledFunction() == nullptr || inst.getCalledFunction()->isDeclaration();
        if (isExternal) {
            dumpExternFuncCall(inst, stackFrame, builder);
            return;
        }
        int argNum = inst.getNumOperands() - 1;
        // fmt::print("CallInst: {}, isExternal: {}, argNum: {}\n", calledFuncName, isExternal, argNum);
        int regIdx = 0;
        auto candidateRegs = getCallerSavedRegCandidates(calledFuncName);
        auto savedRegs = candidateRegs | ranges::views::filter([&](auto reg) {
            return shouldCallerSaveReg(stackFrame->getInstLoc(), inst, callerName, calledFuncName, reg, stackFrame);
        }) | ranges::to<std::vector<Register>>();
        for (auto reg : savedRegs) {
            auto regName = getRegisterName(reg);
            builder.addOp("mov", fmt::format("qword ptr [rsp + {}]", stackFrame->getCallerSavedRegLocByRsp(regIdx)),
                regName);
            regIdx++;
        }
        if (argNum > 6) {
            // TODO: handle more than 6 arguments
            return;
        }
        for (int i = 0; i < std::min(argNum, 6); i++) {
            auto targetReg = FUNCTION_PARAM_REGISTERS[i];
            auto& op = *inst.getArgOperand(i);
            auto name = op.getName().str();
            auto size = stackFrame->getDataLayout().getTypeAllocSize(op.getType());
            if (size == 4)
                targetReg = getLowerRegister(targetReg);
            string arg;
            if (op.getType()->isPointerTy()) {
                if (stackFrame->isOnReg(name)) {
                    arg = getRegisterName(stackFrame->getReg(name));
                } else {
                    arg = dumpOperandAddr(op, stackFrame, size);
                }
            } else {
                arg = dumpOperand(op, stackFrame, size);
            }
            builder.addOp("mov", targetReg, arg);
        }
        builder.addOp("call", calledFuncName);
        if (inst.hasName()) {
            auto name = inst.getName().str();
            auto size = stackFrame->getDataLayout().getTypeAllocSize(inst.getType());
            auto isResOnRax = stackFrame->isOnReg(name) && stackFrame->getPhysicalReg(name) == Register::RAX;
            if (!isResOnRax) {
                auto resReg = size == 4 ? Register::EAX : Register::RAX;
                builder.addOp("mov", dumpOperand(inst, stackFrame), getRegisterName(resReg));
            }
        }
        regIdx = 0;
        for (auto reg : savedRegs) {
            auto regName = getRegisterName(reg);
            builder.addOp("mov", regName,
                fmt::format("qword ptr [rsp + {}]", stackFrame->getCallerSavedRegLocByRsp(regIdx)));
            regIdx++;
        }
    }

    void dumpReturnInst(const llvm::ReturnInst& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        if (inst.getNumOperands() == 0) {
            stackFrame->generateEpilouge(builder);
        } else {
            auto retOp = inst.getOperand(0);
            if (auto constInt = llvm::dyn_cast<llvm::ConstantInt>(retOp)) {
                builder.addOp("mov", Register::RAX, fmt::format("{}", constInt->getSExtValue()));
            } else {
                auto retOpName = retOp->getName().str();
                auto ret = dumpOperand(*retOp, stackFrame);
                auto size = stackFrame->getDataLayout().getTypeAllocSize(inst.getReturnValue()->getType());
                auto isValOnRax = stackFrame->isOnReg(retOpName) && stackFrame->getPhysicalReg(retOpName) == Register::RAX;
                if (!isValOnRax) {
                    auto reg = size == 4 ? Register::EAX : Register::RAX;
                    builder.addOp("mov", reg, ret);
                }
            }
            stackFrame->generateEpilouge(builder);
        }
    }

    std::string dumpOperandAddr(const llvm::Value& value, StackFramePtr stackFrame, size_t size, int offset = 0)
    {
        string baseAddr;
        if (auto instOperand = llvm::dyn_cast<llvm::Instruction>(&value)) {
            if (instOperand->hasName()) {
                auto name = instOperand->getName().str();
                if (!stackFrame->isOnReg(name)) {
                    auto loc = stackFrame->getLocByRsp(name);
                    if (loc == 0)
                        baseAddr = "rsp";
                    else
                        baseAddr = fmt::format("rsp + {}", loc);
                }
            }
        } else if (auto globalVar = llvm::dyn_cast<llvm::GlobalVariable>(&value)) {
            baseAddr = fmt::format("rip + {}", globalVar->getName().str());
        }
        if (offset == 0)
            return fmt::format("[{}]", baseAddr);
        return fmt::format("[{} + {}]", baseAddr, offset);
    }

    std::string dumpOperand(const llvm::Value& value, StackFramePtr stackFrame, int size = 0)
    {
        if (auto constInt = llvm::dyn_cast<llvm::ConstantInt>(&value)) {
            return fmt::format("{}", constInt->getSExtValue());
        } else if (auto instOperand = llvm::dyn_cast<llvm::Instruction>(&value)) {
            if (instOperand->hasName()) {
                auto name = instOperand->getName().str();
                if (stackFrame->isOnReg(name)) {
                    if (stackFrame->isPtr(name)) {
                        return fmt::format("{} [{}]", dumpPtrType(size), getRegisterName(stackFrame->getReg(name)));
                    }
                    return getRegisterName(stackFrame->getReg(name));
                } else {
                    if (size == 0)
                        size = stackFrame->getDataLayout().getTypeAllocSize(instOperand->getType());
                    return fmt::format("{} [rsp + {}]", dumpPtrType(size), stackFrame->getLocByRsp(name));
                }
            }
        } else if (auto argOperand = llvm::dyn_cast<llvm::Argument>(&value)) {
            auto name = argOperand->getName().str();
            if (stackFrame->isOnReg(name)) {
                return getRegisterName(stackFrame->getReg(name));
            } else {
                if (size == 0)
                    size = stackFrame->getDataLayout().getTypeAllocSize(argOperand->getType());
                return fmt::format("{} [rsp + {}]", dumpPtrType(size), stackFrame->getLocByRsp(name));
            }
        } else if (auto globalVar = llvm::dyn_cast<llvm::GlobalVariable>(&value)) {
            if (size == 0)
                size = stackFrame->getDataLayout().getTypeAllocSize(globalVar->getValueType());
            return fmt::format("{} [rip + {}]", dumpPtrType(size), globalVar->getName().str());
        }
        return "";
    }

    std::string dumpPtrType(int size)
    {
        if (size == 1) {
            return "byte ptr";
        } else if (size == 2) {
            return "word ptr";
        } else if (size == 4) {
            return "dword ptr";
        } else if (size == 8) {
            return "qword ptr";
        } else {
            return "qword ptr";
        }
    }

    void dumpLoadInst(const llvm::LoadInst& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        string lVal;
        auto lValName = inst.getName().str();
        if (stackFrame->isOnReg(lValName)) {
            lVal = getRegisterName(stackFrame->getReg(lValName));
        } else {
            // FIXME
            lVal = fmt::format("[rsp + {}]", stackFrame->getLocByRsp(lValName));
        }
        int size = stackFrame->getDataLayout().getTypeAllocSize(inst.getType());
        auto rValOp = inst.getPointerOperand();
        auto rValName = rValOp->getName().str();
        auto rVal = dumpOperand(*rValOp, stackFrame, size);
        // fmt::println("LoadInst: {} = {} <- {}", lValName, rVal, rValName);
        builder.addOp("mov", lVal, rVal);
    }

    void dumpStoreInst(const llvm::StoreInst& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        auto lValOp = inst.getPointerOperand();
        auto lValName = lValOp->getName().str();
        auto rValOp = inst.getValueOperand();
        auto rValSize = stackFrame->getDataLayout().getTypeAllocSize(rValOp->getType());
        auto lVal = dumpOperand(*lValOp, stackFrame, rValSize);
        auto rVal = dumpOperand(*rValOp, stackFrame, rValSize);
        if (rVal == "0"s && stackFrame->isOnReg(lValName) && !lValOp->getType()->isPointerTy())
            builder.addOp("xor", lVal, lVal);
        else
            builder.addOp("mov", lVal, rVal);
    }

    void dumpIDivInst(const llvm::BinaryOperator& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        auto lop = dumpOperand(*inst.getOperand(0), stackFrame);
        auto rop = dumpOperand(*inst.getOperand(1), stackFrame);
        builder.addOp("mov", Register::EAX, lop);
        if (auto imm = llvm::dyn_cast<llvm::ConstantInt>(inst.getOperand(1))) {
            auto immVal = imm->getSExtValue();
            if (immVal == 0) {
                fmt::print("Divide by zero\n");
                exit(1);
            }
            builder.addOp("mov", Register::ECX, std::to_string(immVal));
            builder.addOp("cdq");
            builder.addOp("idiv", Register::ECX);
            return;
        }
        builder.addOp("cdq");
        builder.addOp("idiv", rop);
    }

    void dumpBinaryOperator(const llvm::BinaryOperator& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        auto lValName = inst.getName().str();
        auto lop = dumpOperand(*inst.getOperand(0), stackFrame);
        auto rop = dumpOperand(*inst.getOperand(1), stackFrame);
        auto op = inst.getOpcode();
        if (op == llvm::Instruction::SDiv) {
            dumpIDivInst(inst, stackFrame, builder);
            if (stackFrame->isOnReg(lValName)) {
                auto resReg = stackFrame->getReg(lValName);
                builder.addOp("mov", getRegisterName(resReg), Register::EAX);
            } else {
                auto lVal = fmt::format("[rsp + {}]", stackFrame->getLocByRsp(lValName));
                builder.addOp("mov", lVal, Register::EAX);
            }
            return;
        } else if (op == llvm::Instruction::SRem) {
            dumpIDivInst(inst, stackFrame, builder);
            if (stackFrame->isOnReg(lValName)) {
                auto resReg = stackFrame->getReg(lValName);
                builder.addOp("mov", getRegisterName(resReg), Register::EDX);
            } else {
                auto lVal = fmt::format("[rsp + {}]", stackFrame->getLocByRsp(lValName));
                builder.addOp("mov", lVal, Register::EDX);
            }
            return;
        }
        switch (op) {
        case llvm::Instruction::Add:
            builder.addOp("add", lop, rop);
            break;
        case llvm::Instruction::Sub:
            builder.addOp("sub", lop, rop);
            break;
        case llvm::Instruction::Mul:
            builder.addOp("imul", lop, rop);
            break;
        default:
            break;
        }
        if (stackFrame->isOnReg(lValName)) {
            auto resReg = stackFrame->getReg(lValName);
            builder.addOp("mov", getRegisterName(resReg), lop);
        } else {
            auto lVal = fmt::format("[rsp + {}]", stackFrame->getLocByRsp(lValName));
            builder.addOp("mov", lVal, lop);
        }
    }

    void dumpICmpInst(const llvm::ICmpInst& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        auto lVal = dumpOperand(*inst.getOperand(0), stackFrame);
        auto rVal = dumpOperand(*inst.getOperand(1), stackFrame);
        auto op = inst.getPredicate();
        currentCmpOp = op;
        if (lVal == "0"s) {
            builder.addOp("test", rVal, rVal);
        } else {
            builder.addOp("cmp", lVal, rVal);
        }
    }

    void dumpBranchInst(const llvm::BranchInst& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        if (inst.isConditional()) {
            auto trueLabel = inst.getSuccessor(0)->getName().str();
            auto falseLabel = inst.getSuccessor(1)->getName().str();
            switch (currentCmpOp) {
            case llvm::CmpInst::ICMP_EQ:
                builder.addOp("je", trueLabel);
                builder.addOp("jmp", falseLabel);
                break;
            case llvm::CmpInst::ICMP_NE:
                builder.addOp("jne", trueLabel);
                builder.addOp("jmp", falseLabel);
                break;
            case llvm::CmpInst::ICMP_SGT:
                builder.addOp("jg", trueLabel);
                builder.addOp("jmp", falseLabel);
                break;
            case llvm::CmpInst::ICMP_SGE:
                builder.addOp("jge", trueLabel);
                builder.addOp("jmp", falseLabel);
                break;
            case llvm::CmpInst::ICMP_SLT:
                builder.addOp("jl", trueLabel);
                builder.addOp("jmp", falseLabel);
                break;
            case llvm::CmpInst::ICMP_SLE:
                builder.addOp("jle", trueLabel);
                builder.addOp("jmp", falseLabel);
                break;
            default:
                break;
            }
        } else {
            auto label = inst.getSuccessor(0)->getName().str();
            builder.addOp("jmp", label);
        }
    }

    bool isConstOperands(const llvm::Instruction& inst, int start)
    {
        for (int i = start; i < inst.getNumOperands(); i++) {
            if (!llvm::dyn_cast<llvm::ConstantInt>(inst.getOperand(i)))
                return false;
        }
        return true;
    }

    string dumpReg(const llvm::Value& op, StackFramePtr stackFrame)
    {
        if (auto globalVar = llvm::dyn_cast<llvm::GlobalVariable>(&op)) {
            return fmt::format("rip + {}", globalVar->getName().str());
        } else {
            if (stackFrame->isOnReg(op.getName().str()))
                return getRegisterName(stackFrame->getReg(op.getName()));
            else
                return "rsp"s;
        }
    }

    auto getElementBaseType(const llvm::GetElementPtrInst& inst) -> llvm::Type*
    {
        auto type = inst.getSourceElementType();
        while (type->isArrayTy()) {
            type = type->getArrayElementType();
        }
        return type;
    }

    void dumpGetElementPtrInst(const llvm::GetElementPtrInst& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        auto lValName = inst.getName().str();
        auto lValReg = stackFrame->getReg(lValName);
        auto lValRegName = getRegisterName(lValReg);
        auto baseType = getElementBaseType(inst);
        auto srcType = inst.getSourceElementType();
        int baseTypeSize = stackFrame->getDataLayout().getTypeAllocSize(baseType);
        int srcArraySize = srcType->isArrayTy() ? srcType->getArrayNumElements() : 1;
        int opNum = inst.getNumOperands();
        auto& ptrOp = *inst.getPointerOperand();
        string base = dumpReg(ptrOp, stackFrame);
        int baseOffset = 0;
        if (!llvm::isa<llvm::GlobalVariable>(ptrOp) && !stackFrame->isOnReg(ptrOp.getName().str())) {
            baseOffset = stackFrame->getLocByRsp(ptrOp.getName());
        }
        if (isConstOperands(inst, 1)) {
            int offset = 0;
            for (int i = 1; i < opNum; i++) {
                auto constInt = llvm::dyn_cast<llvm::ConstantInt>(inst.getOperand(i));
                offset += constInt->getSExtValue() * srcArraySize;
                if (i < opNum - 1) {
                    srcType = srcType->getArrayElementType();
                    srcArraySize = srcType->isArrayTy() ? srcType->getArrayNumElements() : 1;
                }
            }
            baseOffset += offset * baseTypeSize;
            if (baseOffset != 0) {
                base = fmt::format("{} + {}", base, baseOffset);
            }
            if (base != lValRegName)
                builder.addOp("lea", lValRegName, fmt::format("[{}]", base));
            return;
        }
        if (baseOffset != 0) {
            base = fmt::format("{} + {}", base, baseOffset);
        }
        if (base != lValRegName)
            builder.addOp("lea", lValRegName, fmt::format("[{}]", base));
        int idxNum = inst.getNumIndices();

        for (int i = 0; i < idxNum; i++) {
            auto opIdx = i + 1;
            if (isConstOperands(inst, opIdx)) {
                int offset = 0;
                for (int j = opIdx; j < opNum; j++) {
                    auto constInt = llvm::dyn_cast<llvm::ConstantInt>(inst.getOperand(j));
                    offset += constInt->getSExtValue() * srcArraySize;
                    if (j < opNum - 1) {
                        srcType = srcType->getArrayElementType();
                        srcArraySize = srcType->isArrayTy() ? srcType->getArrayNumElements() : 1;
                    }
                }
                if (offset * baseTypeSize != 0)
                    builder.addOp("add", lValRegName, fmt::format("{}", offset * baseTypeSize));
                break;
            }
            auto op = inst.getOperand(opIdx);
            auto idx = dumpOperand(*op, stackFrame);
            builder.addOp("mov", Register::RCX, idx);
            if (idx != "0"s)
                builder.addOp("imul", Register::RCX, Register::RCX, fmt::format("{}", srcArraySize * baseTypeSize));
            builder.addOp("add", lValRegName, Register::RCX);
            if (i < idxNum - 1) {
                srcType = srcType->getArrayElementType();
                srcArraySize = srcType->isArrayTy() ? srcType->getArrayNumElements() : 1;
            }
        }
    }

    void dumpSExtInst(const llvm::SExtInst& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        auto lVal = inst.getName().str();
        auto rVal = dumpOperand(*inst.getOperand(0), stackFrame);
        auto size = stackFrame->getDataLayout().getTypeAllocSize(inst.getType());
        auto lValReg = stackFrame->getReg(lVal);
        builder.addOp("movsxd", getRegisterName(lValReg), rVal);
    }

    void dumpZExtInst(const llvm::ZExtInst& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        auto lVal = inst.getName().str();
        auto rVal = dumpOperand(*inst.getOperand(0), stackFrame);
        auto size = stackFrame->getDataLayout().getTypeAllocSize(inst.getType());
        auto lValReg = stackFrame->getReg(lVal);
        builder.addOp("movzxd", getRegisterName(lValReg), rVal);
    }

    void dumpInstruction(const llvm::Instruction& inst, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        if (inst.hasName()) {
            auto name = inst.getName().str();
            auto size = stackFrame->getDataLayout().getTypeAllocSize(inst.getType());
            if (auto allocInst = llvm::dyn_cast<llvm::AllocaInst>(&inst)) {
                size = stackFrame->getDataLayout().getTypeAllocSize(allocInst->getAllocatedType());
            }
            bool isPtr = false;
            if (llvm::isa<llvm::GetElementPtrInst>(inst)) {
                isPtr = true;
            }
            stackFrame->allocate(name, size, isPtr);
        }
        if (auto callInst = llvm::dyn_cast<llvm::CallInst>(&inst)) {
            dumpCallInst(*callInst, stackFrame, builder);
        } else if (auto loadInst = llvm::dyn_cast<llvm::LoadInst>(&inst)) {
            dumpLoadInst(*loadInst, stackFrame, builder);
        } else if (auto storeInst = llvm::dyn_cast<llvm::StoreInst>(&inst)) {
            dumpStoreInst(*storeInst, stackFrame, builder);
        } else if (auto binOp = llvm::dyn_cast<llvm::BinaryOperator>(&inst)) {
            dumpBinaryOperator(*binOp, stackFrame, builder);
        } else if (auto icmpInst = llvm::dyn_cast<llvm::ICmpInst>(&inst)) {
            dumpICmpInst(*icmpInst, stackFrame, builder);
        } else if (auto retInst = llvm::dyn_cast<llvm::ReturnInst>(&inst)) {
            dumpReturnInst(*retInst, stackFrame, builder);
        } else if (auto branchInst = llvm::dyn_cast<llvm::BranchInst>(&inst)) {
            dumpBranchInst(*branchInst, stackFrame, builder);
        } else if (auto gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(&inst)) {
            dumpGetElementPtrInst(*gepInst, stackFrame, builder);
        } else if (auto sextInst = llvm::dyn_cast<llvm::SExtInst>(&inst)) {
            dumpSExtInst(*sextInst, stackFrame, builder);
        } else if (auto zextInst = llvm::dyn_cast<llvm::ZExtInst>(&inst)) {
            dumpZExtInst(*zextInst, stackFrame, builder);
        }
    }

    void dumpBasicBlock(const llvm::BasicBlock& bb, StackFramePtr stackFrame, AsmBuilder& builder)
    {
        if (bb.hasName())
            builder.addLabel(bb.getName());
        for (auto& inst : bb) {
            dumpInstruction(inst, stackFrame, builder);
            stackFrame->step();
        }
    }

    void dumpFunction(const llvm::Function& func)
    {
        auto stackFrame = stackFrames[func.getName().str()];
        AsmBuilder funcBuilder;
        for (auto& args : func.args()) {
            auto name = args.getName().str();
            stackFrame->allocate(name, stackFrame->getDataLayout().getTypeAllocSize(args.getType()));
        }
        for (auto& bb : func) {
            dumpBasicBlock(bb, stackFrame, funcBuilder);
        }
        builder.addOp(".globl", func.getName());
        builder.addOp(".type", func.getName(), "@function");
        builder.addLabel(func.getName());
        stackFrame->generateProlouge(builder);
        builder.append(funcBuilder);
        // stackFrame->generateEpilouge(builder);
    }

    string escapeString(string_view str)
    {
        string escaped;
        for (auto c : str) {
            if (c == '\n') {
                escaped += "\\n";
            } else if (c == '\t') {
                escaped += "\\t";
            } else {
                escaped += c;
            }
        }
        return escaped;
    }

    void dumpGlobalInitializer(const llvm::Type& type, const llvm::Constant& constant)
    {
        if (type.isArrayTy()) {
            auto elementType = type.getArrayElementType();
            // 如果是字符串，直接输出
            if (elementType->isIntegerTy(8)) {
                auto constArray = llvm::dyn_cast<llvm::ConstantDataArray>(&constant);
                auto str = constArray->getAsCString();
                builder.addOp(".asciz", fmt::format("\"{}\"", escapeString(str)));
            } else {
                auto numElements = type.getArrayNumElements();
                for (int i = 0; i < numElements; i++) {
                    auto element = constant.getAggregateElement(i);
                    dumpGlobalInitializer(*elementType, *element);
                }
            }
        } else if (type.isIntegerTy()) {
            auto constInt = llvm::dyn_cast<llvm::ConstantInt>(&constant);
            builder.addOp(".long", fmt::format("{}", constInt->getSExtValue()));
        }
    }

    void dumpGlobalVariable(const llvm::GlobalVariable& globalVar)
    {
        auto name = globalVar.getName().str();
        auto type = globalVar.getValueType();
        builder.addOp(".globl", name);
        builder.addLabel(name);
        if (globalVar.hasInitializer())
            dumpGlobalInitializer(*type, *globalVar.getInitializer());
    }

    bool shouldCallerSaveReg(int loc, const llvm::CallInst& callInst, string_view callerName, string_view calleeName,
        Register reg, StackFramePtr stackFrame)
    {
        auto bb = callInst.getParent();
        auto cfgNode = stackFrame->getCFGNode(bb);
        auto liveInVar = (cfgNode->isEntryBlock() ? cfgNode->defSet : cfgNode->inSet) | ranges::views::filter([&](auto& var) { return stackFrame->isOnReg(var) && isOnSameRegister(stackFrame->getReg(var), reg); }) | ranges::to<std::set<string>>();
        auto liveOutVar = cfgNode->outSet | ranges::views::filter([&](auto& var) { return stackFrame->isOnReg(var) && isOnSameRegister(stackFrame->getReg(var), reg); }) | ranges::to<std::set<string>>();
        auto shouldSave = !intersectSet(liveInVar, liveOutVar).empty();
        if (!shouldSave) {
            // 处理 in block 的情况
            auto lValName = callInst.getName().str();
            auto lastVarInBlock = stackFrame->getLastVarOnRegInBlockBefore(cfgNode, reg, loc);
            if (lastVarInBlock.has_value()) {
                auto lastVar = lastVarInBlock.value();
                if (lastVar != lValName) {
                    auto relativeLoc = loc - cfgNode->start;
                    // fmt::println("Check lastVar: {} reg: {} caller: {} callee: {} relativeLoc: {}", lastVar, getRegisterName(reg), callerName, calleeName, relativeLoc);
                    auto dfgNodeInBlock = cfgNode->dfgInBlock.getNodeByName(lastVar);
                    auto deps = dfgNodeInBlock->getDependentNodes();
                    shouldSave = std::any_of(deps.begin(), deps.end(), [&](auto& dep) {
                        return dep->defLoc > relativeLoc;
                    });
                }
            }
        }
        return shouldSave;
    }

    void analyzeCallerSaveRegs(const llvm::Function& func, StackFramePtr stackFrame)
    {
        auto callerName = func.getName().str();
        int i = 0;
        for (auto& bb : func)
            for (auto& inst : bb) {
                if (auto callInst = llvm::dyn_cast<llvm::CallInst>(&inst)) {
                    auto calledFunc = callInst->getCalledOperand();
                    if (calledFunc) {
                        auto calleeName = calledFunc->getName().str();
                        auto callerSaveRegNum = 0;
                        auto regCandidates = getCallerSavedRegCandidates(calleeName);
                        for (auto& reg : regCandidates)
                            if (shouldCallerSaveReg(i, *callInst, callerName, calleeName, reg, stackFrame))
                                callerSaveRegNum++;
                        stackFrame->setCallerSavedRegSize(callerSaveRegNum * 8);
                    }
                }
                i++;
            }
    }

public:
    AsmGenerator()
    {
    }

    void generate(std::unique_ptr<llvm::Module>& module)
    {
        visit(module);
        builder.addOp(".intel_syntax", "noprefix");
        for (auto& func : *module) {
            if (func.isDeclaration())
                continue;
            dumpFunction(func);
        }
        builder.addOp(".section", ".data");
        for (auto& globalVar : module->globals()) {
            if (globalVar.isConstant())
                continue;
            dumpGlobalVariable(globalVar);
        }
        builder.addOp(".section", ".rodata");
        for (auto& globalVar : module->globals()) {
            if (!globalVar.isConstant())
                continue;
            dumpGlobalVariable(globalVar);
        }
    }

    std::string getAsm() const
    {
        return builder.getAsm();
    }
};
} // namespace sysy