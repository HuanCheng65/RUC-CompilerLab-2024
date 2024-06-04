#pragma once
#include "backend/instructions.hpp"
#include "backend/registers.hpp"
#include "backend/stack_frame.hpp"
#include <algorithm>
#include <backend/utils.hpp>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
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
#include <ranges>
#include <set>
#include <tuple>
#include <unordered_map>

using std::set;

namespace sysy {
class InstructionGenerator {
    using CallerSaveInsertionPoint = std::tuple<AsmInstructionPtr, shared_ptr<CallInstruction>,
        AsmInstructionPtr, set<Register>>;

    std::unordered_map<string, OperandPtr> operandMap;
    std::unordered_map<string, BasicBlockPtr> bbMap;
    AsmNameManager nameManager;
    const llvm::DataLayout* dataLayout;
    StackFramePtr currentFrame;
    std::unordered_map<AsmFunctionPtr, std::vector<CallerSaveInsertionPoint>>
        callerSaveInsertionPoints;

    TypeInfoPtr generateTypeInfo(llvm::Type* type)
    {
        if (type->isArrayTy()) {
            auto elemType = type->getArrayElementType();
            auto elemTypeInfo = generateTypeInfo(elemType);
            auto arraySize = type->getArrayNumElements();
            return elemTypeInfo->createArray(arraySize);
        } else if (type->isIntegerTy()) {
            auto size = dataLayout->getTypeAllocSize(type);
            return TypeInfo::createPrimitive(size);
        } else if (type->isPointerTy()) {
            return TypeInfo::createPointer(nullptr);
        }
        std::println("WARNING: unknown type");
        return nullptr;
    }

    bool canBeOnRegister(const llvm::Value* val)
    {
        if (auto callInst = llvm::dyn_cast<llvm::CallInst>(val)) {
            return false;
        }
        auto allocaInst = llvm::dyn_cast<llvm::AllocaInst>(val);
        return !allocaInst
            || (!allocaInst->getAllocatedType()->isArrayTy()
                && std::none_of(allocaInst->users().begin(), allocaInst->users().end(),
                    [&](auto user) { return llvm::isa<llvm::CallInst>(*user); }));
    }

    OperandPtr generatePrecoloredOperand(string_view name, Register reg, TypeInfoPtr type)
    {
        if (operandMap.contains(name.data())) {
            return operandMap[name.data()];
        }
        auto operand = std::make_shared<PhysicalRegister>(name, reg, type);
        operandMap[name.data()] = operand;
        return operand;
    }

    OperandPtr generatePrecoloredOperand(string_view name, Register reg, int size)
    {
        return generatePrecoloredOperand(name, reg, TypeInfo::createPrimitive(size));
    }

    TypeInfoPtr generateTypeInfoForGlobal(const llvm::Type* type)
    {
        if (type->isArrayTy()) {
            auto elementType = type->getArrayElementType();
            TypeInfoPtr elementTypeInfo = generateTypeInfoForGlobal(elementType);
            return elementTypeInfo->createArray(type->getArrayNumElements());
        } else if (type->isIntegerTy()) {
            return TypeInfo::createPrimitive(type->getIntegerBitWidth() / 8);
        }
        std::println("WARNING: unknown global type");
        return nullptr;
    }

    OperandPtr generateOperand(const llvm::Value* val)
    {
        if (auto inst = llvm::dyn_cast<llvm::Instruction>(val); inst && inst->hasName()) {
            auto name = inst->getName().str();
            auto type = inst->getType();
            auto typeInfo = generateTypeInfo(type);
            if (auto allocainst = llvm::dyn_cast<llvm::AllocaInst>(inst)) {
                type = allocainst->getAllocatedType();
                typeInfo = generateTypeInfo(type);
                typeInfo->setAllocatedType();
            } else if (auto callInst = llvm::dyn_cast<llvm::CallInst>(inst)) {
                type = callInst->getType();
                typeInfo = generateTypeInfo(type);
            }
            if (auto gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(inst)) {
                typeInfo
                    = TypeInfo::createPointer(generateTypeInfo(gepInst->getResultElementType()));
            }
            auto canBeOnReg = canBeOnRegister(inst);
            if (canBeOnReg) {
                if (typeInfo->isArrayType())
                    std::println("WARNING: array type on register");
                return std::make_shared<VirtualRegister>(name, typeInfo);
            } else {
                return std::make_shared<StackLocation>(
                    name, typeInfo, currentFrame->allocateStackVar(typeInfo->getAllocSize()));
            }
        } else if (auto arg = llvm::dyn_cast<llvm::Argument>(val); arg && arg->hasName()) {
            auto name = arg->getName().str();
            if (arg->getArgNo() < 6) {
                return generatePrecoloredOperand(name, FUNCTION_PARAM_REGISTERS[arg->getArgNo()],
                    generateTypeInfo(arg->getType()));
            } else {
                auto offset = 8 * (arg->getArgNo() - 6);
                return std::make_shared<StackLocation>(name, generateTypeInfo(arg->getType()),
                    -offset, StackLocationBase::RBP_RELATIVE);
            }
        } else if (auto global = llvm::dyn_cast<llvm::GlobalVariable>(val);
                   global && global->hasName()) {
            auto name = global->getName().str();
            return std::make_shared<GlobalVariable>(name,
                generateTypeInfoForGlobal(global->getValueType()), global->getInitializer(),
                global->isConstant());
        } else if (auto constant = llvm::dyn_cast<llvm::ConstantInt>(val)) {
            auto value = constant->getSExtValue();
            return std::make_shared<Immediate>(value, generateTypeInfo(constant->getType()));
        }
        std::println("WARNING: unknown value type");
        return nullptr;
    }

    OperandPtr getOperand(const llvm::Value* val)
    {
        if (val->hasName()) {
            auto name = val->getName().str();
            if (operandMap.contains(name)) {
                return operandMap[name];
            } else {
                auto operand = generateOperand(val);
                operandMap[name] = operand;
                return operand;
            }
        } else {
            return generateOperand(val);
        }
    }

    void defOperand(OperandPtr operand, AsmInstructionPtr inst)
    {
        if (operand->isImmediate()) {
            return;
        }
        if (auto memOp = std::dynamic_pointer_cast<Memory>(operand)) {
            auto base = memOp->getBase();
            auto index = memOp->getIndex();
            if (base) {
                if (auto regBase = std::dynamic_pointer_cast<PhysicalRegister>(base);
                    !regBase || regBase->getRegister() != Register::RBP) {
                    useOperand(base, inst);
                }
            }
            if (index) {
                useOperand(index, inst);
            }
        }
        auto def = std::make_shared<OperandDef>(inst, operand);
        operand->addDef(def);
        inst->addDef(def);
    }

    void useOperand(OperandPtr operand, AsmInstructionPtr inst)
    {
        if (operand->isImmediate()) {
            return;
        }
        if (auto memOp = std::dynamic_pointer_cast<Memory>(operand)) {
            auto base = memOp->getBase();
            auto index = memOp->getIndex();
            if (base) {
                if (auto regBase = std::dynamic_pointer_cast<PhysicalRegister>(base);
                    !regBase || regBase->getRegister() != Register::RBP) {
                    useOperand(base, inst);
                }
            }
            if (index) {
                useOperand(index, inst);
            }
        }
        auto use = std::make_shared<OperandUse>(inst, operand);
        operand->addUse(use);
        inst->addUse(use);
    }

    void getOperand(string_view name, OperandPtr defaultVal) { }

    void generateCallInst(BasicBlockPtr bb, const llvm::CallInst& inst)
    {
        auto func = inst.getCalledOperand();
        if (func && func->hasName()) {
            auto hasRetVal = !inst.getType()->isVoidTy();
            auto funcName = func->getName().str();
            auto rax = generatePrecoloredOperand(
                nameManager.genUniqueName("call.ret"), Register::RAX, TypeInfo::createPrimitive(8));
            bool isExternal
                = inst.getCalledFunction() == nullptr || inst.getCalledFunction()->isDeclaration();
            vector<OperandPtr> args;
            int argNum = inst.getNumOperands() - 1;
            int regArgNum = FUNCTION_PARAM_REGISTERS.size();
            int stackArgNum = std::max(0, argNum - regArgNum);
            int stackArgSize = 8 * stackArgNum;
            int externStackSpace = 0;
            auto callerSaveCandidates = CALLER_SAVED_REGISTERS | std::ranges::to<set>();
            if (isExternal) {
                if (funcName == "printf") {
                    callerSaveCandidates.insert(Register::RBX);
                }
            }
            if (stackArgSize % 16) {
                externStackSpace += 16 - (externStackSpace % 16);
            }
            // for (auto saveReg : callerSaveCandidates) {
            //     auto saveOpName = std::format("caller.save.{}", getRegisterName(saveReg));
            //     auto reg
            //         = generatePrecoloredOperand(saveOpName, saveReg,
            //         TypeInfo::createPrimitive(8));
            //     auto pushInst = std::make_shared<PushInstruction>(reg);
            //     // useOperand(reg, pushInst);
            //     bb->appendInstruction(pushInst);
            // }
            auto saveInsertionPoint = std::make_shared<FakeInstruction>();
            bb->appendInstruction(saveInsertionPoint);
            if (externStackSpace > 0) {
                auto subInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::SUB,
                    generatePrecoloredOperand("rsp", Register::RSP, 8),
                    std::make_shared<Immediate>(externStackSpace, TypeInfo::createPrimitive(8)));
                bb->appendInstruction(subInst);
            }
            vector<const llvm::Value*> instArgs;
            for (auto arg = inst.arg_begin(); arg != inst.arg_end(); ++arg) {
                instArgs.push_back(*arg);
            }
            for (int i = 0; i < std::min(argNum, regArgNum); i++) {
                auto arg = instArgs[i];
                auto argOp = getOperand(arg);
                auto name = nameManager.genUniqueName(std::format("call.arg.{}", i));
                OperandPtr passOp = generatePrecoloredOperand(
                    name, FUNCTION_PARAM_REGISTERS[i], generateTypeInfo(arg->getType()));
                if (argOp->getType()->isArrayType() || argOp->getType()->isAllocatedType()) {
                    args.push_back(passOp);
                    auto leaInst = std::make_shared<LeaInstruction>(passOp, argOp);
                    useOperand(argOp, leaInst);
                    defOperand(passOp, leaInst);
                    bb->appendInstruction(leaInst);
                } else {
                    args.push_back(passOp);
                    appendMovInstruction(bb, passOp, argOp);
                }
            }
            if (stackArgNum > 0) {
                auto stackOps = instArgs | std::views::drop(regArgNum) | std::views::reverse;
                std::for_each(stackOps.begin(), stackOps.end(), [&](const auto& arg) {
                    auto argOp = getOperand(arg);
                    std::println("pushing arg {}", argOp->getName());
                    if (argOp->getType()->isArrayType() || argOp->getType()->isAllocatedType()) {
                        auto temp = std::make_shared<VirtualRegister>(
                            nameManager.genUniqueName("stack.lea"),
                            TypeInfo::createPointer(nullptr));
                        auto leaInst = std::make_shared<LeaInstruction>(temp, argOp);
                        useOperand(argOp, leaInst);
                        defOperand(temp, leaInst);
                        bb->appendInstruction(leaInst);
                        auto pushInst = std::make_shared<PushInstruction>(temp);
                        useOperand(temp, pushInst);
                        bb->appendInstruction(pushInst);
                    } else {
                        if (argOp->getType()->isIntegerType() && argOp->getSize() < 8) {
                            auto temp = std::make_shared<VirtualRegister>(
                                nameManager.genUniqueName("stack.mov"),
                                TypeInfo::createPrimitive(8));
                            appendMovInstruction(bb, temp, argOp, MovType::MOVSX);
                            auto pushInst = std::make_shared<PushInstruction>(temp);
                            useOperand(temp, pushInst);
                            bb->appendInstruction(pushInst);
                        } else {
                            auto pushInst = std::make_shared<PushInstruction>(argOp);
                            useOperand(argOp, pushInst);
                            bb->appendInstruction(pushInst);
                        }
                    };
                });
            }
            if (isExternal) {
                std::println("calling c stdlib {}, clear rax; hasRetVal: {}", funcName, hasRetVal);
                auto xorInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::XOR, rax, rax);
                // useOperand(rax, xorInst);
                defOperand(rax, xorInst);
                bb->appendInstruction(xorInst);
            }
            auto callInst = std::make_shared<CallInstruction>(funcName);
            std::for_each(args.begin(), args.end(), [&](auto arg) { useOperand(arg, callInst); });
            bb->appendInstruction(callInst);
            if ((stackArgSize + externStackSpace) > 0) {
                auto addInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::ADD,
                    generatePrecoloredOperand("rsp", Register::RSP, 8),
                    std::make_shared<Immediate>(
                        stackArgSize + externStackSpace, TypeInfo::createPrimitive(8)));
                bb->appendInstruction(addInst);
            }
            if (hasRetVal) {
                defOperand(rax, callInst);
                if (inst.hasName()) {
                    auto lVal = getOperand(&inst);
                    appendMovInstruction(bb, lVal, rax);
                    callInst->isRetValUsed = true;
                }
            }
            auto restoreInsertionPoint = std::make_shared<FakeInstruction>();
            bb->appendInstruction(restoreInsertionPoint);
            callerSaveInsertionPoints[bb->parent.lock()].push_back(std::make_tuple(
                saveInsertionPoint, callInst, restoreInsertionPoint, callerSaveCandidates));
        }
    }

    void generateLoadInst(BasicBlockPtr bb, const llvm::LoadInst& loadInst)
    {
        auto ptr = loadInst.getPointerOperand();
        auto src = getOperand(ptr);
        auto dest = getOperand(&loadInst);
        if (src->getType()->isPointerType() && !dest->getType()->isPointerType()) {
            auto name = nameManager.genUniqueName(std::format("load.{}", dest->getName()));
            src = std::make_shared<Memory>(name, src,
                src->getType()->getElementType() ? src->getType()->getElementType()->clone()
                                                 : TypeInfo::createPointer(nullptr));
            operandMap[name] = src;
        }
        appendMovInstruction(bb, dest, src);
    }

    void generateStoreInst(BasicBlockPtr bb, const llvm::StoreInst& storeInst)
    {
        auto ptr = storeInst.getPointerOperand();
        auto src = storeInst.getValueOperand();
        auto dest = getOperand(ptr);
        auto value = getOperand(src);
        if (dest->getType()->isPointerType() && !value->getType()->isPointerType()) {
            auto name = nameManager.genUniqueName(std::format("store.{}", dest->getName()));
            dest = std::make_shared<Memory>(name, dest, dest->getType()->getElementType()->clone());
            operandMap[name] = dest;
        }
        appendMovInstruction(bb, dest, value);
    }

    bool isOperandOnMemory(OperandPtr operand)
    {
        return operand->getOperandType() == OperandType::MEMORY
            || operand->getOperandType() == OperandType::STACK_VARIABLE
            || operand->getOperandType() == OperandType::GLOBAL_VARIABLE;
    }

    MovType getMovType(OperandPtr dest, OperandPtr src) const
    {
        if (!src->isImmediate() && dest->getSize() > src->getSize()) {
            return MovType::MOVSX;
        } else {
            return MovType::MOV;
        }
    }

    AsmInstructionPtr appendMovInstruction(
        BasicBlockPtr bb, OperandPtr dest, OperandPtr src, MovType movType)
    {
        if (isOperandOnMemory(dest) && isOperandOnMemory(src)) {
            auto temp = std::make_shared<VirtualRegister>(
                nameManager.genUniqueName("bridge"), dest->getType());
            auto inst1 = std::make_shared<MovInstruction>(movType, temp, src);
            useOperand(src, inst1);
            defOperand(temp, inst1);
            auto inst2 = std::make_shared<MovInstruction>(dest, temp);
            useOperand(temp, inst2);
            defOperand(dest, inst2);
            bb->appendInstruction(inst1);
            bb->appendInstruction(inst2);
            return inst2;
        } else {
            auto inst = std::make_shared<MovInstruction>(movType, dest, src);
            useOperand(src, inst);
            defOperand(dest, inst);
            bb->appendInstruction(inst);
            return inst;
        }
    }

    AsmInstructionPtr appendMovInstruction(BasicBlockPtr bb, OperandPtr dest, OperandPtr src)
    {
        return appendMovInstruction(bb, dest, src, getMovType(dest, src));
    }

    void generateIDivInst(BasicBlockPtr bb, OperandPtr lhs, OperandPtr rhs, OperandPtr dest,
        const llvm::BinaryOperator& inst)
    {
        auto rax = generatePrecoloredOperand(
            nameManager.genUniqueName("div.src"), Register::RAX, lhs->getType());
        auto rdx = generatePrecoloredOperand(
            nameManager.genUniqueName("div.mod"), Register::RDX, dest->getType());
        appendMovInstruction(bb, rax, lhs);
        auto cdqInst = std::make_shared<CdqInstruction>();
        useOperand(rax, cdqInst);
        defOperand(rdx, cdqInst);
        bb->appendInstruction(cdqInst);
        if (rhs->isImmediate()) {
            auto divByOp = std::make_shared<VirtualRegister>(
                nameManager.genUniqueName("div.by"), rhs->getType());
            appendMovInstruction(bb, divByOp, rhs);
            auto idivInst = std::make_shared<IDivInstruction>(divByOp);
            useOperand(divByOp, idivInst);
            useOperand(rax, idivInst);
            useOperand(rdx, idivInst);
            bb->appendInstruction(idivInst);
        } else {
            auto idivInst = std::make_shared<IDivInstruction>(rhs);
            useOperand(rhs, idivInst);
            useOperand(rax, idivInst);
            useOperand(rdx, idivInst);
            bb->appendInstruction(idivInst);
        }
        appendMovInstruction(bb, dest, rax);
    }

    void generateModInst(BasicBlockPtr bb, OperandPtr lhs, OperandPtr rhs, OperandPtr dest,
        const llvm::BinaryOperator& inst)
    {
        auto rax = generatePrecoloredOperand(
            nameManager.genUniqueName("div.src"), Register::RAX, lhs->getType());
        auto rdx = generatePrecoloredOperand(
            nameManager.genUniqueName("div.mod"), Register::RDX, dest->getType());
        appendMovInstruction(bb, rax, lhs);
        auto cdqInst = std::make_shared<CdqInstruction>();
        useOperand(rax, cdqInst);
        defOperand(rdx, cdqInst);
        bb->appendInstruction(cdqInst);
        if (rhs->isImmediate()) {
            auto divByOp = std::make_shared<VirtualRegister>(
                nameManager.genUniqueName("div.by"), rhs->getType());
            appendMovInstruction(bb, divByOp, rhs);
            auto idivInst = std::make_shared<IDivInstruction>(divByOp);
            useOperand(divByOp, idivInst);
            useOperand(rax, idivInst);
            useOperand(rdx, idivInst);
            bb->appendInstruction(idivInst);
        } else {
            auto idivInst = std::make_shared<IDivInstruction>(rhs);
            useOperand(rhs, idivInst);
            useOperand(rax, idivInst);
            useOperand(rdx, idivInst);
            bb->appendInstruction(idivInst);
        }
        appendMovInstruction(bb, dest, rdx);
    }

    bool isImmediate(OperandPtr operand)
    {
        return operand->getOperandType() == OperandType::IMMEDIATE;
    }

    void generateBinaryOperator(BasicBlockPtr bb, const llvm::BinaryOperator& inst)
    {
        auto lhs = inst.getOperand(0);
        auto rhs = inst.getOperand(1);
        auto dest = getOperand(&inst);
        auto src1 = getOperand(lhs);
        auto src2 = getOperand(rhs);
        switch (inst.getOpcode()) {
        case llvm::Instruction::Add: {
            appendMovInstruction(bb, dest, src1);
            auto addInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::ADD, dest, src2);
            useOperand(src2, addInst);
            useOperand(dest, addInst);
            defOperand(dest, addInst);
            bb->appendInstruction(addInst);
            break;
        }
        case llvm::Instruction::Sub: {
            appendMovInstruction(bb, dest, src1);
            auto subInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::SUB, dest, src2);
            useOperand(src2, subInst);
            useOperand(dest, subInst);
            defOperand(dest, subInst);
            bb->appendInstruction(subInst);
            break;
        }
        case llvm::Instruction::Mul:
            if (src1->isImmediate()) {
                auto mulInst = std::make_shared<IMulInstruction>(dest, src2, src1);
                useOperand(src2, mulInst);
                defOperand(dest, mulInst);
                bb->appendInstruction(mulInst);
            } else if (src2->isImmediate()) {
                auto mulInst = std::make_shared<IMulInstruction>(dest, src1, src2);
                useOperand(src1, mulInst);
                defOperand(dest, mulInst);
                bb->appendInstruction(mulInst);
            } else if (src1->isMemory() && src2->isMemory() && dest->isMemory()) {
                auto temp = std::make_shared<VirtualRegister>(
                    nameManager.genUniqueName("mul.dest"), dest->getType());
                appendMovInstruction(bb, temp, src1);
                auto mulInst = std::make_shared<IMulInstruction>(temp, src2);
                useOperand(src2, mulInst);
                defOperand(temp, mulInst);
                appendMovInstruction(bb, dest, temp);
            } else if (src1->isMemory() && src2->isMemory()) {
                appendMovInstruction(bb, dest, src1);
                auto mulInst = std::make_shared<IMulInstruction>(dest, src2);
                useOperand(src2, mulInst);
                defOperand(dest, mulInst);
                bb->appendInstruction(mulInst);
            } else if (dest->isMemory()) {
                auto temp = std::make_shared<VirtualRegister>(
                    nameManager.genUniqueName("mul.dest"), dest->getType());
                appendMovInstruction(bb, temp, src1);
                auto mulInst = std::make_shared<IMulInstruction>(src2);
                useOperand(src2, mulInst);
                defOperand(temp, mulInst);
                bb->appendInstruction(mulInst);
                appendMovInstruction(bb, dest, temp);
            } else {
                appendMovInstruction(bb, dest, src1);
                auto mulInst = std::make_shared<IMulInstruction>(dest, src2);
                useOperand(src1, mulInst);
                useOperand(src2, mulInst);
                defOperand(dest, mulInst);
                bb->appendInstruction(mulInst);
            }
            break;
        case llvm::Instruction::SDiv:
            generateIDivInst(bb, src1, src2, dest, inst);
            break;
        case llvm::Instruction::SRem:
            generateModInst(bb, src1, src2, dest, inst);
            break;
        default:
            break;
        }
    }

    void generateReturnInst(BasicBlockPtr bb, const llvm::ReturnInst& inst)
    {
        if (inst.getNumOperands() == 0) {
            auto retInst = std::make_shared<RetInstruction>();
            bb->appendInstruction(retInst);
            bb->setTerminator(retInst);
        } else {
            auto ret = inst.getOperand(0);
            auto src = getOperand(ret);
            auto ax = generatePrecoloredOperand(nameManager.genUniqueName("ret.val"), Register::RAX,
                generateTypeInfo(ret->getType()));
            appendMovInstruction(bb, ax, src);
            auto retInst = std::make_shared<RetInstruction>();
            useOperand(ax, retInst);
            bb->appendInstruction(retInst);
            bb->setTerminator(retInst);
        }
        bb->killBlock();
    }

    void generateICmpInst(BasicBlockPtr bb, const llvm::ICmpInst& inst)
    {
        auto lhs = inst.getOperand(0);
        auto rhs = inst.getOperand(1);
        auto src1 = getOperand(lhs);
        auto src2 = getOperand(rhs);
        if (src1->isImmediate())
            std::swap(src1, src2);
        auto cmpInst = std::make_shared<CmpInstruction>(src1, src2);
        useOperand(src1, cmpInst);
        useOperand(src2, cmpInst);
        bb->appendInstruction(cmpInst);
    }

    bool isConstIndex(const llvm::GetElementPtrInst& inst, int fromIdx = 1)
    {
        for (int i = fromIdx; i < inst.getNumOperands(); i++) {
            if (!llvm::isa<llvm::ConstantInt>(inst.getOperand(i))) {
                return false;
            }
        }
        return true;
    }

    auto getElementBaseType(const llvm::GetElementPtrInst& inst) -> llvm::Type*
    {
        auto type = inst.getSourceElementType();
        while (type->isArrayTy()) {
            type = type->getArrayElementType();
        }
        return type;
    }

    int calcConstOffset(const llvm::GetElementPtrInst& inst, int fromIdx = 1)
    {
        auto srcType = inst.getSourceElementType();
        int srcArraySize = srcType->isArrayTy() ? srcType->getArrayNumElements() : 1;
        int offset = 0, opNum = inst.getNumOperands();
        for (int i = fromIdx; i < opNum; i++) {
            auto constInt = llvm::dyn_cast<llvm::ConstantInt>(inst.getOperand(i));
            offset += constInt->getSExtValue() * srcArraySize;
            if (i < opNum - 1) {
                srcType = srcType->getArrayElementType();
                srcArraySize = srcType->isArrayTy() ? srcType->getArrayNumElements() : 1;
            }
        }
        return offset;
    }

    void generateGetElementPtrInst(BasicBlockPtr bb, const llvm::GetElementPtrInst& inst)
    {
        auto ptr = inst.getPointerOperand();
        auto dest = getOperand(&inst);
        auto base = getOperand(ptr);
        auto baseType = getElementBaseType(inst);
        int baseTypeSize = dataLayout->getTypeAllocSize(baseType);
        if (isConstIndex(inst)) {
            auto offset = calcConstOffset(inst);
            auto type = generateTypeInfo(inst.getResultElementType());
            auto srcName = nameManager.genUniqueName("gep.src");
            auto src = std::make_shared<Memory>(srcName, base, offset * baseTypeSize, type);
            operandMap[srcName] = src;
            auto leaInst = std::make_shared<LeaInstruction>(dest, src);
            useOperand(src, leaInst);
            defOperand(dest, leaInst);
            bb->appendInstruction(leaInst);
        } else {
            auto baseAddrName = nameManager.genUniqueName("gep.base");
            auto baseAddr
                = std::make_shared<VirtualRegister>(baseAddrName, TypeInfo::createPrimitive(8));
            operandMap[baseAddrName] = baseAddr;
            if (base->getType()->isPointerType()) {
                appendMovInstruction(bb, baseAddr, base);
            } else {
                auto leaInst = std::make_shared<LeaInstruction>(baseAddr, base);
                useOperand(base, leaInst);
                defOperand(baseAddr, leaInst);
                bb->appendInstruction(leaInst);
            }
            auto idxName = nameManager.genUniqueName("gep.idx");
            auto idxOp = std::make_shared<VirtualRegister>(idxName, TypeInfo::createPrimitive(8));
            operandMap[idxName] = idxOp;
            int opNum = inst.getNumOperands();
            auto srcType = inst.getSourceElementType();
            int srcArraySize = srcType->isArrayTy() ? srcType->getArrayNumElements() : 1;
            for (int i = 1; i < opNum; i++) {
                auto idx = inst.getOperand(i);
                if (auto constInt = llvm::dyn_cast<llvm::ConstantInt>(idx);
                    !constInt || !constInt->isZero()) {
                    auto src = getOperand(idx);
                    appendMovInstruction(bb, idxOp, src);
                    auto imulInst = std::make_shared<IMulInstruction>(idxOp, idxOp,
                        std::make_shared<Immediate>(
                            srcArraySize * baseTypeSize, TypeInfo::createPrimitive(8)));
                    useOperand(idxOp, imulInst);
                    defOperand(idxOp, imulInst);
                    bb->appendInstruction(imulInst);
                    auto addInst
                        = std::make_shared<BinaryOpInstruction>(BinaryOpType::ADD, baseAddr, idxOp);
                    useOperand(idxOp, addInst);
                    useOperand(baseAddr, addInst);
                    defOperand(baseAddr, addInst);
                    bb->appendInstruction(addInst);
                }
                if (i < opNum - 1) {
                    srcType = srcType->getArrayElementType();
                    srcArraySize = srcType->isArrayTy() ? srcType->getArrayNumElements() : 1;
                }
            }
            appendMovInstruction(bb, dest, baseAddr);
        }
    }

    void generateSExtInst(BasicBlockPtr bb, const llvm::SExtInst& inst)
    {
        auto src = inst.getOperand(0);
        auto dest = getOperand(&inst);
        auto srcOperand = getOperand(src);
        appendMovInstruction(bb, dest, srcOperand, MovType::MOVSX);
    }

    void generateZExtInst(BasicBlockPtr bb, const llvm::ZExtInst& inst)
    {
        auto src = inst.getOperand(0);
        auto dest = getOperand(&inst);
        auto srcOperand = getOperand(src);
        appendMovInstruction(bb, dest, srcOperand, MovType::MOVZX);
    }

    void generateInstruction(BasicBlockPtr bb, const llvm::Instruction& inst)
    {
        if (auto branchInst = llvm::dyn_cast<llvm::BranchInst>(&inst)) {
            if (branchInst->isConditional()) {
                auto cond = branchInst->getCondition();
                auto trueBBName = branchInst->getSuccessor(0)->getName().str();
                auto falseBBName = branchInst->getSuccessor(1)->getName().str();
                auto trueBB = bbMap[trueBBName], falseBB = bbMap[falseBBName];
                if (auto icmpInst = llvm::dyn_cast<llvm::ICmpInst>(cond)) {
                    ConditionType condType;
                    auto pred = icmpInst->getPredicate();
                    switch (pred) {
                    case llvm::CmpInst::ICMP_EQ:
                        condType = ConditionType::EQ;
                        break;
                    case llvm::CmpInst::ICMP_NE:
                        condType = ConditionType::NE;
                        break;
                    case llvm::CmpInst::ICMP_SGT:
                        condType = ConditionType::GT;
                        break;
                    case llvm::CmpInst::ICMP_SGE:
                        condType = ConditionType::GE;
                        break;
                    case llvm::CmpInst::ICMP_SLT:
                        condType = ConditionType::LT;
                        break;
                    case llvm::CmpInst::ICMP_SLE:
                        condType = ConditionType::LE;
                        break;
                    default:
                        throw std::runtime_error("Unsupported icmp predicate");
                    }
                    trueBB->addPredecessor(bb), falseBB->addPredecessor(bb);
                    bb->setConditionalNext(trueBB, falseBB, condType);
                }
            } else {
                auto nextBBName = branchInst->getSuccessor(0)->getName().str();
                auto nextBB = bbMap[nextBBName];
                nextBB->addPredecessor(bb);
                bb->setUnconditionalNext(nextBB);
            }
        } else if (auto allocaInst = llvm::dyn_cast<llvm::AllocaInst>(&inst)) {
            auto operand = getOperand(allocaInst);
            auto fakeInst = std::make_shared<FakeInstruction>();
            defOperand(operand, fakeInst);
            bb->appendInstruction(fakeInst);
        } else if (auto callInst = llvm::dyn_cast<llvm::CallInst>(&inst)) {
            generateCallInst(bb, *callInst);
        } else if (auto loadInst = llvm::dyn_cast<llvm::LoadInst>(&inst)) {
            generateLoadInst(bb, *loadInst);
        } else if (auto storeInst = llvm::dyn_cast<llvm::StoreInst>(&inst)) {
            generateStoreInst(bb, *storeInst);
        } else if (auto binOp = llvm::dyn_cast<llvm::BinaryOperator>(&inst)) {
            generateBinaryOperator(bb, *binOp);
        } else if (auto icmpInst = llvm::dyn_cast<llvm::ICmpInst>(&inst)) {
            generateICmpInst(bb, *icmpInst);
        } else if (auto retInst = llvm::dyn_cast<llvm::ReturnInst>(&inst)) {
            generateReturnInst(bb, *retInst);
        } else if (auto gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(&inst)) {
            generateGetElementPtrInst(bb, *gepInst);
        } else if (auto sextInst = llvm::dyn_cast<llvm::SExtInst>(&inst)) {
            generateSExtInst(bb, *sextInst);
        } else if (auto zextInst = llvm::dyn_cast<llvm::ZExtInst>(&inst)) {
            generateZExtInst(bb, *zextInst);
        }
    }

    string getBBName(const llvm::BasicBlock& bb, string_view funcName, int bbIdx)
    {
        if (bb.hasName()) {
            return bb.getName().str();
        }
        if (bb.isEntryBlock()) {
            return std::format("{}_entry", funcName);
        }
        return std::format("{}_bb{}", funcName, bbIdx);
    }

    JmpType getConditionTrueJmpType(ConditionType condType)
    {
        switch (condType) {
        case ConditionType::EQ:
            return JmpType::JE;
        case ConditionType::NE:
            return JmpType::JNE;
        case ConditionType::GT:
            return JmpType::JG;
        case ConditionType::GE:
            return JmpType::JGE;
        case ConditionType::LT:
            return JmpType::JL;
        case ConditionType::LE:
            return JmpType::JLE;
        default:
            return JmpType::JMP;
        }
    }

    JmpType getConditionFalseJmpType(ConditionType condType)
    {
        switch (condType) {
        case ConditionType::EQ:
            return JmpType::JNE;
        case ConditionType::NE:
            return JmpType::JE;
        case ConditionType::GT:
            return JmpType::JLE;
        case ConditionType::GE:
            return JmpType::JL;
        case ConditionType::LT:
            return JmpType::JGE;
        case ConditionType::LE:
            return JmpType::JG;
        default:
            return JmpType::JMP;
        }
    }

    unordered_map<AsmFunctionPtr, set<Register>> calleeSaveRegisters;

public:
    InstructionGenerator() = default;

    void setCalleeSaveRegisters(AsmFunctionPtr func, set<Register> calleeSaveRegs)
    {
        calleeSaveRegisters[func] = calleeSaveRegs;
    }

    void generateCalleeSave(AsmFunctionPtr func)
    {
        auto savedRegs = calleeSaveRegisters[func];
        auto insertLoc = std::make_shared<FakeInstruction>();
        func->getEntry()->prependInstruction(insertLoc);
        for (auto saveReg : savedRegs) {
            auto saveOpName = std::format("callee.save.{}", getRegisterName(saveReg));
            auto reg = generatePrecoloredOperand(saveOpName, saveReg, TypeInfo::createPrimitive(8));
            auto pushInst = std::make_shared<PushInstruction>(reg);
            useOperand(reg, pushInst);
            func->getEntry()->insertInstructionBefore(pushInst, insertLoc);
        }
    }

    void generateCalleeSaveRestore(AsmFunctionPtr func)
    {
        auto savedRegs = calleeSaveRegisters[func];
        func->getExit()->traversePredecessors([&](auto bb) {
            for (auto saveReg : set(savedRegs) | std::views::reverse) {
                auto saveOpName = std::format("callee.save.{}", getRegisterName(saveReg));
                auto reg
                    = generatePrecoloredOperand(saveOpName, saveReg, TypeInfo::createPrimitive(8));
                auto popInst = std::make_shared<PopInstruction>(reg);
                defOperand(reg, popInst);
                bb->insertInstructionBefore(popInst, bb->getTerminator());
            }
        });
    }

    void generateCallerSave(CallerSaveInsertionPoint insertPoint)
    {
        auto [saveInsertionPoint, callInst, restoreInsertionPoint, savedRegs] = insertPoint;
        auto bb = callInst->parent.lock();
        int savedRegNum = savedRegs.size();
        if (savedRegNum % 2 != 0) {
            auto subInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::SUB,
                generatePrecoloredOperand("rsp", Register::RSP, 8),
                std::make_shared<Immediate>(8, TypeInfo::createPrimitive(8)));
            bb->insertInstructionBefore(subInst, saveInsertionPoint);
        }
        for (auto saveReg : savedRegs) {
            auto saveOpName = std::format("caller.save.{}", getRegisterName(saveReg));
            auto reg = generatePrecoloredOperand(saveOpName, saveReg, TypeInfo::createPrimitive(8));
            auto pushInst = std::make_shared<PushInstruction>(reg);
            // useOperand(reg, pushInst);
            bb->insertInstructionBefore(pushInst, saveInsertionPoint);
        }
        AsmInstructionPtr realInstructionPoint = restoreInsertionPoint;
        if (callInst->isRetValUsed) {
            // 从 callInst 向后寻找，直到遇到第一个使用了返回值的指令
            realInstructionPoint = callInst;
            bool found = false;
            while (realInstructionPoint->next) {
                realInstructionPoint = realInstructionPoint->next;
                if (auto movRelInst
                    = std::dynamic_pointer_cast<MovRelatedInstruction>(realInstructionPoint)) {
                    auto src = movRelInst->getSrc();
                    if (auto regSrc = std::dynamic_pointer_cast<PhysicalRegister>(src)) {
                        if (regSrc->getRegister() == Register::RAX) {
                            found = true;
                            break;
                        }
                    }
                }
            }
            if (!found) {
                realInstructionPoint = restoreInsertionPoint;
            }
        }
        if (savedRegNum % 2 != 0) {
            auto addInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::ADD,
                generatePrecoloredOperand("rsp", Register::RSP, 8),
                std::make_shared<Immediate>(8, TypeInfo::createPrimitive(8)));
            bb->insertInstructionAfter(addInst, realInstructionPoint);
        }
        for (auto saveReg : savedRegs) {
            auto saveOpName = std::format("caller.save.{}", getRegisterName(saveReg));
            auto reg = generatePrecoloredOperand(saveOpName, saveReg, TypeInfo::createPrimitive(8));
            auto popInst = std::make_shared<PopInstruction>(reg);
            // defOperand(reg, popInst);
            bb->insertInstructionAfter(popInst, realInstructionPoint);
        }
    }

    void generateCallerSave(AsmFunctionPtr func)
    {
        for (auto [saveInsertionPoint, callInst, restoreInsertionPoint, savedRegs] :
            callerSaveInsertionPoints[func]) {
            generateCallerSave(
                std::make_tuple(saveInsertionPoint, callInst, restoreInsertionPoint, savedRegs));
        }
    }

    void updateRelativeStackLocation(AsmFunctionPtr func)
    {
        auto savedRegs = calleeSaveRegisters[func];
        int savedRegNum = savedRegs.size();
        int extraSize;
        if (func->getStackFrame()->isStackUsed()) {
            extraSize = (savedRegNum + 1) * 8 + 8;
        } else {
            extraSize = savedRegNum * 8 + 8;
        }
        func->buildOperandMap();
        for (auto& op : func->getOperands()) {
            if (auto stackVar = std::dynamic_pointer_cast<StackLocation>(op);
                stackVar && stackVar->base == StackLocationBase::RBP_RELATIVE) {
                stackVar->offset -= extraSize;
                stackVar->base = StackLocationBase::RBP;
            }
        }
    }

    void generatePrologue(AsmFunctionPtr func)
    {
        auto savedArgs = calleeSaveRegisters[func];
        int savedArgNum = savedArgs.size();
        if (func->getStackFrame()->isStackUsed()) {
            auto stackSize = func->getStackFrame()->getAlignedStackSize((savedArgNum + 1) % 2 == 0);
            auto subInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::SUB,
                generatePrecoloredOperand("rsp", Register::RSP, 8),
                std::make_shared<Immediate>(stackSize, TypeInfo::createPrimitive(8)));
            func->getEntry()->prependInstruction(subInst);
            auto movInst = std::make_shared<MovInstruction>(
                generatePrecoloredOperand("rbp", Register::RBP, TypeInfo::createPrimitive(8)),
                generatePrecoloredOperand("rsp", Register::RSP, TypeInfo::createPrimitive(8)));
            func->getEntry()->prependInstruction(movInst);
            generateCalleeSave(func);
            auto pushInst = std::make_shared<PushInstruction>(
                generatePrecoloredOperand("rbp", Register::RBP, TypeInfo::createPrimitive(8)));
            func->getEntry()->prependInstruction(pushInst);
        } else if (savedArgNum % 2 == 0) {
            auto subInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::SUB,
                generatePrecoloredOperand("rsp", Register::RSP, 8),
                std::make_shared<Immediate>(8, TypeInfo::createPrimitive(8)));
            func->getEntry()->prependInstruction(subInst);
            generateCalleeSave(func);
        } else {
            generateCalleeSave(func);
        }
    }

    void generateEpilogue(AsmFunctionPtr func)
    {
        auto savedArgs = calleeSaveRegisters[func];
        int savedArgNum = savedArgs.size();
        if (func->getStackFrame()->isStackUsed()) {
            auto stackSize = func->getStackFrame()->getAlignedStackSize((savedArgNum + 1) % 2 == 0);
            func->getExit()->traversePredecessors([&](auto bb) {
                auto addInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::ADD,
                    generatePrecoloredOperand("rsp", Register::RSP, 8),
                    std::make_shared<Immediate>(stackSize, TypeInfo::createPrimitive(8)));
                bb->insertInstructionBefore(addInst, bb->getTerminator());
                generateCalleeSaveRestore(func);
                auto popInst = std::make_shared<PopInstruction>(
                    generatePrecoloredOperand("rbp", Register::RBP, TypeInfo::createPrimitive(8)));
                bb->insertInstructionBefore(popInst, bb->getTerminator());
            });
        } else if (savedArgNum % 2 == 0) {
            func->getExit()->traversePredecessors([&](auto bb) {
                auto addInst = std::make_shared<BinaryOpInstruction>(BinaryOpType::ADD,
                    generatePrecoloredOperand("rsp", Register::RSP, 8),
                    std::make_shared<Immediate>(8, TypeInfo::createPrimitive(8)));
                bb->insertInstructionBefore(addInst, bb->getTerminator());
                generateCalleeSaveRestore(func);
            });
        } else {
            func->getExit()->traversePredecessors(
                [&](auto bb) { generateCalleeSaveRestore(func); });
        }
    }

    AsmModulePtr generate(std::unique_ptr<llvm::Module>& module)
    {
        dataLayout = &module->getDataLayout();
        AsmModulePtr asmModule = std::make_shared<AsmModule>();
        for (auto& global : module->globals()) {
            auto globalVar = std::dynamic_pointer_cast<GlobalVariable>(getOperand(&global));
            asmModule->addGlobal(globalVar);
        }
        for (auto& func : *module) {
            if (func.isDeclaration()) {
                continue;
            }
            operandMap.clear();
            bbMap.clear();
            auto funcName = func.getName().str();
            currentFrame = std::make_shared<StackFrame>();
            if (func.arg_size() > 6) {
                currentFrame->setArgOnStack();
            }
            AsmFunctionPtr asmFunc = std::make_shared<AsmFunction>(funcName, currentFrame);
            auto exitBlock
                = std::make_shared<BasicBlock>(std::format("{}_exit", funcName), asmFunc);
            auto bbIdx = 0;
            for (auto& bb : func) {
                auto bbName = getBBName(bb, funcName, bbIdx++);
                BasicBlockPtr basicBlock = std::make_shared<BasicBlock>(bbName, asmFunc);
                bbMap[bbName] = basicBlock;
                asmFunc->addBlock(basicBlock);
            }
            bbIdx = 0;
            BasicBlockPtr prevBB = nullptr;
            for (auto& bb : func) {
                auto bbName = getBBName(bb, funcName, bbIdx++);
                auto basicBlock = bbMap[bbName];
                for (const auto& inst : bb) {
                    generateInstruction(basicBlock, inst);
                }
                if (!prevBB) {
                    asmFunc->setEntry(basicBlock);
                } else if (prevBB->isConditional) {
                    prevBB->setUnconditionalNext(basicBlock);
                }
                prevBB = basicBlock;
            }
            for (auto& [_, bb] : bbMap) {
                if (!bb->isAlive) {
                    bb->setUnconditionalNext(exitBlock);
                    exitBlock->addPredecessor(bb);
                }
                if (bb->isConditional) {
                    auto trueBB = bb->trueBranch;
                    auto falseBB = bb->falseBranch;
                    auto nextBB = bb->next;
                    auto pred = bb->condition;
                    if (nextBB == trueBB) {
                        auto jmpInst = std::make_shared<JmpInstruction>(
                            getConditionFalseJmpType(pred), falseBB);
                        bb->appendInstruction(jmpInst);
                    } else if (nextBB == falseBB) {
                        auto jmpInst = std::make_shared<JmpInstruction>(
                            getConditionTrueJmpType(pred), trueBB);
                        bb->appendInstruction(jmpInst);
                    } else {
                        auto trueJmpInst = std::make_shared<JmpInstruction>(
                            getConditionTrueJmpType(pred), trueBB);
                        auto falseJmpInst = std::make_shared<JmpInstruction>(JmpType::JMP, falseBB);
                        bb->appendInstruction(trueJmpInst);
                        bb->appendInstruction(falseJmpInst);
                    }
                } else {
                    auto nextBB = bb->next;
                    if (nextBB && nextBB != exitBlock) {
                        auto jmpInst = std::make_shared<JmpInstruction>(JmpType::JMP, nextBB);
                        bb->appendInstruction(jmpInst);
                    }
                }
            }
            asmFunc->setExit(exitBlock);
            // generateCalleeSave(asmFunc->getEntry());
            // generateRestoreCalleeSave(asmFunc->getExit());
            auto argDummyInst = std::make_shared<FakeInstruction>();
            for (auto& arg : func.args()) {
                auto argOp = getOperand(&arg);
                defOperand(argOp, argDummyInst);
            }
            asmFunc->getEntry()->prependInstruction(argDummyInst);
            asmFunc->setOperandMap(operandMap);
            asmModule->addFunction(asmFunc);
        }
        return asmModule;
    }
};
} // namespace sysy
