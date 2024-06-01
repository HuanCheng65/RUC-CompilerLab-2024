#include "backend/cfg.hpp"
#include "backend/dfg.hpp"
#include "backend/local_var.hpp"
#include "backend/reg_manage.hpp"
#include "backend/registers.hpp"
#include "backend/symbols.hpp"
#include "backend/utils.hpp"
#include "cfg.hpp"
#include <algorithm>
#include <fmt/core.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/raw_ostream.h>
#include <map>
#include <memory>
#include <numeric>
#include <ranges>
#include <string>
#include <unordered_map>

using std::string_view;

namespace sysy {
class StackFrame : public std::enable_shared_from_this<StackFrame> {
    const llvm::DataLayout& dataLayout;
    RegisterManagerPtr regAlloc { std::make_shared<RegisterManager>() };
    LocalVariableManager localVarManager { regAlloc };
    std::unordered_map<const llvm::BasicBlock*, CFGNodePtr> bbMap;
    CFGNodePtr cfgRoot;
    InterferenceGraph graph;
    size_t instLoc = 0;
    string funcName;

    bool canBePlaceOnReg(const llvm::AllocaInst& allocaInst) const
    {
        if (allocaInst.getAllocatedType()->isArrayTy()) {
            return false;
        }
        for (auto user : allocaInst.users()) {
            if (llvm::dyn_cast<llvm::CallInst>(user)) {
                return false;
            }
        }
        return true;
    }

    void preAllocateVars(const llvm::Function& func)
    {
        auto i = 0;
        for (auto& bb : func) {
            for (auto& inst : bb) {
                if (inst.hasName()) {
                    auto name = inst.getName().str();
                    if (!localVarManager.isVarPreAllocated(name)) {
                        int size = dataLayout.getTypeAllocSize(inst.getType());
                        bool canBeOnReg = true;
                        if (auto allocInst = llvm::dyn_cast<llvm::AllocaInst>(&inst)) {
                            size = dataLayout.getTypeAllocSize(allocInst->getAllocatedType());
                            canBeOnReg = canBePlaceOnReg(*allocInst);
                        }
                        localVarManager.preAllocate(name, size, i, !canBeOnReg);
                    }
                }
                i++;
            }
        }
    }

    void analyzeVars(const llvm::Function& func)
    {
        auto i = 0;
        for (auto& arg : func.args()) {
            auto name = arg.getName().str();
            localVarManager.ping(name, i);
        }
        for (auto& bb : func) {
            for (auto& inst : bb) {
                if (inst.hasName()) {
                    auto name = inst.getName().str();
                    // fmt::println("Inst LVal: {} use count: {}", name, inst.getNumUses());
                    localVarManager.ping(name, i);
                }
                for (auto& op : inst.operands()) {
                    if (auto instOp = llvm::dyn_cast<const llvm::Instruction>(&op)) {
                        if (instOp->hasName()) {
                            auto name = instOp->getName().str();
                            // fmt::println("Inst Operand: {}", name);
                            localVarManager.ping(name, i);
                        }
                    } else if (auto argOp = llvm::dyn_cast<const llvm::Argument>(&op)) {
                        if (argOp->hasName()) {
                            auto name = argOp->getName().str();
                            // fmt::println("Arg Operand: {}", name);
                            localVarManager.ping(name, i);
                        }
                    }
                }
                i++;
            }
        }
    }

    void analyzeFuncCall(const llvm::Function& func)
    {
        int instLoc = 0;
        for (auto& bb : func) {
            for (auto& inst : bb) {
                if (auto callInst = llvm::dyn_cast<llvm::CallInst>(&inst)) {
                    int argSize = callInst->arg_size();
                    int regArgSize = FUNCTION_PARAM_REGISTERS.size();
                    if (argSize > regArgSize) {
                        auto extraArgSizeView = callInst->args() | std::views::drop(regArgSize) | std::views::transform([&](auto& arg) {
                            return dataLayout.getTypeAllocSize(arg->getType());
                        });
                        auto extraArgSize = std::accumulate(extraArgSizeView.begin(), extraArgSizeView.end(), 0);
                        localVarManager.setMaxStackArgSize(extraArgSize);
                    }
                    for (int i = 0; i < std::min(argSize, regArgSize); i++) {
                        regAlloc->preAllocate(FUNCTION_PARAM_REGISTERS[i], instLoc, instLoc);
                    }
                    regAlloc->preAllocate(Register::RAX, instLoc, instLoc);
                }
                instLoc++;
            }
        }
    }

    void analyzeFuncReturn(const llvm::Function& func)
    {
        int instLoc = 0;
        for (auto& bb : func) {
            for (auto& inst : bb) {
                if (llvm::dyn_cast<llvm::ReturnInst>(&inst)) {
                    regAlloc->preAllocate(Register::RAX, instLoc, instLoc);
                }
                instLoc++;
            }
        }
    }

    void preAllocRegForIDivInst(const llvm::Function& func)
    {
        int instLoc = 0;
        for (auto& bb : func) {
            for (auto& inst : bb) {
                if (auto binOp = llvm::dyn_cast<llvm::BinaryOperator>(&inst)) {
                    // 除法需要预占用寄存器 rax 和 rdx
                    if (binOp->getOpcode() == llvm::Instruction::SDiv || binOp->getOpcode() == llvm::Instruction::SRem) {
                        regAlloc->preAllocate(Register::RAX, instLoc, instLoc);
                        regAlloc->preAllocate(Register::RDX, instLoc, instLoc);
                        // 检查除数是否为立即数
                        if (auto imm = llvm::dyn_cast<llvm::ConstantInt>(binOp->getOperand(1)); imm) {
                            // 如果是，那么还需要为立即数分配一个寄存器（idiv 指令的操作数只能是寄存器）
                            regAlloc->preAllocate(Register::RCX, instLoc, instLoc);
                        }
                    }
                }
                instLoc++;
            }
        }
    }

    void preAllocateArgs(const llvm::Function& func)
    {
        size_t argIdx = 0;
        for (auto& arg : func.args()) {
            auto name = arg.getName().str();
            auto size = dataLayout.getTypeAllocSize(arg.getType());
            auto [start, end] = localVarManager.getLifeTime(name);
            if (argIdx < FUNCTION_PARAM_REGISTERS.size()) {
                auto reg = FUNCTION_PARAM_REGISTERS[argIdx];
                regAlloc->preAllocate(reg, start, end);
                localVarManager.preAllocateArg(name, size, reg);
            }
            argIdx++;
            // print debug info
            // llvm::outs() << "Argument: " << name << ", size: " << size << "\n";
            // llvm::outs() << "  - Life time: [" << start << ", " << end << "]\n";
        }
    }

    void preAllocateRegForGeps(const llvm::Function& func)
    {
        int instLoc = 0;
        for (auto& bb : func) {
            for (auto& inst : bb) {
                if (auto gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(&inst)) {
                    auto lValName = gepInst->getName().str();
                    localVarManager.preAllocateReg(Register::RCX, instLoc);
                    localVarManager.preAllocateOnReg(lValName, 8);
                }
                instLoc++;
            }
        }
    }

    void preAllocateRegForCallReturn(const llvm::Function& func)
    {
        for (auto& bb : func) {
            for (auto& inst : bb) {
                if (auto callInst = llvm::dyn_cast<llvm::CallInst>(&inst)) {
                    if (callInst->hasName()) {
                        auto lValName = callInst->getName().str();
                        auto size = dataLayout.getTypeAllocSize(callInst->getType());
                        localVarManager.preAllocateOnReg(lValName, size, Register::RAX);
                        // fmt::println("Pre-allocate return value {} on register rax", lValName);
                    }
                }
            }
        }
    }

    string getBlockName(const llvm::BasicBlock& bb, string_view funcName, int i) const
    {
        if (bb.hasName()) {
            return bb.getName().str();
        }
        if (bb.isEntryBlock()) {
            return fmt::format("{}.entry", funcName);
        }
        return fmt::format("{}.bb{}", funcName, i);
    }

    void buildCFG(const llvm::Function& func)
    {
        auto funcName = func.getName().str();
        CFGNodePtr current = nullptr;
        auto bbIdx = 0;
        auto exitNode = std::make_shared<CFGNode>(fmt::format("{}.exit", funcName), nullptr);
        for (auto& bb : func) {
            auto bbName = getBlockName(bb, funcName, bbIdx);
            auto bbNode = std::make_shared<CFGNode>(bbName, &bb);
            bbMap[&bb] = bbNode;
            bbIdx++;
        }
        for (auto& [name, node] : bbMap) {
            if (!node->next && !node->trueBranch && !node->falseBranch) {
                node->next = exitNode;
                exitNode->addPredecessor(node);
            }
        }
        auto instLoc = 0;
        bbIdx = 0;
        for (auto& bb : func) {
            auto bbName = getBlockName(bb, funcName, bbIdx);
            auto bbNode = bbMap[&bb];
            if (!current) {
                cfgRoot = bbNode;
            }
            current = bbNode;
            current->start = instLoc;
            for (auto& inst : bb) {
                if (auto brInst = llvm::dyn_cast<llvm::BranchInst>(&inst)) {
                    if (brInst->isConditional()) {
                        auto trueBB = brInst->getSuccessor(0);
                        auto falseBB = brInst->getSuccessor(1);
                        auto trueNode = bbMap[trueBB];
                        auto falseNode = bbMap[falseBB];
                        current->trueBranch = trueNode;
                        current->falseBranch = falseNode;
                        current->isConditional = true;
                        trueNode->addPredecessor(current);
                        falseNode->addPredecessor(current);
                    } else {
                        auto nextBB = brInst->getSuccessor(0);
                        auto nextNode = bbMap[nextBB];
                        current->next = nextNode;
                        current->isConditional = false;
                        nextNode->addPredecessor(current);
                    }
                }
                instLoc++;
            }
            current->end = instLoc;
            bbIdx++;
        }
        for (auto& arg : func.args()) {
            auto name = arg.getName().str();
            cfgRoot->inSet.insert(name);
        }
        analyzeLiveness(exitNode);
    }

    void analyzeVarLifetimeByCFG(const llvm::Function& func)
    {
        auto funcName = func.getName().str();
        for (auto& bb : func) {
            auto bbNode = bbMap[&bb];
            for (auto& var : bbNode->outSet) {
                localVarManager.ping(var, bbNode->end);
            }
        }
    }

    void generateCalleeSaveReg(AsmBuilder& builder) const
    {
        int i = 0;
        for (auto reg : CALLEE_SAVED_REGISTERS) {
            if (localVarManager.isRegUsed(reg)) {
                builder.addOp("mov", fmt::format("qword ptr [rsp + {}]", localVarManager.getCalleeSavedRegLocByRsp(i)),
                    reg);
                i++;
            }
        }
    }

    void restoreCalleeSavedReg(AsmBuilder& builder) const
    {
        int i = 0;
        for (auto reg : CALLEE_SAVED_REGISTERS) {
            if (localVarManager.isRegUsed(reg)) {
                builder.addOp("mov", reg,
                    fmt::format("qword ptr [rsp + {}]", localVarManager.getCalleeSavedRegLocByRsp(i)));
                i++;
            }
        }
    }

public:
    string getFuncName() const
    {
        return funcName;
    }

    void analyzeCalleeSaveRegCount(const llvm::Function& func)
    {
        auto calleeSaveRegCount = 0;
        for (auto reg : CALLEE_SAVED_REGISTERS) {
            if (localVarManager.isRegUsed(reg)) {
                calleeSaveRegCount++;
            }
        }
        localVarManager.setCalleeSavedRegCount(calleeSaveRegCount);
    }

    // print graphviz for debug
    void printCFGNode(const CFGNodePtr& node, std::unordered_map<string, bool>& visited)
    {
        if (!node) {
            return;
        }
        auto bbName = node->name;
        if (visited[bbName]) {
            return;
        }
        visited[bbName] = true;
        if (node->isConditional) {
            fmt::println("  \"{}\" -> \"{}\" [label=\"true\"];", bbName, node->trueBranch->name);
            fmt::println("  \"{}\" -> \"{}\" [label=\"false\"];", bbName, node->falseBranch->name);
        } else if (node->next) {
            fmt::println("  \"{}\" -> \"{}\";", bbName, node->next->name);
        } else {
            fmt::println("  \"{}\" -> \"{}\" [style=dotted];", bbName, "exit");
            fmt::println("  \"{}\" [shape=box];", "exit");
        }
        printCFGNode(node->trueBranch, visited);
        printCFGNode(node->falseBranch, visited);
        printCFGNode(node->next, visited);
    }

    void printVarInfoDbg()
    {
        for (const auto& [name, varInfo] : localVarManager.getVarInfoMap()) {
            auto [start, end] = varInfo.lifeTime;
            fmt::print("Variable: {} at {}, size: {}\n", name, start, varInfo.size);
            fmt::print("  - Life time: [{}, {}]\n", start, end);
            if (localVarManager.isOnReg(name)) {
                fmt::print("  - Register: {}\n", sysy::getRegisterName(localVarManager.getReg(name)));
            } else {
                fmt::print("  - Stack position: [rsp + {}]\n", localVarManager.getLocByRsp(name));
            }
        }
    }

    void printCFGInfoDbg(CFGNodePtr node) const
    {
        fmt::println("BB: {}", node->name);
        fmt::println("  - Def: {}", fmt::join(node->defSet, ", "));
        fmt::println("  - Use: {}", fmt::join(node->useSet, ", "));
        fmt::println("  - In: {}", fmt::join(node->inSet, ", "));
        fmt::println("  - Out: {}", fmt::join(node->outSet, ", "));
        if (node->isConditional) {
            fmt::println("  - True branch: {}", node->trueBranch->name);
            fmt::println("  - False branch: {}", node->falseBranch->name);
        } else if (node->next) {
            fmt::println("  - Next: {}", node->next->name);
        }
    }

    StackFrame(const llvm::Function& func, const llvm::DataLayout& dataLayout)
        : dataLayout(dataLayout)
    {
        funcName = func.getName().str();
        buildCFG(func);
        traverseCFG(cfgRoot, [&](const CFGNodePtr& node) {
            node->buildDFGInBlock();
            printCFGInfoDbg(node);
            // std::for_each(node->defSet.begin(), node->defSet.end(), [&](const auto& var) {
            //     auto dfgNode = node->dfgInBlock.getNodeByName(var);
            //     if (dfgNode) {
            //         fmt::println("  - Var {}: Dependent Count: {}", var, dfgNode->countDependentNodes());
            //     }
            // });
        });
        buildInterferenceGraph(cfgRoot, graph);
        fmt::println("Interference Graph for function: {}", funcName);
        graph.printGraph();
        analyzeVars(func);
        analyzeVarLifetimeByCFG(func);
        analyzeFuncCall(func);
        analyzeFuncReturn(func);
        preAllocRegForIDivInst(func);
        preAllocateArgs(func);
        // preAllocateRegForCallReturn(func);
        preAllocateRegForGeps(func);
        preAllocateVars(func);
        // print var life time for debug
        printVarInfoDbg();
    }

public:
    void step()
    {
        localVarManager.freeDeadVars(instLoc);
        instLoc++;
    }

    size_t getInstLoc() const
    {
        return instLoc;
    }

    void allocate(string_view name, size_t size, bool isPtr = false)
    {
        localVarManager.allocate(name, size, isPtr);
        // print debug info
        // fmt::print("Allocated variable: {} at {}, size: {}\n", name, instLoc, size);
        // auto lifeTime = localVarManager.getLifeTime(name);
        // fmt::print("  - Life time: [{}, {}]\n", lifeTime.start, lifeTime.end);
        // if (localVarManager.isOnReg(name))
        // {
        //     fmt::print("  - Register: {}\n", sysy::getRegisterName(localVarManager.getReg(name)));
        // }
        // else
        // {
        //     fmt::print("  - Stack position: [rbp - {}]\n", localVarManager.getLocByRbp(name));
        // }
    }

    bool isPtr(string_view name) const
    {
        return localVarManager.isPtr(name);
    }

    bool isOnReg(string_view name) const
    {
        return localVarManager.isOnReg(name);
    }

    bool isRegUsed(Register reg) const
    {
        return localVarManager.isRegUsed(reg);
    }

    bool isRegUsedAfter(Register reg, int loc) const
    {
        return localVarManager.isRegUsedAfter(reg, loc);
    }

    auto getFirstVarOnRegAfter(Register reg, int loc) const -> std::optional<string>
    {
        return localVarManager.getFirstVarOnRegAfter(reg, loc);
    }

    auto getLastVarOnRegInBlockBefore(CFGNodePtr cfgNode, Register reg, int loc) const -> std::optional<string>
    {
        return localVarManager.getLastVarOnRegInBlockBefore(cfgNode, reg, loc);
    }

    auto getCFGNode(const llvm::BasicBlock* bb) const -> CFGNodePtr
    {
        return bbMap.at(bb);
    }

    Register getReg(string_view name) const
    {
        return localVarManager.getReg(name);
    }

    Register getPhysicalReg(string_view name) const
    {
        return sysy::getPhysicalRegister(localVarManager.getReg(name));
    }

    void setCallerSavedRegSize(size_t size)
    {
        localVarManager.setCallerSavedRegSize(size);
    }

    int getLocByRsp(string_view name) const
    {
        return localVarManager.getLocByRsp(name);
    }

    int getCallerSavedRegLocByRsp(int index) const
    {
        return localVarManager.getCallerSavedRegLocByRsp(index);
    }

    void generateCallerSaveReg(AsmBuilder& builder) const
    {
        for (auto reg : CALLER_SAVED_REGISTERS) {
            if (localVarManager.isRegUsed(reg)) {
                builder.addOp("push", reg);
            }
        }
    }

    void generateRestoreCallerSaveReg(AsmBuilder& builder) const
    {
        for (auto reg : CALLER_SAVED_REGISTERS | std::views::reverse) {
            if (localVarManager.isRegUsed(reg)) {
                builder.addOp("pop", reg);
            }
        }
    }

    void generateProlouge(AsmBuilder& builder) const
    {
        if (localVarManager.isStackUsed()) {
            auto stackSize = localVarManager.getAlignedTotalSize();
            builder.addOp("push", Register::RBP);
            builder.addOp("mov", Register::RBP, Register::RSP);
            builder.addOp("sub", Register::RSP, std::to_string(stackSize));
            generateCalleeSaveReg(builder);
        } else {
            builder.addOp("sub", Register::RSP, "8");
        }
    }

    void generateEpilouge(AsmBuilder& builder) const
    {
        if (localVarManager.isStackUsed()) {
            restoreCalleeSavedReg(builder);
            auto stackSize = localVarManager.getAlignedTotalSize();
            builder.addOp("add", Register::RSP, std::to_string(stackSize));
            builder.addOp("pop", Register::RBP);
        } else {
            builder.addOp("add", Register::RSP, "8");
        }
        builder.addOp("ret");
    }

    const llvm::DataLayout& getDataLayout() const
    {
        return dataLayout;
    }

    auto getVarLifetimeEnd(string_view name) const -> int
    {
        return localVarManager.getLifeTime(name).end;
    }
};

using StackFramePtr = std::shared_ptr<StackFrame>;

} // namespace sysy
