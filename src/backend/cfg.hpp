#pragma once
#include "dfg.hpp"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/Casting.h>
#include <memory>
#include <ranges>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using std::set;
using std::shared_ptr;
using std::string, std::string_view;
using std::unordered_map;
using std::unordered_set;
using std::vector;
using namespace std::string_view_literals;

namespace sysy {
template <typename T>
set<T> unionSet(const set<T>& a, const set<T>& b)
{
    set<T> result = a;
    for (const auto& elem : b) {
        result.insert(elem);
    }
    return result;
}

template <typename T>
set<T> intersectSet(const set<T>& a, const set<T>& b)
{
    set<T> result;
    for (const auto& elem : a) {
        if (b.contains(elem)) {
            result.insert(elem);
        }
    }
    return result;
}

template <typename T>
set<T> exceptSet(const set<T>& a, const set<T>& b)
{
    set<T> result = a;
    for (const auto& elem : b) {
        result.erase(elem);
    }
    return result;
}

template <typename T>
unordered_set<T> unionSet(const unordered_set<T>& a, const unordered_set<T>& b)
{
    unordered_set<T> result = a;
    for (const auto& elem : b) {
        result.insert(elem);
    }
    return result;
}

template <typename T>
unordered_set<T> intersectSet(const unordered_set<T>& a, const unordered_set<T>& b)
{
    unordered_set<T> result;
    for (const auto& elem : a) {
        if (b.contains(elem)) {
            result.insert(elem);
        }
    }
    return result;
}

template <typename T>
unordered_set<T> exceptSet(const unordered_set<T>& a, const unordered_set<T>& b)
{
    unordered_set<T> result = a;
    for (const auto& elem : b) {
        result.erase(elem);
    }
    return result;
}

struct CFGNode;

using CFGNodePtr = shared_ptr<CFGNode>;

struct CFGNode : public std::enable_shared_from_this<CFGNode> {
    string name;
    const llvm::BasicBlock* block;
    bool isConditional = false;
    CFGNodePtr trueBranch, falseBranch;
    CFGNodePtr next;
    vector<CFGNodePtr> predecessors;
    int start, end;
    unordered_set<string> defSet, useSet, inSet, outSet;
    unordered_map<const llvm::Value*, unordered_set<string>> inSetMap, outSetMap;
    unordered_map<const llvm::Value*, int> instLocMap;

    DFG dfgInBlock;

    CFGNode(string_view name, const llvm::BasicBlock* block)
        : name(name)
        , block(block)
    {
    }

    CFGNode(string_view name, const llvm::BasicBlock* block, CFGNodePtr next)
        : name(name)
        , block(block)
        , next(next)
    {
    }

    CFGNode(string_view name, const llvm::BasicBlock* block, CFGNodePtr trueBranch, CFGNodePtr falseBranch)
        : name(name)
        , block(block)
        , isConditional(true)
        , trueBranch(trueBranch)
        , falseBranch(falseBranch)
    {
    }

    void traverseInstructionSuccessors(int loc, std::function<void(int, const llvm::Instruction&)> f)
    {
        if (!block)
            return;
        int instLoc = 0;
        for (auto& inst : *block) {
            if (instLoc > loc) {
                f(instLoc, inst);
            }
            instLoc++;
        }
    }

    void analyzeLivenessInBlock()
    {
        if (!block)
            return;

        auto visit = [&](int loc, const llvm::Instruction& inst) {
            unordered_set<string> instDefSet, instUseSet;

            analyzeInstructionDefUse(loc, inst, [&](int loc, string_view def) { instDefSet.insert(def.data()); }, [&](int loc, string_view use, string_view user) { instUseSet.insert(use.data()); });

            auto oldIn = inSetMap[&inst];

            outSetMap[&inst].clear();
            traverseInstructionSuccessors(loc, [&](int succLoc, const llvm::Instruction& succInst) {
                outSetMap[&inst] = unionSet(outSetMap[&inst], inSetMap[&succInst]);
            });

            inSetMap[&inst] = unionSet(instUseSet, exceptSet(outSetMap[&inst], instDefSet));

            return oldIn != inSetMap[&inst];
        };

        bool changed = true;

        while (changed) {
            changed = false;
            traverseInstructionsReverse(
                [&](int loc, const llvm::Instruction& inst) {
                    changed |= visit(loc, inst);
                });
        }
    }

    void buildDFGInBlock()
    {
        if (!block)
            return;
        traverseInstructions(
            [&](int loc, const llvm::Instruction& inst) {
                instLocMap[&inst] = loc;
            });
        std::unordered_map<string, DFGNodePtr> dfgNodeMap;
        for (auto& liveVar : inSet) {
            auto node = std::make_shared<DFGNode>(-1, liveVar);
            dfgNodeMap[liveVar] = node;
            dfgInBlock.addNode(node);
        }
        traverseDefUse(
            [&](int loc, string_view def) {
                if (!dfgNodeMap.contains(def.data())) {
                    // fmt::println("build dfg of {}: def {} at {}", name, def, loc);
                    auto node = std::make_shared<DFGNode>(loc, def);
                    dfgNodeMap[def.data()] = node;
                    dfgInBlock.addNode(node);
                }
            },
            [&](int loc, string_view use, string_view user) {
                // fmt::println("build dfg of {}: use {} by {} at {}", name, use, user, loc);
                auto useNode = dfgNodeMap[use.data()];
                if (!useNode)
                    return;
                auto userNode = dfgNodeMap[user.data()];
                if (!userNode)
                    userNode = std::make_shared<DFGNode>(loc, user);
                userNode->addDependency(useNode);
            });
    }

    void addPredecessor(CFGNodePtr pred)
    {
        predecessors.push_back(pred);
    }

    void analyzeInstructionDefUse(int instLoc, const llvm::Instruction& inst, std::function<void(int, string_view)> def, std::function<void(int, string_view, string_view)> use)
    {
        if (auto storeInst = llvm::dyn_cast<llvm::StoreInst>(&inst)) {
            auto lVal = storeInst->getPointerOperand();
            auto rVal = storeInst->getValueOperand();
            auto userName { lVal->hasName() ? lVal->getName().str() : "store"sv };

            if (auto lValGEPInst = llvm::dyn_cast<llvm::GetElementPtrInst>(lVal); lValGEPInst && lValGEPInst->hasName()) {
                use(instLoc, lValGEPInst->getName().str(), "store"sv);
                if (auto gepSrcInst = llvm::dyn_cast<llvm::Instruction>(lValGEPInst->getPointerOperand()); gepSrcInst && gepSrcInst->hasName()) {
                    def(instLoc, gepSrcInst->getName().str());
                }
            } else {
                if (auto lValInst = llvm::dyn_cast<llvm::Instruction>(lVal); lValInst && lValInst->hasName()) {
                    def(instLoc, lValInst->getName().str());
                }
            }
            if (auto rValInst = llvm::dyn_cast<llvm::Instruction>(rVal); rValInst && rValInst->hasName()) {
                use(instLoc, rValInst->getName().str(), userName);
            } else if (auto rValArg = llvm::dyn_cast<llvm::Argument>(rVal); rValArg) {
                use(instLoc, rValArg->getName().str(), userName);
            }
        } else if (auto callInst = llvm::dyn_cast<llvm::CallInst>(&inst)) {
            if (callInst->hasName()) {
                def(instLoc, callInst->getName().str());
            }
            for (auto& op : callInst->args()) {
                if (auto argInst = llvm::dyn_cast<llvm::Instruction>(op.get()); argInst && argInst->hasName()) {
                    auto userName { callInst->hasName() ? callInst->getName().str() : "call"sv };
                    use(instLoc, argInst->getName().str(), userName);
                }
            }
        } else if (auto retInst = llvm::dyn_cast<llvm::ReturnInst>(&inst)) {
            if (auto retVal = retInst->getReturnValue(); retVal) {
                if (auto retValInst = llvm::dyn_cast<llvm::Instruction>(retVal); retValInst && retValInst->hasName()) {
                    use(instLoc, retValInst->getName().str(), "ret"sv);
                }
            }
        } else if (auto brInst = llvm::dyn_cast<llvm::BranchInst>(&inst)) {
            if (brInst->isConditional()) {
                auto cond = brInst->getCondition();
                if (auto condInst = llvm::dyn_cast<llvm::Instruction>(cond); condInst && condInst->hasName()) {
                    use(instLoc, condInst->getName().str(), "br"sv);
                }
            }
        } else {
            if (inst.hasName()) {
                def(instLoc, inst.getName().str());
            }

            for (auto& op : inst.operands()) {
                if (auto opInst = llvm::dyn_cast<llvm::Instruction>(op.get()); opInst && opInst->hasName()) {
                    auto userName { inst.hasName() ? inst.getName().str() : ""sv };
                    use(instLoc, opInst->getName().str(), userName);
                }
            }
        }
    }

    void traverseInstructions(std::function<void(int, const llvm::Instruction&)> f)
    {
        if (!block)
            return;
        auto instLoc = 0;
        for (auto& inst : *block) {
            f(instLoc, inst);
            instLoc++;
        }
    }

    void traverseInstructionsReverse(std::function<void(int, const llvm::Instruction&)> f)
    {
        if (!block)
            return;
        for (auto& inst : *block | std::views::reverse) {
            auto instLoc = instLocMap[&inst];
            f(instLoc, inst);
        }
    }

    void traverseDefUse(std::function<void(int, string_view)> def, std::function<void(int, string_view, string_view)> use)
    {
        if (!block)
            return;
        auto instLoc = 0;
        for (auto& inst : *block) {
            analyzeInstructionDefUse(instLoc, inst, def, use);
            instLoc++;
        }
    }

    void traverseDefUseReverse(std::function<void(int, string_view)> def, std::function<void(int, string_view, string_view)> use)
    {
        if (!block)
            return;
        auto instLoc = block->size() - 1;
        for (auto& inst : *block | std::views::reverse) {
            analyzeInstructionDefUse(instLoc, inst, def, use);
            instLoc--;
        }
    }

    bool isEntryBlock() const
    {
        if (!block)
            return false;
        return block->isEntryBlock();
    }

    template <typename F>
    void traverseSuccessors(F&& f)
    {
        if (next) {
            f(next);
        }

        if (isConditional) {
            f(trueBranch);
            f(falseBranch);
        }
    }

    template <typename F>
    void traversePredecessors(F&& f)
    {
        for (auto& pred : predecessors) {
            f(pred);
        }
    }
};

void traverseCFG(const CFGNodePtr& entry, const std::function<void(const CFGNodePtr&)>& f);

void traverseCFGReverse(const CFGNodePtr& exit, const std::function<void(const CFGNodePtr&)>& f);

void computeDefUseForLivenessAnalysis(const CFGNodePtr& exit, std::set<CFGNodePtr>& visited);

void emptyInSetReverse(const CFGNodePtr& node, std::set<CFGNodePtr>& visited);

// 活跃变量分析
void analyzeLiveness(const CFGNodePtr& exitNode);
}; // namespace sysy