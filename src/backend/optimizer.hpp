#pragma once
#include "backend/analyze.hpp"
#include "backend/instructions.hpp"
#include "backend/registers.hpp"
#include "backend/stack_frame.hpp"
#include <algorithm>
#include <deque>
#include <fstream>
#include <memory>
#include <print>
#include <ranges>
#include <stack>
#include <unordered_map>
#include <unordered_set>

using std::deque;
using std::stack;
using std::unordered_map;
using std::unordered_set;

namespace sysy {
template <typename T> unordered_set<T> unionSet(const deque<T>& a, const deque<T>& b)
{
    unordered_set<T> result(a.begin(), a.end());
    result.insert(b.begin(), b.end());
    return result;
}

class AsmOptimizer {
    StackFramePtr currentFrame;

    static constexpr int K = PHYSICAL_REGISTERS.size();

    struct NodePair {
        OperandPtr u, v;

        NodePair(OperandPtr u, OperandPtr v)
            : u(u)
            , v(v)
        {
        }

        bool operator==(const NodePair& other) const { return u == other.u && v == other.v; }
    };

    struct NodePairHash {
        size_t operator()(const NodePair& pair) const
        {
            return std::hash<OperandPtr>()(pair.u) ^ std::hash<OperandPtr>()(pair.v);
        }
    };

    AsmModulePtr asmModule;
    unordered_set<OperandPtr> precolored, initial, simplifyWorklist, freezeWorklist, spillWorklist,
        spilledNodes, coalescedNodes, coloredNodes;
    deque<OperandPtr> selectStack;
    unordered_set<shared_ptr<MovRelatedInstruction>> coalescedMoves, constrainedMoves, frozenMoves,
        worklistMoves, activeMoves;
    unordered_map<OperandPtr, unordered_set<shared_ptr<MovRelatedInstruction>>> moveList;
    unordered_map<OperandPtr, OperandPtr> alias;
    unordered_map<OperandPtr, int> degree;
    unordered_map<OperandPtr, Register> color;
    unordered_map<OperandPtr, unordered_set<OperandPtr>> adjList;
    unordered_set<NodePair, NodePairHash> adjSet;

    void prepare(AsmFunctionPtr func)
    {
        // clear all data structures
        precolored.clear();
        initial.clear();
        simplifyWorklist.clear();
        freezeWorklist.clear();
        spillWorklist.clear();
        spilledNodes.clear();
        coalescedNodes.clear();
        coloredNodes.clear();
        selectStack.clear();
        coalescedMoves.clear();
        constrainedMoves.clear();
        frozenMoves.clear();
        worklistMoves.clear();
        activeMoves.clear();
        moveList.clear();
        alias.clear();
        adjList.clear();
        adjSet.clear();
        degree.clear();
        color.clear();

        // prepare precolored and initial
        func->buildOperandMap();
        initial = func->getOperands() | std::views::filter([](const auto& operand) {
            return operand->getOperandType() == OperandType::VIRTUAL_REGISTER;
        }) | std::ranges::to<unordered_set<OperandPtr>>();
        precolored = func->getOperands() | std::views::filter([](const auto& operand) {
            return operand->getOperandType() == OperandType::REGISTER;
        }) | std::ranges::to<unordered_set<OperandPtr>>();

        const int INF = 1e9;
        std::for_each(precolored.begin(), precolored.end(), [&](const auto& operand) {
            degree[operand] = INF;
            color[operand] = std::static_pointer_cast<PhysicalRegister>(operand)->getRegister();
        });
    }

    auto inPrecolored(OperandPtr node) const -> bool { return precolored.contains(node); }

    bool hasEdge(OperandPtr u, OperandPtr v) const { return adjSet.contains(NodePair(u, v)); }

    OperandPtr createSpillSlot(string_view name, TypeInfoPtr type)
    {
        auto operand = std::make_shared<StackLocation>(std::format("{}.spill", name), type,
            currentFrame->allocateStackVar(type->getAllocSize()));
        return operand;
    }

    OperandPtr createSpillTemp(TypeInfoPtr type)
    {
        static int counter = 0;
        auto tempOperand
            = std::make_shared<VirtualRegister>("spill.temp" + std::to_string(counter++), type);
        return tempOperand;
    }

    void defOperand(OperandPtr operand, AsmInstructionPtr inst)
    {
        auto def = std::make_shared<OperandDef>(inst, operand);
        operand->addDef(def);
        inst->addDef(def);
    }

    void useOperand(OperandPtr operand, AsmInstructionPtr inst)
    {
        auto use = std::make_shared<OperandUse>(inst, operand);
        operand->addUse(use);
        inst->addUse(use);
    }

    void updateUse(OperandUsePtr oldUse, OperandPtr newOperand, AsmInstructionPtr inst)
    {
        auto oldOperand = oldUse->getOperand().lock();
        inst->replaceOperand(oldOperand, newOperand);
        inst->removeUse(oldUse);
        useOperand(newOperand, inst);
    }

    void updateDef(OperandDefPtr oldDef, OperandPtr newOperand, AsmInstructionPtr inst)
    {
        auto oldOperand = oldDef->getOperand().lock();
        inst->replaceOperand(oldOperand, newOperand);
        inst->removeDef(oldDef);
        defOperand(newOperand, inst);
    }

    int epoch = 0;

    void doOptimize(AsmFunctionPtr func)
    {
        ++epoch;
        analyzeLiveness(func);
        build(func);
        mkWorklist();
        while (!simplifyWorklist.empty() || !worklistMoves.empty() || !freezeWorklist.empty()
            || !spillWorklist.empty()) {
            if (!simplifyWorklist.empty()) {
                simplify();
            } else if (!worklistMoves.empty()) {
                coalesce();
            } else if (!freezeWorklist.empty()) {
                freeze();
            } else if (!spillWorklist.empty()) {
                selectSpill();
            }
        }
        assignColors();
        if (!spilledNodes.empty()) {
            rewriteProgram(func);
            doOptimize(func);
        }
    }

    void removeNode(OperandPtr u)
    {
        for (const auto& v : adjList[u]) {
            adjList[v].erase(u);
            degree[v]--;
        }
        adjList.erase(u);
        degree.erase(u);
        for (auto it = adjSet.begin(); it != adjSet.end();) {
            if (it->u == u || it->v == u) {
                it = adjSet.erase(it);
            } else {
                ++it;
            }
        }
    }

    void addEdge(OperandPtr u, OperandPtr v)
    {
        if (!u || !v) {
            std::println("WARNING: addEdge called with nullptr operands");
        }
        if (!u || !v || u == v)
            return;
        if (adjSet.find(NodePair(u, v)) != adjSet.end())
            return;
        adjSet.insert(NodePair(u, v));
        adjSet.insert(NodePair(v, u));
        if (!inPrecolored(u)) {
            adjList[u].insert(v);
            degree[u]++;
        }
        if (!inPrecolored(v)) {
            adjList[v].insert(u);
            degree[v]++;
        }
    }

    void build(AsmFunctionPtr func)
    {
        func->traverse([&](BasicBlockPtr block) {
            unordered_set<OperandPtr> live = block->liveOut;

            block->traverseInstructionsReverse([&](AsmInstructionPtr inst) {
                unordered_set<OperandPtr> instDefSet = inst->defs
                    | std::views::transform(
                        [](const auto& def) { return def->getOperand().lock(); })
                    | std::ranges::to<unordered_set<OperandPtr>>();
                unordered_set<OperandPtr> instUseSet = inst->uses
                    | std::views::transform(
                        [](const auto& use) { return use->getOperand().lock(); })
                    | std::ranges::to<unordered_set<OperandPtr>>();

                if (inst->isMoveRelated()) {
                    auto moveInst = std::static_pointer_cast<MovRelatedInstruction>(inst);

                    live = exceptSet(live, instUseSet);

                    for (const auto& node : unionSet(instDefSet, instUseSet)) {
                        moveList[node].insert(moveInst);
                    }

                    worklistMoves.insert(moveInst);
                }

                live = unionSet(live, instDefSet);

                for (const auto& def : instDefSet) {
                    for (const auto& liveNode : live) {
                        addEdge(liveNode, def);
                    }
                }

                live = unionSet(instUseSet, exceptSet(live, instDefSet));

                std::println("Live after {}: {}", inst->dump(),
                    live | std::views::transform([](const auto& operand) {
                        return operand->getName();
                    }) | std::views::join_with(", "s)
                        | std::ranges::to<string>());
            });
        });
        cleanup();
    }

    auto getAdjacent(OperandPtr node) const -> unordered_set<OperandPtr>
    {
        if (!adjList.contains(node)) {
            return {};
        }
        return exceptSet(adjList.at(node),
            unionSet(selectStack | std::ranges::to<unordered_set>(), coalescedNodes));
    }

    auto getNodeMoves(OperandPtr node) const -> unordered_set<shared_ptr<MovRelatedInstruction>>
    {
        if (!moveList.contains(node)) {
            return {};
        }
        return intersectSet(moveList.at(node), unionSet(activeMoves, worklistMoves));
    }

    auto isMoveRelated(OperandPtr node) const -> bool { return !getNodeMoves(node).empty(); }

    void mkWorklist()
    {
        for (const auto& node : initial) {
            if (degree[node] >= K) {
                spillWorklist.insert(node);
            } else if (isMoveRelated(node)) {
                freezeWorklist.insert(node);
            } else {
                simplifyWorklist.insert(node);
            }
        }
        initial.clear();
    }

    void simplify()
    {
        auto node = *simplifyWorklist.begin();
        simplifyWorklist = exceptSet(simplifyWorklist, { node });
        if (!isRegister(node))
            return;
        selectStack.push_back(node);
        for (const auto& m : getAdjacent(node)) {
            decrementDegree(m);
        }
    }

    void decrementDegree(OperandPtr node)
    {
        int d = degree[node];
        degree[node]--;
        if (d == K) {
            enableMoves(unionSet(getAdjacent(node), { node }));
            spillWorklist = exceptSet(spillWorklist, { node });
            if (isMoveRelated(node)) {
                freezeWorklist.insert(node);
            } else {
                simplifyWorklist.insert(node);
            }
        }
    }

    void enableMoves(const unordered_set<OperandPtr>& nodes)
    {
        for (const auto& node : nodes) {
            for (const auto& move : getNodeMoves(node)) {
                if (activeMoves.contains(move)) {
                    activeMoves = exceptSet(activeMoves, { move });
                    worklistMoves.insert(move);
                }
            }
        }
    }

    bool isRegister(OperandPtr node) const
    {
        return node->getOperandType() == OperandType::REGISTER
            || node->getOperandType() == OperandType::VIRTUAL_REGISTER;
    }

    void cleanup()
    {
        auto removeNodes = adjList
            | std::views::transform([](const auto& pair) { return pair.first; })
            | std::views::filter(
                [](const auto& operand) { return operand->isImmediate() || operand->isMemory(); })
            | std::ranges::to<unordered_set<OperandPtr>>();
        auto removeInsts = worklistMoves | std::views::filter([&](const auto& move) {
            return removeNodes.contains(move->getSrc()) || removeNodes.contains(move->getDest());
        }) | std::ranges::to<unordered_set<shared_ptr<MovRelatedInstruction>>>();
        for (const auto& node : removeNodes) {
            removeNode(node);
            if (moveList.contains(node)) {
                moveList.erase(node);
            }
        }
        worklistMoves = exceptSet(worklistMoves, removeInsts);
        for (const auto& [node, _] : moveList) {
            moveList[node] = exceptSet(moveList[node], removeInsts);
        }
    }

    void coalesce()
    {
        auto move = *worklistMoves.begin();
        worklistMoves = exceptSet(worklistMoves, { move });
        auto x = getAlias(move->getSrc());
        auto y = getAlias(move->getDest());
        // if (!isRegister(x) || !isRegister(y)) {
        //     return;
        // }
        if (x->isImmediate() || y->isImmediate()) {
            return;
        }
        auto u = x;
        auto v = y;
        if (inPrecolored(y)) {
            u = y;
            v = x;
        }
        std::println("Coalescing {}({}) -> {}({}), inst: {}", u->getName(), u->dump(true),
            v->getName(), v->dump(true), move->dump());
        worklistMoves = exceptSet(worklistMoves, { move });
        if (u == v) {
            coalescedMoves.insert(move);
            addWorkList(u);
        } else if (inPrecolored(v) || hasEdge(u, v)) {
            constrainedMoves.insert(move);
            addWorkList(u);
            addWorkList(v);
        } else if ((inPrecolored(u)
                       && std::ranges::all_of(
                           getAdjacent(v), [&](const auto& t) { return ok(t, u); }))
            || (!inPrecolored(u) && conservative(unionSet(getAdjacent(u), getAdjacent(v))))) {
            coalescedMoves.insert(move);
            combine(u, v);
            addWorkList(u);
        } else {
            activeMoves.insert(move);
        }
        cleanup();
    }

    void addWorkList(OperandPtr node)
    {
        if (!precolored.contains(node) && !isMoveRelated(node) && degree[node] < K) {
            freezeWorklist = exceptSet(freezeWorklist, { node });
            simplifyWorklist.insert(node);
        }
    }

    bool ok(OperandPtr t, OperandPtr r)
    {
        return degree[t] < K || inPrecolored(t) || hasEdge(t, r);
    }

    auto conservative(const unordered_set<OperandPtr>& nodes) -> bool
    {
        return std::ranges::count_if(nodes, [&](const auto& node) { return degree[node] >= K; })
            < K;
    }

    auto getAlias(OperandPtr node) const -> OperandPtr
    {
        if (coalescedNodes.contains(node)) {
            return getAlias(alias.at(node));
        } else {
            return node;
        }
    }

    void combine(OperandPtr u, OperandPtr v)
    {
        if (freezeWorklist.contains(v)) {
            freezeWorklist = exceptSet(freezeWorklist, { v });
        } else {
            spillWorklist = exceptSet(spillWorklist, { v });
        }
        coalescedNodes.insert(v);
        alias[v] = u;
        moveList[u] = unionSet(moveList[u], moveList[v]);
        for (const auto& t : getAdjacent(v)) {
            addEdge(t, u);
            decrementDegree(t);
        }
        if (degree[u] >= K && freezeWorklist.contains(u)) {
            freezeWorklist = exceptSet(freezeWorklist, { u });
            spillWorklist.insert(u);
        }
    }

    void freeze()
    {
        auto node = *freezeWorklist.begin();
        freezeWorklist = exceptSet(freezeWorklist, { node });
        if (!isRegister(node))
            return;
        simplifyWorklist.insert(node);
        freezeMoves(node);
    }

    void freezeMoves(OperandPtr u)
    {
        for (const auto& move : getNodeMoves(u)) {
            if (activeMoves.contains(move)) {
                activeMoves = exceptSet(activeMoves, { move });
            } else {
                worklistMoves = exceptSet(worklistMoves, { move });
            }

            frozenMoves.insert(move);

            auto v = move->getSrc() == u ? move->getDest() : move->getSrc();

            if (getNodeMoves(v).empty() && degree[v] < K) {
                freezeWorklist = exceptSet(freezeWorklist, { v });
                simplifyWorklist.insert(v);
            }
        }
    }

    OperandPtr selectOperandToSpill()
    {
        // TODO: implement a better heuristic
        return *std::ranges::max_element(
            spillWorklist, [&](const auto& a, const auto& b) { return degree[a] < degree[b]; });
    }

    void selectSpill()
    {
        auto node = selectOperandToSpill();
        spillWorklist = exceptSet(spillWorklist, { node });
        if (!isRegister(node))
            return;
        simplifyWorklist.insert(node);
        freezeMoves(node);
    }

    void assignColors()
    {
        while (!selectStack.empty()) {
            auto node = selectStack.back();
            selectStack.pop_back();
            // if (!isRegister(node))
            //     continue;
            auto okColors
                = std::deque<Register>(PHYSICAL_REGISTERS.begin(), PHYSICAL_REGISTERS.end());
            if (adjList.contains(node)) {
                for (const auto& w : adjList.at(node)) {
                    auto alias = getAlias(w);
                    if (coloredNodes.contains(alias) || precolored.contains(alias)) {
                        okColors.erase(std::remove(okColors.begin(), okColors.end(), color[alias]),
                            okColors.end());
                    }
                }
            }
            if (okColors.empty()) {
                spilledNodes.insert(node);
            } else {
                coloredNodes.insert(node);
                auto selectedColor = okColors.front();
                okColors.pop_front();
                color[node] = selectedColor;
            }
        }
        for (const auto& node : coalescedNodes) {
            color[node] = color[getAlias(node)];
        }
    }

    void rewriteProgram(AsmFunctionPtr func)
    {
        unordered_set<OperandPtr> newTemps;

        for (const auto& node : spilledNodes) {
            auto spillSlot = createSpillSlot(node->getName(), node->getType());
            // std::println(
            //     "Spilled {}({}) at {}", node->getName(), node->dump(true),
            //     spillSlot->dump(true));
            auto oldUses = node->getUses();
            for (auto& use : oldUses) {
                auto inst = use->getInst();
                if (std::dynamic_pointer_cast<FakeInstruction>(inst))
                    continue;
                auto temp = createSpillTemp(use->getOperand().lock()->getType());
                newTemps.insert(temp);
                // std::println("Rewriting use of {}({}) to {}({}) at inst ({})", node->getName(),
                //     node->dump(true), temp->getName(), temp->dump(true), inst->dump());
                auto loadInst = std::make_shared<MovInstruction>(temp, spillSlot);
                defOperand(temp, loadInst);
                useOperand(spillSlot, loadInst);
                inst->parent.lock()->insertInstructionBefore(loadInst, inst);
                updateUse(use, temp, inst);
            }
            node->clearUses();
            auto oldDefs = node->getDefs();
            for (auto& def : oldDefs) {
                auto inst = def->getInst();
                if (std::dynamic_pointer_cast<FakeInstruction>(inst))
                    continue;
                auto temp = createSpillTemp(def->getOperand().lock()->getType());
                newTemps.insert(temp);
                // std::println("Rewriting def of {}({}) to {}({}) at inst ({})", node->getName(),
                //     node->dump(true), temp->getName(), temp->dump(true), inst->dump());
                auto storeInst = std::make_shared<MovInstruction>(spillSlot, temp);
                useOperand(temp, storeInst);
                defOperand(spillSlot, storeInst);
                inst->parent.lock()->insertInstructionAfter(storeInst, inst);
                updateDef(def, temp, inst);
            }
            node->clearDefs();
        }

        spilledNodes.clear();
        initial = unionSet(coloredNodes, unionSet(coalescedNodes, newTemps));
        coloredNodes.clear();
        coalescedNodes.clear();
    }

public:
    AsmOptimizer(AsmModulePtr asmModule)
        : asmModule(asmModule)
    {
    }

    void printColorForDbg() const
    {
        for (auto [u, _] : adjList) {
            if (color.contains(u) && color.at(u) != Register::INVALID) {
                std::println("{}: {}", u->getName(), REGISTER_NAMES[static_cast<int>(color.at(u))]);
            } else if (auto physicalReg = std::dynamic_pointer_cast<PhysicalRegister>(u)) {
                std::println("{}: ({})", physicalReg->getName(), physicalReg->dump(true));
            } else if (u->isMemory()) {
                std::println("{}: ({})", u->getName(), u->dump(true));
            } else {
                std::println("{}: ({}), degree: {}", u->getName(), u->dump(true), degree.at(u));
            }
        }
    }

    void printGraphForDbg() const
    {
        std::ofstream ofs("graph.dot");
        ofs << "graph G {\n";
        for (const auto& [u, adj] : adjList) {
            for (const auto& v : adj) {
                ofs << "\"" << u->getName() << "\""
                    << " -- "
                    << "\"" << v->getName() << "\""
                    << ";\n";
            }
        }
        ofs << "}\n";
        ofs.close();
    }

    void optimize(AsmFunctionPtr func)
    {
        prepare(func);
        doOptimize(func);
    }

    unordered_map<string, OperandPtr> physicalRegisters;

    OperandPtr getPhysicalRegister(OperandPtr v)
    {
        if (physicalRegisters.contains(v->getName())) {
            return physicalRegisters.at(v->getName());
        }
        if (color.contains(v) && color.at(v) != Register::INVALID) {
            if (v->getName() == "t12") {
                std::println("");
            }
            auto reg = color.at(v);
            auto physicalReg
                = std::make_shared<PhysicalRegister>(v->getName(), reg, v->getType()->clone());
            physicalRegisters[v->getName()] = physicalReg;
            return physicalReg;
        }
        throw std::runtime_error("No physical register found for " + v->getName());
    }

    void doCoalescing(AsmFunctionPtr func)
    {
        unordered_set<AsmInstructionPtr> removeInsts;
        for (auto v : coalescedNodes) {
            auto u = getAlias(v);
            v->replaceBy(u);
        }
        func->traverse([&](BasicBlockPtr block) {
            block->traverseInstructions([&](AsmInstructionPtr inst) {
                if (auto movInst = std::dynamic_pointer_cast<MovRelatedInstruction>(inst)) {
                    auto src = movInst->getSrc();
                    auto dest = movInst->getDest();
                    if (*src == *dest) {
                        removeInsts.insert(inst);
                    }
                }
            });
        });
        for (const auto& inst : removeInsts) {
            inst->parent.lock()->removeInstruction(inst);
        }
    }

    void assignPhysicalRegisters(AsmFunctionPtr func)
    {
        doCoalescing(func);
        for (auto [u, _] : adjList) {
            auto v = getAlias(u);
            if (color.contains(v) && color.at(v) != Register::INVALID) {
                auto physicalReg = getPhysicalRegister(v);
                u->replaceBy(physicalReg);
            }
        }
        doCoalescing(func);
    }

    void optimize()
    {
        for (auto& func : asmModule->getFunctions()) {
            physicalRegisters.clear();
            currentFrame = func->getStackFrame();
            optimize(func);
            // prepare(func);
            // analyzeLiveness(func);
            // build(func);
            // mkWorklist();
            // while (!simplifyWorklist.empty() || !worklistMoves.empty() || !freezeWorklist.empty()
            //     || !spillWorklist.empty()) {
            //     if (!simplifyWorklist.empty()) {
            //         simplify();
            //     } else if (!worklistMoves.empty()) {
            //         coalesce();
            //     } else if (!freezeWorklist.empty()) {
            //         freeze();
            //     } else if (!spillWorklist.empty()) {
            //         selectSpill();
            //     }
            // }
            // assignColors();
            printColorForDbg();
            assignPhysicalRegisters(func);
        }
    }
};
}