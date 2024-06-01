#include "cfg.hpp"

namespace sysy {
void traverseCFG(const CFGNodePtr& entry, const std::function<void(const CFGNodePtr&)>& f)
{
    std::set<CFGNodePtr> visited;
    std::function<void(const CFGNodePtr&)> visit = [&](const CFGNodePtr& node) {
        if (visited.contains(node)) {
            return;
        }
        visited.insert(node);
        f(node);
        node->traverseSuccessors(visit);
    };
    visit(entry);
}

void traverseCFGReverse(const CFGNodePtr& exit, const std::function<void(const CFGNodePtr&)>& f)
{
    std::set<CFGNodePtr> visited;
    std::function<void(const CFGNodePtr&)> visit = [&](const CFGNodePtr& node) {
        if (visited.contains(node)) {
            return;
        }
        visited.insert(node);
        f(node);
        node->traversePredecessors(visit);
    };
    visit(exit);
}

void computeDefUseForLivenessAnalysis(const CFGNodePtr& exit)
{
    traverseCFGReverse(exit, [&](const CFGNodePtr& node) {
        node->traverseDefUse(
            [&](int loc, string_view def) {
                if (!node->useSet.contains(def.data())) {
                    node->defSet.insert(def.data());
                }
            },
            [&](int loc, string_view use, string_view user) {
                if (!node->defSet.contains(use.data())) {
                    node->useSet.insert(use.data());
                }
            });
    });
}

void emptyInSetReverse(const CFGNodePtr& node)
{
    traverseCFGReverse(node, [&](const CFGNodePtr& node) {
        node->inSet.clear();
    });
}

void analyzeLiveness(const CFGNodePtr& exitNode)
{
    computeDefUseForLivenessAnalysis(exitNode);
    // emptyInSetReverse(exitNode);

    auto visit = [&](const CFGNodePtr& node) {
        auto oldIn = node->inSet;

        node->outSet.clear();
        node->traverseSuccessors([&](const CFGNodePtr& succ) {
            node->outSet = unionSet(node->outSet, succ->inSet);
        });

        node->inSet = unionSet(node->useSet, exceptSet(node->outSet, node->defSet));

        return oldIn != node->inSet;
    };

    bool changed = true;

    while (changed) {
        changed = false;
        traverseCFGReverse(exitNode, [&](const CFGNodePtr& node) {
            changed |= visit(node);
        });
    }
}
}