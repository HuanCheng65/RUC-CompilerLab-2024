#include "local_var.hpp"
#include "cfg.hpp"

namespace sysy {
void analyzeInstructionLiveness(CFGNodePtr node, InterferenceGraph& graph)
{
    std::unordered_set<std::string> live = node->outSet;

    node->traverseInstructionsReverse([&](int loc, const llvm::Instruction& inst) {
        unordered_set<string> instDefSet, instUseSet;

        node->analyzeInstructionDefUse(loc, inst, [&](int loc, string_view def) { instDefSet.insert(def.data()); }, [&](int loc, string_view use, string_view user) { instUseSet.insert(use.data()); });

        for (const auto& use : instUseSet) {
            live.insert(use);
        }

        for (const auto& def : instDefSet) {
            for (const auto& var : live) {
                graph.addEdge(def, var);
            }
            live.erase(def);
        }
    });
}

void buildInterferenceGraph(CFGNodePtr cfgRoot, InterferenceGraph& graph)
{
    traverseCFG(cfgRoot, [&](const CFGNodePtr& node) {
        analyzeInstructionLiveness(node, graph);
    });
}
}