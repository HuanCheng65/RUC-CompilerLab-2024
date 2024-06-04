#include "analyze.hpp"
#include "backend/instructions.hpp"
#include <print>

namespace sysy {
void computeDefUseForLivenessAnalysis(AsmFunctionPtr func)
{
    func->traverseReverse([&](const auto& block) {
        block->defSet.clear();
        block->useSet.clear();

        block->traverseInstructions([&](const auto& inst) {
            for (const auto& use : inst->getUses()) {
                if (!block->defSet.contains(use->getOperand().lock())) {
                    block->useSet.insert(use->getOperand().lock());
                }
            }

            for (const auto& def : inst->getDefs()) {
                if (!block->useSet.contains(def->getOperand().lock())) {
                    block->defSet.insert(def->getOperand().lock());
                }
            }
        });

        // std::println("Block: {}", block->getName());
        // std::println("Def: {}",
        //     block->defSet | std::views::transform([](const auto& op) { return op->getName(); })
        //         | std::views::join_with(", "s) | std::ranges::to<std::string>());
        // std::println("Use: {}",
        //     block->useSet | std::views::transform([](const auto& op) { return op->getName(); })
        //         | std::views::join_with(", "s) | std::ranges::to<std::string>());
    });
}

void emptySets(AsmFunctionPtr func)
{
    func->traverse([&](const auto& block) { block->liveIn.clear(); });
}

void analyzeLiveness(AsmFunctionPtr func)
{
    computeDefUseForLivenessAnalysis(func);

    emptySets(func);

    auto visit = [&](const auto& block) {
        auto oldIn = block->liveIn;

        block->liveOut.clear();
        block->traverseSuccessors(
            [&](const auto& succ) { block->liveOut = unionSet(block->liveOut, succ->liveIn); });

        block->liveIn = unionSet(block->useSet, exceptSet(block->liveOut, block->defSet));

        return oldIn != block->liveIn;
    };

    bool changed = true;
    while (changed) {
        changed = false;
        func->traverseReverse([&](const auto& block) { changed |= visit(block); }, false);
    }

    // // print liveness analysis result
    // func->traverse([&](const auto& block) {
    //     std::println("Block: {}", block->getName());
    //     std::println("In: {}",
    //         block->liveIn | std::views::transform([](const auto& op) { return op->getName(); })
    //             | std::views::join_with(", "s) | std::ranges::to<std::string>());
    //     std::println("Out: {}",
    //         block->liveOut | std::views::transform([](const auto& op) { return op->getName(); })
    //             | std::views::join_with(" "s) | std::ranges::to<std::string>());
    // });
}
}