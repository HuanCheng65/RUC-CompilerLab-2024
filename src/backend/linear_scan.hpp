#include "backend/analyze.hpp"
#include "backend/instructions.hpp"
#include "backend/registers.hpp"
#include "backend/stack_frame.hpp"
#include "backend/utils.hpp"
#include <algorithm>
#include <deque>
#include <memory>
#include <print>
#include <queue>
#include <ranges>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <variant>

using std::multiset;
using std::priority_queue;
using std::set;
using std::unordered_map;
using std::unordered_set;

namespace sysy {
struct LiveRange {
    int start;
    int end;
    OperandPtr operand;

    // sort by end
    bool operator<(const LiveRange& other) const { return end < other.end; }

    bool operator==(const LiveRange& other) const
    {
        return start == other.start && end == other.end && operand == other.operand;
    }
};

struct RegisterAllocation {
    Register reg;
    int start;
    int end;

    // sort by end
    bool operator<(const LiveRange& other) const { return end < other.end; }
};

struct MemoryAllocation {
    StackLocationBase base;
    OperandPtr spillSlot, saveRegSlot;
};

class LinearScanAllocator {
    unordered_map<OperandPtr, LiveRange> operandLiveRange;
    unordered_map<AsmInstructionPtr, int> instToId;
    unordered_map<int, AsmInstructionPtr> idToInst;
    unordered_map<Register, bool> usedRegisters;
    unordered_map<Register, OperandPtr> operandUsingRegister;
    unordered_map<OperandPtr, Register> registers;
    unordered_map<OperandPtr, MemoryAllocation> locations;
    unordered_map<int, set<Register>> availableRegisters;
    multiset<LiveRange> active;
    StackFramePtr currentFrame;

    void livenessAnalysis(AsmFunctionPtr func)
    {
        analyzeLiveness(func); // analyze on block level
        func->traverse([&](const auto& block) {
            unordered_set<OperandPtr> live = block->liveOut;

            block->traverseInstructionsReverse([&](const auto& inst) {
                unordered_set<OperandPtr> instDefSet = inst->defs
                    | std::views::transform(
                        [](const auto& def) { return def->getOperand().lock(); })
                    | std::ranges::to<unordered_set>();
                unordered_set<OperandPtr> instUseSet = inst->uses
                    | std::views::transform(
                        [](const auto& use) { return use->getOperand().lock(); })
                    | std::ranges::to<unordered_set>();

                live = unionSet(instUseSet, exceptSet(live, instDefSet));

                auto instLoc = instToId[inst];

                for (const auto& def : instDefSet) {
                    if (operandLiveRange.find(def) == operandLiveRange.end()) {
                        operandLiveRange[def] = { instLoc, instLoc, def };
                    }
                    operandLiveRange[def].start = std::min(instLoc, operandLiveRange[def].start);
                    operandLiveRange[def].end = std::max(instLoc, operandLiveRange[def].end);
                }

                for (const auto& use : instUseSet) {
                    if (operandLiveRange.find(use) == operandLiveRange.end()) {
                        operandLiveRange[use] = { instLoc, instLoc, use };
                    }
                    operandLiveRange[use].start = std::min(instLoc, operandLiveRange[use].start);
                    operandLiveRange[use].end = std::max(instLoc, operandLiveRange[use].end);
                }

                for (const auto& liveOperand : live) {
                    if (operandLiveRange.find(liveOperand) == operandLiveRange.end()) {
                        operandLiveRange[liveOperand] = { instLoc, instLoc, liveOperand };
                    }
                    operandLiveRange[liveOperand].start
                        = std::min(instLoc, operandLiveRange[liveOperand].start);
                    operandLiveRange[liveOperand].end
                        = std::max(instLoc, operandLiveRange[liveOperand].end);
                }
            });
        });
        fmt::print("Liveness analysis done for function {}\n", func->getName());
        for (const auto& [operand, range] : operandLiveRange) {
            if (operand->isMemory() || operand->isImmediate())
                continue;
            fmt::print("  Operand: {}\n", operand->getName());
            fmt::print("  Live range: [{}, {}]\n", range.start, range.end);
        }
    }

    set<Register> getAvailableRegisters() const
    {
        return PHYSICAL_REGISTERS | std::views::filter([&](const auto& reg) {
            return !usedRegisters.contains(reg) || !usedRegisters.at(reg);
        }) | std::ranges::to<set>();
    }

    Register preAllocateRegister(OperandPtr operand) const
    {
        auto availableRegisters = getAvailableRegisters();
        if (auto regOp = std::dynamic_pointer_cast<PhysicalRegister>(operand)) {
            if (!availableRegisters.contains(regOp->reg)) {
                return Register::INVALID;
            }
            return regOp->reg;
        } else {
            if (availableRegisters.empty()) {
                return Register::INVALID;
            }
            auto reg = *availableRegisters.begin();
            return reg;
        }
    }

    void allocateRegister(OperandPtr operand, Register reg)
    {
        registers[operand] = reg;
        usedRegisters[reg] = true;
        operandUsingRegister[reg] = operand;
    }

    void freeRegister(Register reg)
    {
        usedRegisters[reg] = false;
        operandUsingRegister.erase(reg);
    }

    void expireOldRanges(int currentId)
    {
        while (!active.empty() && active.begin()->end < currentId) {
            auto [start, end, operand] = *active.begin();
            active.erase(active.begin());
            freeRegister(registers.at(operand));
            std::println("Freeing register {} for {} at {}", getRegisterName(registers.at(operand)),
                operand->getName(), end);
        }
    }

    bool canReallocateOrSpill(OperandPtr operand)
    {
        return operand->getOperandType() == OperandType::VIRTUAL_REGISTER;
    }

    bool reallocateOrSpill(OperandPtr operand, int instLoc)
    {
        if (!canReallocateOrSpill(operand)) {
            return false;
        }
        // auto availableRegisters = getAvailableRegisters();
        // if (!availableRegisters.empty()) {
        //     auto reg = *availableRegisters.begin();
        //     allocateRegister(operand, reg);
        //     std::println(
        //         "Reallocating {} to {} at {}", operand->getName(), getRegisterName(reg),
        //         instLoc);
        //     return true;
        // } else {
        //     spillToMemory(operand);
        //     std::println("Spilling {} at {}", operand->getName(), instLoc);
        //     return true;
        // }
        spillToMemory(operand);
        std::println("Spilling {} at {}", operand->getName(), instLoc);
        return true;
    }

    void spillAt(LiveRange current)
    {
        std::deque<LiveRange> liveRanges;
        LiveRange spill = *active.begin();
        bool hasSpill = canReallocateOrSpill(spill.operand);
        while (!hasSpill && !active.empty()) {
            liveRanges.push_back(spill);
            active.erase(active.begin());
            if (active.empty()) {
                break;
            }
            spill = *active.begin();
            hasSpill = canReallocateOrSpill(spill.operand);
        }
        if (hasSpill && spill.end > current.end && canReallocateOrSpill(current.operand)) {
            allocateRegister(current.operand, registers.at(spill.operand));
            std::println("Allocating {} to {} at {}", current.operand->getName(),
                getRegisterName(registers.at(spill.operand)), current.start);
            spillToMemory(spill.operand);
            std::println("Spilling {} at {}", spill.operand->getName(), current.start);
            active.erase(active.begin());
            active.insert(current);
        } else if (canReallocateOrSpill(current.operand)) {
            spillToMemory(current.operand);
            std::println("Spilling {} at {}", current.operand->getName(), current.start);
        } else {
            auto regOp = std::static_pointer_cast<PhysicalRegister>(current.operand);
            auto reg = regOp->getRegister();
            auto usingOp = operandUsingRegister.at(reg);
            allocateRegister(current.operand, reg);
            std::println("Allocating {} to {} at {}", current.operand->getName(),
                getRegisterName(reg), current.start);
            if (!reallocateOrSpill(usingOp, current.start)) {
                throw std::runtime_error(std::format("Cannot reallocate or spill {} for {} at {}",
                    usingOp->getName(), current.operand->getName(), current.start));
            }
            if (auto it = std::find_if(active.begin(), active.end(),
                    [&](const auto& range) { return range.operand == usingOp; });
                it != active.end()) {
                active.erase(it);
            } else if (auto it = std::find_if(liveRanges.begin(), liveRanges.end(),
                           [&](const auto& range) { return range.operand == usingOp; });
                       it != liveRanges.end()) {
                liveRanges.erase(it);
            }
            active.insert(current);
        }
        while (!liveRanges.empty()) {
            active.insert(liveRanges.front());
            liveRanges.pop_front();
        }
    }

    void spillToMemory(OperandPtr operand)
    {
        registers.erase(operand);
        auto stackLoc = currentFrame->allocateStackVar(operand->getType()->getAllocSize());
        auto saveRegLoc = currentFrame->allocateStackVar(8);
        auto slot = std::make_shared<StackLocation>(
            operand->getName(), operand->getType(), stackLoc, StackLocationBase::RBP);
        auto saveRegSlot = std::make_shared<StackLocation>(operand->getName() + ".save",
            TypeInfo::createPrimitive(8), saveRegLoc, StackLocationBase::RBP);
        locations[operand] = { StackLocationBase::RBP, slot, saveRegSlot };
    }

    Register selectTempRegister(int instLoc) const
    {
        auto candidates = PHYSICAL_REGISTERS | std::ranges::to<std::vector>();
        auto inst = idToInst.at(instLoc);
        for (const auto& use : inst->getUses()) {
            auto operand = use->getOperand().lock();
            if (operand->isMemory() || operand->isImmediate())
                continue;
            if (registers.contains(operand)) {
                candidates.erase(
                    std::remove(candidates.begin(), candidates.end(), registers.at(operand)),
                    candidates.end());
            }
            if (auto regOp = std::dynamic_pointer_cast<PhysicalRegister>(operand)) {
                candidates.erase(
                    std::remove(candidates.begin(), candidates.end(), regOp->getRegister()),
                    candidates.end());
            }
        }
        for (const auto& def : inst->getDefs()) {
            auto operand = def->getOperand().lock();
            if (operand->isMemory() || operand->isImmediate())
                continue;
            if (registers.contains(operand)) {
                candidates.erase(
                    std::remove(candidates.begin(), candidates.end(), registers.at(operand)),
                    candidates.end());
            }
            if (auto regOp = std::dynamic_pointer_cast<PhysicalRegister>(operand)) {
                candidates.erase(
                    std::remove(candidates.begin(), candidates.end(), regOp->getRegister()),
                    candidates.end());
            }
        }
        if (candidates.empty()) {
            std::println(
                "WARNING: No available register for temporary use at instruction {}", inst->dump());
            return PHYSICAL_REGISTERS[0];
        } else {
            return candidates[0];
        }
    }

    OperandPtr createSpillTemp(int instLoc, TypeInfoPtr typeInfo) const
    {
        auto temp = std::make_shared<PhysicalRegister>(
            "spill.temp", selectTempRegister(instLoc), typeInfo);
        return temp;
    }

public:
    void allocate(AsmFunctionPtr func)
    {
        operandLiveRange.clear();
        instToId.clear();
        idToInst.clear();
        usedRegisters.clear();
        operandUsingRegister.clear();
        registers.clear();
        locations.clear();
        active.clear();
        currentFrame = func->getStackFrame();

        int instLoc = 0;
        func->traverse([&](const auto& block) {
            block->traverseInstructions([&](const auto& inst) {
                idToInst[instLoc] = inst;
                instToId[inst] = instLoc;
                instLoc++;
            });
        });

        livenessAnalysis(func);

        auto liveRanges = operandLiveRange | std::views::values
            | std::views::filter([&](const auto& range) {
                  return !range.operand->isMemory() && !range.operand->isImmediate();
              })
            | std::ranges::to<std::vector>();

        std::sort(liveRanges.begin(), liveRanges.end(),
            [](const auto& a, const auto& b) { return a.start < b.start; });

        for (const auto& range : liveRanges) {
            expireOldRanges(range.start);

            if (range.operand->getName().starts_with("call.arg")) {
                std::println("Argument {}", range.operand->getName());
            }

            auto reg = preAllocateRegister(range.operand);
            if (reg == Register::INVALID) {
                spillAt(range);
            } else {
                std::println("Allocating {} to {} at {}", range.operand->getName(),
                    getRegisterName(reg), range.start);
                allocateRegister(range.operand, reg);
                active.insert(range);
            }
        }

        assignRegisters(func);
    }

    set<Register> getUsedRegisters() const
    {
        return registers | std::views::values | std::ranges::to<set>();
    }

    using OperandDefOrUse = std::tuple<AsmInstructionPtr, OperandPtr, std::optional<OperandDefPtr>,
        std::optional<OperandUsePtr>>;

    void doSpill(AsmFunctionPtr func)
    {
        for (const auto& [operand, loc] : locations) {
            std::println("Operand: {}, StackLoc: {}, SaveRegLoc: {}", operand->getName(),
                loc.spillSlot->dump(false), loc.saveRegSlot->dump(false));
            auto spillSlot = loc.spillSlot;
            auto saveRegSlot = loc.saveRegSlot;
            auto oldUses = operand->getUses()
                | std::views::transform([](const auto& use) -> OperandDefOrUse {
                      return std::make_tuple(
                          use->getInst(), use->getOperand().lock(), std::nullopt, use);
                  })
                | std::ranges::to<set>();
            auto oldDefs = operand->getDefs()
                | std::views::transform([&](const auto& oldDef) -> OperandDefOrUse {
                      auto inst = oldDef->getInst();
                      auto useAtDef = std::find_if(oldUses.begin(), oldUses.end(),
                          [&](const auto& use) { return inst == std::get<0>(use); });
                      if (useAtDef != oldUses.end()) {
                          return std::make_tuple(
                              inst, oldDef->getOperand().lock(), oldDef, std::get<3>(*useAtDef));
                      }
                      return std::make_tuple(
                          inst, oldDef->getOperand().lock(), oldDef, std::nullopt);
                  })
                | std::ranges::to<set>();
            auto oldUseAtDefs = oldDefs | std::views::filter([&](const auto& oldDef) {
                auto& [inst, op, _, use] = oldDef;
                return use.has_value();
            });
            auto useAtDefInsts = oldUseAtDefs
                | std::views::transform([](const auto& def) { return std::get<0>(def); })
                | std::ranges::to<set>();
            if (!oldUseAtDefs.empty()) {
                std::println("Operand {} has uses at defs", operand->getName());
            }
            for (auto& oldUse : oldUses) {
                auto& [inst, op, _, use] = oldUse;
                if (std::dynamic_pointer_cast<FakeInstruction>(inst))
                    continue;
                if (useAtDefInsts.contains(inst))
                    continue;
                auto instLoc = instToId[inst];
                auto saveRegTemp = createSpillTemp(instLoc, TypeInfo::createPrimitive(8));
                auto loadTemp = std::make_shared<PhysicalRegister>("spill.temp",
                    std::static_pointer_cast<PhysicalRegister>(saveRegTemp)->getRegister(),
                    op->getType());
                auto parent = inst->parent.lock();
                auto loadInst = std::make_shared<MovInstruction>(loadTemp, spillSlot);
                parent->insertInstructionBefore(loadInst, inst);
                parent->insertInstructionBefore(
                    std::make_shared<MovInstruction>(saveRegSlot, saveRegTemp), loadInst);
                parent->insertInstructionAfter(
                    std::make_shared<MovInstruction>(saveRegTemp, saveRegSlot), inst);
                inst->replaceOperand(op, loadTemp);
                auto newUse = std::make_shared<OperandUse>(inst, loadTemp);
                loadTemp->addUse(newUse);
                inst->addUse(newUse);
            }
            for (auto& oldDef : oldDefs) {
                auto& [inst, op, def, use] = oldDef;
                if (std::dynamic_pointer_cast<FakeInstruction>(inst))
                    continue;

                if (useAtDefInsts.contains(inst)) {
                    std::println("  Use at def: {}", inst->dump());
                    auto instLoc = instToId[inst];
                    auto saveRegTemp = createSpillTemp(instLoc, TypeInfo::createPrimitive(8));
                    auto loadAndStoreTemp = std::make_shared<PhysicalRegister>("spill.temp",
                        std::static_pointer_cast<PhysicalRegister>(saveRegTemp)->getRegister(),
                        op->getType());
                    auto parent = inst->parent.lock();
                    auto loadInst = std::make_shared<MovInstruction>(loadAndStoreTemp, spillSlot);
                    auto storeInst = std::make_shared<MovInstruction>(spillSlot, loadAndStoreTemp);
                    parent->insertInstructionBefore(
                        std::make_shared<MovInstruction>(saveRegSlot, saveRegTemp), inst);
                    parent->insertInstructionBefore(loadInst, inst);
                    parent->insertInstructionAfter(storeInst, inst);
                    parent->insertInstructionAfter(
                        std::make_shared<MovInstruction>(saveRegTemp, saveRegSlot), storeInst);
                    inst->replaceOperand(op, loadAndStoreTemp);
                    auto newDef = std::make_shared<OperandDef>(inst, loadAndStoreTemp);
                    auto newUse = std::make_shared<OperandUse>(inst, loadAndStoreTemp);
                    loadAndStoreTemp->addDef(newDef);
                    loadAndStoreTemp->addUse(newUse);
                    inst->addDef(newDef);
                    inst->addUse(newUse);
                    inst->removeDef(def.value());
                    inst->removeUse(use.value());
                } else {
                    auto instLoc = instToId[inst];
                    auto saveRegTemp = createSpillTemp(instLoc, TypeInfo::createPrimitive(8));
                    auto storeTemp = std::make_shared<PhysicalRegister>("spill.temp",
                        std::static_pointer_cast<PhysicalRegister>(saveRegTemp)->getRegister(),
                        op->getType());
                    auto parent = inst->parent.lock();
                    auto storeInst = std::make_shared<MovInstruction>(spillSlot, storeTemp);
                    parent->insertInstructionBefore(
                        std::make_shared<MovInstruction>(saveRegSlot, saveRegTemp), inst);
                    parent->insertInstructionAfter(storeInst, inst);
                    parent->insertInstructionAfter(
                        std::make_shared<MovInstruction>(saveRegTemp, saveRegSlot), storeInst);
                    inst->replaceOperand(op, storeTemp);
                    auto newDef = std::make_shared<OperandDef>(inst, storeTemp);
                    storeTemp->addDef(newDef);
                    inst->addDef(newDef);
                    inst->removeDef(def.value());
                }
            }
            operand->clearUses();
            operand->clearDefs();
        }
        vector<AsmInstructionPtr> toRemove;
        func->traverse([&](const auto& block) {
            block->traverseInstructions([&](const auto& inst) {
                if (auto movInst = std::dynamic_pointer_cast<MovRelatedInstruction>(inst)) {
                    if (movInst->getDest()->getOperandType() == OperandType::VIRTUAL_REGISTER) {
                        toRemove.push_back(inst);
                    }
                }
            });
        });
        for (const auto& inst : toRemove) {
            inst->parent.lock()->removeInstruction(inst);
        }
    }

    void assignRegisters(AsmFunctionPtr func)
    {
        std::println("Assigning registers for function {}", func->getName());
        for (const auto& [operand, reg] : registers) {
            std::println("Operand: {}, Register: {}", operand->getName(), getRegisterName(reg));
            auto physicalReg
                = std::make_shared<PhysicalRegister>(operand->getName(), reg, operand->getType());
            operand->replaceBy(physicalReg);
        }
        vector<AsmInstructionPtr> toRemove;
        func->traverse([&](const auto& block) {
            block->traverseInstructions([&](const auto& inst) {
                if (auto movInst = std::dynamic_pointer_cast<MovRelatedInstruction>(inst)) {
                    auto src = movInst->getSrc();
                    auto dest = movInst->getDest();
                    if (*src == *dest) {
                        toRemove.push_back(inst);
                    }
                }
            });
        });
        for (const auto& inst : toRemove) {
            inst->parent.lock()->removeInstruction(inst);
        }
    }
};
}