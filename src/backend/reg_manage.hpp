#pragma once
#include "registers.hpp"
#include "utils.hpp"
#include <array>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace sysy {
class RegisterManager : public std::enable_shared_from_this<RegisterManager> {
    std::unordered_set<Register> usedPhysicalRegisters;
    std::unordered_map<Register, SegmentTree> registerUsage;

public:
    RegisterManager() = default;

    /**
     * @brief Pre-allocates a register that is currently not in use.
     *
     * This function searches for an available register from a pool of 16 registers.
     * If an unused register is found, it is returned. Otherwise, std::nullopt is returned.
     *
     * @return An optional Register object representing the pre-allocated register, or std::nullopt if no register is
     * available.
     */
    std::optional<Register> preAllocate(int start, int end)
    {
        for (auto reg : PHYSICAL_REGISTERS) {
            if (availableIn(reg, start, end)) {
                preAllocate(reg, start, end);
                return reg;
            }
        }
        return std::nullopt;
    }

    void preAllocate(Register reg, int start, int end)
    {
        reg = getPhysicalRegister(reg);
        registerUsage[reg].insert(start, end, true);
    }

    /**
     * Allocates a register for a given location.
     *
     * @param reg The register to allocate.
     * @param loc The location to allocate the register for.
     * @return True if the register was successfully allocated, false otherwise.
     */
    bool allocate(Register reg)
    {
        reg = getPhysicalRegister(reg);
        if (usedPhysicalRegisters.find(reg) == usedPhysicalRegisters.end()) {
            usedPhysicalRegisters.insert(reg);
            return true;
        }
        return false;
    }

    void free(Register reg)
    {
        reg = getPhysicalRegister(reg);
        usedPhysicalRegisters.erase(reg);
    }

    bool isUsed(Register reg) const
    {
        reg = getPhysicalRegister(reg);
        return usedPhysicalRegisters.find(reg) != usedPhysicalRegisters.end();
    }

    bool isUsed(const std::string& regName) const
    {
        for (auto reg : REGISTER_NAMES) {
            if (reg == regName) {
                return isUsed(getRegisterByName(reg));
            }
        }
        return false;
    }

    std::vector<Register> getUsedRegisters() const
    {
        return std::vector<Register>(usedPhysicalRegisters.begin(), usedPhysicalRegisters.end());
    }

    bool available() const
    {
        return usedPhysicalRegisters.size() < PHYSICAL_REGISTERS.size();
    }

    bool availableIn(Register reg, int start, int end)
    {
        auto& tree = registerUsage[reg];
        return !tree.query(start, end);
    }
};

using RegisterManagerPtr = std::shared_ptr<RegisterManager>;

} // namespace sysy
