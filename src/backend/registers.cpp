#include "registers.hpp"
#include <algorithm>
#include <stdexcept>

namespace sysy {
Register getLowerRegister(Register reg)
{
    switch (reg) {
    case Register::RAX:
        return Register::EAX;
    case Register::RBX:
        return Register::EBX;
    case Register::RCX:
        return Register::ECX;
    case Register::RDX:
        return Register::EDX;
    case Register::RSI:
        return Register::ESI;
    case Register::RDI:
        return Register::EDI;
    case Register::RSP:
        return Register::ESP;
    case Register::RBP:
        return Register::EBP;
    case Register::R8:
        return Register::R8D;
    case Register::R9:
        return Register::R9D;
    case Register::R10:
        return Register::R10D;
    case Register::R11:
        return Register::R11D;
    case Register::R12:
        return Register::R12D;
    case Register::R13:
        return Register::R13D;
    case Register::R14:
        return Register::R14D;
    case Register::R15:
        return Register::R15D;
    default:
        throw std::runtime_error("Invalid register");
    }
}

Register getPhysicalRegister(Register reg)
{
    switch (reg) {
    case Register::EAX:
    case Register::RAX:
        return Register::RAX;
    case Register::EBX:
    case Register::RBX:
        return Register::RBX;
    case Register::ECX:
    case Register::RCX:
        return Register::RCX;
    case Register::EDX:
    case Register::RDX:
        return Register::RDX;
    case Register::ESI:
    case Register::RSI:
        return Register::RSI;
    case Register::EDI:
    case Register::RDI:
        return Register::RDI;
    case Register::ESP:
    case Register::RSP:
        return Register::RSP;
    case Register::EBP:
    case Register::RBP:
        return Register::RBP;
    case Register::R8:
    case Register::R8D:
        return Register::R8;
    case Register::R9:
    case Register::R9D:
        return Register::R9;
    case Register::R10:
    case Register::R10D:
        return Register::R10;
    case Register::R11:
    case Register::R11D:
        return Register::R11;
    case Register::R12:
    case Register::R12D:
        return Register::R12;
    case Register::R13:
    case Register::R13D:
        return Register::R13;
    case Register::R14:
    case Register::R14D:
        return Register::R14;
    case Register::R15:
    case Register::R15D:
        return Register::R15;
    default:
        throw std::runtime_error("Invalid register");
    }
}

bool isOnSameRegister(Register reg1, Register reg2)
{
    return getPhysicalRegister(reg1) == getPhysicalRegister(reg2);
}

Register getRegisterByName(const std::string& regName)
{
    for (int i = 0; i < 16; i++) {
        if (REGISTER_NAMES[i] == regName) {
            return static_cast<Register>(i);
        }
    }
    throw std::runtime_error("Invalid register name");
}

std::string getRegisterName(Register reg)
{
    return REGISTER_NAMES[static_cast<int>(reg)];
}

/**
 * Checks if a given register is a callee-saved register under the System V AMD64 ABI.
 *
 * @param reg The register to check.
 * @return True if the register is a callee-saved register, false otherwise.
 */
bool isCalleeSaved(Register reg)
{
    reg = getPhysicalRegister(reg);
    return std::find(CALLEE_SAVED_REGISTERS.begin(), CALLEE_SAVED_REGISTERS.end(), reg) != CALLEE_SAVED_REGISTERS.end();
}

/**
 * Checks if a given register is a caller-saved register under the System V AMD64 ABI.
 *
 * @param reg The register to check.
 * @return True if the register is a caller-saved register, false otherwise.
 */
bool isCallerSaved(Register reg)
{
    reg = getPhysicalRegister(reg);
    return std::find(CALLER_SAVED_REGISTERS.begin(), CALLER_SAVED_REGISTERS.end(), reg) != CALLER_SAVED_REGISTERS.end();
}
} // namespace sysy