#pragma once
#include <array>
#include <string>

namespace sysy {

enum class Register : int {
    INVALID = -1,
    RAX = 0,
    EAX,
    RBX,
    EBX,
    RCX,
    ECX,
    RDX,
    EDX,
    RSI,
    ESI,
    RDI,
    EDI,
    RSP,
    ESP,
    RBP,
    EBP,
    R8,
    R8D,
    R9,
    R9D,
    R10,
    R10D,
    R11,
    R11D,
    R12,
    R12D,
    R13,
    R13D,
    R14,
    R14D,
    R15,
    R15D
};

constexpr auto REGISTER_NAMES
    = std::array<std::string, 32> { "rax", "eax", "rbx", "ebx", "rcx", "ecx", "rdx", "edx", "rsi",
          "esi", "rdi", "edi", "rsp", "esp", "rbp", "ebp", "r8", "r8d", "r9", "r9d", "r10", "r10d",
          "r11", "r11d", "r12", "r12d", "r13", "r13d", "r14", "r14d", "r15", "r15d" };

constexpr std::array<Register, 14> PHYSICAL_REGISTERS { Register::RAX, Register::RBX, Register::RCX,
    Register::RDX, Register::RSI, Register::RDI, Register::R8, Register::R9, Register::R10,
    Register::R11, Register::R12, Register::R13, Register::R14, Register::R15 };

constexpr std::array<Register, 5> CALLEE_SAVED_REGISTERS { Register::RBX, Register::R12,
    Register::R13, Register::R14, Register::R15 };

constexpr std::array<Register, 9> CALLER_SAVED_REGISTERS { Register::RAX, Register::RCX,
    Register::RDX, Register::RSI, Register::RDI, Register::R8, Register::R9, Register::R10,
    Register::R11 };

constexpr std::array<Register, 6> FUNCTION_PARAM_REGISTERS { Register::RDI, Register::RSI,
    Register::RDX, Register::RCX, Register::R8, Register::R9 };

Register getLowerRegister(Register reg);

Register getPhysicalRegister(Register reg);

bool isOnSameRegister(Register reg1, Register reg2);

Register getRegisterByName(const std::string& regName);

std::string getRegisterName(Register reg);

bool isCalleeSaved(Register reg);

bool isCallerSaved(Register reg);
} // namespace sysy
