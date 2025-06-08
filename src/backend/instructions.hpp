#pragma once
#include "backend/registers.hpp"
#include "backend/stack_frame.hpp"
#include "backend/utils.hpp"
#include <array>
#include <format>
#include <functional>
#include <llvm/IR/Constants.h>
#include <memory>
#include <print>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using std::shared_ptr, std::make_shared, std::enable_shared_from_this;
using std::string, std::string_view;
using std::unordered_map;
using std::unordered_set;
using std::vector;
using std::weak_ptr;
using namespace std::string_literals;
using namespace std::string_view_literals;

namespace sysy {
class TypeInfo;
class Operand;
class OperandDef;
class OperandUse;
struct BasicBlock;
struct AsmInstruction;
class AsmFunction;
class AsmModule;

using TypeInfoPtr = shared_ptr<TypeInfo>;
using OperandPtr = shared_ptr<Operand>;
using WeakOperandPtr = weak_ptr<Operand>;
using OperandDefPtr = shared_ptr<OperandDef>;
using OperandUsePtr = shared_ptr<OperandUse>;
using AsmInstructionPtr = shared_ptr<AsmInstruction>;
using WeakAsmInstructionPtr = weak_ptr<AsmInstruction>;
using BasicBlockPtr = shared_ptr<BasicBlock>;
using WeakBasicBlockPtr = weak_ptr<BasicBlock>;
using AsmFunctionPtr = shared_ptr<AsmFunction>;
using WeakAsmFunctionPtr = weak_ptr<AsmFunction>;
using AsmModulePtr = shared_ptr<AsmModule>;

enum class OperandType {
    IMMEDIATE,
    VIRTUAL_REGISTER,
    REGISTER,
    MEMORY,
    STACK_VARIABLE,
    GLOBAL_VARIABLE
};

class TypeInfo : public enable_shared_from_this<TypeInfo> {
    int baseSize;
    bool isArray { false }, isPointer { false }, isAllocated { false };
    TypeInfoPtr elementType;
    int arraySize;

public:
    explicit TypeInfo(int baseSize)
        : baseSize(baseSize)
        , isArray(false)
        , isPointer(false)
    {
    }

    explicit TypeInfo(TypeInfoPtr elementType, int arraySize)
        : baseSize(elementType->baseSize)
        , isArray(true)
        , isPointer(false)
        , elementType(elementType)
        , arraySize(arraySize)
    {
    }

    explicit TypeInfo(TypeInfoPtr elementType = nullptr)
        : baseSize(8)
        , isArray(false)
        , isPointer(true)
        , elementType(elementType)
    {
    }

    TypeInfo(const TypeInfo&) = delete;
    TypeInfo& operator=(const TypeInfo&) = delete;

    auto getAllocSize() const -> int
    {
        return isArray ? elementType->getAllocSize() * arraySize : baseSize;
    }

    auto isAllocatedType() const -> bool { return isAllocated; }

    auto setAllocatedType() -> void { isAllocated = true; }

    auto getArraySize() const -> int { return arraySize; }

    auto getBaseSize() const -> int { return baseSize; }

    auto getElementType() const -> TypeInfoPtr { return elementType; }

    auto isArrayType() const -> bool { return isArray; }

    auto isPointerType() const -> bool { return isPointer; }

    auto isIntegerType() const -> bool { return !isArray && !isPointer; }

    auto createPointer() -> TypeInfoPtr { return make_shared<TypeInfo>(shared_from_this()); }

    auto createArray(int size) -> TypeInfoPtr
    {
        return make_shared<TypeInfo>(shared_from_this(), size);
    }

    auto arrayToPointer() -> TypeInfoPtr { return make_shared<TypeInfo>(elementType); }

    auto clone() -> TypeInfoPtr
    {
        if (isArray) {
            return make_shared<TypeInfo>(elementType ? elementType->clone() : nullptr, arraySize);
        } else if (isPointer) {
            return make_shared<TypeInfo>(elementType ? elementType->clone() : nullptr);
        } else {
            return make_shared<TypeInfo>(baseSize);
        }
    }

    static auto createPrimitive(int size) -> TypeInfoPtr { return make_shared<TypeInfo>(size); }

    static auto createPointer(TypeInfoPtr elementType = nullptr) -> TypeInfoPtr
    {
        return make_shared<TypeInfo>(elementType);
    }
};

enum AsmInstructionType {
    MOV,
    BINARY_OP,
    IMUL,
    IDIV,
    CDQ,
    CMP,
    JMP,
    CALL,
    RET,
    LEA,
    PUSH,
    POP,
    FAKE
};

struct AsmInstruction : public enable_shared_from_this<AsmInstruction> {
    AsmInstructionType type;
    AsmInstructionPtr prev, next;
    WeakBasicBlockPtr parent;
    vector<OperandDefPtr> defs;
    vector<OperandUsePtr> uses;
    unordered_set<OperandPtr> liveIn, liveOut;

    explicit AsmInstruction(AsmInstructionType type)
        : type(type)
    {
    }

    virtual ~AsmInstruction() = default;
    virtual auto getOperands() -> vector<OperandPtr> = 0;
    virtual auto getOperands() const -> vector<OperandPtr> = 0;
    virtual string dump() const = 0;
    virtual auto getOperandNum() const -> int { return getOperands().size(); }
    virtual void replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) = 0;

    auto addDef(OperandDefPtr def) -> void { defs.push_back(def); }

    auto addUse(OperandUsePtr use) -> void { uses.push_back(use); }

    auto getDefs() const -> const vector<OperandDefPtr>& { return defs; }

    auto getUses() const -> const vector<OperandUsePtr>& { return uses; }

    auto removeDef(OperandDefPtr def) -> void
    {
        defs.erase(std::remove(defs.begin(), defs.end(), def), defs.end());
    }

    auto removeUse(OperandUsePtr use) -> void
    {
        uses.erase(std::remove(uses.begin(), uses.end(), use), uses.end());
    }

    auto setParent(BasicBlockPtr parent) -> void { this->parent = parent; }

    virtual auto isMoveRelated() const -> bool { return false; }
};

class Operand : public enable_shared_from_this<Operand> {
    OperandType operandType;
    TypeInfoPtr type;
    vector<OperandDefPtr> defs;
    vector<OperandUsePtr> uses;
    string name;

protected:
    explicit Operand(OperandType operandType, TypeInfoPtr type, string_view name)
        : operandType(operandType)
        , type(type)
        , name(name.data())
    {
    }

public:
    Operand(const Operand&) = delete;
    Operand& operator=(const Operand&) = delete;

    virtual ~Operand() = default;
    virtual string dump(bool withType) const = 0;

    auto addDef(OperandDefPtr def) -> void { defs.push_back(def); }

    auto removeDef(OperandDefPtr def) -> void
    {
        defs.erase(std::remove(defs.begin(), defs.end(), def), defs.end());
    }

    auto addUse(OperandUsePtr use) -> void { uses.push_back(use); }

    auto removeUse(OperandUsePtr use) -> void
    {
        uses.erase(std::remove(uses.begin(), uses.end(), use), uses.end());
    }

    auto getDefs() const -> const vector<OperandDefPtr>& { return defs; }

    auto getUses() const -> const vector<OperandUsePtr>& { return uses; }

    auto clearDefs() -> void { defs.clear(); }

    auto clearUses() -> void { uses.clear(); }

    auto getOperandType() const -> OperandType { return operandType; }

    auto getType() const -> TypeInfoPtr { return type; }

    auto setType(TypeInfoPtr type) -> void { this->type = type; }

    auto getSize() const -> int { return type->getAllocSize(); }

    auto getName() const -> string { return name; }

    template <typename T> bool is() const
    {
        auto self { shared_from_this() };
        return ptrIs<const T>(self);
    }

    bool isImmediate() const { return operandType == OperandType::IMMEDIATE; }

    bool isMemory() const
    {
        return operandType == OperandType::MEMORY || operandType == OperandType::STACK_VARIABLE
            || operandType == OperandType::GLOBAL_VARIABLE;
    }

    void replaceBy(OperandPtr other);

    virtual void replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) { };

    virtual bool operator==(const Operand& other) const { return false; }
};

class OperandDef : public enable_shared_from_this<OperandDef> {
    AsmInstructionPtr inst;
    WeakOperandPtr operand;

public:
    explicit OperandDef(AsmInstructionPtr inst, OperandPtr operand)
        : inst(inst)
        , operand(operand)
    {
    }

    OperandDef(const OperandDef&) = delete;
    OperandDef& operator=(const OperandDef&) = delete;

    auto getInst() const -> AsmInstructionPtr { return inst; }

    auto getOperand() const -> WeakOperandPtr { return operand; }

    auto replaceBy(OperandPtr other) -> void
    {
        inst->replaceOperand(operand.lock(), other);
        operand = other;
    }
};

class OperandUse : public enable_shared_from_this<OperandUse> {
    AsmInstructionPtr inst;
    WeakOperandPtr operand;

public:
    explicit OperandUse(AsmInstructionPtr inst, OperandPtr operand)
        : inst(inst)
        , operand(operand)
    {
    }

    OperandUse(const OperandUse&) = delete;
    OperandUse& operator=(const OperandUse&) = delete;

    auto getInst() const -> AsmInstructionPtr { return inst; }

    auto getOperand() const -> WeakOperandPtr { return operand; }

    auto replaceBy(OperandPtr other) -> void
    {
        inst->replaceOperand(operand.lock(), other);
        operand = other;
    }
};

struct Immediate : Operand {
    int value;

    explicit Immediate(int value, int size)
        : Operand(OperandType::IMMEDIATE, TypeInfo::createPrimitive(size), std::to_string(value))
        , value(value)
    {
    }

    explicit Immediate(int value, TypeInfoPtr type)
        : Operand(OperandType::IMMEDIATE, type, std::to_string(value))
        , value(value)
    {
    }

    Immediate(const Immediate&) = delete;
    Immediate& operator=(const Immediate&) = delete;

    auto dump(bool withType = true) const -> string override { return std::format("{}", value); }

    bool operator==(const Operand& other) const override
    {
        if (other.is<Immediate>()) {
            return value == static_cast<const Immediate&>(other).value;
        }
        return false;
    }
};

struct VirtualRegister : Operand {
    explicit VirtualRegister(string_view name, TypeInfoPtr type)
        : Operand(OperandType::VIRTUAL_REGISTER, type, name)
    {
    }

    VirtualRegister(const VirtualRegister&) = delete;
    VirtualRegister& operator=(const VirtualRegister&) = delete;

    auto dump(bool withType) const -> string override { return getName(); }

    bool operator==(const Operand& other) const override
    {
        if (other.is<VirtualRegister>()) {
            return getName() == static_cast<const VirtualRegister&>(other).getName();
        }
        return false;
    }
};

struct PhysicalRegister : Operand {
    Register reg;

    explicit PhysicalRegister(string_view name, Register reg, int size)
        : Operand(OperandType::REGISTER, TypeInfo::createPrimitive(size), name)
        , reg(reg)
    {
    }

    explicit PhysicalRegister(string_view name, Register reg, TypeInfoPtr type)
        : Operand(OperandType::REGISTER, type, name)
        , reg(reg)
    {
    }

    PhysicalRegister(const PhysicalRegister&) = delete;
    PhysicalRegister& operator=(const PhysicalRegister&) = delete;

    auto dump(bool withType) const -> string override
    {
        if (!withType)
            return getRegisterName(reg);
        else {
            if (getSize() < 8) {
                return getRegisterName(getLowerRegister(reg));
            } else {
                return getRegisterName(reg);
            }
        }
    }

    auto getRegister() const -> Register { return reg; }

    bool operator==(const Operand& other) const override
    {
        if (other.is<PhysicalRegister>()) {
            return reg == static_cast<const PhysicalRegister&>(other).reg;
        }
        return false;
    }
};

enum class StackLocationBase { RBP, RSP, RBP_RELATIVE };

struct StackLocation : Operand {
    int offset;
    StackLocationBase base;

    explicit StackLocation(string_view name, TypeInfoPtr type, int offset = 0,
        StackLocationBase base = StackLocationBase::RBP)
        : Operand(OperandType::STACK_VARIABLE, type, name)
        , offset(offset)
        , base(base)
    {
    }

    StackLocation(const StackLocation&) = delete;
    StackLocation& operator=(const StackLocation&) = delete;

    auto dumpAddress() const -> string
    {
        if (base == StackLocationBase::RSP) {
            if (offset > 0) {
                return std::format("[rsp + {}]", offset);
            } else {
                return std::format("[rsp - {}]", -offset);
            }
        } else {
            if (offset > 0) {
                return std::format("[rbp - {}]", offset);
            } else {
                return std::format("[rbp + {}]", -offset);
            }
        }
    }

    auto dump(bool withType = true) const -> string override
    {
        auto typePrefix = ""s;
        if (withType) {
            typePrefix = std::format("{} ",
                getSize() == 1       ? "byte ptr"s
                    : getSize() == 2 ? "word ptr"s
                    : getSize() == 4 ? "dword ptr"s
                    : getSize() == 8 ? "qword ptr"s
                                     : ""s);
        }
        return typePrefix + dumpAddress();
    }

    auto setOffset(int offset) -> void { this->offset = offset; }

    auto getOffset() const -> int { return offset; }

    bool operator==(const Operand& other) const override
    {
        if (other.is<StackLocation>()) {
            return offset == static_cast<const StackLocation&>(other).offset;
        }
        return false;
    }
};

struct Memory : Operand {
    OperandPtr base;
    OperandPtr index;
    int scale { 1 };
    int displacement { 0 };

    explicit Memory(string_view name, OperandPtr base, OperandPtr index, int scale,
        int displacement, TypeInfoPtr type)
        : Operand(OperandType::MEMORY, type, name)
        , base(base)
        , index(index)
        , scale(scale)
        , displacement(displacement)
    {
        flatten();
    }

    explicit Memory(
        string_view name, OperandPtr base, OperandPtr index, int scale, TypeInfoPtr type)
        : Operand(OperandType::MEMORY, type, name)
        , base(base)
        , index(index)
        , scale(scale)
    {
        flatten();
    }

    explicit Memory(string_view name, OperandPtr base, int displacement, TypeInfoPtr type)
        : Operand(OperandType::MEMORY, type, name)
        , base(base)
        , displacement(displacement)
    {
        flatten();
    }

    explicit Memory(string_view name, OperandPtr base, TypeInfoPtr type)
        : Operand(OperandType::MEMORY, type, name)
        , base(base)
    {
        flatten();
    }

    void flatten()
    {
        if (auto stackBase = std::dynamic_pointer_cast<StackLocation>(base)) {
            base = std::make_shared<PhysicalRegister>("rbp", Register::RBP, 8);
            displacement -= stackBase->getOffset();
        }
    }

    Memory(const Memory&) = delete;
    Memory& operator=(const Memory&) = delete;

    auto dumpIndex() const -> string
    {
        if (index) {
            return std::format(" + {}{}{}", index->dump(false), scale > 1 ? " * "s : ""s,
                scale > 1 ? std::to_string(scale) : ""s);
        }
        return ""s;
    }

    auto dumpDisplacement() const -> string
    {
        if (displacement) {
            return std::format(" {} {}", displacement > 0 ? "+"s : "-"s, std::abs(displacement));
        }
        return ""s;
    }

    auto dump(bool withType = true) const -> string override
    {
        auto typePrefix = ""s;
        auto addr = std::format("[{}{}{}]", base->dump(false), dumpIndex(), dumpDisplacement());
        if (withType) {
            typePrefix = std::format("{} ",
                getSize() == 1       ? "byte ptr"s
                    : getSize() == 2 ? "word ptr"s
                    : getSize() == 4 ? "dword ptr"s
                    : getSize() == 8 ? "qword ptr"s
                                     : ""s);
        }
        return typePrefix + addr;
    }

    void replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) override
    {
        if (base == oldOperand) {
            base = newOperand;
        }
        base->replaceOperand(oldOperand, newOperand);
        if (index) {
            if (index == oldOperand) {
                index = newOperand;
            }
            index->replaceOperand(oldOperand, newOperand);
        }
    }

    auto getBase() const -> OperandPtr { return base; }

    auto getIndex() const -> OperandPtr { return index; }

    auto getScale() const -> int { return scale; }

    auto getDisplacement() const -> int { return displacement; }

    bool operator==(const Operand& other) const override
    {
        if (other.is<Memory>()) {
            auto& mem = static_cast<const Memory&>(other);
            return base == mem.base && index == mem.index && scale == mem.scale
                && displacement == mem.displacement;
        }
        return false;
    }
};

struct GlobalVariable : Operand {
    const llvm::Constant* initializer;
    bool isConst { false };

    explicit GlobalVariable(string_view name, TypeInfoPtr type)
        : Operand(OperandType::GLOBAL_VARIABLE, type, name)
    {
    }

    explicit GlobalVariable(string_view name, TypeInfoPtr type, const llvm::Constant* initializer)
        : Operand(OperandType::GLOBAL_VARIABLE, type, name)
        , initializer(initializer)
    {
    }

    explicit GlobalVariable(string_view name, TypeInfoPtr type, bool isConst)
        : Operand(OperandType::GLOBAL_VARIABLE, type, name)
        , isConst(isConst)
    {
    }

    explicit GlobalVariable(
        string_view name, TypeInfoPtr type, const llvm::Constant* initializer, bool isConst)
        : Operand(OperandType::GLOBAL_VARIABLE, type, name)
        , initializer(initializer)
        , isConst(isConst)
    {
    }

    GlobalVariable(const GlobalVariable&) = delete;
    GlobalVariable& operator=(const GlobalVariable&) = delete;

    auto dump(bool withType) const -> string override
    {
        auto typePrefix = ""s;
        if (withType && !isConst) {
            typePrefix = std::format("{} ",
                getSize() == 1       ? "byte ptr"s
                    : getSize() == 2 ? "word ptr"s
                    : getSize() == 4 ? "dword ptr"s
                    : getSize() == 8 ? "qword ptr"s
                                     : ""s);
        }
        return typePrefix + std::format("[rip + {}]", getName());
    }

    auto hasInitializer() const -> bool { return initializer != nullptr; }

    auto getInitializer() const -> const llvm::Constant* { return initializer; }

    bool operator==(const Operand& other) const override
    {
        if (other.is<GlobalVariable>()) {
            return getName() == static_cast<const GlobalVariable&>(other).getName();
        }
        return false;
    }

    auto isConstant() const -> bool { return isConst; }
};

enum class MovType { MOV, MOVZX, MOVSX };

const std::array movNames = { "mov"sv, "movzx"sv, "movsx"sv };

struct MovRelatedInstruction : AsmInstruction {
    explicit MovRelatedInstruction(AsmInstructionType type)
        : AsmInstruction(type)
    {
    }

    virtual auto getDest() const -> OperandPtr = 0;

    virtual auto getSrc() const -> OperandPtr = 0;

    auto isMoveRelated() const -> bool override { return true; }
};

struct PushInstruction : AsmInstruction {
    OperandPtr src;

    explicit PushInstruction(OperandPtr src)
        : AsmInstruction(AsmInstructionType::PUSH)
        , src(src)
    {
    }

    PushInstruction(const PushInstruction&) = delete;
    PushInstruction& operator=(const PushInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return { src }; }

    auto getOperands() const -> vector<OperandPtr> override { return { src }; }

    auto dump() const -> string override { return std::format("push\t{}", src->dump(true)); }

    auto replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) -> void override
    {
        if (src == oldOperand) {
            src = newOperand;
        } else {
            src->replaceOperand(oldOperand, newOperand);
        }
    }
};

struct PopInstruction : AsmInstruction {
    OperandPtr dest;

    explicit PopInstruction(OperandPtr dest)
        : AsmInstruction(AsmInstructionType::POP)
        , dest(dest)
    {
    }

    PopInstruction(const PopInstruction&) = delete;
    PopInstruction& operator=(const PopInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return { dest }; }

    auto getOperands() const -> vector<OperandPtr> override { return { dest }; }

    auto dump() const -> string override { return std::format("pop\t\t{}", dest->dump(true)); }

    auto replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) -> void override
    {
        if (dest == oldOperand) {
            dest = newOperand;
        } else {
            dest->replaceOperand(oldOperand, newOperand);
        }
    }
};

struct FakeInstruction : AsmInstruction {
    explicit FakeInstruction()
        : AsmInstruction(AsmInstructionType::FAKE)
    {
    }

    auto getOperands() -> vector<OperandPtr> override { return {}; }

    auto getOperands() const -> vector<OperandPtr> override { return {}; }

    auto dump() const -> string override { return ""; }

    auto replaceOperand(OperandPtr, OperandPtr) -> void override { }
};

struct MovInstruction : MovRelatedInstruction {
    MovType type;
    OperandPtr dest, src;

    explicit MovInstruction(OperandPtr dest, OperandPtr src)
        : MovRelatedInstruction(AsmInstructionType::MOV)
        , type(MovType::MOV)
        , dest(dest)
        , src(src)
    {
    }

    explicit MovInstruction(MovType type, OperandPtr dest, OperandPtr src)
        : MovRelatedInstruction(AsmInstructionType::MOV)
        , type(type)
        , dest(dest)
        , src(src)
    {
    }

    MovInstruction(const MovInstruction&) = delete;
    MovInstruction& operator=(const MovInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return { dest, src }; }

    auto getOperands() const -> vector<OperandPtr> override { return { dest, src }; }

    auto dump() const -> string override
    {
        auto type = dest->getSize() > src->getSize() && !src->isImmediate() ? MovType::MOVSX
                                                                            : MovType::MOV;
        if (dest->getSize() < src->getSize()) {
            auto oldType = src->getType();
            src->setType(dest->getType());
            auto res = std::format("{}\t\t{}, {}", movNames[static_cast<int>(type)],
                dest->dump(true), src->dump(true));
            src->setType(oldType);
            return res;
        }
        return std::format(
            "{}\t\t{}, {}", movNames[static_cast<int>(type)], dest->dump(true), src->dump(true));
    }

    auto replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) -> void override
    {
        if (dest == oldOperand) {
            dest = newOperand;
        } else {
            dest->replaceOperand(oldOperand, newOperand);
        }
        if (src == oldOperand) {
            src = newOperand;
        } else {
            src->replaceOperand(oldOperand, newOperand);
        }
    }

    auto getDest() const -> OperandPtr override { return dest; }

    auto getSrc() const -> OperandPtr override { return src; }
};

enum class ConditionType { EQ, NE, LT, LE, GT, GE };

struct BasicBlock : public enable_shared_from_this<BasicBlock> {
    string name;
    WeakAsmFunctionPtr parent;
    AsmInstructionPtr head, tail, terminator;
    BasicBlockPtr next;
    BasicBlockPtr trueBranch, falseBranch;
    vector<BasicBlockPtr> predecessors;
    bool isConditional = false, isAlive = true;
    ConditionType condition;
    unordered_set<OperandPtr> liveIn {}, liveOut {}, defSet {}, useSet {};

    explicit BasicBlock(string_view name, const AsmFunctionPtr& parent)
        : name(name)
        , parent(parent)
    {
    }

    explicit BasicBlock(string_view name, const AsmFunctionPtr& parent, BasicBlockPtr next)
        : name(name)
        , parent(parent)
        , next(next)
    {
    }

    explicit BasicBlock(string_view name, const AsmFunctionPtr& parent, BasicBlockPtr trueBranch,
        BasicBlockPtr falseBranch, ConditionType condition)
        : name(name)
        , parent(parent)
        , trueBranch(trueBranch)
        , falseBranch(falseBranch)
        , isConditional(true)
        , condition(condition)
    {
    }

    BasicBlock(const BasicBlock&) = delete;
    BasicBlock& operator=(const BasicBlock&) = delete;

    auto getName() const -> string_view { return name; }

    auto dump() const -> string
    {
        auto result = std::format("{}:\n", name);
        for (auto inst = head; inst; inst = inst->next) {
            auto instAsm = inst->dump();
            if (!instAsm.empty() && instAsm != "\n") {
                result += "\t"s + instAsm + "\n"s;
            }
        }
        return result;
    }

    auto setParent(AsmFunctionPtr parent) -> void { this->parent = parent; }

    auto getParent() const -> AsmFunctionPtr { return parent.lock(); }

    auto getTerminator() const -> AsmInstructionPtr { return terminator; }

    auto setTerminator(AsmInstructionPtr terminator) -> void { this->terminator = terminator; }

    auto prependInstruction(AsmInstructionPtr inst) -> void
    {
        if (!tail) {
            tail = inst;
        }
        if (head) {
            head->prev = inst;
        }
        inst->next = head;
        head = inst;
        inst->setParent(shared_from_this());
    }

    auto appendInstruction(AsmInstructionPtr inst) -> void
    {
        if (!head) {
            head = inst;
        }
        if (tail) {
            tail->next = inst;
        }
        inst->prev = tail;
        tail = inst;
        inst->setParent(shared_from_this());
    }

    auto insertInstructionBefore(AsmInstructionPtr inst, AsmInstructionPtr before) -> void
    {
        if (before == head) {
            head = inst;
        }
        inst->prev = before->prev;
        inst->next = before;
        before->prev = inst;
        if (inst->prev) {
            inst->prev->next = inst;
        }
        inst->setParent(shared_from_this());
    }

    auto insertInstructionAfter(AsmInstructionPtr inst, AsmInstructionPtr after) -> void
    {
        if (after == tail) {
            tail = inst;
        }
        inst->prev = after;
        inst->next = after->next;
        after->next = inst;
        if (inst->next) {
            inst->next->prev = inst;
        }
        inst->setParent(shared_from_this());
    }

    auto removeInstruction(AsmInstructionPtr inst) -> void
    {
        if (inst == head) {
            head = inst->next;
        }
        if (inst == tail) {
            tail = inst->prev;
        }
        if (inst->prev) {
            inst->prev->next = inst->next;
        }
        if (inst->next) {
            inst->next->prev = inst->prev;
        }
        inst->setParent(nullptr);
    }

    auto replaceInstruction(AsmInstructionPtr inst, AsmInstructionPtr replacement) -> void
    {
        if (inst == head) {
            head = replacement;
        }
        if (inst == tail) {
            tail = replacement;
        }
        if (inst->prev) {
            inst->prev->next = replacement;
        }
        if (inst->next) {
            inst->next->prev = replacement;
        }
        replacement->prev = inst->prev;
        replacement->next = inst->next;
        inst->setParent(nullptr);
        replacement->setParent(shared_from_this());
    }

    auto addPredecessor(BasicBlockPtr predecessor) -> void { predecessors.push_back(predecessor); }

    auto setUnconditionalNext(BasicBlockPtr next) -> void { this->next = next; }

    auto setConditionalNext(
        BasicBlockPtr trueBranch, BasicBlockPtr falseBranch, ConditionType condition) -> void
    {
        this->trueBranch = trueBranch;
        this->falseBranch = falseBranch;
        this->isConditional = true;
        this->condition = condition;
    }

    auto getInstructions() const -> vector<AsmInstructionPtr>
    {
        vector<AsmInstructionPtr> instructions;
        for (auto inst = head; inst; inst = inst->next) {
            instructions.push_back(inst);
        }
        return instructions;
    }

    void traverseSuccessors(const std::function<void(BasicBlockPtr)>& f) const
    {
        if (!isConditional) {
            if (next)
                f(next);
        } else {
            f(trueBranch);
            f(falseBranch);
        }
    }

    void traversePredecessors(const std::function<void(BasicBlockPtr)>& f) const
    {
        for (const auto& pred : predecessors) {
            f(pred);
        }
    }

    void traverseInstructions(const std::function<void(AsmInstructionPtr)>& f) const
    {
        for (auto inst = head; inst; inst = inst->next) {
            f(inst);
        }
    }

    void traverseInstructionsReverse(const std::function<void(AsmInstructionPtr)>& f) const
    {
        for (auto inst = tail; inst; inst = inst->prev) {
            f(inst);
        }
    }

    void traverseDefUse(const std::function<void(string_view)>& onDef,
        const std::function<void(string_view)>& onUse) const
    {
        for (auto inst = head; inst; inst = inst->next) {
            for (const auto& def : inst->getDefs()) {
                onDef(def->getOperand().lock()->getName());
            }
            for (const auto& use : inst->getUses()) {
                onUse(use->getOperand().lock()->getName());
            }
        }
    }

    void traverseDefUseReverse(const std::function<void(string_view)>& onDef,
        const std::function<void(string_view)>& onUse) const
    {
        for (auto inst = tail; inst; inst = inst->prev) {
            for (const auto& def : inst->getDefs()) {
                onDef(def->getOperand().lock()->getName());
            }
            for (const auto& use : inst->getUses()) {
                onUse(use->getOperand().lock()->getName());
            }
        }
    }

    void killBlock() { isAlive = false; }
};

enum class BinaryOpType {
    ADD,
    SUB,
    // MUL,
    // DIV,
    // MOD,
    // AND,
    // OR,
    XOR,
    // SHL,
    // SHR
};

const std::array binaryOpNames = {
    "add"sv, "sub"sv,
    // "and"sv,
    // "or"sv,
    "xor"sv,
    // "shl"sv,
    // "shr"sv
};

struct BinaryOpInstruction : AsmInstruction {
    BinaryOpType op;
    OperandPtr dest, src;

    explicit BinaryOpInstruction(BinaryOpType op, OperandPtr dest, OperandPtr src)
        : AsmInstruction(AsmInstructionType::BINARY_OP)
        , op(op)
        , dest(dest)
        , src(src)
    {
    }

    BinaryOpInstruction(const BinaryOpInstruction&) = delete;
    BinaryOpInstruction& operator=(const BinaryOpInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return { dest, src }; }

    auto getOperands() const -> vector<OperandPtr> override { return { dest, src }; }

    auto dump() const -> string override
    {
        return std::format(
            "{}\t\t{}, {}", binaryOpNames[static_cast<int>(op)], dest->dump(true), src->dump(true));
    }

    auto replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) -> void override
    {
        if (dest == oldOperand) {
            dest = newOperand;
        } else {
            dest->replaceOperand(oldOperand, newOperand);
        }
        if (src == oldOperand) {
            src = newOperand;
        } else {
            src->replaceOperand(oldOperand, newOperand);
        }
    }
};

struct IMulInstruction : AsmInstruction {
    OperandPtr dest, src1, src2;

    explicit IMulInstruction(OperandPtr src1)
        : AsmInstruction(AsmInstructionType::IMUL)
        , src1(src1)
    {
    }

    explicit IMulInstruction(OperandPtr dest, OperandPtr src1)
        : AsmInstruction(AsmInstructionType::IMUL)
        , dest(dest)
        , src1(src1)
    {
    }

    explicit IMulInstruction(OperandPtr dest, OperandPtr src1, OperandPtr src2)
        : AsmInstruction(AsmInstructionType::IMUL)
        , dest(dest)
        , src1(src1)
        , src2(src2)
    {
    }

    IMulInstruction(const IMulInstruction&) = delete;
    IMulInstruction& operator=(const IMulInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override
    {
        vector<OperandPtr> operands;
        if (dest) {
            operands.push_back(dest);
        }
        operands.push_back(src1);
        if (src2) {
            operands.push_back(src2);
        }
        return operands;
    }

    auto getOperands() const -> vector<OperandPtr> override
    {
        vector<OperandPtr> operands;
        if (dest) {
            operands.push_back(dest);
        }
        operands.push_back(src1);
        if (src2) {
            operands.push_back(src2);
        }
        return operands;
    }

    auto dump() const -> string override
    {
        if (src2) {
            return std::format(
                "imul\t{}, {}, {}", dest->dump(true), src1->dump(true), src2->dump(true));
        } else if (dest) {
            return std::format("imul\t{}, {}", dest->dump(true), src1->dump(true));
        } else {
            return std::format("imul\t{}", src1->dump(true));
        }
    }

    auto replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) -> void override
    {
        if (dest == oldOperand) {
            dest = newOperand;
        } else if (dest) {
            dest->replaceOperand(oldOperand, newOperand);
        }
        if (src1 == oldOperand) {
            src1 = newOperand;
        } else if (src1) {
            src1->replaceOperand(oldOperand, newOperand);
        }
        if (src2 == oldOperand) {
            src2 = newOperand;
        } else if (src2) {
            src2->replaceOperand(oldOperand, newOperand);
        }
    }
};

struct IDivInstruction : AsmInstruction {
    OperandPtr src;

    explicit IDivInstruction(OperandPtr src)
        : AsmInstruction(AsmInstructionType::IDIV)
        , src(src)
    {
    }

    IDivInstruction(const IDivInstruction&) = delete;
    IDivInstruction& operator=(const IDivInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return { src }; }

    auto getOperands() const -> vector<OperandPtr> override { return { src }; }

    auto dump() const -> string override { return std::format("idiv\t{}", src->dump(true)); }

    auto replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) -> void override
    {
        if (src == oldOperand) {
            src = newOperand;
        } else {
            src->replaceOperand(oldOperand, newOperand);
        }
    }
};

struct CdqInstruction : AsmInstruction {
    explicit CdqInstruction()
        : AsmInstruction(AsmInstructionType::CDQ)
    {
    }

    CdqInstruction(const CdqInstruction&) = delete;
    CdqInstruction& operator=(const CdqInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return {}; }

    auto getOperands() const -> vector<OperandPtr> override { return {}; }

    auto dump() const -> string override { return "cdq\t\t"; }

    auto replaceOperand(OperandPtr, OperandPtr) -> void override { }
};

struct CmpInstruction : AsmInstruction {
    OperandPtr lhs, rhs;

    explicit CmpInstruction(OperandPtr lhs, OperandPtr rhs)
        : AsmInstruction(AsmInstructionType::CMP)
        , lhs(lhs)
        , rhs(rhs)
    {
    }

    CmpInstruction(const CmpInstruction&) = delete;
    CmpInstruction& operator=(const CmpInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return { lhs, rhs }; }

    auto getOperands() const -> vector<OperandPtr> override { return { lhs, rhs }; }

    auto dump() const -> string override
    {
        return std::format("cmp\t\t{}, {}", lhs->dump(true), rhs->dump(true));
    }

    auto replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) -> void override
    {
        if (lhs == oldOperand) {
            lhs = newOperand;
        } else {
            lhs->replaceOperand(oldOperand, newOperand);
        }
        if (rhs == oldOperand) {
            rhs = newOperand;
        } else {
            rhs->replaceOperand(oldOperand, newOperand);
        }
    }
};

enum class JmpType { JMP, JE, JNE, JL, JLE, JG, JGE };

const std::array jmpNames = { "jmp"sv, "je"sv, "jne"sv, "jl"sv, "jle"sv, "jg"sv, "jge"sv };

struct JmpInstruction : AsmInstruction {
    JmpType type;
    BasicBlockPtr target;

    explicit JmpInstruction(JmpType type, BasicBlockPtr target)
        : AsmInstruction(AsmInstructionType::JMP)
        , type(type)
        , target(target)
    {
    }

    JmpInstruction(const JmpInstruction&) = delete;
    JmpInstruction& operator=(const JmpInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return {}; }

    auto getOperands() const -> vector<OperandPtr> override { return {}; }

    auto dump() const -> string override
    {
        return std::format("{}\t\t{}", jmpNames[static_cast<int>(type)], target->name);
    }

    auto getOperandNum() const -> int override { return 0; }

    auto replaceOperand(OperandPtr, OperandPtr) -> void override { }
};

struct CallInstruction : AsmInstruction {
    string target;
    bool isRetValUsed { false };

    explicit CallInstruction(string_view target)
        : AsmInstruction(AsmInstructionType::CALL)
        , target(target)
    {
    }

    CallInstruction(const CallInstruction&) = delete;
    CallInstruction& operator=(const CallInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return {}; }

    auto getOperands() const -> vector<OperandPtr> override { return {}; }

    auto dump() const -> string override { return std::format("call\t{}", target); }

    auto getOperandNum() const -> int override { return 0; }

    auto replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) -> void override { }
};

struct RetInstruction : AsmInstruction {
    explicit RetInstruction()
        : AsmInstruction(AsmInstructionType::RET)
    {
    }

    RetInstruction(const RetInstruction&) = delete;
    RetInstruction& operator=(const RetInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return {}; }

    auto getOperands() const -> vector<OperandPtr> override { return {}; }

    auto dump() const -> string override { return "ret\t\t"; }

    auto getOperandNum() const -> int override { return 0; }

    auto replaceOperand(OperandPtr, OperandPtr) -> void override { }
};

struct LeaInstruction : AsmInstruction {
    OperandPtr dest, src;

    explicit LeaInstruction(OperandPtr dest, OperandPtr src)
        : AsmInstruction(AsmInstructionType::LEA)
        , dest(dest)
        , src(src)
    {
    }

    LeaInstruction(const LeaInstruction&) = delete;
    LeaInstruction& operator=(const LeaInstruction&) = delete;

    auto getOperands() -> vector<OperandPtr> override { return { dest, src }; }

    auto getOperands() const -> vector<OperandPtr> override { return { dest, src }; }

    auto dump() const -> string override
    {
        return std::format("lea\t\t{}, {}", dest->dump(true), src->dump(false));
    }

    auto replaceOperand(OperandPtr oldOperand, OperandPtr newOperand) -> void override
    {
        if (dest == oldOperand) {
            dest = newOperand;
        } else {
            dest->replaceOperand(oldOperand, newOperand);
        }
        if (src == oldOperand) {
            src = newOperand;
        } else {
            src->replaceOperand(oldOperand, newOperand);
        }
    }
};

class AsmFunction : public enable_shared_from_this<AsmFunction> {
    string name;
    BasicBlockPtr entry, exit;
    vector<BasicBlockPtr> blocks;
    unordered_map<string, BasicBlockPtr> blockMap;
    unordered_map<string, OperandPtr> operandMap;
    StackFramePtr stackFrame;

public:
    explicit AsmFunction(string_view name, StackFramePtr stackFrame)
        : name(name)
        , stackFrame(stackFrame)
    {
    }

    AsmFunction(const AsmFunction&) = delete;
    AsmFunction& operator=(const AsmFunction&) = delete;

    auto getName() const -> string { return name; }

    auto getEntry() const -> BasicBlockPtr { return entry; }

    auto getBlocks() const -> const vector<BasicBlockPtr>& { return blocks; }

    auto getStackFrame() const -> StackFramePtr { return stackFrame; }

    auto dump() const -> string
    {
        auto result = std::format("{}:\n", name);
        for (const auto& block : blocks) {
            result += block->dump();
        }
        return result;
    }

    auto addBlock(BasicBlockPtr block) -> void
    {
        auto self { shared_from_this() };
        block->parent = self;
        blocks.push_back(block);
        blockMap[block->name] = block;
    }

    auto getBlock(string_view name) -> BasicBlockPtr const
    {
        if (blockMap.contains(name.data())) {
            return blockMap[name.data()];
        }
        return nullptr;
    }

    auto setEntry(BasicBlockPtr entry) -> void { this->entry = entry; }

    auto setExit(BasicBlockPtr exit) -> void { this->exit = exit; }

    auto getExit() const -> BasicBlockPtr { return exit; }

    auto setOperandMap(const unordered_map<string, OperandPtr>& operandMap) -> void
    {
        this->operandMap = operandMap;
    }

    auto buildOperandMap() -> void
    {
        for (const auto& block : blocks) {
            block->traverseInstructions([&](AsmInstructionPtr inst) {
                for (const auto& operand : inst->getDefs()) {
                    if (!operand->getOperand().expired())
                        operandMap[operand->getOperand().lock()->getName()]
                            = operand->getOperand().lock();
                }
                for (const auto& operand : inst->getUses()) {
                    if (!operand->getOperand().expired())
                        operandMap[operand->getOperand().lock()->getName()]
                            = operand->getOperand().lock();
                }
            });
        }
    }

    auto getOperands() const -> const unordered_set<OperandPtr>&
    {
        static unordered_set<OperandPtr> operands;
        for (const auto& [_, operand] : operandMap) {
            operands.insert(operand);
        }
        return operands;
    }

    void traverse(const std::function<void(BasicBlockPtr)>& f) const
    {
        std::unordered_set<BasicBlockPtr> visited;
        std::function<void(BasicBlockPtr)> visit = [&](BasicBlockPtr block) {
            if (visited.contains(block)) {
                return;
            }
            visited.insert(block);
            f(block);
            block->traverseSuccessors(visit);
        };
        visit(entry);
    }

    void traverseReverse(const std::function<void(BasicBlockPtr)>& f, bool visitExit = true) const
    {
        std::unordered_set<BasicBlockPtr> visited;
        std::function<void(BasicBlockPtr)> visit = [&](BasicBlockPtr block) {
            if (visited.contains(block)) {
                return;
            }
            visited.insert(block);
            f(block);
            block->traversePredecessors(visit);
        };
        if (visitExit)
            visit(exit);
        else
            exit->traversePredecessors(visit);
    }
};

class AsmModule : public enable_shared_from_this<AsmModule> {
    vector<AsmFunctionPtr> functions;
    unordered_map<string, AsmFunctionPtr> functionMap;
    vector<shared_ptr<GlobalVariable>> globals;

public:
    AsmModule() = default;

    AsmModule(const AsmModule&) = delete;
    AsmModule& operator=(const AsmModule&) = delete;

    auto addFunction(AsmFunctionPtr function) -> void
    {
        auto self { shared_from_this() };
        functions.push_back(function);
        functionMap[function->getName()] = function;
    }

    auto getFunction(string_view name) -> AsmFunctionPtr const
    {
        if (functionMap.contains(name.data())) {
            return functionMap[name.data()];
        }
        return nullptr;
    }

    auto getFunctions() const -> const vector<AsmFunctionPtr>& { return functions; }

    auto addGlobal(shared_ptr<GlobalVariable> global) -> void { globals.push_back(global); }

    auto getGlobals() const -> const vector<shared_ptr<GlobalVariable>>& { return globals; }

    static string escapeString(string_view str)
    {
        string escaped;
        for (auto c : str) {
            if (c == '\n') {
                escaped += "\\n";
            } else if (c == '\t') {
                escaped += "\\t";
            } else {
                escaped += c;
            }
        }
        return escaped;
    }

    auto dumpGlobalInitializer(
        string_view name, TypeInfoPtr type, const llvm::Constant* constant) const -> string
    {
        auto ss = std::stringstream {};
        if (type->isArrayType()) {
            auto elementType = type->getElementType();
            // 如果是字符串，直接输出
            if (elementType->getBaseSize() == 1) {
                auto constArray = llvm::dyn_cast<llvm::ConstantDataArray>(constant);
                auto str = constArray->getAsCString();
                ss << "\t.asciz\t\"" << escapeString(str) << "\"" << std::endl;
            } else {
                auto numElements = type->getArraySize();
                for (int i = 0; i < numElements; i++) {
                    auto element = constant->getAggregateElement(i);
                    ss << dumpGlobalInitializer(name, elementType, element);
                }
            }
        } else if (type->isIntegerType()) {
            auto constInt = llvm::dyn_cast<llvm::ConstantInt>(constant);
            // builder.addOp(".long", fmt::format("{}", constInt->getSExtValue()));
            ss << "\t.long\t" << constInt->getSExtValue() << std::endl;
        }
        return ss.str();
    }

    auto dump() const -> string
    {
        auto ss = std::stringstream {};
        ss << "\t.intel_syntax\tnoprefix\n"
           << "\t.text\n";
        for (const auto& function : functions) {
            ss << "\t.globl\t" << function->getName() << "\n"
               << "\t.type\t" << function->getName() << ", @function\n"
               << function->dump();
        }
        ss << "\t.section\t.data\n";
        for (const auto& global : globals) {
            if (global->isConstant())
                continue;
            ss << "\t.globl\t" << global->getName()
               << "\n"
               //    << "\t.data\n"
               //    << "\t.align\t8\n"
               //    << "\t.type\t" << global->getName() << ", @object\n"
               //    << "\t.size\t" << global->getName() << ", " << global->getSize() << "\n"
               << global->getName() << ":\n";
            if (global->hasInitializer()) {
                ss << dumpGlobalInitializer(
                    global->getName(), global->getType(), global->getInitializer());
            } else {
                ss << "\t.zero\t" << global->getSize() << std::endl;
            }
        }
        ss << "\t.section\t.rodata\n";
        for (const auto& global : globals) {
            if (!global->isConstant())
                continue;
            ss << "\t.globl\t" << global->getName()
               << "\n"
               //    << "\t.data\n"
               //    << "\t.align\t8\n"
               //    << "\t.type\t" << global->getName() << ", @object\n"
               //    << "\t.size\t" << global->getName() << ", " << global->getSize() << "\n"
               << global->getName() << ":\n";
            if (global->hasInitializer()) {
                ss << dumpGlobalInitializer(
                    global->getName(), global->getType(), global->getInitializer());
            } else {
                ss << "\t.zero\t" << global->getSize() << std::endl;
            }
        }
        ss << "\t.ident\t\"Compiled by mycc\"\n";
        return ss.str();
    }
};
}