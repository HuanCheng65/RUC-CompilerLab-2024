#include "cfg.hpp"
#include "reg_manage.hpp"
#include "registers.hpp"
#include <llvm/IR/Instruction.h>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace sysy {
class InterferenceGraph {
public:
    void addEdge(const std::string& u, const std::string& v)
    {
        if (u == v)
            return;
        adjList[u].insert(v);
        adjList[v].insert(u);
    }

    const std::unordered_map<std::string, std::unordered_set<std::string>>& getGraph() const
    {
        return adjList;
    }

    void printGraph() const
    {
        for (const auto& node : adjList) {
            fmt::println("{}: {}", node.first, fmt::join(node.second, " "));
        }
    }

private:
    std::unordered_map<std::string, std::unordered_set<std::string>> adjList;
};

void buildInterferenceGraph(CFGNodePtr cfgRoot, InterferenceGraph& graph);

class LocalVariableManager {
    struct LocalVarLifeTime {
        int start, end;
    };

    enum class VarLoc {
        Stack,
        Register
    };

    enum class VarType {
        Int,
        Ptr
    };

    struct VariableInfo : public std::enable_shared_from_this<VariableInfo> {
        LocalVarLifeTime lifeTime { -1, -1 };
        VarLoc loc;
        VarType type;
        Register reg;
        int stackOffset, size;
        bool isPtr, preAllocated = false;
    };

    RegisterManagerPtr regAlloc;
    std::unordered_map<std::string, VariableInfo> varInfoMap;
    size_t callerSavedRegSize = 0, calleeSavedRegSize = 0, maxStackArgSize = 0, stackVarSize = 0;
    bool alignCalleeSavedReg = false;

    void setSize(std::string_view name, size_t size)
    {
        varInfoMap[name.data()].size = size;
    }

    void setVarLoc(std::string_view name, VarLoc loc)
    {
        varInfoMap[name.data()].loc = loc;
    }

    void setVarReg(std::string_view name, Register reg)
    {
        varInfoMap[name.data()].loc = VarLoc::Register;
        varInfoMap[name.data()].reg = reg;
    }

    void setVarStackOffset(std::string_view name, int offset)
    {
        varInfoMap[name.data()].loc = VarLoc::Stack;
        varInfoMap[name.data()].stackOffset = offset;
    }

    void setVarPreAllocated(std::string_view name)
    {
        varInfoMap[name.data()].preAllocated = true;
    }

    void setVarIsPtr(std::string_view name, bool isPtr)
    {
        varInfoMap[name.data()].isPtr = isPtr;
    }

    void setVarLifeTime(std::string_view name, int loc)
    {
        if (varInfoMap[name.data()].lifeTime.start == -1) {
            varInfoMap[name.data()].lifeTime.start = loc;
        }
        varInfoMap[name.data()].lifeTime.start = std::min(varInfoMap[name.data()].lifeTime.start, loc);
        varInfoMap[name.data()].lifeTime.end = std::max(varInfoMap[name.data()].lifeTime.end, loc);
    }

public:
    LocalVariableManager(RegisterManagerPtr regAlloc)
        : regAlloc(regAlloc)
    {
    }

    VarLoc getVarLoc(std::string_view name) const
    {
        return varInfoMap.at(name.data()).loc;
    }

    Register getVarReg(std::string_view name) const
    {
        return varInfoMap.at(name.data()).reg;
    }

    int getVarStackOffset(std::string_view name) const
    {
        return varInfoMap.at(name.data()).stackOffset;
    }

    bool isVarPreAllocated(std::string_view name) const
    {
        return varInfoMap.at(name.data()).preAllocated;
    }

    void preAllocate(std::string_view name, size_t size, int loc, bool forceStack = false)
    {
        setSize(name, size);
        if (!forceStack) {
            auto& [start, end] = getLifeTime(name);
            auto regOrNull = regAlloc->preAllocate(start, end);
            if (regOrNull.has_value()) {
                auto reg = regOrNull.value();
                if (size == 4) {
                    reg = getLowerRegister(reg);
                }
                setVarReg(name, reg);
                setVarPreAllocated(name);
                return;
            }
        }
        setVarStackOffset(name, stackVarSize);
        setVarPreAllocated(name);
        stackVarSize += size;
    }

    void preAllocateOnReg(std::string_view name, size_t size, Register reg)
    {
        reg = getPhysicalRegister(reg);
        if (size == 4) {
            reg = getLowerRegister(reg);
        }
        auto& [start, end] = getLifeTime(name);
        regAlloc->preAllocate(reg, start, end);
        setVarReg(name, reg);
        setVarPreAllocated(name);
    }

    void preAllocateOnReg(std::string_view name, size_t size)
    {
        auto& [start, end] = getLifeTime(name);
        auto reg = regAlloc->preAllocate(start, end);
        if (!reg) {
            throw std::runtime_error(
                fmt::format("Variable {} not pre-allocated because of no available register", name));
        }
        if (size == 4) {
            reg = getLowerRegister(reg.value());
        }
        regAlloc->preAllocate(reg.value(), start, end);
        setVarReg(name, reg.value());
        setVarPreAllocated(name);
    }

    void preAllocateReg(Register reg, int loc)
    {
        regAlloc->preAllocate(reg, loc, loc);
    }

    void preAllocateArg(std::string_view name, size_t size, Register reg)
    {
        if (size == 4) {
            reg = getLowerRegister(reg);
        }
        setVarReg(name, reg);
        setVarPreAllocated(name);
    }

    void allocate(std::string_view name, size_t size, bool isPtr = false)
    {
        if (!exist(name)) {
            throw std::runtime_error(fmt::format("Variable {} not pre-allocated", name));
        }
        if (getVarLoc(name) == VarLoc::Register) {
            regAlloc->allocate(getVarReg(name));
        }
        setVarIsPtr(name, isPtr);
    }

    void free(std::string_view name)
    {
        if (!exist(name)) {
            throw std::runtime_error(fmt::format("Variable {} not pre-allocated", name));
        }
        if (getVarLoc(name) == VarLoc::Register) {
            regAlloc->free(getVarReg(name));
        }
    }

    void setCallerSavedRegSize(size_t size)
    {
        callerSavedRegSize = std::max(callerSavedRegSize, size);
    }

    void setCalleeSavedRegCount(size_t count)
    {
        calleeSavedRegSize = count * 8;
        alignCalleeSavedReg = (count % 2 != 0);
    }

    void setMaxStackArgSize(size_t size)
    {
        maxStackArgSize = std::max(maxStackArgSize, size);
    }

    void ping(std::string_view name, int loc)
    {
        setVarLifeTime(name, loc);
    }

    void freeDeadVars(int loc)
    {
        for (auto& [name, varInfo] : varInfoMap) {
            auto lifeTime = varInfo.lifeTime;
            if (lifeTime.end < loc) {
                if (varInfo.loc == VarLoc::Register) {
                    regAlloc->free(varInfo.reg);
                }
            }
        }
    }

    bool isAlive(std::string_view name, int loc) const
    {
        if (!exist(name)) {
            return false;
        }
        auto& [start, end] = getLifeTime(name);
        return start <= loc && loc <= end;
    }

    bool exist(std::string_view name) const
    {
        return varInfoMap.find(name.data()) != varInfoMap.end();
    }

    int getAlignmentSize() const
    {
        return getAlignedTotalSize() - stackVarSize - callerSavedRegSize - maxStackArgSize - calleeSavedRegSize;
    }

    int getCallerSavedRegLocByRsp(int index) const
    {
        return maxStackArgSize + index * 8;
    }

    int getCalleeSavedRegLocByRsp(int index) const
    {
        return maxStackArgSize + callerSavedRegSize + getAlignmentSize() + stackVarSize + index * 8;
    }

    int getStackArgLocByRsp(int index) const
    {
        return index * 8;
    }

    int getLocByRsp(std::string_view name) const
    {
        return maxStackArgSize + callerSavedRegSize + getAlignmentSize() + getVarStackOffset(name);
    }

    int getAlignedTotalSize() const
    {
        auto totalSize = stackVarSize + callerSavedRegSize + maxStackArgSize + calleeSavedRegSize;
        auto alignTotalSize = (totalSize + 15) / 16 * 16;
        return alignTotalSize;
    }

    bool shouldAlignCalleeSavedReg() const
    {
        return alignCalleeSavedReg;
    }

    bool isStackUsed() const
    {
        return stackVarSize || maxStackArgSize || callerSavedRegSize || calleeSavedRegSize;
    }

    bool isPtr(std::string_view name) const
    {
        return varInfoMap.at(name.data()).isPtr;
    }

    bool isOnReg(std::string_view name) const
    {
        return varInfoMap.at(name.data()).loc == VarLoc::Register;
    }

    Register getReg(std::string_view name) const
    {
        return varInfoMap.at(name.data()).reg;
    }

    auto getFirstVarOnRegAfter(Register reg, int loc) const -> std::optional<std::string>
    {
        std::optional<std::string> ret;
        int minLoc = INT_MAX;
        for (const auto& [name, varInfo] : varInfoMap) {
            auto& [start, end] = varInfo.lifeTime;
            if (varInfo.loc == VarLoc::Register && isOnSameRegister(reg, varInfo.reg) && start > loc && start < minLoc) {
                minLoc = start;
                ret = name;
            }
        }
        return ret;
    }

    auto getLastVarOnRegInBlockBefore(CFGNodePtr cfgNode, Register reg, int loc) const -> std::optional<std::string>
    {
        auto blockEnd = cfgNode->end;
        std::optional<std::string> ret;
        int maxLoc = -1;
        for (const auto& [name, varInfo] : varInfoMap) {
            if (varInfo.loc != VarLoc::Register || !isOnSameRegister(reg, varInfo.reg))
                continue;
            auto& [start, end] = varInfo.lifeTime;
            if (isAlive(name, loc) && end <= blockEnd && end > maxLoc) {
                maxLoc = end;
                ret = name;
            }
        }
        return ret;
    }

    const LocalVarLifeTime& getLifeTime(std::string_view name) const
    {
        return varInfoMap.at(name.data()).lifeTime;
    }

    bool isRegUsed(Register reg) const
    {
        reg = getPhysicalRegister(reg);
        if (reg == Register::RBP) {
            return isStackUsed();
        }
        for (const auto& [name, varInfo] : varInfoMap) {
            if (varInfo.loc == VarLoc::Register && isOnSameRegister(reg, varInfo.reg)) {
                return true;
            }
        }
        return false;
    }

    bool isRegUsedAfter(Register reg, int loc) const
    {
        for (const auto& [name, varInfo] : varInfoMap) {
            auto& [start, end] = varInfo.lifeTime;
            if (varInfo.loc == VarLoc::Register && isOnSameRegister(reg, varInfo.reg) && end > loc) {
                return true;
            }
        }
        return false;
    }

    auto getVarInfoMap() const
    {
        return varInfoMap;
    }
};
} // namespace sysy
