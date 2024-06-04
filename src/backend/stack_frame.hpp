#pragma once
#include "backend/registers.hpp"
#include <memory>
#include <unordered_set>

using std::shared_ptr;
using std::unordered_set;

namespace sysy {
class StackFrame;

using StackFramePtr = shared_ptr<StackFrame>;

class StackFrame : public std::enable_shared_from_this<StackFrame> {
    int stackVarSize { 0 };
    bool argOnStack { false };

public:
    StackFrame() = default;

    int getStackVarSize() const { return stackVarSize; }

    int allocateStackVar(int size)
    {
        stackVarSize += size;
        return stackVarSize;
    }

    bool hasArgOnStack() const { return argOnStack; }

    void setArgOnStack() { argOnStack = true; }

    bool isStackUsed() const { return stackVarSize > 0 || argOnStack; }

    auto getStackSize() const { return stackVarSize; }

    auto getAlignedStackSize(bool notAlignTo16 = false) const
    {
        if (notAlignTo16) {
            auto alignedSize = (stackVarSize + 7) & ~7;
            if (alignedSize % 16 == 0) {
                alignedSize += 8;
            }
            return alignedSize;
        }
        return (stackVarSize + 15) & ~15;
    }
};
}
