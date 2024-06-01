#include "ir_string.hpp"
#include <memory>

IRNodePtr IRString::ref()
{
    auto selfPtr = shared_from_this();
    auto selfRef = make_shared<IRStringRef>(selfPtr);
    return std::static_pointer_cast<IRNode>(selfRef);
}
