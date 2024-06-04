#include "backend/instructions.hpp"
#include "backend/registers.hpp"
#include <print>
#include <unordered_map>
#include <unordered_set>

using std::unordered_map;
using std::unordered_set;

namespace sysy {
void analyzeLiveness(AsmFunctionPtr func);
}