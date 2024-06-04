#include "backend/instructions.hpp"

namespace sysy {
void Operand::replaceBy(OperandPtr other)
{
    auto self { shared_from_this() };
    for (auto& use : uses) {
        use->replaceBy(other);
        other->addUse(use);
    }
    uses.clear();
    for (auto& def : defs) {
        def->replaceBy(other);
        other->addDef(def);
    }
    defs.clear();
    if (self->getType()->getAllocSize() > other->getType()->getAllocSize()) {
        other->setType(self->getType());
    }
}
}