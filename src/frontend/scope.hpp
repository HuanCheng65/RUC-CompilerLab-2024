#pragma once
#include "frontend/return.hpp"
#include "frontend/utils.hpp"
#include <deque>
#include <memory>
#include <stack>
#include <string>

using std::deque;
using std::shared_ptr, std::enable_shared_from_this, std::make_shared;
using std::stack;
using std::string;
using std::string_view;
using std::stringstream;
using namespace std::string_literals;
using namespace std::string_view_literals;

enum class ScopeStatus {
    Alive,
    Dead,
};

enum class ScopeType {
    Function,
    Loop,
    If,
};

class Scope;

using ScopePtr = shared_ptr<Scope>;

class Scope : public enable_shared_from_this<Scope> {
    ScopeType type;
    ScopeStatus status;
    ScopePtr parent;

protected:
    Scope(ScopeType type, ScopePtr parent = nullptr)
        : type(type)
        , status(ScopeStatus::Alive)
        , parent(parent)
    {
    }

public:
    virtual ~Scope() = default;

    virtual IRLabelPtr getEndLabel() const = 0;

    void kill()
    {
        status = ScopeStatus::Dead;
    }

    void killFunc()
    {
        auto it = shared_from_this();
        while (it->getType() != ScopeType::Function) {
            it->kill();
            it = it->getParent();
        }
        it->kill();
    }

    void killLoop()
    {
        auto it = shared_from_this();
        while (it->getType() != ScopeType::Loop) {
            it->kill();
            it = it->getParent();
        }
        it->kill();
    }

    bool isAlive() const
    {
        if (status == ScopeStatus::Dead) {
            return false;
        }
        if (parent) {
            return parent->isAlive();
        }
        return true;
    }

    ScopeType getType() const
    {
        return type;
    }

    template <typename T>
    shared_ptr<T> getAs()
    {
        return std::static_pointer_cast<T>(shared_from_this());
    }

    template <typename T>
    shared_ptr<const T> getAs() const
    {
        return std::static_pointer_cast<const T>(shared_from_this());
    }

    ScopePtr getParent() const
    {
        return parent;
    }
};

class FunctionScope : public Scope {
    string name;
    bool hasReturnLabel { false };
    IRLabelPtr returnLabel;

public:
    FunctionScope(string_view name, IRLabelPtr returnLabel, ScopePtr parent = nullptr)
        : Scope(ScopeType::Function, parent)
        , name(name)
        , returnLabel(returnLabel)
    {
        hasReturnLabel = returnLabel != nullptr;
    }

    IRLabelPtr getEndLabel() const override
    {
        return returnLabel;
    }

    string getName() const
    {
        return name;
    }

    bool hasReturn() const
    {
        return hasReturnLabel;
    }
};

class LoopScope : public Scope {
    string condLabel, bodyLabel;
    IRLabelPtr endLabel;

public:
    LoopScope(string_view condLabel, string_view bodyLabel, IRLabelPtr endLabel, ScopePtr parent = nullptr)
        : Scope(ScopeType::Loop, parent)
        , condLabel(condLabel)
        , bodyLabel(bodyLabel)
        , endLabel(endLabel)
    {
    }

    string getCondLabel() const
    {
        return condLabel;
    }

    string getBodyLabel() const
    {
        return bodyLabel;
    }

    IRLabelPtr getEndLabel() const override
    {
        return endLabel;
    }
};

class IfScope : public Scope {
    string thenLabel;
    IRLabelPtr elseLabel, endLabel;
    bool hasElse;

public:
    IfScope(string_view thenLabel, IRLabelPtr elseLabel, IRLabelPtr endLabel, ScopePtr parent = nullptr)
        : Scope(ScopeType::If, parent)
        , thenLabel(thenLabel)
        , elseLabel(elseLabel)
        , endLabel(endLabel)
        , hasElse(true)
    {
    }

    IfScope(string_view thenLabel, IRLabelPtr endLabel, ScopePtr parent = nullptr)
        : Scope(ScopeType::If, parent)
        , thenLabel(thenLabel)
        , endLabel(endLabel)
        , hasElse(false)
    {
    }

    string getThenLabel() const
    {
        return thenLabel;
    }

    IRLabelPtr getElseLabel() const
    {
        return elseLabel;
    }

    IRLabelPtr getEndLabel() const override
    {
        return endLabel;
    }

    bool hasElseBlock() const
    {
        return hasElse;
    }
};

class ScopeManager {
private:
    deque<ScopePtr> scopeStack;
    stack<IRLabelPtr> endLabels;
    IRLabelPtr endLabel { nullptr };

public:
    shared_ptr<FunctionScope> pushFunction(string_view name, IRLabelPtr returnLabel)
    {
        // fmt::println("Push function scope");
        auto scope = make_shared<FunctionScope>(name, returnLabel, getCurrent());
        scopeStack.push_back(scope);
        endLabels.push(returnLabel);
        return scope;
    }

    shared_ptr<LoopScope> pushLoop(string_view condLabel, string_view bodyLabel, IRLabelPtr endLabel)
    {
        // fmt::println("Push loop scope");
        auto scope = make_shared<LoopScope>(condLabel, bodyLabel, endLabel, getCurrent());
        scopeStack.push_back(scope);
        endLabels.push(endLabel);
        return scope;
    }

    shared_ptr<IfScope> pushIfElse(string_view thenLabel, IRLabelPtr elseLabel, IRLabelPtr endLabel)
    {
        // fmt::println("Push if-else scope");
        auto scope = make_shared<IfScope>(thenLabel, elseLabel, endLabel, getCurrent());
        scopeStack.push_back(scope);
        endLabels.push(endLabel);
        return scope;
    }

    shared_ptr<IfScope> pushIf(string_view thenLabel, IRLabelPtr endLabel)
    {
        // fmt::println("Push if scope");
        auto scope = make_shared<IfScope>(thenLabel, endLabel, getCurrent());
        scopeStack.push_back(scope);
        endLabels.push(endLabel);
        return scope;
    }

    void kill()
    {
        scopeStack.back()->kill();
    }

    void pop()
    {
        // fmt::println("Pop scope");
        auto curScope = getCurrent();
        scopeStack.pop_back();
        auto newEndLabel = endLabels.top();
        auto deleteEndLabel = true;
        if (curScope->getType() == ScopeType::Loop) {
            deleteEndLabel = false;
        } else if (curScope->getType() == ScopeType::Function) {
            auto funcScope = curScope->getAs<FunctionScope>();
            if (!funcScope->hasReturn()) {
                deleteEndLabel = false;
            }
        }
        if (endLabel && deleteEndLabel) {
            // fmt::println("Set end label {} to {}", endLabel->str(), newEndLabel->str());
            endLabel->setDecl("");
            endLabel->setRef(newEndLabel->refRef());
        }
        endLabel = newEndLabel;
        endLabels.pop();
    }

    void step()
    {
        endLabel = nullptr;
    }

    bool isAlive() const
    {
        return scopeStack.back()->isAlive();
    }

    ScopePtr getCurrent() const
    {
        if (scopeStack.empty()) {
            return nullptr;
        }
        return scopeStack.back();
    }

    shared_ptr<FunctionScope> getCurrentFunction()
    {
        auto cur = getCurrent();
        while (cur) {
            if (cur->getType() == ScopeType::Function) {
                return cur->getAs<FunctionScope>();
            }
            cur = cur->getParent();
        }
        return nullptr;
    }

    shared_ptr<LoopScope> getCurrentLoop()
    {
        auto cur = getCurrent();
        while (cur) {
            if (cur->getType() == ScopeType::Loop) {
                return cur->getAs<LoopScope>();
            }
            cur = cur->getParent();
        }
        return nullptr;
    }

    shared_ptr<IfScope> getCurrentIf()
    {
        auto cur = getCurrent();
        while (cur) {
            if (cur->getType() == ScopeType::If) {
                return cur->getAs<IfScope>();
            }
            cur = cur->getParent();
        }
        return nullptr;
    }
};