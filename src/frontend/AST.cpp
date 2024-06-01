#include "AST.hpp"
#include <algorithm>

int AddExpAST::eval() const
{
    if (type == Type::Single) {
        return mulExp->eval();
    }

    auto left = addExp->eval();
    auto right = mulExp->eval();
    return op == Op::Add ? left + right : left - right;
}

bool AddExpAST::isConst() const
{
    if (type == Type::Single) {
        return mulExp->isConst();
    }

    auto left = addExp->isConst();
    auto right = mulExp->isConst();
    return left && right;
}

int MulExpAST::eval() const
{
    if (type == Type::Single) {
        return unaryExp->eval();
    }

    auto left = mulExp->eval();
    auto right = unaryExp->eval();
    if (op == Op::Mul) {
        return left * right;
    } else if (op == Op::Div) {
        return left / right;
    } else {
        return left % right;
    }
}

bool MulExpAST::isConst() const
{
    if (type == Type::Single) {
        return unaryExp->isConst();
    }

    auto left = mulExp->isConst();
    auto right = unaryExp->isConst();
    return left && right;
}

int UnaryExpAST::eval() const
{
    auto val = unaryExp->eval();
    if (op == Op::Pos) {
        return val;
    } else if (op == Op::Neg) {
        return -val;
    } else {
        return !val;
    }
}

int PrimaryUnaryExpAST::eval() const
{
    return primaryExp->eval();
}

[[noreturn]] int FuncCallUnaryExpAST::eval() const
{
    throw std::runtime_error("Function call in expression");
}

int PrimaryExpAST::eval() const
{
    return exp->eval();
}

bool PrimaryExpAST::isConst() const
{
    return exp->isConst();
}

[[noreturn]] int LValPrimaryExpAST::eval() const
{
    throw std::runtime_error("LVal in expression");
}

int RelExpAST::eval() const
{
    if (type == Type::Single) {
        return addExp->eval();
    }

    auto left = relExp->eval();
    auto right = addExp->eval();
    if (op == Op::Lt) {
        return left < right;
    } else if (op == Op::Le) {
        return left <= right;
    } else if (op == Op::Gt) {
        return left > right;
    } else {
        return left >= right;
    }
}

bool RelExpAST::isConst() const
{
    if (type == Type::Single) {
        return addExp->isConst();
    }

    auto left = relExp->isConst();
    auto right = addExp->isConst();
    return left && right;
}

int EqExpAST::eval() const
{
    if (type == Type::Single) {
        return relExp->eval();
    }

    auto left = eqExp->eval();
    auto right = relExp->eval();

    return op == Op::Eq ? left == right : left != right;
}

bool EqExpAST::isConst() const
{
    if (type == Type::Single) {
        return relExp->isConst();
    }

    auto left = eqExp->isConst();
    auto right = relExp->isConst();
    return left && right;
}

int LAndExpAST::eval() const
{
    return std::all_of(eqExps.begin(), eqExps.end(), [](auto const& exp) { return exp->eval(); });
}

bool LAndExpAST::isConst() const
{
    return std::all_of(eqExps.begin(), eqExps.end(), [](auto const& exp) { return exp->isConst(); });
}

int LOrExpAST::eval() const
{
    return std::any_of(lAndExps.begin(), lAndExps.end(), [](auto const& exp) { return exp->eval(); });
}

bool LOrExpAST::isConst() const
{
    return std::all_of(lAndExps.begin(), lAndExps.end(), [](auto const& exp) { return exp->isConst(); });
}

int CondAST::eval() const
{
    return lOrExp->eval();
}

int InitValAST::eval() const
{
    return exp->eval();
}

bool PrimaryExpAST::operator==(const BasePrimaryExpAST& rhs) const
{
    if (auto rhsPtr = dynamic_cast<const PrimaryExpAST*>(&rhs)) {
        return *exp == *rhsPtr->exp;
    }
    return false;
}

bool FuncCallUnaryExpAST::operator==(const BaseUnaryExpAST& rhs) const
{
    return false;
}

shared_ptr<InitValAST> InitValAST::create(int constVal)
{
    return make_shared<InitValAST>(ExpAST::create(
        AddExpAST::create(MulExpAST::create(PrimaryUnaryExpAST::create(NumberPrimaryExpAST::create(constVal))))));
}

shared_ptr<ConstInitValAST> ConstInitValAST::create(int constVal)
{
    return make_shared<ConstInitValAST>(ConstExpAST::create(
        AddExpAST::create(MulExpAST::create(PrimaryUnaryExpAST::create(NumberPrimaryExpAST::create(constVal))))));
}