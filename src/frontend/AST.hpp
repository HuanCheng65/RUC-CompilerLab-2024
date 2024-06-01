#pragma once
#include "utils.hpp"
#include <cstdio>
#include <memory>
#include <string>
#include <vector>

using std::shared_ptr, std::enable_shared_from_this, std::make_shared;
using std::string, std::string_view;
using std::vector;

class BaseAST;

class ConstArrayAST;
class ArrayAST;

class CompUnitAST;

class FuncDefAST;
class FuncFParamsAST;
class FuncFParamAST;
class FuncRParamsAST;

class DeclAST;

class ConstDeclAST;
class ConstDefAST;
class ConstInitAST;
class ConstInitValAST;
class ConstInitListAST;

class VarDeclAST;
class VarDefAST;
class InitAST;
class InitValAST;
class InitListAST;

class BlockAST;
class StmtAST;
class BlockItemAST;

class LValAST;

class ConstExpAST;
class ExpAST;
class AddExpAST;
class MulExpAST;
class BaseUnaryExpAST;
class BasePrimaryExpAST;
class RelExpAST;
class EqExpAST;
class LAndExpAST;
class LOrExpAST;

enum class DeclType {
    Const,
    Var,
};

enum class DefType {
    Var,
    Array,
};

enum class ParamType {
    Var,
    Array,
};

enum class LValType {
    Var,
    Array,
};

enum class BlockItemType {
    Decl,
    Stmt,
};

enum class StmtType {
    Assign,
    Empty,
    Exp,
    Block,
    If,
    While,
    Break,
    Continue,
    Return,
};

enum class Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Not,
    Pos,
    Neg,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
};

enum class Btype {
    Void,
    Int,
};

using BaseASTPtr = shared_ptr<BaseAST>;

class BaseAST : public enable_shared_from_this<BaseAST> {
public:
    virtual ~BaseAST() = default;
};

class DeclAST : public BaseAST {
protected:
    DeclAST(DeclType declType)
        : declType(declType)
    {
    }

public:
    DeclType declType;
    Btype btype;
};

class CompUnitAST : public BaseAST {
public:
    vector<shared_ptr<DeclAST>> decls;
    vector<shared_ptr<FuncDefAST>> funcDefs;

    void addDecl(shared_ptr<DeclAST> decl)
    {
        decls.push_back(decl);
    }
    void addFuncDef(shared_ptr<FuncDefAST> funcDef)
    {
        funcDefs.push_back(funcDef);
    }

    static shared_ptr<CompUnitAST> create()
    {
        return make_shared<CompUnitAST>();
    }
};

class ConstDeclAST : public DeclAST {
public:
    vector<shared_ptr<ConstDefAST>> constDefs;

    ConstDeclAST()
        : DeclAST(DeclType::Const)
    {
    }

    void addConstDef(shared_ptr<ConstDefAST> constDef)
    {
        constDefs.push_back(constDef);
    }

    static shared_ptr<ConstDeclAST> create()
    {
        return make_shared<ConstDeclAST>();
    }
};

class ConstArrayAST : public BaseAST {
public:
    vector<shared_ptr<ConstExpAST>> constExps;

    void addConstExp(shared_ptr<ConstExpAST> constExp)
    {
        constExps.push_back(constExp);
    }

    static shared_ptr<ConstArrayAST> create()
    {
        return make_shared<ConstArrayAST>();
    }
};

class ArrayAST : public BaseAST {
public:
    vector<shared_ptr<ExpAST>> exps;

    void addExp(shared_ptr<ExpAST> exp)
    {
        exps.push_back(exp);
    }

    static shared_ptr<ArrayAST> create()
    {
        return make_shared<ArrayAST>();
    }
};

class ConstDefAST : public BaseAST {
public:
    DefType defType;
    string ident;
    vector<shared_ptr<ConstExpAST>> arrayDim {};
    shared_ptr<ConstInitAST> constInit;

    ConstDefAST(string_view ident, shared_ptr<ConstInitAST> constInit)
        : defType(DefType::Var)
        , ident(ident)
        , constInit(constInit)
    {
    }

    ConstDefAST(string_view ident, vector<shared_ptr<ConstExpAST>> arrayDim, shared_ptr<ConstInitAST> constInit)
        : defType(DefType::Array)
        , ident(ident)
        , arrayDim(arrayDim)
        , constInit(constInit)
    {
    }

    static shared_ptr<ConstDefAST> create(string_view ident, shared_ptr<ConstInitAST> constInit)
    {
        return make_shared<ConstDefAST>(ident, constInit);
    }

    static shared_ptr<ConstDefAST> create(string_view ident, vector<shared_ptr<ConstExpAST>> arrayDim,
        shared_ptr<ConstInitAST> constInit)
    {
        return make_shared<ConstDefAST>(ident, arrayDim, constInit);
    }
};

class ConstInitAST : public BaseAST {
};

class ConstInitValAST : public ConstInitAST {
public:
    shared_ptr<ConstExpAST> constExp;

    ConstInitValAST(shared_ptr<ConstExpAST> constExp)
        : constExp(constExp)
    {
    }

    static shared_ptr<ConstInitValAST> create(shared_ptr<ConstExpAST> constExp)
    {
        return make_shared<ConstInitValAST>(constExp);
    }

    static shared_ptr<ConstInitValAST> create(int constVal);
};

class ConstInitListAST : public ConstInitAST {
public:
    vector<shared_ptr<ConstInitAST>> constInits;

    void addConstInit(shared_ptr<ConstInitAST> constInit)
    {
        constInits.push_back(constInit);
    }

    static shared_ptr<ConstInitListAST> create()
    {
        return make_shared<ConstInitListAST>();
    }
};

class VarDeclAST : public DeclAST {
public:
    vector<shared_ptr<VarDefAST>> varDefs;

    VarDeclAST()
        : DeclAST(DeclType::Var)
    {
    }

    void addVarDef(shared_ptr<VarDefAST> varDef)
    {
        varDefs.push_back(varDef);
    }

    static shared_ptr<VarDeclAST> create()
    {
        return make_shared<VarDeclAST>();
    }
};

class VarDefAST : public BaseAST {
public:
    DefType defType;
    string ident;
    vector<shared_ptr<ConstExpAST>> arrayDim {};
    shared_ptr<InitAST> init = nullptr;

    VarDefAST(string_view ident, shared_ptr<InitAST> init = nullptr)
        : defType(DefType::Var)
        , ident(ident)
        , init(init)
    {
    }

    VarDefAST(string_view ident, vector<shared_ptr<ConstExpAST>> arrayDim, shared_ptr<InitAST> init = nullptr)
        : defType(DefType::Array)
        , ident(ident)
        , arrayDim(arrayDim)
        , init(init)
    {
    }

    static shared_ptr<VarDefAST> create(string_view ident, shared_ptr<InitAST> init = nullptr)
    {
        return make_shared<VarDefAST>(ident, init);
    }

    static shared_ptr<VarDefAST> create(string_view ident, vector<shared_ptr<ConstExpAST>> arrayDim,
        shared_ptr<InitAST> init = nullptr)
    {
        return make_shared<VarDefAST>(ident, arrayDim, init);
    }
};

class InitAST : public BaseAST {
};

class InitValAST : public InitAST {
public:
    shared_ptr<ExpAST> exp;

    InitValAST(shared_ptr<ExpAST> exp)
        : exp(exp)
    {
    }

    int eval() const;

    static shared_ptr<InitValAST> create(shared_ptr<ExpAST> exp)
    {
        return make_shared<InitValAST>(exp);
    }

    static shared_ptr<InitValAST> create(int constVal);
};

class InitListAST : public InitAST {
public:
    vector<shared_ptr<InitAST>> inits;

    void addInit(shared_ptr<InitAST> init)
    {
        inits.push_back(init);
    }

    static shared_ptr<InitListAST> create()
    {
        return make_shared<InitListAST>();
    }

    static shared_ptr<InitListAST> zeros(int size)
    {
        auto initList = make_shared<InitListAST>();
        for (int i = 0; i < size; i++) {
            initList->addInit(InitValAST::create(0));
        }
        return initList;
    }
};

class LValAST : public BaseAST {
public:
    LValType lValType;
    string ident;
    vector<shared_ptr<ExpAST>> exps;

    LValAST(string_view ident)
        : lValType(LValType::Var)
        , ident(ident)
    {
    }

    LValAST(string_view ident, vector<shared_ptr<ExpAST>> exps)
        : lValType(LValType::Array)
        , ident(ident)
        , exps(exps)
    {
    }

    bool operator==(const LValAST& rhs) const
    {
        if (lValType != rhs.lValType)
            return false;
        if (lValType == LValType::Var)
            return ident == rhs.ident;
        return ident == rhs.ident && exps == rhs.exps;
    }

    void addExp(shared_ptr<ExpAST> exp)
    {
        lValType = LValType::Array;
        exps.push_back(exp);
    }

    static shared_ptr<LValAST> create(string_view ident)
    {
        return make_shared<LValAST>(ident);
    }

    static shared_ptr<LValAST> create(string_view ident, vector<shared_ptr<ExpAST>> exps)
    {
        return make_shared<LValAST>(ident, exps);
    }
};

class BasePrimaryExpAST : public BaseAST {
public:
    virtual int eval() const = 0;

    virtual bool isConst() const
    {
        return false;
    }

    virtual bool operator==(const BasePrimaryExpAST& rhs) const = 0;
};

class PrimaryExpAST : public BasePrimaryExpAST {
public:
    shared_ptr<ExpAST> exp;

    PrimaryExpAST(shared_ptr<ExpAST> exp)
        : exp(exp)
    {
    }

    int eval() const override;

    bool isConst() const override;

    bool operator==(const BasePrimaryExpAST& rhs) const override;

    static shared_ptr<PrimaryExpAST> create(shared_ptr<ExpAST> exp)
    {
        return make_shared<PrimaryExpAST>(exp);
    }
};

class LValPrimaryExpAST : public BasePrimaryExpAST {
public:
    shared_ptr<LValAST> lVal;

    LValPrimaryExpAST(shared_ptr<LValAST> lVal)
        : lVal(lVal)
    {
    }

    [[noreturn]] int eval() const override;

    bool operator==(const BasePrimaryExpAST& rhs) const override
    {
        if (auto rhsPtr = dynamic_cast<const LValPrimaryExpAST*>(&rhs)) {
            return *lVal == *rhsPtr->lVal;
        }
        return false;
    }

    static shared_ptr<LValPrimaryExpAST> create(shared_ptr<LValAST> lVal)
    {
        return make_shared<LValPrimaryExpAST>(lVal);
    }
};

class BaseUnaryExpAST : public BaseAST {
public:
    virtual int eval() const = 0;

    virtual bool isConst() const
    {
        return false;
    }

    virtual bool operator==(const BaseUnaryExpAST& rhs) const = 0;
};

class UnaryExpAST : public BaseUnaryExpAST {
public:
    Op op;
    shared_ptr<BaseUnaryExpAST> unaryExp;

    UnaryExpAST(Op op, shared_ptr<BaseUnaryExpAST> unaryExp)
        : op(op)
        , unaryExp(unaryExp)
    {
    }

    int eval() const override;

    bool isConst() const override
    {
        return unaryExp->isConst();
    }

    bool operator==(const BaseUnaryExpAST& rhs) const override
    {
        if (auto rhsPtr = dynamic_cast<const UnaryExpAST*>(&rhs)) {
            return op == rhsPtr->op && *unaryExp == *rhsPtr->unaryExp;
        }
        return false;
    }

    static shared_ptr<UnaryExpAST> create(Op op, shared_ptr<BaseUnaryExpAST> unaryExp)
    {
        return make_shared<UnaryExpAST>(op, unaryExp);
    }
};

class PrimaryUnaryExpAST : public BaseUnaryExpAST {
public:
    shared_ptr<BasePrimaryExpAST> primaryExp;

    PrimaryUnaryExpAST(shared_ptr<BasePrimaryExpAST> primaryExp)
        : primaryExp(primaryExp)
    {
    }

    int eval() const override;

    bool isConst() const override
    {
        return primaryExp->isConst();
    }

    bool operator==(const BaseUnaryExpAST& rhs) const override
    {
        if (auto rhsPtr = dynamic_cast<const PrimaryUnaryExpAST*>(&rhs)) {
            return *primaryExp == *rhsPtr->primaryExp;
        }
        return false;
    }

    static shared_ptr<PrimaryUnaryExpAST> create(shared_ptr<BasePrimaryExpAST> primaryExp)
    {
        return make_shared<PrimaryUnaryExpAST>(primaryExp);
    }
};

class FuncCallUnaryExpAST : public BaseUnaryExpAST {
public:
    string ident;
    shared_ptr<FuncRParamsAST> funcRParams;

    FuncCallUnaryExpAST(string_view ident, shared_ptr<FuncRParamsAST> funcRParams)
        : ident(ident)
        , funcRParams(funcRParams)
    {
    }

    [[noreturn]] int eval() const override;

    bool operator==(const BaseUnaryExpAST& rhs) const override;

    static shared_ptr<FuncCallUnaryExpAST> create(string_view ident, shared_ptr<FuncRParamsAST> funcRParams)
    {
        return make_shared<FuncCallUnaryExpAST>(ident, funcRParams);
    }
};

class MulExpAST : public BaseAST {
public:
    enum class Type {
        Single,
        Complex,
    };

    Type type;
    Op op;
    shared_ptr<BaseUnaryExpAST> unaryExp;
    shared_ptr<MulExpAST> mulExp;

    MulExpAST(shared_ptr<BaseUnaryExpAST> unaryExp)
        : type(Type::Single)
        , unaryExp(unaryExp)
    {
    }

    MulExpAST(shared_ptr<MulExpAST> mulExp, Op op, shared_ptr<BaseUnaryExpAST> unaryExp)
        : type(Type::Complex)
        , op(op)
        , unaryExp(unaryExp)
        , mulExp(mulExp)
    {
    }

    int eval() const;

    bool isConst() const;

    bool operator==(const MulExpAST& rhs) const
    {
        if (type != rhs.type)
            return false;
        if (type == Type::Single)
            return *unaryExp == *rhs.unaryExp;
        return op == rhs.op && *mulExp == *rhs.mulExp && *unaryExp == *rhs.unaryExp;
    }

    static shared_ptr<MulExpAST> create(shared_ptr<BaseUnaryExpAST> unaryExp)
    {
        return make_shared<MulExpAST>(unaryExp);
    }

    static shared_ptr<MulExpAST> create(shared_ptr<MulExpAST> mulExp, Op op, shared_ptr<BaseUnaryExpAST> unaryExp)
    {
        return make_shared<MulExpAST>(mulExp, op, unaryExp);
    }
};

class AddExpAST : public BaseAST {
public:
    enum class Type {
        Single,
        Complex,
    };

    Type type;
    Op op;
    shared_ptr<MulExpAST> mulExp;
    shared_ptr<AddExpAST> addExp;

    AddExpAST(shared_ptr<MulExpAST> mulExp)
        : type(Type::Single)
        , mulExp(mulExp)
    {
    }

    AddExpAST(shared_ptr<AddExpAST> addExp, Op op, shared_ptr<MulExpAST> mulExp)
        : type(Type::Complex)
        , op(op)
        , mulExp(mulExp)
        , addExp(addExp)
    {
    }

    int eval() const;

    bool isConst() const;

    bool operator==(const AddExpAST& rhs) const
    {
        if (type != rhs.type)
            return false;
        if (type == Type::Single)
            return *mulExp == *rhs.mulExp;
        return op == rhs.op && *addExp == *rhs.addExp && *mulExp == *rhs.mulExp;
    }

    static shared_ptr<AddExpAST> create(shared_ptr<MulExpAST> mulExp)
    {
        return make_shared<AddExpAST>(mulExp);
    }

    static shared_ptr<AddExpAST> create(shared_ptr<AddExpAST> addExp, Op op, shared_ptr<MulExpAST> mulExp)
    {
        return make_shared<AddExpAST>(addExp, op, mulExp);
    }
};

class RelExpAST : public BaseAST {
public:
    enum class Type {
        Single,
        Complex,
    };

    Type type;
    shared_ptr<AddExpAST> addExp;
    shared_ptr<RelExpAST> relExp;
    Op op;

    RelExpAST(shared_ptr<AddExpAST> addExp)
        : type(Type::Single)
        , addExp(addExp)
    {
    }

    RelExpAST(shared_ptr<RelExpAST> relExp, Op op, shared_ptr<AddExpAST> addExp)
        : type(Type::Complex)
        , addExp(addExp)
        , relExp(relExp)
        , op(op)
    {
    }

    int eval() const;

    bool isConst() const;

    static shared_ptr<RelExpAST> create(shared_ptr<AddExpAST> addExp)
    {
        return make_shared<RelExpAST>(addExp);
    }

    static shared_ptr<RelExpAST> create(shared_ptr<RelExpAST> relExp, Op op, shared_ptr<AddExpAST> addExp)
    {
        return make_shared<RelExpAST>(relExp, op, addExp);
    }
};

class EqExpAST : public BaseAST {
public:
    enum class Type {
        Single,
        Complex,
    };

    Type type;
    Op op;
    shared_ptr<RelExpAST> relExp;
    shared_ptr<EqExpAST> eqExp;

    EqExpAST(shared_ptr<RelExpAST> relExp)
        : type(Type::Single)
        , relExp(relExp)
    {
    }

    EqExpAST(shared_ptr<EqExpAST> eqExp, Op op, shared_ptr<RelExpAST> relExp)
        : type(Type::Complex)
        , op(op)
        , relExp(relExp)
        , eqExp(eqExp)
    {
    }

    int eval() const;

    bool isConst() const;

    static shared_ptr<EqExpAST> create(shared_ptr<RelExpAST> relExp)
    {
        return make_shared<EqExpAST>(relExp);
    }

    static shared_ptr<EqExpAST> create(shared_ptr<EqExpAST> eqExp, Op op, shared_ptr<RelExpAST> relExp)
    {
        return make_shared<EqExpAST>(eqExp, op, relExp);
    }
};

class LAndExpAST : public BaseAST {
public:
    vector<shared_ptr<EqExpAST>> eqExps;

    int eval() const;

    bool isConst() const;

    void addEqExp(shared_ptr<EqExpAST> eqExp)
    {
        eqExps.push_back(eqExp);
    }

    static shared_ptr<LAndExpAST> create()
    {
        return make_shared<LAndExpAST>();
    }
};

class LOrExpAST : public BaseAST {
public:
    vector<shared_ptr<LAndExpAST>> lAndExps;

    int eval() const;

    bool isConst() const;

    void addLAndExp(shared_ptr<LAndExpAST> lAndExp)
    {
        lAndExps.push_back(lAndExp);
    }

    static shared_ptr<LOrExpAST> create()
    {
        return make_shared<LOrExpAST>();
    }
};

class CondAST : public BaseAST {
public:
    shared_ptr<LOrExpAST> lOrExp;

    CondAST(shared_ptr<LOrExpAST> lOrExp)
        : lOrExp(lOrExp)
    {
    }

    int eval() const;

    bool isConst() const;

    static shared_ptr<CondAST> create(shared_ptr<LOrExpAST> lOrExp)
    {
        return make_shared<CondAST>(lOrExp);
    }
};

class NumberPrimaryExpAST : public BasePrimaryExpAST {
public:
    int number;

    NumberPrimaryExpAST(int number)
        : number(number)
    {
    }

    int eval() const override
    {
        return number;
    }

    bool isConst() const override
    {
        return true;
    }

    bool operator==(const BasePrimaryExpAST& rhs) const override
    {
        if (auto rhsPtr = dynamic_cast<const NumberPrimaryExpAST*>(&rhs)) {
            return number == rhsPtr->number;
        }
        return false;
    }

    static shared_ptr<NumberPrimaryExpAST> create(int number)
    {
        return make_shared<NumberPrimaryExpAST>(number);
    }
};

class ConstExpAST : public BaseAST {
public:
    shared_ptr<AddExpAST> addExp;

    ConstExpAST(shared_ptr<AddExpAST> addExp)
        : addExp(addExp)
    {
    }

    int eval() const
    {
        return addExp->eval();
    }

    bool isConst() const
    {
        return addExp->isConst();
    }

    bool operator==(const ConstExpAST& rhs) const
    {
        return *addExp == *rhs.addExp;
    }

    static shared_ptr<ConstExpAST> create(shared_ptr<AddExpAST> addExp)
    {
        return make_shared<ConstExpAST>(addExp);
    }
};

class ExpAST : public BaseAST {
public:
    shared_ptr<AddExpAST> addExp;

    ExpAST(shared_ptr<AddExpAST> addExp)
        : addExp(addExp)
    {
    }

    int eval() const
    {
        return addExp->eval();
    }

    bool isConst() const
    {
        return addExp->isConst();
    }

    bool operator==(const ExpAST& rhs) const
    {
        return *addExp == *rhs.addExp;
    }

    static shared_ptr<ExpAST> create(shared_ptr<AddExpAST> addExp)
    {
        return make_shared<ExpAST>(addExp);
    }
};

class BaseFuncRParamAST : public BaseAST {
public:
    virtual ~BaseFuncRParamAST() = default;

    virtual bool operator==(const BaseFuncRParamAST& rhs) const = 0;
};

class FuncRParamsAST : public BaseAST {
public:
    vector<shared_ptr<BaseFuncRParamAST>> funcRParams;

    void addFuncRParam(shared_ptr<BaseFuncRParamAST> funcRParam)
    {
        funcRParams.push_back(funcRParam);
    }

    bool operator==(const FuncRParamsAST& rhs) const
    {
        if (funcRParams.size() != rhs.funcRParams.size())
            return false;
        for (size_t i = 0; i < funcRParams.size(); i++) {
            if (*funcRParams[i] != *rhs.funcRParams[i])
                return false;
        }
        return true;
    }

    static shared_ptr<FuncRParamsAST> create()
    {
        return make_shared<FuncRParamsAST>();
    }
};

class ExpFuncRParamAST : public BaseFuncRParamAST {
public:
    shared_ptr<ExpAST> exp;

    ExpFuncRParamAST(shared_ptr<ExpAST> exp)
        : exp(exp)
    {
    }

    bool operator==(const BaseFuncRParamAST& rhs) const override
    {
        if (auto rhsPtr = dynamic_cast<const ExpFuncRParamAST*>(&rhs)) {
            return *exp == *rhsPtr->exp;
        }
        return false;
    }

    static shared_ptr<ExpFuncRParamAST> create(shared_ptr<ExpAST> exp)
    {
        return make_shared<ExpFuncRParamAST>(exp);
    }
};

class StringLiteralFuncRParamAST : public BaseFuncRParamAST {
public:
    string str;

    StringLiteralFuncRParamAST(string_view str)
        : str(str)
    {
    }

    bool operator==(const BaseFuncRParamAST& rhs) const override
    {
        if (auto rhsPtr = dynamic_cast<const StringLiteralFuncRParamAST*>(&rhs)) {
            return str == rhsPtr->str;
        }
        return false;
    }

    static shared_ptr<StringLiteralFuncRParamAST> create(string_view str)
    {
        return make_shared<StringLiteralFuncRParamAST>(str);
    }
};

class AddressFuncRParamAST : public BaseFuncRParamAST {
public:
    shared_ptr<LValAST> lVal;

    AddressFuncRParamAST(shared_ptr<LValAST> lVal)
        : lVal(lVal)
    {
    }

    bool operator==(const BaseFuncRParamAST& rhs) const override
    {
        if (auto rhsPtr = dynamic_cast<const AddressFuncRParamAST*>(&rhs)) {
            return *lVal == *rhsPtr->lVal;
        }
        return false;
    }

    static shared_ptr<AddressFuncRParamAST> create(shared_ptr<LValAST> lVal)
    {
        return make_shared<AddressFuncRParamAST>(lVal);
    }
};

class FuncDefAST : public BaseAST {
public:
    Btype btype;
    string ident;
    shared_ptr<FuncFParamsAST> funcFParams;
    shared_ptr<BlockAST> block;

    FuncDefAST(Btype btype, string_view ident)
        : btype(btype)
        , ident(ident)
    {
    }

    static shared_ptr<FuncDefAST> create(Btype btype, string_view ident)
    {
        return make_shared<FuncDefAST>(btype, ident);
    }
};

class FuncFParamsAST : public BaseAST {
public:
    vector<shared_ptr<FuncFParamAST>> funcFParams;

    void addFuncFParam(shared_ptr<FuncFParamAST> funcFParam)
    {
        funcFParams.push_back(funcFParam);
    }

    static shared_ptr<FuncFParamsAST> create()
    {
        return make_shared<FuncFParamsAST>();
    }
};

class FuncFParamAST : public BaseAST {
public:
    ParamType paramType;
    Btype btype;
    string ident;

protected:
    FuncFParamAST(ParamType paramType, Btype btype, string_view ident)
        : paramType(paramType)
        , btype(btype)
        , ident(ident)
    {
    }
};

class VarFuncFParamAST : public FuncFParamAST {
public:
    VarFuncFParamAST(Btype btype, string_view ident)
        : FuncFParamAST(ParamType::Var, btype, ident)
    {
    }

    static shared_ptr<VarFuncFParamAST> create(Btype btype, string_view ident)
    {
        return make_shared<VarFuncFParamAST>(btype, ident);
    }
};

class ArrayFuncFParamAST : public FuncFParamAST {
public:
    vector<shared_ptr<ExpAST>> arrayDim;

    ArrayFuncFParamAST(Btype btype, string_view ident, vector<shared_ptr<ExpAST>> arrayDim = {})
        : FuncFParamAST(ParamType::Array, btype, ident)
        , arrayDim(arrayDim)
    {
    }

    static shared_ptr<ArrayFuncFParamAST> create(Btype btype, string_view ident,
        vector<shared_ptr<ExpAST>> arrayDim = {})
    {
        return make_shared<ArrayFuncFParamAST>(btype, ident, arrayDim);
    }
};

class BlockAST : public BaseAST {
public:
    vector<shared_ptr<BlockItemAST>> blockItems;

    void addBlockItem(shared_ptr<BlockItemAST> blockItem)
    {
        blockItems.push_back(blockItem);
    }

    static shared_ptr<BlockAST> create()
    {
        return make_shared<BlockAST>();
    }
};

class BlockItemAST : public BaseAST {
public:
    BlockItemType blockItemType;

protected:
    BlockItemAST(BlockItemType blockItemType)
        : blockItemType(blockItemType)
    {
    }
};

class DeclBlockItemAST : public BlockItemAST {
public:
    shared_ptr<DeclAST> decl;

    DeclBlockItemAST(shared_ptr<DeclAST> decl)
        : BlockItemAST(BlockItemType::Decl)
        , decl(decl)
    {
    }

    static shared_ptr<DeclBlockItemAST> create(shared_ptr<DeclAST> decl)
    {
        return make_shared<DeclBlockItemAST>(decl);
    }
};

class StmtBlockItemAST : public BlockItemAST {
public:
    shared_ptr<StmtAST> stmt;

    StmtBlockItemAST(shared_ptr<StmtAST> stmt)
        : BlockItemAST(BlockItemType::Stmt)
        , stmt(stmt)
    {
    }

    static shared_ptr<StmtBlockItemAST> create(shared_ptr<StmtAST> stmt)
    {
        return make_shared<StmtBlockItemAST>(stmt);
    }
};

class StmtAST : public BaseAST {
public:
    StmtType stmtType;

protected:
    StmtAST(StmtType stmtType)
        : stmtType(stmtType)
    {
    }
};

class AssignStmtAST : public StmtAST {
public:
    shared_ptr<LValAST> lVal;
    shared_ptr<ExpAST> exp;

    AssignStmtAST(shared_ptr<LValAST> lVal, shared_ptr<ExpAST> exp)
        : StmtAST(StmtType::Assign)
        , lVal(lVal)
        , exp(exp)
    {
    }

    static shared_ptr<AssignStmtAST> create(shared_ptr<LValAST> lVal, shared_ptr<ExpAST> exp)
    {
        return make_shared<AssignStmtAST>(lVal, exp);
    }
};

class EmptyStmtAST : public StmtAST {
public:
    EmptyStmtAST()
        : StmtAST(StmtType::Empty)
    {
    }

    static shared_ptr<EmptyStmtAST> create()
    {
        return make_shared<EmptyStmtAST>();
    }
};

class ExpStmtAST : public StmtAST {
public:
    shared_ptr<ExpAST> exp;

    ExpStmtAST(shared_ptr<ExpAST> exp)
        : StmtAST(StmtType::Exp)
        , exp(exp)
    {
    }

    static shared_ptr<ExpStmtAST> create(shared_ptr<ExpAST> exp)
    {
        return make_shared<ExpStmtAST>(exp);
    }
};

class BlockStmtAST : public StmtAST {
public:
    shared_ptr<BlockAST> block;

    BlockStmtAST(shared_ptr<BlockAST> block)
        : StmtAST(StmtType::Block)
        , block(block)
    {
    }

    static shared_ptr<BlockStmtAST> create(shared_ptr<BlockAST> block)
    {
        return make_shared<BlockStmtAST>(block);
    }
};

class IfStmtAST : public StmtAST {
public:
    shared_ptr<CondAST> cond;
    shared_ptr<StmtAST> thenStmt;
    shared_ptr<StmtAST> elseStmt;

    IfStmtAST(shared_ptr<CondAST> cond, shared_ptr<StmtAST> thenStmt, shared_ptr<StmtAST> elseStmt = nullptr)
        : StmtAST(StmtType::If)
        , cond(cond)
        , thenStmt(thenStmt)
        , elseStmt(elseStmt)
    {
    }

    static shared_ptr<IfStmtAST> create(shared_ptr<CondAST> cond, shared_ptr<StmtAST> thenStmt,
        shared_ptr<StmtAST> elseStmt = nullptr)
    {
        return make_shared<IfStmtAST>(cond, thenStmt, elseStmt);
    }
};

class WhileStmtAST : public StmtAST {
public:
    shared_ptr<CondAST> cond;
    shared_ptr<StmtAST> stmt;

    WhileStmtAST(shared_ptr<CondAST> cond, shared_ptr<StmtAST> stmt)
        : StmtAST(StmtType::While)
        , cond(cond)
        , stmt(stmt)
    {
    }

    static shared_ptr<WhileStmtAST> create(shared_ptr<CondAST> cond, shared_ptr<StmtAST> stmt)
    {
        return make_shared<WhileStmtAST>(cond, stmt);
    }
};

class BreakStmtAST : public StmtAST {
public:
    BreakStmtAST()
        : StmtAST(StmtType::Break)
    {
    }

    static shared_ptr<BreakStmtAST> create()
    {
        return make_shared<BreakStmtAST>();
    }
};

class ContinueStmtAST : public StmtAST {
public:
    ContinueStmtAST()
        : StmtAST(StmtType::Continue)
    {
    }

    static shared_ptr<ContinueStmtAST> create()
    {
        return make_shared<ContinueStmtAST>();
    }
};

class ReturnStmtAST : public StmtAST {
public:
    shared_ptr<ExpAST> exp = nullptr;

    ReturnStmtAST(shared_ptr<ExpAST> exp = nullptr)
        : StmtAST(StmtType::Return)
        , exp(exp)
    {
    }

    static shared_ptr<ReturnStmtAST> create(shared_ptr<ExpAST> exp = nullptr)
    {
        return make_shared<ReturnStmtAST>(exp);
    }
};
