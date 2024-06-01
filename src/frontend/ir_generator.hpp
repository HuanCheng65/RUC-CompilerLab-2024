#include "frontend/AST.hpp"
#include "frontend/return.hpp"
#include "frontend/scope.hpp"
#include "frontend/symbol.hpp"
#include "frontend/utils.hpp"
#include <cstring>
#include <fmt/core.h>
#include <fmt/format.h>
#include <memory>
#include <range/v3/range.hpp>
#include <range/v3/view.hpp>
#include <range/v3/view/for_each.hpp>
#include <stdexcept>
#include <string>
#include <vector>

using std::shared_ptr;
using namespace ranges;

class IRGenerator {
private:
    LLVMIRBuilder builder, globalDefBuilder;
    SymbolTableManager symbolTableManager {};
    ScopeManager scopeMgr;
    ReturnManager retMgr;

    bool isConstInitList(shared_ptr<InitListAST> initList) const
    {
        auto inits = initList->inits;
        return std::all_of(inits.begin(), inits.end(), [&](auto init) {
            if (auto initVal = std::dynamic_pointer_cast<InitValAST>(init)) {
                return isConst(initVal->exp);
            } else if (auto initList = std::dynamic_pointer_cast<InitListAST>(init)) {
                return isConstInitList(initList);
            } else {
                return false;
            }
        });
    }

    void fillArrayInitList(shared_ptr<InitListAST> initList, SymbolTypeInfoPtr typeInfo)
    {
        if (typeInfo->elementType->isInt()) {
            auto& inits = initList->inits;
            auto size = typeInfo->size;
            if (inits.size() < size) {
                auto diff = size - inits.size();
                for (auto i = 0; i < diff; i++) {
                    initList->addInit(InitValAST::create(0));
                }
            }
        } else if (typeInfo->elementType->isArray()) {
            auto& inits = initList->inits;
            for (auto& init : inits) {
                assert_ptr_type<InitListAST>(init, "Invalid init expression of array");
                auto initList = std::dynamic_pointer_cast<InitListAST>(init);
                fillArrayInitList(initList, typeInfo->elementType);
            }
            auto size = typeInfo->size;
            if (inits.size() < size) {
                auto diff = size - inits.size();
                for (auto i = 0; i < diff; i++) {
                    initList->addInit(createZeroInitList(typeInfo->elementType));
                }
            }
        }
    }

    shared_ptr<InitListAST> chunkInitList(vector<shared_ptr<ExpAST>>& exps, SymbolTypeInfoPtr typeInfo)
    {
        auto initList = InitListAST::create();
        if (typeInfo->elementType->isInt()) {
            int valueSize = exps.size();
            int size = std::min(valueSize, typeInfo->size);
            for (auto i = 0; i < size; i++) {
                initList->addInit(InitValAST::create(exps[i]));
            }
            exps.erase(exps.begin(), exps.begin() + size);
            fillArrayInitList(initList, typeInfo);
        } else {
            while (!exps.empty()) {
                auto subInitList = chunkInitList(exps, typeInfo->elementType);
                initList->addInit(subInitList);
            }
        }
        return initList;
    }

    shared_ptr<InitListAST> normalizeInitList(shared_ptr<InitListAST> initList, SymbolTypeInfoPtr arrayType)
    {
        auto& inits = initList->inits;
        if (inits.empty()) {
            return createZeroInitList(arrayType);
        }
        vector<shared_ptr<InitAST>> results;
        auto i = 0;
        while (i < inits.size()) {
            auto& init = inits[i];
            if (arrayType->elementType->isInt()) {
                if (is_ptr_type<InitValAST>(init)) {
                    results.push_back(init);
                    i++;
                } else {
                    throw std::runtime_error("Invalid expression of array element");
                }
            } else if (arrayType->elementType->isArray()) {
                if (auto initList = std::dynamic_pointer_cast<InitListAST>(init)) {
                    if (arrayType->elementType->elementType->isInt()) {
                        fillArrayInitList(initList, arrayType->elementType);
                    }
                    auto init = normalizeInitList(initList, arrayType->elementType);
                    results.push_back(std::dynamic_pointer_cast<InitAST>(init));
                    i++;
                } else if (is_ptr_type<InitValAST>(init)) {
                    auto temp = init;
                    auto exps = vector<shared_ptr<ExpAST>>();
                    while (is_ptr_type<InitValAST>(temp)) {
                        auto initVal = std::dynamic_pointer_cast<InitValAST>(temp);
                        exps.push_back(initVal->exp);
                        i++;
                        if (i >= inits.size())
                            break;
                        temp = inits[i];
                    }
                    auto subInitList = chunkInitList(exps, arrayType);
                    for (auto& init : subInitList->inits) {
                        assert_ptr_type<InitListAST>(init, "Invalid sub array");
                        auto initList = std::dynamic_pointer_cast<InitListAST>(init);
                        auto value = normalizeInitList(initList, arrayType->elementType);
                        results.push_back(std::dynamic_pointer_cast<InitAST>(value));
                    }
                } else
                    throw std::runtime_error("Invalid expression of array element");
            } else
                throw std::runtime_error("Unknown elementType");
        }
        auto result = InitListAST::create();
        result->inits = results;
        fillArrayInitList(result, arrayType);
        if (result->inits.size() > arrayType->size) {
            result->inits.erase(result->inits.begin() + arrayType->size, result->inits.end());
        }
        return result;
    }

    shared_ptr<ConstInitListAST> normalizeConstInitList(shared_ptr<ConstInitListAST> constInitList,
        SymbolTypeInfoPtr arrayType)
    {
        auto& constInits = constInitList->constInits;
        if (constInits.empty()) {
            return createZeroConstInitList(arrayType);
        }
        vector<shared_ptr<ConstInitAST>> results;
        auto i = 0;
        while (i < constInits.size()) {
            auto& constInit = constInits[i];
            if (arrayType->elementType->isInt()) {
                if (is_ptr_type<ConstInitValAST>(constInit)) {
                    results.push_back(constInit);
                    i++;
                } else {
                    throw std::runtime_error("Invalid expression of array element");
                }
            } else if (arrayType->elementType->isArray()) {
                if (auto constInitList = std::dynamic_pointer_cast<ConstInitListAST>(constInit)) {
                    if (arrayType->elementType->elementType->isInt()) {
                        fillArrayConstInitList(constInitList, arrayType->elementType);
                    }
                    auto constInit = normalizeConstInitList(constInitList, arrayType->elementType);
                    results.push_back(std::dynamic_pointer_cast<ConstInitAST>(constInit));
                    i++;
                } else if (is_ptr_type<ConstInitValAST>(constInit)) {
                    auto temp = constInit;
                    auto values = vector<int>();
                    while (is_ptr_type<ConstInitValAST>(temp)) {
                        auto initVal = std::dynamic_pointer_cast<ConstInitValAST>(temp);
                        values.push_back(eval(initVal->constExp));
                        i++;
                        if (i >= constInits.size())
                            break;
                        temp = constInits[i];
                    }
                    auto subConstInitList = chunkConstInitList(values, arrayType);
                    for (auto& constInit : subConstInitList->constInits) {
                        assert_ptr_type<ConstInitListAST>(constInit, "Invalid sub array");
                        auto constInitList = std::dynamic_pointer_cast<ConstInitListAST>(constInit);
                        auto value = normalizeConstInitList(constInitList, arrayType->elementType);
                        results.push_back(std::dynamic_pointer_cast<ConstInitAST>(value));
                    }
                } else
                    throw std::runtime_error("Unknown elementType");
            } else
                throw std::runtime_error("Unknown elementType");
        }
        auto result = ConstInitListAST::create();
        result->constInits = results;
        fillArrayConstInitList(result, arrayType);
        if (result->constInits.size() > arrayType->size) {
            result->constInits.erase(result->constInits.begin() + arrayType->size, result->constInits.end());
        }
        return result;
    }

    string dumpInitList(shared_ptr<InitListAST> initList, SymbolTypeInfoPtr arrayType)
    {
        auto normalizedInitList = normalizeInitList(initList, arrayType);
        auto results = normalizedInitList->inits | views::transform([&](auto init) {
            if (auto initVal = std::dynamic_pointer_cast<InitValAST>(init)) {
                return fmt::format("i32 {}", eval(initVal->exp));
            } else if (auto initList = std::dynamic_pointer_cast<InitListAST>(init)) {
                return fmt::format("{} {}", dumpTypeInfo(arrayType->elementType),
                    dumpInitList(initList, arrayType->elementType));
            } else {
                throw std::runtime_error("Invalid expression of array element");
            }
        }) | views::join(", "s)
            | to<string>();
        return fmt::format("[{}]", results);
    }

    void fillArrayConstInitList(shared_ptr<ConstInitListAST> constInitList, SymbolTypeInfoPtr typeInfo)
    {
        if (typeInfo->elementType->isInt()) {
            auto& inits = constInitList->constInits;
            auto size = typeInfo->size;
            if (inits.size() < size) {
                int diff = size - inits.size();
                for (auto i = 0; i < diff; i++) {
                    constInitList->addConstInit(ConstInitValAST::create(0));
                }
            }
        } else if (typeInfo->elementType->isArray()) {
            auto& inits = constInitList->constInits;
            for (auto& init : inits) {
                assert_ptr_type<ConstInitListAST>(init, "Invalid init expression of array");
                auto initList = std::dynamic_pointer_cast<ConstInitListAST>(init);
                fillArrayConstInitList(initList, typeInfo->elementType);
            }
            auto size = typeInfo->size;
            if (inits.size() < size) {
                int diff = size - inits.size();
                for (auto i = 0; i < diff; i++) {
                    constInitList->addConstInit(createZeroConstInitList(typeInfo->elementType));
                }
            }
        }
    }

    shared_ptr<ConstInitListAST> chunkConstInitList(vector<int>& values, SymbolTypeInfoPtr typeInfo)
    {
        auto constInitList = ConstInitListAST::create();
        if (typeInfo->elementType->isInt()) {
            int valueSize = values.size();
            int size = std::min(valueSize, typeInfo->size);
            for (auto i = 0; i < size; i++) {
                constInitList->addConstInit(ConstInitValAST::create(values[i]));
            }
            values.erase(values.begin(), values.begin() + size);
            fillArrayConstInitList(constInitList, typeInfo);
        } else {
            while (!values.empty()) {
                auto subConstInitList = chunkConstInitList(values, typeInfo->elementType);
                constInitList->addConstInit(subConstInitList);
            }
        }
        return constInitList;
    }

    string dumpConstInitList(shared_ptr<ConstInitListAST> constInitList, SymbolTypeInfoPtr arrayType)
    {
        auto normalizedConstInitList = normalizeConstInitList(constInitList, arrayType);
        auto results = normalizedConstInitList->constInits | views::transform([&](auto constInit) {
            if (auto constInitVal = std::dynamic_pointer_cast<ConstInitValAST>(constInit)) {
                return fmt::format("i32 {}", eval(constInitVal->constExp));
            } else if (auto constInitList = std::dynamic_pointer_cast<ConstInitListAST>(constInit)) {
                return fmt::format("{} {}", dumpTypeInfo(arrayType->elementType),
                    dumpConstInitList(constInitList, arrayType->elementType));
            } else {
                throw std::runtime_error("Invalid expression of array element");
            }
        }) | views::join(", "s)
            | to<string>();
        return fmt::format("[{}]", results);
    }

    string dumpTypeInfo(SymbolTypeInfoPtr typeInfo)
    {
        // fmt::println("typeInfo: {}", std::to_underlying(typeInfo->type));
        if (typeInfo->isInt()) {
            return "i32";
        } else if (typeInfo->isArray()) {
            return fmt::format("[{} x {}]", typeInfo->size, dumpTypeInfo(typeInfo->elementType));
        } else if (typeInfo->isPointer()) {
            return "ptr";
        } else {
            throw std::runtime_error("Unknown array type");
        }
    }

    vector<int> flattenConstInitList(shared_ptr<ConstInitListAST> constInitList)
    {
        auto& constInits = constInitList->constInits;
        vector<int> results;
        for (auto& constInit : constInits) {
            if (auto constInitVal = std::dynamic_pointer_cast<ConstInitValAST>(constInit)) {
                results.push_back(eval(constInitVal->constExp));
            } else if (auto constInitList = std::dynamic_pointer_cast<ConstInitListAST>(constInit)) {
                auto subResults = flattenConstInitList(constInitList);
                results.insert(results.end(), subResults.begin(), subResults.end());
            } else {
                throw std::runtime_error("Invalid expression of array element");
            }
        }
        return results;
    }

    void dumpLocalConstArrayInit(string_view name, shared_ptr<ConstInitListAST> constInitList,
        SymbolTypeInfoPtr arrayType)
    {
        auto normalizedConstInitList = normalizeConstInitList(constInitList, arrayType);
        auto constInits = normalizedConstInitList->constInits;
        auto index = 0;
        auto elementType = arrayType->elementType;
        for (auto& init : constInits) {
            if (elementType->isInt()) {
                assert_ptr_type<ConstInitValAST>(init, "Invalid expression of array element");
                auto initVal = std::dynamic_pointer_cast<ConstInitValAST>(init);
                if (isConst(initVal->constExp)) {
                    auto value = eval(initVal->constExp);
                    auto temp = symbolTableManager.genTempName();
                    builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(arrayType), name, builder.arrayIndex("i64", std::to_string(index))));
                    builder.addStore("i32", std::to_string(value), temp);
                } else {
                    auto value = dumpConstExp(initVal->constExp);
                    auto temp = symbolTableManager.genTempName();
                    builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(arrayType), name, builder.arrayIndex("i64", std::to_string(index))));
                    builder.addStore("i32", value, temp);
                }
            } else if (elementType->isArray()) {
                assert_ptr_type<ConstInitListAST>(init, "Invalid sub array");
                auto initList = std::dynamic_pointer_cast<ConstInitListAST>(init);
                auto temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(arrayType), name, builder.arrayIndex("i64", std::to_string(index))));
                dumpLocalConstArrayInit(temp, initList, elementType);
            } else {
                throw std::runtime_error("Unknown elementType");
            }
            index++;
        }
    }

    void dumpLocalArrayInit(string_view name, shared_ptr<InitListAST> initList, SymbolTypeInfoPtr arrayType)
    {
        auto normalizedInitList = normalizeInitList(initList, arrayType);
        auto inits = normalizedInitList->inits;
        auto index = 0;
        auto elementType = arrayType->elementType;
        for (auto& init : inits) {
            if (elementType->isInt()) {
                assert_ptr_type<InitValAST>(init, "Invalid expression of array element");
                auto initVal = std::dynamic_pointer_cast<InitValAST>(init);
                if (isConst(initVal->exp)) {
                    auto value = eval(initVal->exp);
                    auto temp = symbolTableManager.genTempName();
                    builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(arrayType), name, builder.arrayIndex("i64", std::to_string(index))));
                    builder.addStore("i32", std::to_string(value), temp);
                } else {
                    auto value = dumpExp(initVal->exp);
                    auto temp = symbolTableManager.genTempName();
                    builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(arrayType), name, builder.arrayIndex("i64", std::to_string(index))));
                    builder.addStore("i32", value, temp);
                }
            } else if (elementType->isArray()) {
                assert_ptr_type<InitListAST>(init, "Invalid sub array");
                auto initList = std::dynamic_pointer_cast<InitListAST>(init);
                auto temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(arrayType), name, builder.arrayIndex("i64", std::to_string(index))));
                dumpLocalArrayInit(temp, initList, elementType);
            } else {
                throw std::runtime_error("Unknown elementType");
            }
            index++;
        }
    }

    SymbolTypeInfoPtr createArrayTypeInfo(vector<shared_ptr<ConstExpAST>> arrayDim) const
    {
        SymbolTypeInfoPtr arrayTypeInfo = SymbolTypeInfo::createInt();
        for (auto& arrayDim : arrayDim | views::reverse) {
            if (!isConst(arrayDim)) {
                throw std::runtime_error("Array dimension must be constant");
            }
            auto size = eval(arrayDim);
            arrayTypeInfo = SymbolTypeInfo::createArray(arrayTypeInfo, size);
        }
        return arrayTypeInfo;
    }

    SymbolTypeInfoPtr createArrayTypeInfo(vector<shared_ptr<ExpAST>> arrayDim) const
    {
        SymbolTypeInfoPtr arrayTypeInfo = SymbolTypeInfo::createInt();
        for (auto& arrayDim : arrayDim | views::reverse) {
            if (!isConst(arrayDim)) {
                throw std::runtime_error("Array dimension must be constant");
            }
            auto size = eval(arrayDim);
            arrayTypeInfo = SymbolTypeInfo::createArray(arrayTypeInfo, size);
        }
        return arrayTypeInfo;
    }

    shared_ptr<InitListAST> createZeroInitList(SymbolTypeInfoPtr typeInfo)
    {
        auto initList = InitListAST::create();
        if (typeInfo->elementType->isInt()) {
            for (auto i = 0; i < typeInfo->size; i++) {
                initList->addInit(InitValAST::create(0));
            }
        } else if (typeInfo->elementType->isArray()) {
            for (auto i = 0; i < typeInfo->size; i++) {
                initList->addInit(createZeroInitList(typeInfo->elementType));
            }
        }
        return initList;
    }

    shared_ptr<ConstInitListAST> createZeroConstInitList(SymbolTypeInfoPtr typeInfo)
    {
        auto constInitList = ConstInitListAST::create();
        if (typeInfo->elementType->isInt()) {
            for (auto i = 0; i < typeInfo->size; i++) {
                constInitList->addConstInit(ConstInitValAST::create(0));
            }
        } else if (typeInfo->elementType->isArray()) {
            for (auto i = 0; i < typeInfo->size; i++) {
                constInitList->addConstInit(createZeroConstInitList(typeInfo->elementType));
            }
        }
        return constInitList;
    }

    bool isZeroInitList(shared_ptr<InitListAST> initList) const
    {
        auto inits = initList->inits;
        return inits.empty() || std::all_of(inits.begin(), inits.end(), [&](auto init) {
            if (auto initVal = std::dynamic_pointer_cast<InitValAST>(init))
                return isConst(initVal->exp) && eval(initVal->exp) == 0;
            else if (auto initList = std::dynamic_pointer_cast<InitListAST>(init))
                return isZeroInitList(initList);
            else
                return false;
        });
    }

    int getFlattenSize(SymbolTypeInfoPtr arrayType)
    {
        if (arrayType->elementType->isInt()) {
            return arrayType->size;
        } else if (arrayType->elementType->isArray()) {
            return arrayType->size * getFlattenSize(arrayType->elementType);
        } else {
            throw std::runtime_error("Unknown elementType");
        }
    }

    void dumpArrayVarDef(shared_ptr<VarDefAST> varDef, bool isGlobal)
    {
        auto ident = varDef->ident;
        auto init = varDef->init;
        SymbolTypeInfoPtr arrayTypeInfo = createArrayTypeInfo(varDef->arrayDim);
        string arrayType = dumpTypeInfo(arrayTypeInfo);
        if (isGlobal) {
            string initVal;
            if (init) {
                auto initList = std::dynamic_pointer_cast<InitListAST>(init);
                if (initList->inits.empty() || isZeroInitList(initList)) {
                    initVal = "zeroinitializer"s;
                } else {
                    if (!isConstInitList(initList)) {
                        throw std::runtime_error("Global array must be initialized by constant");
                    }
                    initVal = dumpInitList(initList, arrayTypeInfo);
                }
            } else {
                initVal = "zeroinitializer"s;
            }
            auto name = symbolTableManager.insertArray(ident, arrayTypeInfo->elementType, arrayTypeInfo->size, true);
            globalDefBuilder.addGlobalVarDef(name, arrayType, initVal);
        } else {
            // alloc -> [ getelementptr -> store ]
            auto name = symbolTableManager.insertArray(ident, arrayTypeInfo->elementType, arrayTypeInfo->size, false);
            builder.addAssign(name, builder.alloc(arrayType));
            if (init) {
                auto initList = std::dynamic_pointer_cast<InitListAST>(init);
                if (isZeroInitList(initList)) {
                    // auto temp = symbolTableManager.genTempName();
                    // builder.addAssign(
                    //     temp, builder.getelementptr(dumpTypeInfo(arrayTypeInfo), name, builder.arrayIndex("i64",
                    //     "0")));
                    dumpMemset(name, "0", std::to_string(4 * getFlattenSize(arrayTypeInfo)));
                } else
                    dumpLocalArrayInit(name, initList, arrayTypeInfo);
            }
        }
    }

    template <typename Derived, typename Base>
    inline bool is_ptr_type(shared_ptr<Base> ptr)
    {
        return ptr && std::dynamic_pointer_cast<Derived>(ptr) != nullptr;
    }

    template <typename Derived, typename Base>
    inline void assert_ptr_type(shared_ptr<Base> ptr, string_view message)
    {
        if (!is_ptr_type<Derived>(ptr)) {
            throw std::runtime_error(string(message));
        }
    }

    template <typename T>
    inline void assert_not_null(shared_ptr<T> ptr, string_view message)
    {
        if (!ptr) {
            throw std::runtime_error(string(message));
        }
    }

    void dumpArrayConstDef(shared_ptr<ConstDefAST> constDef, bool isGlobal)
    {
        auto ident = constDef->ident;
        auto init = constDef->constInit;
        assert_not_null(init, "Const array must be initialized");
        assert_ptr_type<ConstInitListAST>(init, "Invalid init expression of array");
        auto initList = std::dynamic_pointer_cast<ConstInitListAST>(init);
        SymbolTypeInfoPtr arrayTypeInfo = createArrayTypeInfo(constDef->arrayDim);
        string arrayType = dumpTypeInfo(arrayTypeInfo);
        if (isGlobal) {
            auto normalizedConstInitList = normalizeConstInitList(initList, arrayTypeInfo);
            auto initValStr = dumpConstInitList(initList, arrayTypeInfo);
            auto initVals = flattenConstInitList(normalizedConstInitList);
            auto name = symbolTableManager.insertArrayConst(ident, arrayTypeInfo->elementType, arrayTypeInfo->size,
                initVals, true);
            globalDefBuilder.addGlobalConstDef(name, arrayType, initValStr);
        } else {
            auto normalizedConstInitList = normalizeConstInitList(initList, arrayTypeInfo);
            auto initVals = flattenConstInitList(normalizedConstInitList);
            auto name = symbolTableManager.insertArrayConst(ident, arrayTypeInfo->elementType, arrayTypeInfo->size,
                initVals, false);
            builder.addAssign(name, builder.alloc(arrayType));
            dumpLocalConstArrayInit(name, initList, arrayTypeInfo);
        }
    }

    bool isConst(shared_ptr<BasePrimaryExpAST> basePrimaryExp) const
    {
        if (basePrimaryExp->isConst()) {
            return true;
        }
        if (auto primaryExp = std::dynamic_pointer_cast<PrimaryExpAST>(basePrimaryExp)) {
            return isConst(primaryExp->exp);
        } else if (auto lValPrimaryExp = std::dynamic_pointer_cast<LValPrimaryExpAST>(basePrimaryExp)) {
            auto ident = lValPrimaryExp->lVal->ident;
            auto type = symbolTableManager.lookupType(ident);
            if (lValPrimaryExp->lVal->lValType == LValType::Var && type->isIntConst()) {
                return true;
            } else if (lValPrimaryExp->lVal->lValType == LValType::Array && type->isArrayConst() && type->getDim() == lValPrimaryExp->lVal->exps.size()) {
                return true;
            }
            return false;
        }
        return false;
    }

    int getFlatIndex(shared_ptr<LValAST> lVal) const
    {
        auto ident = lVal->ident;
        auto type = symbolTableManager.lookupType(ident);
        auto dim = type->getDim();
        auto exps = lVal->exps;
        if (dim != exps.size()) {
            throw std::runtime_error("Invalid array dimension");
        }
        auto flatIndex = 0;
        for (auto i = 0; i < dim; i++) {
            if (!isConst(exps[i])) {
                throw std::runtime_error("Array index must be constant");
            }
            auto index = eval(exps[i]);
            flatIndex = flatIndex * type->getDimSize(i) + index;
        }
        return flatIndex;
    }

    int eval(shared_ptr<BasePrimaryExpAST> basePrimaryExp) const
    {
        if (basePrimaryExp->isConst()) {
            return basePrimaryExp->eval();
        }
        if (!isConst(basePrimaryExp)) {
            return -1;
        }
        if (auto primaryExp = std::dynamic_pointer_cast<PrimaryExpAST>(basePrimaryExp)) {
            return eval(primaryExp->exp);
        } else if (auto lValPrimaryExp = std::dynamic_pointer_cast<LValPrimaryExpAST>(basePrimaryExp)) {
            auto ident = lValPrimaryExp->lVal->ident;
            auto type = symbolTableManager.lookupType(ident);
            if (type->isIntConst()) {
                return type->initVal;
            } else if (type->isArrayConst() && type->getDim() == lValPrimaryExp->lVal->exps.size()) {
                auto flatIndex = getFlatIndex(lValPrimaryExp->lVal);
                return type->initVals[flatIndex];
            } else {
                throw std::runtime_error("Invalid constant expression");
            }
        }
        return -1;
    }

    bool isConst(shared_ptr<BaseUnaryExpAST> baseUnaryExp) const
    {
        if (baseUnaryExp->isConst()) {
            return true;
        } else if (auto unaryExp = std::dynamic_pointer_cast<UnaryExpAST>(baseUnaryExp)) {
            return isConst(unaryExp->unaryExp);
        } else if (auto primaryExp = std::dynamic_pointer_cast<PrimaryUnaryExpAST>(baseUnaryExp)) {
            return isConst(primaryExp->primaryExp);
        }
        return false;
    }

    int eval(shared_ptr<BaseUnaryExpAST> baseUnaryExp) const
    {
        if (baseUnaryExp->isConst()) {
            return baseUnaryExp->eval();
        }
        if (!isConst(baseUnaryExp)) {
            throw std::runtime_error("Expression is not constant");
        }
        if (auto unaryExp = std::dynamic_pointer_cast<UnaryExpAST>(baseUnaryExp)) {
            auto val = eval(unaryExp->unaryExp);
            if (unaryExp->op == Op::Pos) {
                return val;
            } else if (unaryExp->op == Op::Neg) {
                return -val;
            } else {
                return !val;
            }
        } else if (auto primaryExp = std::dynamic_pointer_cast<PrimaryUnaryExpAST>(baseUnaryExp)) {
            return eval(primaryExp->primaryExp);
        }
        throw std::runtime_error("Invalid constant expression");
    }

    bool isConst(shared_ptr<MulExpAST> mulExp) const
    {
        if (mulExp->type == MulExpAST::Type::Single) {
            return isConst(mulExp->unaryExp);
        }
        return isConst(mulExp->mulExp) && isConst(mulExp->unaryExp);
    }

    int eval(shared_ptr<MulExpAST> mulExp) const
    {
        if (mulExp->isConst()) {
            return mulExp->eval();
        }
        if (!isConst(mulExp)) {
            throw std::runtime_error("Expression is not constant");
        }
        if (mulExp->type == MulExpAST::Type::Single) {
            return eval(mulExp->unaryExp);
        }
        auto left = eval(mulExp->mulExp);
        auto right = eval(mulExp->unaryExp);
        if (mulExp->op == Op::Mul) {
            return left * right;
        } else if (mulExp->op == Op::Div) {
            return left / right;
        } else {
            return left % right;
        }
    }

    bool isConst(shared_ptr<AddExpAST> addExp) const
    {
        if (addExp->type == AddExpAST::Type::Single) {
            return isConst(addExp->mulExp);
        }
        return isConst(addExp->addExp) && isConst(addExp->mulExp);
    }

    int eval(shared_ptr<AddExpAST> addExp) const
    {
        if (addExp->isConst()) {
            return addExp->eval();
        }
        if (!isConst(addExp)) {
            throw std::runtime_error("Expression is not constant");
        }
        if (addExp->type == AddExpAST::Type::Single) {
            return eval(addExp->mulExp);
        }
        auto left = eval(addExp->addExp);
        auto right = eval(addExp->mulExp);
        if (addExp->op == Op::Add) {
            return left + right;
        } else {
            return left - right;
        }
    }

    bool isConst(shared_ptr<ExpAST> exp) const
    {
        return isConst(exp->addExp);
    }

    int eval(shared_ptr<ExpAST> exp) const
    {
        return eval(exp->addExp);
    }

    bool isConst(shared_ptr<ConstExpAST> constExp) const
    {
        return isConst(constExp->addExp);
    }

    int eval(shared_ptr<ConstExpAST> constExp) const
    {
        return eval(constExp->addExp);
    }

    bool isConst(shared_ptr<RelExpAST> relExp) const
    {
        if (relExp->type == RelExpAST::Type::Single) {
            return isConst(relExp->addExp);
        }
        return isConst(relExp->relExp) && isConst(relExp->addExp);
    }

    bool isConst(shared_ptr<EqExpAST> eqExp) const
    {
        if (eqExp->type == EqExpAST::Type::Single) {
            return isConst(eqExp->relExp);
        }
        return isConst(eqExp->eqExp) && isConst(eqExp->relExp);
    }

    bool isConst(shared_ptr<LAndExpAST> lAndExp) const
    {
        return std::all_of(lAndExp->eqExps.begin(), lAndExp->eqExps.end(), [&](auto eqExp) { return isConst(eqExp); });
    }

    bool isConst(shared_ptr<LOrExpAST> lOrExp) const
    {
        return std::all_of(lOrExp->lAndExps.begin(), lOrExp->lAndExps.end(),
            [&](auto lAndExp) { return isConst(lAndExp); });
    }

    bool isConst(shared_ptr<CondAST> cond) const
    {
        return isConst(cond->lOrExp);
    }

    int eval(shared_ptr<RelExpAST> relExp) const
    {
        if (relExp->isConst()) {
            return relExp->eval();
        }
        if (!isConst(relExp)) {
            throw std::runtime_error("Expression is not constant");
        }
        if (relExp->type == RelExpAST::Type::Single) {
            return eval(relExp->addExp);
        }
        auto left = eval(relExp->relExp);
        auto right = eval(relExp->addExp);
        if (relExp->op == Op::Lt) {
            return left < right;
        } else if (relExp->op == Op::Le) {
            return left <= right;
        } else if (relExp->op == Op::Gt) {
            return left > right;
        } else {
            return left >= right;
        }
    }

    int eval(shared_ptr<EqExpAST> eqExp) const
    {
        if (eqExp->isConst()) {
            return eqExp->eval();
        }
        if (!isConst(eqExp)) {
            throw std::runtime_error("Expression is not constant");
        }
        if (eqExp->type == EqExpAST::Type::Single) {
            return eval(eqExp->relExp);
        }
        auto left = eval(eqExp->eqExp);
        auto right = eval(eqExp->relExp);
        return eqExp->op == Op::Eq ? left == right : left != right;
    }

    int eval(shared_ptr<LAndExpAST> lAndExp) const
    {
        if (lAndExp->isConst()) {
            return lAndExp->eval();
        }
        if (!isConst(lAndExp)) {
            throw std::runtime_error("Expression is not constant");
        }
        return std::all_of(lAndExp->eqExps.begin(), lAndExp->eqExps.end(), [&](auto eqExp) { return eval(eqExp); });
    }

    int eval(shared_ptr<LOrExpAST> lOrExp) const
    {
        if (lOrExp->isConst()) {
            return lOrExp->eval();
        }
        if (!isConst(lOrExp)) {
            throw std::runtime_error("Expression is not constant");
        }
        return std::any_of(lOrExp->lAndExps.begin(), lOrExp->lAndExps.end(),
            [&](auto lAndExp) { return eval(lAndExp); });
    }

    int eval(shared_ptr<CondAST> cond) const
    {
        return eval(cond->lOrExp);
    }

    SymbolTypeInfoPtr getFuncFParamType(shared_ptr<FuncFParamAST> funcFParam) const
    {
        if (funcFParam->paramType == ParamType::Var) {
            return SymbolTypeInfo::createInt();
        } else {
            auto arrayFuncFParam = std::dynamic_pointer_cast<ArrayFuncFParamAST>(funcFParam);
            auto arrayDim = arrayFuncFParam->arrayDim;
            auto arrayTypeInfo = createArrayTypeInfo(arrayDim);
            return SymbolTypeInfo::createPointer(arrayTypeInfo);
        }
    }

    string evalType(shared_ptr<BasePrimaryExpAST> basePrimaryExp)
    {
        if (isConst(basePrimaryExp)) {
            return "i32"s;
        }
        if (auto primaryExp = std::dynamic_pointer_cast<PrimaryExpAST>(basePrimaryExp)) {
            return evalType(primaryExp->exp);
        } else if (auto lValPrimaryExp = std::dynamic_pointer_cast<LValPrimaryExpAST>(basePrimaryExp)) {
            auto ident = lValPrimaryExp->lVal->ident;
            auto type = symbolTableManager.lookupType(ident);
            if (lValPrimaryExp->lVal->lValType == LValType::Var) {
                // fmt::println("var ident: {}, type: {}", ident, std::to_underlying(type->type),
                //              std::to_underlying(lValPrimaryExp->lVal->lValType));
                if (type->isInt()) {
                    return "i32"s;
                } else if (type->isArray() || type->isPointer()) {
                    return "ptr"s;
                }
            } else {
                // fmt::println("arr ident: {}, type: {}", ident, std::to_underlying(type->type),
                //              std::to_underlying(lValPrimaryExp->lVal->lValType));
                // fmt::println("arr ident: {}, type: {}, exps.size: {}, dim: {}", ident,
                // std::to_underlying(type->type),
                //              lValPrimaryExp->lVal->exps.size(), type->getDim());
                if (type->getDim() == lValPrimaryExp->lVal->exps.size()) {
                    return "i32"s;
                } else {
                    return "ptr"s;
                }
            }
        }
        return "i32"s;
    }

    string evalType(shared_ptr<BaseUnaryExpAST> baseUnaryExp)
    {
        if (auto unaryExp = std::dynamic_pointer_cast<UnaryExpAST>(baseUnaryExp)) {
            if (unaryExp->op == Op::Pos || unaryExp->op == Op::Neg) {
                return "i32"s;
            } else {
                return "i1"s;
            }
            return evalType(unaryExp->unaryExp);
        } else if (auto primaryExp = std::dynamic_pointer_cast<PrimaryUnaryExpAST>(baseUnaryExp)) {
            return evalType(primaryExp->primaryExp);
        } else if (auto funcCall = std::dynamic_pointer_cast<FuncCallUnaryExpAST>(baseUnaryExp)) {
            return "i32"s;
        }
        throw std::runtime_error("Unknown unary expression");
    }

    string evalType(shared_ptr<MulExpAST> mulExp)
    {
        if (mulExp->type == MulExpAST::Type::Single) {
            return evalType(mulExp->unaryExp);
        }
        auto left = evalType(mulExp->mulExp);
        auto right = evalType(mulExp->unaryExp);
        if (left == right) {
            return left;
        } else {
            throw std::runtime_error("Invalid type of expression");
        }
    }

    string evalType(shared_ptr<AddExpAST> addExp)
    {
        if (addExp->type == AddExpAST::Type::Single) {
            return evalType(addExp->mulExp);
        }
        auto left = evalType(addExp->addExp);
        auto right = evalType(addExp->mulExp);
        if (left == right) {
            return left;
        } else {
            throw std::runtime_error("Invalid type of expression");
        }
    }

    string evalType(shared_ptr<RelExpAST> relExp)
    {
        if (relExp->type == RelExpAST::Type::Single) {
            return evalType(relExp->addExp);
        }
        return "i1"s;
    }

    string evalType(shared_ptr<EqExpAST> eqExp)
    {
        if (eqExp->type == EqExpAST::Type::Single) {
            return evalType(eqExp->relExp);
        }
        return "i1"s;
    }

    string evalType(shared_ptr<ExpAST> exp)
    {
        return evalType(exp->addExp);
    }

public:
    void dumpVarDef(shared_ptr<VarDefAST> varDef, bool isGlobal)
    {
        if (varDef->defType == DefType::Array) {
            dumpArrayVarDef(varDef, isGlobal);
            return;
        }
        auto ident = varDef->ident;
        auto init = varDef->init;
        if (init) {
            auto initVal = std::dynamic_pointer_cast<InitValAST>(init);
            if (!initVal) {
                throw std::runtime_error("Invalid init expression");
            }
            if (isGlobal) {
                if (!isConst(initVal->exp)) {
                    throw std::runtime_error("Global variable must be initialized by constant");
                }
                auto value = eval(initVal->exp);
                auto name = symbolTableManager.insertInt(ident, value, true);
                globalDefBuilder.addGlobalVarDef(name, "i32", std::to_string(value));
            } else {
                // alloc -> store
                if (isConst(initVal->exp)) {
                    auto value = eval(initVal->exp);
                    auto name = symbolTableManager.insertInt(ident, value, false);
                    builder.addAssign(name, builder.alloc("i32"));
                    builder.addStore("i32", std::to_string(value), name);
                } else {
                    auto value = dumpExp(initVal->exp);
                    auto name = symbolTableManager.insertInt(ident, 0, false);
                    builder.addAssign(name, builder.alloc("i32"));
                    builder.addStore("i32", value, name);
                }
            }
        } else {
            if (isGlobal) {
                auto name = symbolTableManager.insertInt(ident, 0, true);
                globalDefBuilder.addGlobalVarDef(name, "i32");
            } else {
                auto name = symbolTableManager.insertInt(ident, 0, false);
                builder.addAssign(name, builder.alloc("i32"));
            }
        }
    }

    void dumpConstDef(shared_ptr<ConstDefAST> constDef, bool isGlobal)
    {
        if (constDef->defType == DefType::Array) {
            dumpArrayConstDef(constDef, isGlobal);
            return;
        }
        auto ident = constDef->ident;
        auto init = constDef->constInit;
        if (!init) {
            throw std::runtime_error("Const must be initialized");
        }
        auto initVal = std::dynamic_pointer_cast<ConstInitValAST>(init);
        if (!initVal) {
            throw std::runtime_error("Invalid init expression");
        }
        if (!isConst(initVal->constExp)) {
            throw std::runtime_error("Const must be initialized by constant");
        }
        auto value = eval(initVal->constExp);
        if (isGlobal) {
            auto name = symbolTableManager.insertIntConst(ident, value, true);
            globalDefBuilder.addGlobalConstDef(name, "i32", std::to_string(value));
        } else {
            // 局部常量和局部变量的处理方式是一样的
            symbolTableManager.insertIntConst(ident, value, false);
            auto name = symbolTableManager.insertIntConst(ident, value, false);
            builder.addAssign(name, builder.alloc("i32"));
            builder.addStore("i32", std::to_string(value), name);
        }
    }

    void dumpVarDecl(shared_ptr<VarDeclAST> varDecl, bool isGlobal)
    {
        for (auto& varDef : varDecl->varDefs) {
            dumpVarDef(varDef, isGlobal);
        }
    }
    void dumpConstDecl(shared_ptr<ConstDeclAST> constDecl, bool isGlobal)
    {
        for (auto& constDef : constDecl->constDefs) {
            dumpConstDef(constDef, isGlobal);
        }
    }

    string dumpPointerLVal(string_view name, SymbolTypeInfoPtr type, vector<shared_ptr<ExpAST>> exps)
    {
        // 递归地生成
        auto elementType = type->elementType;
        if (elementType->isInt()) // int
        {
            if (isConst(exps[0])) {
                auto index = eval(exps[0]);
                auto temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.load("ptr", name));
                auto oldTemp = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(elementType), oldTemp, builder.ptrIndex("i64", std::to_string(index))));
                return temp;
            } else {
                auto index = dumpExp(exps[0]);
                auto temp = symbolTableManager.genTempName();
                builder.addUnary("sext", temp, "i32", fmt::format("{} to i64", index));
                index = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.load("ptr", name));
                auto oldTemp = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(
                    temp, builder.getelementptr(dumpTypeInfo(elementType), oldTemp, builder.ptrIndex("i64", index)));
                return temp;
            }
        } // array
        else {
            if (isConst(exps[0])) {
                auto index = eval(exps[0]);
                auto temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.load("ptr", name));
                auto oldTemp = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(type->elementType), oldTemp, builder.ptrIndex("i64", std::to_string(index))));
                temp = dumpArrayLVal(temp, elementType, vector<shared_ptr<ExpAST>>(exps.begin() + 1, exps.end()));
                return temp;
            } else {
                auto index = dumpExp(exps[0]);
                auto temp = symbolTableManager.genTempName();
                builder.addUnary("sext", temp, "i32", fmt::format("{} to i64", index));
                index = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.load("ptr", name));
                auto oldTemp = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(type->elementType), oldTemp, builder.ptrIndex("i64", index)));
                temp = dumpArrayLVal(temp, elementType, vector<shared_ptr<ExpAST>>(exps.begin() + 1, exps.end()));
                return temp;
            }
        }
    }

    string dumpPointerAccess(string_view name, SymbolTypeInfoPtr type, vector<shared_ptr<ExpAST>> exps)
    {
        auto ptr = dumpPointerLVal(name, type, exps);
        auto temp = symbolTableManager.genTempName();
        builder.addAssign(temp, builder.load("i32", ptr));
        return temp;
    }

    string dumpArrayAccess(string_view name, SymbolTypeInfoPtr type, vector<shared_ptr<ExpAST>> exps)
    {
        auto elementType = type->elementType;
        string temp;
        // auto all_zero =
        //     std::all_of(exps.begin(), exps.end(), [&](auto &exp) { return isConst(exp) && eval(exp) == 0; });
        // if (type->getDim() == exps.size() && all_zero)
        // {
        //     auto temp = symbolTableManager.genTempName();
        //     builder.addAssign(temp, builder.load("i32", name));
        //     return temp;
        // }
        if (type->elementType->isInt()) {
            if (isConst(exps[0])) {
                auto index = eval(exps[0]);
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(type), name, builder.arrayIndex("i64", std::to_string(index))));
                auto oldTemp = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.load("i32", oldTemp));
            } else {
                auto index = dumpExp(exps[0]);
                temp = symbolTableManager.genTempName();
                builder.addUnary("sext", temp, "i32", fmt::format("{} to i64", index));
                index = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp,
                    builder.getelementptr(dumpTypeInfo(type), name, builder.arrayIndex("i64", index)));
                auto oldTemp = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.load("i32", oldTemp));
            }
        } else {
            if (isConst(exps[0])) {
                auto index = eval(exps[0]);
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(type), name, builder.arrayIndex("i64", std::to_string(index))));
                temp = dumpArrayAccess(temp, type->elementType, vector<shared_ptr<ExpAST>>(exps.begin() + 1, exps.end()));
            } else {
                auto index = dumpExp(exps[0]);
                temp = symbolTableManager.genTempName();
                builder.addUnary("sext", temp, "i32", fmt::format("{} to i64", index));
                index = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp,
                    builder.getelementptr(dumpTypeInfo(type), name, builder.arrayIndex("i64", index)));
                temp = dumpArrayAccess(temp, type->elementType, vector<shared_ptr<ExpAST>>(exps.begin() + 1, exps.end()));
            }
        }
        return temp;
    }

    string dumpArrayLVal(string_view name, SymbolTypeInfoPtr type, vector<shared_ptr<ExpAST>> exps)
    {
        auto elementType = type->elementType;
        string temp;
        // auto all_zero =
        //     std::all_of(exps.begin(), exps.end(), [&](auto &exp) { return isConst(exp) && eval(exp) == 0; });
        // if ((type->getDim() == exps.size()) && all_zero)
        // {
        //     return string{name};
        // }
        if (exps.empty()) {
            return string { name };
        }
        if (type->elementType->isInt()) {
            if (isConst(exps[0])) {
                auto index = eval(exps[0]);
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(type), name, builder.arrayIndex("i64", std::to_string(index))));
            } else {
                auto index = dumpExp(exps[0]);
                temp = symbolTableManager.genTempName();
                builder.addUnary("sext", temp, "i32", fmt::format("{} to i64", index));
                index = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp,
                    builder.getelementptr(dumpTypeInfo(type), name, builder.arrayIndex("i64", index)));
            }
        } else {
            if (isConst(exps[0])) {
                auto index = eval(exps[0]);
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.getelementptr(dumpTypeInfo(type), name, builder.arrayIndex("i64", std::to_string(index))));
                temp = dumpArrayLVal(temp, type->elementType, vector<shared_ptr<ExpAST>>(exps.begin() + 1, exps.end()));
            } else {
                auto index = dumpExp(exps[0]);
                temp = symbolTableManager.genTempName();
                builder.addUnary("sext", temp, "i32", fmt::format("{} to i64", index));
                index = temp;
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp,
                    builder.getelementptr(dumpTypeInfo(type), name, builder.arrayIndex("i64", index)));
                temp = dumpArrayLVal(temp, type->elementType, vector<shared_ptr<ExpAST>>(exps.begin() + 1, exps.end()));
            }
        }
        return temp;
    }

    string dumpLVal(shared_ptr<LValAST> lVal)
    {
        if (lVal->lValType == LValType::Var) {
            auto ident = lVal->ident;
            auto name = symbolTableManager.lookupName(ident);
            return name;
        } else {
            auto ident = lVal->ident;
            auto name = symbolTableManager.lookupName(ident);
            auto type = symbolTableManager.lookupType(ident);
            auto exps = lVal->exps;
            if (type->isPointer()) {
                return dumpPointerLVal(name, type, exps);
            } else {
                return dumpArrayLVal(name, type, exps);
            }
        }
    }

    string dumpElementTypeInfo(SymbolTypeInfoPtr arrayType, int dim)
    {
        auto elementType = arrayType;
        while (dim--) {
            elementType = elementType->elementType;
        }
        return dumpTypeInfo(elementType);
    }

    string dumpPrimaryExp(shared_ptr<BasePrimaryExpAST> basePrimaryExp)
    {
        if (auto primaryExp = std::dynamic_pointer_cast<PrimaryExpAST>(basePrimaryExp)) {
            return dumpExp(primaryExp->exp);
        } else if (auto lValPrimaryExp = std::dynamic_pointer_cast<LValPrimaryExpAST>(basePrimaryExp)) {
            auto lVal = lValPrimaryExp->lVal;
            auto ptr = dumpLVal(lVal);
            string temp;
            auto type = evalType(lValPrimaryExp);
            auto typeInfo = symbolTableManager.lookupType(lVal->ident);
            if (type == "ptr"s && typeInfo->isArray()) {
                // fmt::println("dump lval {} -> {}, type: {}", lVal->ident, ptr,
                //              dumpElementTypeInfo(typeInfo, lVal->exps.size()));
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.getelementptr(dumpElementTypeInfo(typeInfo, lVal->exps.size()), ptr, builder.arrayIndex("i64", "0")));
            } else {
                temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.load(type, ptr));
            }
            return temp;
        } else {
            throw std::runtime_error("Unknown primary expression");
        }
    }

    bool isCStdLibFunc(string_view ident) const
    {
        return ident == "printf"sv || ident == "scanf"sv;
    }

    void dumpMemset(string_view ptr, string_view value, string_view size)
    {
        if (!symbolTableManager.exist("memset")) {
            symbolTableManager.insertExternFuncInt("memset", "@memset");
            globalDefBuilder.addFuncDecl("@memset", "void", { "ptr", "i32", "i64" });
        }
        auto name = symbolTableManager.lookupName("memset");
        vector<string> args { fmt::format("ptr {}", ptr), fmt::format("i32 {}", value), fmt::format("i64 {}", size) };
        builder.addLine(builder.call("void", name, args));
    }

    string dumpCStdLibFuncCall(shared_ptr<FuncCallUnaryExpAST> funcCall)
    {
        auto ident = funcCall->ident;
        auto args = funcCall->funcRParams->funcRParams | views::transform([&](auto& rParam) {
            if (auto expRParam = std::dynamic_pointer_cast<ExpFuncRParamAST>(rParam)) {
                auto exp = expRParam->exp;
                if (isConst(exp)) {
                    return fmt::format("i32 {}", eval(exp));
                } else {
                    return fmt::format("i32 {}", dumpExp(exp));
                }
            } else if (auto stringLiteralRParam = std::dynamic_pointer_cast<StringLiteralFuncRParamAST>(rParam)) {
                auto stringLiteral = stringLiteralRParam->str;
                auto temp = symbolTableManager.genUniqueGlobalName("str");
                globalDefBuilder.addGlobalConstDef(
                    temp, fmt::format("[{} x i8]", stringLiteral.size() + 1),
                    fmt::format("c\"{}\\00\"", builder.escapeStringLiteral(stringLiteral)));
                return fmt::format("ptr {}", temp);
            } else if (auto addressRParam = std::dynamic_pointer_cast<AddressFuncRParamAST>(rParam)) {
                auto lVal = addressRParam->lVal;
                auto ptr = dumpLVal(lVal);
                return fmt::format("ptr {}", ptr);
            } else {
                throw std::runtime_error("Unknown function parameter");
            }
        }) | to<vector<string>>();
        if (ident == "printf"sv) {
            if (!symbolTableManager.exist("printf")) {
                symbolTableManager.insertExternFuncInt("printf", "@printf");
                globalDefBuilder.addFuncDecl("@printf", "i32", { "ptr", "..." });
            }
            auto name = symbolTableManager.lookupName("printf");
            auto temp = symbolTableManager.genTempName();
            builder.addAssign(temp, builder.call("i32", name, args));
            return temp;
        } // scanf
        else if (ident == "scanf"sv) {
            if (!symbolTableManager.exist("scanf")) {
                symbolTableManager.insertExternFuncInt("scanf", "@__isoc99_scanf");
                globalDefBuilder.addFuncDecl("@__isoc99_scanf", "i32", { "ptr", "..." });
            }
            auto name = symbolTableManager.lookupName("scanf");
            auto temp = symbolTableManager.genTempName();
            builder.addAssign(temp, builder.call("i32", name, args));
            return temp;
        } else {
            throw std::runtime_error("Unknown C standard library function");
        }
    }

    string dumpFuncCall(shared_ptr<FuncCallUnaryExpAST> funcCall)
    {
        auto ident = funcCall->ident;
        if (isCStdLibFunc(ident)) {
            return dumpCStdLibFuncCall(funcCall);
        }
        auto name = symbolTableManager.lookupName(ident);
        auto type = symbolTableManager.lookupType(ident);
        auto funcRParams = funcCall->funcRParams;
        auto callArgs = vector<string> {};
        for (auto& rParam : funcRParams->funcRParams) {
            if (auto expRParam = std::dynamic_pointer_cast<ExpFuncRParamAST>(rParam)) {
                auto exp = expRParam->exp;
                if (isConst(exp)) {
                    auto value = eval(exp);
                    callArgs.push_back(fmt::format("i32 {}", value));
                } else {
                    auto value = dumpExp(exp);
                    auto type = evalType(exp);
                    callArgs.push_back(fmt::format("{} {}", type, value));
                }
            } else {
                throw std::runtime_error("Unknown function parameter");
            }
        }
        auto retType = type->type == SymbolType::FUNC_INT ? "i32" : "void";
        if (retType == "void"s) {
            builder.addLine(builder.call("void", name, callArgs));
            return ""s;
        } else {
            auto temp = symbolTableManager.genTempName();
            builder.addAssign(temp, builder.call(retType, name, callArgs));
            return temp;
        }
    }

    string dumpUnaryExp(shared_ptr<BaseUnaryExpAST> baseUnaryExp, bool requireBool = false)
    {
        if (auto unaryExp = std::dynamic_pointer_cast<UnaryExpAST>(baseUnaryExp)) {
            auto exp = unaryExp->unaryExp;
            auto expRes = dumpUnaryExp(exp);
            if (unaryExp->op == Op::Pos) {
                return expRes;
            } else if (unaryExp->op == Op::Neg) {
                auto temp = symbolTableManager.genTempName();
                builder.addBinary("sub", temp, "i32", "0", expRes);
                return temp;
            } else {
                auto temp = symbolTableManager.genTempName();
                builder.addBinary("icmp eq", temp, "i32", expRes, "0");
                if (requireBool) {
                    return temp;
                }
                auto oldTemp = temp;
                temp = symbolTableManager.genTempName();
                builder.addBinary("xor", temp, "i1", oldTemp, "1");
                oldTemp = temp;
                temp = symbolTableManager.genTempName();
                builder.addUnary("zext", temp, "i1", fmt::format("{} to i32", oldTemp));
                return temp;
            }
        } else if (auto primaryExp = std::dynamic_pointer_cast<PrimaryUnaryExpAST>(baseUnaryExp)) {
            return dumpPrimaryExp(primaryExp->primaryExp);
        } else if (auto funcCall = std::dynamic_pointer_cast<FuncCallUnaryExpAST>(baseUnaryExp)) {
            return dumpFuncCall(funcCall);
        } else {
            throw std::runtime_error("Unknown unary expression");
        }
    }

    string dumpMulExp(shared_ptr<MulExpAST> mulExp, bool requireBool = false)
    {
        if (mulExp->type == MulExpAST::Type::Single) {
            return dumpUnaryExp(mulExp->unaryExp, requireBool);
        }
        if (isConst(mulExp->mulExp)) {
            auto left = std::to_string(eval(mulExp->mulExp));
            auto right = dumpUnaryExp(mulExp->unaryExp);
            auto temp = symbolTableManager.genTempName();
            if (mulExp->op == Op::Mul) {
                builder.addBinary("mul", temp, "i32", left, right);
            } else if (mulExp->op == Op::Div) {
                builder.addBinary("sdiv", temp, "i32", left, right);
            } else {
                builder.addBinary("srem", temp, "i32", left, right);
            }
            return temp;
        } else if (isConst(mulExp->unaryExp)) {
            auto left = dumpMulExp(mulExp->mulExp);
            auto right = std::to_string(eval(mulExp->unaryExp));
            auto temp = symbolTableManager.genTempName();
            if (mulExp->op == Op::Mul) {
                builder.addBinary("mul", temp, "i32", left, right);
            } else if (mulExp->op == Op::Div) {
                builder.addBinary("sdiv", temp, "i32", left, right);
            } else {
                builder.addBinary("srem", temp, "i32", left, right);
            }
            return temp;
        }
        if (mulExp->mulExp->type == MulExpAST::Type::Single && *mulExp->mulExp->unaryExp == *mulExp->unaryExp) {
            if (mulExp->op == Op::Mul) {
                auto op = dumpUnaryExp(mulExp->unaryExp);
                auto temp = symbolTableManager.genTempName();
                builder.addBinary("mul", temp, "i32", op, op);
                return temp;
            } else if (mulExp->op == Op::Div) {
                return "1";
            } else {
                return "0";
            }
        }
        auto left = dumpMulExp(mulExp->mulExp);
        auto right = dumpUnaryExp(mulExp->unaryExp);
        auto temp = symbolTableManager.genTempName();
        if (mulExp->op == Op::Mul) {
            builder.addBinary("mul", temp, "i32", left, right);
        } else if (mulExp->op == Op::Div) {
            builder.addBinary("sdiv", temp, "i32", left, right);
        } else {
            builder.addBinary("srem", temp, "i32", left, right);
        }
        return temp;
    }

    string dumpAddExp(shared_ptr<AddExpAST> addExp, bool requireBool = false)
    {
        if (addExp->type == AddExpAST::Type::Single) {
            return dumpMulExp(addExp->mulExp, requireBool);
        }
        if (isConst(addExp->addExp)) {
            auto left = std::to_string(eval(addExp->addExp));
            auto right = dumpMulExp(addExp->mulExp);
            auto temp = symbolTableManager.genTempName();
            if (addExp->op == Op::Add) {
                builder.addBinary("add", temp, "i32", left, right);
            } else {
                builder.addBinary("sub", temp, "i32", left, right);
            }
            return temp;
        } else if (isConst(addExp->mulExp)) {
            auto left = dumpAddExp(addExp->addExp);
            auto right = std::to_string(eval(addExp->mulExp));
            auto temp = symbolTableManager.genTempName();
            if (addExp->op == Op::Add) {
                builder.addBinary("add", temp, "i32", left, right);
            } else {
                builder.addBinary("sub", temp, "i32", left, right);
            }
            return temp;
        }
        if (addExp->addExp->type == AddExpAST::Type::Single && *addExp->addExp->mulExp == *addExp->mulExp) {
            if (addExp->op == Op::Add) {
                auto op = dumpMulExp(addExp->mulExp);
                auto temp = symbolTableManager.genTempName();
                builder.addBinary("add", temp, "i32", op, op);
                return temp;
            } else {
                return "0";
            }
        }
        auto left = dumpAddExp(addExp->addExp);
        auto right = dumpMulExp(addExp->mulExp);
        auto temp = symbolTableManager.genTempName();
        if (addExp->op == Op::Add) {
            builder.addBinary("add", temp, "i32", left, right);
        } else {
            builder.addBinary("sub", temp, "i32", left, right);
        }
        return temp;
    }

    string dumpExp(shared_ptr<ExpAST> exp)
    {
        return dumpAddExp(exp->addExp);
    }

    string dumpConstExp(shared_ptr<ConstExpAST> constExp)
    {
        return dumpAddExp(constExp->addExp);
    }

    string dumpRelExp(shared_ptr<RelExpAST> relExp)
    {
        if (relExp->type == RelExpAST::Type::Single) {
            return dumpAddExp(relExp->addExp, true);
        }
        if (isConst(relExp->relExp)) {
            auto left = std::to_string(eval(relExp->relExp));
            auto right = dumpAddExp(relExp->addExp);
            auto temp = symbolTableManager.genTempName();
            if (relExp->op == Op::Lt) {
                builder.addBinary("icmp slt", temp, "i32", left, right);
            } else if (relExp->op == Op::Le) {
                builder.addBinary("icmp sle", temp, "i32", left, right);
            } else if (relExp->op == Op::Gt) {
                builder.addBinary("icmp sgt", temp, "i32", left, right);
            } else {
                builder.addBinary("icmp sge", temp, "i32", left, right);
            }
            return temp;
        } else if (isConst(relExp->addExp)) {
            auto left = dumpRelExp(relExp->relExp);
            auto right = std::to_string(eval(relExp->addExp));
            auto temp = symbolTableManager.genTempName();
            if (relExp->op == Op::Lt) {
                builder.addBinary("icmp slt", temp, "i32", left, right);
            } else if (relExp->op == Op::Le) {
                builder.addBinary("icmp sle", temp, "i32", left, right);
            } else if (relExp->op == Op::Gt) {
                builder.addBinary("icmp sgt", temp, "i32", left, right);
            } else {
                builder.addBinary("icmp sge", temp, "i32", left, right);
            }
            return temp;
        }
        auto left = dumpRelExp(relExp->relExp);
        auto right = dumpAddExp(relExp->addExp);
        auto temp = symbolTableManager.genTempName();
        if (relExp->op == Op::Lt) {
            builder.addBinary("icmp slt", temp, "i32", left, right);
        } else if (relExp->op == Op::Le) {
            builder.addBinary("icmp sle", temp, "i32", left, right);
        } else if (relExp->op == Op::Gt) {
            builder.addBinary("icmp sgt", temp, "i32", left, right);
        } else {
            builder.addBinary("icmp sge", temp, "i32", left, right);
        }
        return temp;
    }

    string dumpEqExp(shared_ptr<EqExpAST> eqExp, bool expectBool = true)
    {
        if (eqExp->type == EqExpAST::Type::Single) {
            auto type = evalType(eqExp->relExp);
            if (type == "i1"s) {
                return dumpRelExp(eqExp->relExp);
            } else if (expectBool) {
                auto rhs = dumpRelExp(eqExp->relExp);
                auto temp = symbolTableManager.genTempName();
                builder.addBinary("icmp ne", temp, "i32", "0", rhs);
                return temp;
            } else {
                return dumpRelExp(eqExp->relExp);
            }
        }
        if (isConst(eqExp->eqExp)) {
            auto lhs = std::to_string(eval(eqExp->eqExp));
            auto rhs = dumpRelExp(eqExp->relExp);
            auto temp = symbolTableManager.genTempName();
            if (eqExp->op == Op::Eq) {
                builder.addBinary("icmp eq", temp, "i32", lhs, rhs);
            } else {
                builder.addBinary("icmp ne", temp, "i32", lhs, rhs);
            }
            return temp;
        } else if (isConst(eqExp->relExp)) {
            auto lhs = dumpEqExp(eqExp->eqExp, false);
            auto rhs = std::to_string(eval(eqExp->relExp));
            auto temp = symbolTableManager.genTempName();
            if (eqExp->op == Op::Eq) {
                builder.addBinary("icmp eq", temp, "i32", lhs, rhs);
            } else {
                builder.addBinary("icmp ne", temp, "i32", lhs, rhs);
            }
            return temp;
        }
        auto lhs = dumpEqExp(eqExp->eqExp, false);
        auto rhs = dumpRelExp(eqExp->relExp);
        auto temp = symbolTableManager.genTempName();
        if (eqExp->op == Op::Eq) {
            builder.addBinary("icmp eq", temp, "i32", lhs, rhs);
        } else {
            builder.addBinary("icmp ne", temp, "i32", lhs, rhs);
        }
        return temp;
    }

    void dumpLAndExp(shared_ptr<LAndExpAST> lAndExp, string_view true_label, IRLabelPtr false_label)
    {
        if (isConst(lAndExp)) {
            auto value = eval(lAndExp);
            if (value) {
                builder.addBr(true_label);
            } else {
                builder.addBr(false_label);
            }
            return;
        }
        auto labels = lAndExp->eqExps | views::drop(1) | views::transform([&](auto& eqExp) {
            auto label = symbolTableManager.genLabelName("land.lhs");
            return label;
        }) | to<vector<string>>();
        for (size_t i = 0; i < lAndExp->eqExps.size(); i++) {
            if (i > 0) {
                builder.addLabel(labels[i - 1]);
            }
            auto temp = dumpEqExp(lAndExp->eqExps[i]);
            if (i == lAndExp->eqExps.size() - 1) {
                builder.addBr(temp, true_label, false_label);
            } else {
                builder.addBr(temp, labels[i], false_label);
            }
        }
    }

    void dumpLOrExp(shared_ptr<LOrExpAST> lOrExp, string_view true_label, IRLabelPtr false_label)
    {
        if (isConst(lOrExp)) {
            auto value = eval(lOrExp);
            if (value) {
                builder.addBr(true_label);
            } else {
                builder.addBr(false_label);
            }
            return;
        }
        // 如果只有一个
        if (lOrExp->lAndExps.size() == 1) {
            dumpLAndExp(lOrExp->lAndExps[0], true_label, false_label);
            return;
        }
        auto labels = lOrExp->lAndExps | views::drop(1) | views::transform([&](auto& lAndExp) {
            auto label = IRLabel::create(symbolTableManager.genLabelName("lor.lhs"));
            return label;
        }) | to<vector>();
        for (size_t i = 0; i < lOrExp->lAndExps.size(); i++) {
            if (i > 0) {
                builder.addLabel(labels[i - 1]);
            }
            if (i == lOrExp->lAndExps.size() - 1) {
                dumpLAndExp(lOrExp->lAndExps[i], true_label, false_label);
            } else {
                dumpLAndExp(lOrExp->lAndExps[i], true_label, labels[i]);
            }
        }
    }

    void dumpCondExp(shared_ptr<CondAST> cond, string_view true_label, IRLabelPtr false_label)
    {
        dumpLOrExp(cond->lOrExp, true_label, false_label);
    }

    void dumpAssignStmt(shared_ptr<AssignStmtAST> assignStmt)
    {
        auto lVal = assignStmt->lVal;
        auto exp = assignStmt->exp;
        if (isConst(exp)) {
            auto value = eval(exp);
            auto name = dumpLVal(lVal);
            builder.addStore("i32", std::to_string(value), name);
        } else {
            auto value = dumpExp(exp);
            auto name = dumpLVal(lVal);
            builder.addStore("i32", value, name);
        }
    }

    void dumpReturnStmt(shared_ptr<ReturnStmtAST> returnStmt)
    {
        auto currentFuncName = scopeMgr.getCurrentFunction()->getName();
        auto exp = returnStmt->exp;
        bool hasRetVal = exp != nullptr;
        string retVal = ""s;
        if (exp)
            retVal = isConst(exp) ? std::to_string(eval(exp)) : dumpExp(exp);
        if (retMgr.isEndReturn(currentFuncName) && retMgr.isSingleReturn(currentFuncName)) {
            if (hasRetVal)
                builder.addRet("i32", retVal);
            else
                builder.addRet();
        } else {
            if (hasRetVal) {
                builder.addStore("i32", retVal, retMgr.getValName(currentFuncName));
            }
            builder.addBr(retMgr.getLabel(currentFuncName));
        }
        scopeMgr.kill();
    }

    void dumpIfElseStmt(shared_ptr<IfStmtAST> ifStmt)
    {
        auto cond = ifStmt->cond;
        auto thenStmt = ifStmt->thenStmt;
        auto elseStmt = ifStmt->elseStmt;
        auto thenLabel = symbolTableManager.genLabelName("if.then");
        auto elseLabel = IRLabel::create(symbolTableManager.genLabelName("if.else"));
        auto endLabel = IRLabel::create(symbolTableManager.genLabelName("if.end"));
        dumpCondExp(cond, thenLabel, elseLabel);
        scopeMgr.pushIfElse(thenLabel, elseLabel, endLabel);
        builder.addLabel(thenLabel);
        dumpStmt(thenStmt);
        if (scopeMgr.isAlive()) {
            builder.addBr(endLabel);
        }
        scopeMgr.pop();
        builder.addLabel(elseLabel);
        scopeMgr.pushIfElse(thenLabel, elseLabel, endLabel);
        dumpStmt(elseStmt);
        if (scopeMgr.isAlive()) {
            builder.addBr(endLabel);
        }
        builder.addLabel(endLabel);
        scopeMgr.pop();
    }

    void dumpIfStmt(shared_ptr<IfStmtAST> ifStmt)
    {
        if (ifStmt->elseStmt) {
            dumpIfElseStmt(ifStmt);
            return;
        }
        auto cond = ifStmt->cond;
        auto thenStmt = ifStmt->thenStmt;
        auto thenLabel = symbolTableManager.genLabelName("if.then");
        auto endLabel = IRLabel::create(symbolTableManager.genLabelName("if.end"));
        dumpCondExp(cond, thenLabel, endLabel);
        auto scope = scopeMgr.pushIf(thenLabel, endLabel);
        builder.addLabel(thenLabel);
        dumpStmt(thenStmt);
        if (scopeMgr.isAlive()) {
            builder.addBr(endLabel);
        }
        builder.addLabel(endLabel);
        scopeMgr.pop();
    }

    void dumpWhileStmt(shared_ptr<WhileStmtAST> whileStmt)
    {
        auto cond = whileStmt->cond;
        auto stmt = whileStmt->stmt;
        auto condLabel = symbolTableManager.genLabelName("while.cond");
        auto bodyLabel = symbolTableManager.genLabelName("while.body");
        auto endLabel = IRLabel::create(symbolTableManager.genLabelName("while.end"));
        scopeMgr.pushLoop(condLabel, bodyLabel, endLabel);
        builder.addBr(condLabel);
        builder.addLabel(condLabel);
        dumpCondExp(cond, bodyLabel, endLabel);
        builder.addLabel(bodyLabel);
        dumpStmt(stmt);
        builder.addBr(condLabel);
        builder.addLabel(endLabel);
        scopeMgr.pop();
    }

    void dumpStmt(shared_ptr<StmtAST> stmt)
    {
        if (!scopeMgr.isAlive())
            return;
        scopeMgr.step();
        if (auto assignStmt = std::dynamic_pointer_cast<AssignStmtAST>(stmt)) {
            dumpAssignStmt(assignStmt);
        } else if (auto returnStmt = std::dynamic_pointer_cast<ReturnStmtAST>(stmt)) {
            dumpReturnStmt(returnStmt);
        } else if (auto expStmt = std::dynamic_pointer_cast<ExpStmtAST>(stmt)) {
            dumpExp(expStmt->exp);
        } else if (auto ifStmt = std::dynamic_pointer_cast<IfStmtAST>(stmt)) {
            dumpIfStmt(ifStmt);
        } else if (auto whileStmt = std::dynamic_pointer_cast<WhileStmtAST>(stmt)) {
            dumpWhileStmt(whileStmt);
        } else if (auto breakStmt = std::dynamic_pointer_cast<BreakStmtAST>(stmt)) {
            auto cur = scopeMgr.getCurrentLoop();
            if (!cur)
                throw std::runtime_error("break statement not in loop");
            builder.addBr(cur->getEndLabel());
            scopeMgr.kill();
        } else if (auto continueStmt = std::dynamic_pointer_cast<ContinueStmtAST>(stmt)) {
            auto cur = scopeMgr.getCurrentLoop();
            if (!cur)
                throw std::runtime_error("continue statement not in loop");
            builder.addBr(cur->getCondLabel());
            scopeMgr.kill();
        } else if (auto emptyStmt = std::dynamic_pointer_cast<EmptyStmtAST>(stmt)) {
            // do nothing
        } else if (auto blockStmt = std::dynamic_pointer_cast<BlockStmtAST>(stmt)) {
            dumpBlock(blockStmt->block);
        } else {
            throw std::runtime_error("Unknown stmt type");
        }
    }

    void dumpAlloca(shared_ptr<BlockAST> block)
    {
        for (auto& item : block->blockItems) {
            if (item->blockItemType == BlockItemType::Decl) {
            }
        }
    }

    void dumpBlock(shared_ptr<BlockAST> block)
    {
        if (!scopeMgr.isAlive())
            return;
        symbolTableManager.push();
        for (auto& item : block->blockItems) {
            if (item->blockItemType == BlockItemType::Decl) {
                auto declItem = std::dynamic_pointer_cast<DeclBlockItemAST>(item);
                if (auto varDecl = std::dynamic_pointer_cast<VarDeclAST>(declItem->decl)) {
                    dumpVarDecl(varDecl, false);
                } else if (auto constDecl = std::dynamic_pointer_cast<ConstDeclAST>(declItem->decl)) {
                    dumpConstDecl(constDecl, false);
                }
            } else if (item->blockItemType == BlockItemType::Stmt) {
                auto stmtItem = std::dynamic_pointer_cast<StmtBlockItemAST>(item);
                dumpStmt(stmtItem->stmt);
            }
        }
        symbolTableManager.pop();
    }

    void dumpFuncDef(shared_ptr<FuncDefAST> funcDef, string_view name)
    {
        auto btype = funcDef->btype;
        auto ident = funcDef->ident;
        auto fParams = funcDef->funcFParams;
        auto block = funcDef->block;
        auto type = btype == Btype::Int ? "i32"s : "void"s;
        bool multiRet = !retMgr.isSingleReturn(name) || !retMgr.isEndReturn(name);
        bool hasRetLabel = retMgr.hasReturn(name) && multiRet;
        auto retLabel = IRLabel::create(retMgr.getLabel(name));
        scopeMgr.pushFunction(name, hasRetLabel ? retLabel : nullptr);
        auto args = fParams->funcFParams | views::transform([&](auto& fParam) {
            auto ident = fParam->ident;
            if (auto varFuncFParam = std::dynamic_pointer_cast<VarFuncFParamAST>(fParam)) {
                auto name = symbolTableManager.insertInt(ident, 0, false);
                return fmt::format("i32 {}", name);
            } // array
            else {
                auto arrayFuncFParam = std::dynamic_pointer_cast<ArrayFuncFParamAST>(fParam);
                auto arrayDim = arrayFuncFParam->arrayDim;
                auto arrayTypeInfo = createArrayTypeInfo(arrayDim);
                auto name = symbolTableManager.insertPointer(ident, arrayTypeInfo, false);
                return fmt::format("ptr {}", name);
            }
        }) | to<vector<string>>();
        builder.pushFuncDef(name, type, args);
        if (multiRet && type != "void"s) {
            builder.addAssign(retMgr.getValName(name), builder.alloc(type));
        }
        vector<std::tuple<string, string, string>> argsPair;
        for (auto& fParam : fParams->funcFParams) {
            auto ident = fParam->ident;
            auto argName = symbolTableManager.lookupName(ident);
            if (auto varFuncFParam = std::dynamic_pointer_cast<VarFuncFParamAST>(fParam)) {
                auto localName = symbolTableManager.insertInt(ident, 0, false);
                builder.addAssign(localName, builder.alloc("i32"));
                argsPair.push_back({ "i32", argName, localName });
            } else {
                auto arrayFuncFParam = std::dynamic_pointer_cast<ArrayFuncFParamAST>(fParam);
                auto arrayDim = arrayFuncFParam->arrayDim;
                auto arrayTypeInfo = createArrayTypeInfo(arrayDim);
                auto localName = symbolTableManager.insertPointer(ident, arrayTypeInfo, false);
                builder.addAssign(localName, builder.alloc("ptr"));
                argsPair.push_back({ "ptr", argName, localName });
            }
        }
        if (multiRet && type != "void"s) {
            builder.addStore("i32", "0", retMgr.getValName(name));
        }
        for (auto& [type, argName, localName] : argsPair) {
            builder.addStore(type, argName, localName);
        }
        dumpBlock(block);
        if (multiRet) {
            if (retMgr.hasReturn(name)) {
                builder.addLabel(retMgr.getLabel(name));
            }
            if (type == "void"s)
                builder.addRet();
            else {
                auto temp = symbolTableManager.genTempName();
                builder.addAssign(temp, builder.load(type, retMgr.getValName(name)));
                builder.addRet("i32", temp);
            }
        }
        scopeMgr.pop();
        builder.popFuncDef();
    }

    void visitFuncStmt(shared_ptr<StmtAST> stmt, string_view funcName)
    {
        if (auto retStmt = std::dynamic_pointer_cast<ReturnStmtAST>(stmt)) {
            retMgr.addReturn(funcName);
        } else if (auto ifStmt = std::dynamic_pointer_cast<IfStmtAST>(stmt)) {
            retMgr.setNotEndReturn(funcName);
            visitFuncStmt(ifStmt->thenStmt, funcName);
            if (ifStmt->elseStmt) {
                visitFuncStmt(ifStmt->elseStmt, funcName);
            }
        } else if (auto whileStmt = std::dynamic_pointer_cast<WhileStmtAST>(stmt)) {
            retMgr.setNotEndReturn(funcName);
            visitFuncStmt(whileStmt->stmt, funcName);
        } else if (auto blockStmt = std::dynamic_pointer_cast<BlockStmtAST>(stmt)) {
            visitFuncBlock(blockStmt->block, funcName);
        } else {
            retMgr.setNotEndReturn(funcName);
        }
    }

    void visitFuncBlock(shared_ptr<BlockAST> block, string_view funcName)
    {
        for (auto& item : block->blockItems) {
            if (item->blockItemType == BlockItemType::Stmt) {
                auto stmtItem = std::dynamic_pointer_cast<StmtBlockItemAST>(item);
                visitFuncStmt(stmtItem->stmt, funcName);
            }
        }
    }

    string visitFuncDef(shared_ptr<FuncDefAST> funcDef)
    {
        auto btype = funcDef->btype;
        auto ident = funcDef->ident;
        auto name { btype == Btype::Int ? symbolTableManager.insertFuncInt(ident)
                                        : symbolTableManager.insertFuncVoid(ident) };
        auto retLabel = symbolTableManager.genLabelName("return");
        auto retVal = symbolTableManager.insertInt("retVal", 0, false);
        retMgr.newFunc(name, retLabel, retVal);
        visitFuncBlock(funcDef->block, name);
        return name;
    }

    void generateIR(shared_ptr<CompUnitAST> compUnit)
    {
        for (auto& decl : compUnit->decls) {
            if (auto varDecl = std::dynamic_pointer_cast<VarDeclAST>(decl)) {
                dumpVarDecl(varDecl, true);
            } else if (auto constDecl = std::dynamic_pointer_cast<ConstDeclAST>(decl)) {
                dumpConstDecl(constDecl, true);
            } else {
                throw std::runtime_error("Unknown decl type");
            }
        }
        builder.addLine();
        for (auto& funcDef : compUnit->funcDefs) {
            auto ident = funcDef->ident;
            // check if the function exists, because we don't support function overloading currently
            if (symbolTableManager.exist(ident)) {
                throw std::runtime_error(fmt::format("Symbol {} already exists", ident));
            }
            symbolTableManager.resetNameManager();
            symbolTableManager.push();
            auto name = visitFuncDef(funcDef);
            dumpFuncDef(funcDef, name);
            symbolTableManager.pop();
        }
    }

    string getIR() const
    {
        if (!globalDefBuilder.empty()) {
            return fmt::format("{}\n\n{}", globalDefBuilder.getIR(), builder.getIR());
        }
        return builder.getIR();
    }
};
