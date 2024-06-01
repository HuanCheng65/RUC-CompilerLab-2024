#pragma once
#include <fmt/format.h>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

using std::shared_ptr, std::make_shared, std::enable_shared_from_this;
using std::string;
using std::string_view;

class NameManager {
private:
    std::unordered_map<string, int> identCount;
    int tempCount = 0;

    string genUniqueName(string_view ident)
    {
        auto it = identCount.find(ident.data());
        if (it == identCount.end()) {
            identCount[ident.data()] = 1;
            return fmt::format("{}", ident);
        }
        return fmt::format("{}.{}", ident, it->second++);
    }

public:
    void reset()
    {
        // nameCount.clear();
        tempCount = 0;
    }

    string genTempName()
    {
        return fmt::format("%t{}", ++tempCount);
    }

    string genUniqueLocalName(string_view ident)
    {
        return fmt::format("%{}", genUniqueName(ident));
    }

    string genUniqueGlobalName(string_view ident)
    {
        return fmt::format("@{}", genUniqueName(ident));
    }

    string genLabelName(string_view labelIdent)
    {
        auto it = identCount.find(labelIdent.data());
        if (it == identCount.end()) {
            identCount[labelIdent.data()] = 1;
            return fmt::format("{}", labelIdent);
        }
        return fmt::format("{}.{}", labelIdent, it->second++);
    }

    string getLabelRef(string_view labelName)
    {
        return fmt::format("%{}", labelName);
    }
};

enum class SymbolType {
    INT,
    INT_CONST,
    FUNC_INT,
    FUNC_VOID,
    ARRAY,
    ARRAY_CONST,
    POINTER
};

struct SymbolTypeInfo;
struct Symbol;

using SymbolTypeInfoPtr = shared_ptr<SymbolTypeInfo>;
using SymbolPtr = shared_ptr<Symbol>;

struct SymbolTypeInfo : public enable_shared_from_this<SymbolTypeInfo> {
    SymbolType type;
    SymbolTypeInfoPtr elementType;
    int size, initVal;
    std::vector<int> initVals;

    SymbolTypeInfo(SymbolType type)
        : type(type)
    {
    }

    SymbolTypeInfo(SymbolType type, int initVal)
        : type(type)
        , initVal(initVal)
    {
    }

    SymbolTypeInfo(SymbolType type, SymbolTypeInfoPtr elementType, int size = 0, std::vector<int> initVals = {})
        : type(type)
        , elementType(elementType)
        , size(size)
        , initVals(initVals)
    {
    }

    bool isArray() const
    {
        return type == SymbolType::ARRAY || type == SymbolType::ARRAY_CONST;
    }

    bool isInt() const
    {
        return type == SymbolType::INT || type == SymbolType::INT_CONST;
    }

    bool isFunc() const
    {
        return type == SymbolType::FUNC_INT || type == SymbolType::FUNC_VOID;
    }

    bool isConst() const
    {
        return type == SymbolType::INT_CONST || type == SymbolType::ARRAY_CONST;
    }

    bool isIntConst() const
    {
        return type == SymbolType::INT_CONST;
    }

    bool isArrayConst() const
    {
        return type == SymbolType::ARRAY_CONST;
    }

    bool isVoid() const
    {
        return type == SymbolType::FUNC_VOID;
    }

    bool isPointer() const
    {
        return type == SymbolType::POINTER;
    }

    int getDim() const
    {
        if (isArray() || isPointer()) {
            return 1 + elementType->getDim();
        }
        return 0;
    }

    int getDimSize(int dim) const
    {
        if (dim == 0) {
            return size;
        }
        return elementType->getDimSize(dim - 1);
    }

    static SymbolTypeInfoPtr createInt(int initVal = 0)
    {
        return make_shared<SymbolTypeInfo>(SymbolType::INT, initVal);
    }

    static SymbolTypeInfoPtr createIntConst(int constVal)
    {
        return make_shared<SymbolTypeInfo>(SymbolType::INT_CONST, constVal);
    }

    static SymbolTypeInfoPtr createFuncInt()
    {
        return make_shared<SymbolTypeInfo>(SymbolType::FUNC_INT);
    }

    static SymbolTypeInfoPtr createFuncVoid()
    {
        return make_shared<SymbolTypeInfo>(SymbolType::FUNC_VOID);
    }

    static SymbolTypeInfoPtr createArray(SymbolTypeInfoPtr elementType, int size, std::vector<int> initVals = {})
    {
        return make_shared<SymbolTypeInfo>(SymbolType::ARRAY, elementType, size, initVals);
    }

    static SymbolTypeInfoPtr createArrayConst(SymbolTypeInfoPtr elementType, int size, std::vector<int> initVals = {})
    {
        return make_shared<SymbolTypeInfo>(SymbolType::ARRAY_CONST, elementType, size, initVals);
    }

    static SymbolTypeInfoPtr createPointer(SymbolTypeInfoPtr elementType)
    {
        return make_shared<SymbolTypeInfo>(SymbolType::POINTER, elementType);
    }
};

struct Symbol : public enable_shared_from_this<Symbol> {
    string ident;
    string name;
    SymbolTypeInfoPtr type;

    Symbol(string_view ident, string_view name, SymbolTypeInfoPtr type)
        : ident(ident)
        , name(name)
        , type(type)
    {
    }
};

class SymbolTable : public enable_shared_from_this<SymbolTable> {
private:
    shared_ptr<SymbolTable> parent;
    std::unordered_map<string, SymbolPtr> symbols;

public:
    SymbolTable(shared_ptr<SymbolTable> parent = nullptr)
        : parent(parent)
    {
    }

    shared_ptr<Symbol> lookup(string_view ident) const
    {
        auto it = symbols.find(ident.data());
        if (it != symbols.end()) {
            return it->second;
        }
        if (parent) {
            return parent->lookup(ident);
        }
        return nullptr;
    }

    bool exist(string_view ident) const
    {
        return lookup(ident) != nullptr;
    }

    shared_ptr<SymbolTable> getParent() const
    {
        return parent;
    }

    SymbolTypeInfoPtr lookupType(string_view ident) const
    {
        auto symbol = lookup(ident);
        if (symbol) {
            return symbol->type;
        }
        return nullptr;
    }

    string lookupName(string_view ident) const
    {
        auto symbol = lookup(ident);
        if (symbol) {
            return symbol->name;
        }
        return "";
    }

    shared_ptr<Symbol> insert(string_view ident, string_view name, SymbolTypeInfoPtr type)
    {
        auto symbol = make_shared<Symbol>(ident, name, type);
        symbols[ident.data()] = symbol;
        return symbol;
    }

    shared_ptr<SymbolTable> createChild()
    {
        return make_shared<SymbolTable>(shared_from_this());
    }
};

class SymbolTableManager : public enable_shared_from_this<SymbolTableManager> {
private:
    shared_ptr<SymbolTable> global;
    shared_ptr<SymbolTable> current;
    NameManager nameManager;

public:
    SymbolTableManager()
    {
        global = make_shared<SymbolTable>();
        current = global;
    }

    shared_ptr<SymbolTable> getGlobal() const
    {
        return global;
    }

    shared_ptr<SymbolTable> getCurrent() const
    {
        return current;
    }

    void push()
    {
        current = current->createChild();
    }

    void pop()
    {
        if (current->getParent())
            current = current->getParent();
    }

    void resetNameManager()
    {
        nameManager.reset();
    }

    void lookup(string_view ident) const
    {
        current->lookup(ident);
    }

    bool exist(string_view ident) const
    {
        return current->exist(ident);
    }

    SymbolTypeInfoPtr lookupType(string_view ident) const
    {
        return current->lookupType(ident);
    }

    string lookupName(string_view ident) const
    {
        return current->lookupName(ident);
    }

    auto insertExternFuncInt(string_view ident, string_view name) -> string
    {
        global->insert(ident, name, SymbolTypeInfo::createFuncInt());
        return string { name };
    }

    auto insertFuncInt(string_view ident) -> string
    {
        auto name = fmt::format("@{}", ident);
        global->insert(ident, name, SymbolTypeInfo::createFuncInt());
        return name;
    }

    auto insertFuncVoid(string_view ident) -> string
    {
        auto name = fmt::format("@{}", ident);
        global->insert(ident, name, SymbolTypeInfo::createFuncVoid());
        return name;
    }

    auto insertInt(string_view ident, int initVal = 0, bool isGlobal = false) -> string
    {
        auto name = isGlobal ? nameManager.genUniqueGlobalName(ident) : nameManager.genUniqueLocalName(ident);
        current->insert(ident, name, SymbolTypeInfo::createInt(initVal));
        return name;
    }

    auto insertIntConst(string_view ident, int constVal, bool isGlobal = false) -> string
    {
        auto name = isGlobal ? nameManager.genUniqueGlobalName(ident) : nameManager.genUniqueLocalName(ident);
        current->insert(ident, name, SymbolTypeInfo::createIntConst(constVal));
        return name;
    }

    auto insertArray(string_view ident, SymbolTypeInfoPtr elementType, int size, bool isGlobal = false) -> string
    {
        auto name = isGlobal ? nameManager.genUniqueGlobalName(ident) : nameManager.genUniqueLocalName(ident);
        current->insert(ident, name, SymbolTypeInfo::createArray(elementType, size));
        return name;
    }

    auto insertArrayConst(string_view ident, SymbolTypeInfoPtr elementType, int size, std::vector<int> initVals,
        bool isGlobal = false) -> string
    {
        auto name = isGlobal ? nameManager.genUniqueGlobalName(ident) : nameManager.genUniqueLocalName(ident);
        current->insert(ident, name, SymbolTypeInfo::createArrayConst(elementType, size, initVals));
        return name;
    }

    auto insertPointer(string_view ident, SymbolTypeInfoPtr elementType, bool isGlobal = false) -> string
    {
        auto name = isGlobal ? nameManager.genUniqueGlobalName(ident) : nameManager.genUniqueLocalName(ident);
        current->insert(ident, name, SymbolTypeInfo::createPointer(elementType));
        return name;
    }

    string genTempName()
    {
        return nameManager.genTempName();
    }

    string genLabelName(string_view labelIdent)
    {
        return nameManager.genLabelName(labelIdent);
    }

    string getLabelRef(string_view labelName)
    {
        return nameManager.getLabelRef(labelName);
    }

    string genUniqueGlobalName(string_view ident)
    {
        return nameManager.genUniqueGlobalName(ident);
    }

    string genUniqueLocalName(string_view ident)
    {
        return nameManager.genUniqueLocalName(ident);
    }
};
