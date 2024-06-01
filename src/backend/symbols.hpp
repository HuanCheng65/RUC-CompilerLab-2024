#pragma once
#include <memory>
#include <string>
#include <unordered_map>

using std::string_view;

namespace sysy {
enum class SymbolType {
    Int,
    Array,
    StringLiteral,
    Label,
};

struct Symbol;

using SymbolPtr = std::shared_ptr<Symbol>;

struct Symbol : std::enable_shared_from_this<Symbol> {
    std::string ident, name;
    SymbolType type;

    static SymbolPtr createInt(string_view ident, string_view name)
    {
        auto symbol = std::make_shared<Symbol>();
        symbol->ident = ident;
        symbol->name = name;
        symbol->type = SymbolType::Int;
        return symbol;
    }

    static SymbolPtr createArray(string_view ident, string_view name)
    {
        auto symbol = std::make_shared<Symbol>();
        symbol->ident = ident;
        symbol->name = name;
        symbol->type = SymbolType::Array;
        return symbol;
    }

    static SymbolPtr createStringLiteral(string_view ident, string_view name)
    {
        auto symbol = std::make_shared<Symbol>();
        symbol->ident = ident;
        symbol->name = name;
        symbol->type = SymbolType::StringLiteral;
        return symbol;
    }

    static SymbolPtr createLabel(string_view ident, string_view name)
    {
        auto symbol = std::make_shared<Symbol>();
        symbol->ident = ident;
        symbol->name = name;
        symbol->type = SymbolType::Label;
        return symbol;
    }
};

class SymbolTable;

using SymbolTablePtr = std::shared_ptr<SymbolTable>;

class SymbolTable : public std::enable_shared_from_this<SymbolTable> {
    std::unordered_map<std::string, SymbolPtr> symbols {};
    SymbolTablePtr parent;

    SymbolPtr getSymbol(string_view ident)
    {
        if (symbols.find(ident.data()) == symbols.end()) {
            return nullptr;
        }
        return symbols[ident.data()];
    }

public:
    SymbolTable()
        : parent(nullptr)
    {
    }

    SymbolTable(SymbolTablePtr parent)
        : parent(parent)
    {
    }

    void addSymbol(SymbolPtr symbol)
    {
        symbols[symbol->ident] = symbol;
    }

    SymbolPtr findSymbol(string_view ident)
    {
        auto symbol = getSymbol(ident);
        if (symbol) {
            return symbol;
        }
        if (parent) {
            return parent->findSymbol(ident);
        }
        return nullptr;
    }

    SymbolTablePtr createChild()
    {
        return std::make_shared<SymbolTable>(shared_from_this());
    }

    SymbolTablePtr getParent()
    {
        return parent;
    }
};
} // namespace sysy
