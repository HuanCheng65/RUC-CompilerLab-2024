#pragma once
#include "AST.hpp"
#include <memory>

using std::shared_ptr;

namespace yy {
class Parser;
class location;
} // namespace yy

namespace Elite {

class ParserCtx {
public:
    ParserCtx();
    ~ParserCtx();

    void* lexer;
    yy::location* loc;
    yy::Parser* parser;
    shared_ptr<CompUnitAST> result;
};

} // namespace Elite
