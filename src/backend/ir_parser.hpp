#pragma once

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <string>

namespace sysy {

class IRParser {
public:
    // 构造函数
    IRParser() {};

    // 解析IR文件并返回Module
    std::unique_ptr<llvm::Module> parseIRFile(const std::string& filename)
    {
        // 使用LLVM的IRReader来读取IR文件
        auto module = llvm::parseIRFile(filename, err, context);
        if (!module) {
            err.print("IRParser", llvm::errs());
            return nullptr;
        }
        return module;
    }

private:
    llvm::LLVMContext context;
    llvm::SMDiagnostic err;
};
} // namespace sysy
