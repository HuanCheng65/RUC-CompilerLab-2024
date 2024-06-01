#include "backend/asm_generator.hpp"
#include "frontend/ir_generator.hpp"
#include "parser.hpp"
#include <CLI/CLI.hpp>
#include <fstream>
#include <iostream>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Pass.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

void printFunction(const llvm::Function &F)
{
    llvm::outs() << "Function: " << F.getName() << "\n";
    for (const auto &B : F)
    {
        llvm::outs() << "  BasicBlock: " << B.getName() << "\n";
        for (const auto &I : B)
        {
            llvm::outs() << "    Instruction: " << I << "\n";
            if (auto storeInst = llvm::dyn_cast<llvm::StoreInst>(&I))
            {
                llvm::outs() << "      StoreInst: " << *storeInst->getPointerOperand() << " = "
                             << *storeInst->getValueOperand() << "\n";
            }
            else if (auto loadInst = llvm::dyn_cast<llvm::LoadInst>(&I))
            {
                llvm::outs() << "      LoadInst: " << *loadInst->getPointerOperand() << " = " << *loadInst << "\n";
            }
            else if (auto allocaInst = llvm::dyn_cast<llvm::AllocaInst>(&I))
            {
                llvm::outs() << "      AllocaInst: " << allocaInst->getName() << "\n";
            }
        }
    }
}

int main(int argc, char *argv[])
{
    CLI::App app{"SysY Compiler"};

    std::string inputFile;
    std::string outputName = "output";
    bool emitIR = false;
    bool useLLVM = false;

    app.add_option("input", inputFile, "Input file name")->required();
    app.add_option("-o,--output", outputName, "Output file name");
    app.add_flag("-i,--emit-ir", emitIR, "Emit LLVM IR");
    app.add_flag("-l,--use-llvm", useLLVM, "Use LLVM backend");

    CLI11_PARSE(app, argc, argv);

    freopen(inputFile.c_str(), "r", stdin);

    Elite::ParserCtx ctx;
    if (ctx.parser->parse())
    {
        std::cerr << "Parse failed" << std::endl;
        return 1;
    }
    std::cout << "Parse succeeded" << std::endl;
    auto ast = ctx.result;
    auto irGenerator = IRGenerator();
    // try
    // {
    //     irGenerator.generateIR(ast);
    // }
    // catch (std::exception &e)
    // {
    //     std::cerr << e.what() << std::endl;
    //     return 1;
    // }
    irGenerator.generateIR(ast);
    auto result = irGenerator.getIR();

    if (emitIR)
    {
        std::ofstream out(fmt::format("{}.ll", outputName));
        out << result;
        out.flush();
        return 0;
    }

    llvm::LLVMContext context;
    llvm::SMDiagnostic err;
    auto module = llvm::parseIR(llvm::MemoryBufferRef(result, "input"), err, context);

    if (!module)
    {
        err.print(argv[0], llvm::errs());
        return 1;
    }

    if (!useLLVM)
    {
        sysy::AsmGenerator asmGenerator;
        asmGenerator.generate(module);
        auto myAsmResult = asmGenerator.getAsm();
        std::ofstream asmOut(fmt::format("{}.s", outputName));
        asmOut << myAsmResult;
        asmOut.flush();
    }
    else
    {
        const char *args[] = {"sysy", "--x86-asm-syntax=intel"};
        const auto res = llvm::cl::ParseCommandLineOptions(std::size(args), args);
        assert(res && "Failed to parse command line options");

        // 初始化目标
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();

        // 获取目标三元组
        auto targetTriple = llvm::sys::getDefaultTargetTriple();
        module->setTargetTriple(targetTriple);

        std::string error;
        auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
        if (!target)
        {
            llvm::errs() << "Error: " << error;
            return 1;
        }

        auto cpu = "generic";
        auto features = "";

        llvm::TargetOptions opt;
        auto rm = std::optional<llvm::Reloc::Model>();
        auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

        module->setDataLayout(targetMachine->createDataLayout());

        std::error_code ec;
        llvm::raw_fd_ostream dest(fmt::format("{}.s", outputName), ec, llvm::sys::fs::OF_None);

        if (ec)
        {
            llvm::errs() << "Could not open file: " << ec.message();
            return 1;
        }

        // targetMachine.setI
        llvm::legacy::PassManager pass;
        auto fileType = llvm::CodeGenFileType::AssemblyFile;
        if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType))
        {
            llvm::errs() << "TargetMachine can't emit a file of this type";
            return 1;
        }

        pass.run(*module);
        dest.flush();
    }

    return 0;
}
