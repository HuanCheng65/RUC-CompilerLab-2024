#pragma once
#include "frontend/ir_string.hpp"
#include <deque>
#include <fmt/core.h>
#include <format>
#include <memory>
#include <range/v3/range/conversion.hpp>
#include <range/v3/view/join.hpp>
#include <sstream>
#include <stack>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

using std::deque;
using std::endl;
using std::shared_ptr;
using std::string;
using std::string_view;
using std::stringstream;
using std::vector;
using namespace std::string_literals;
using namespace std::string_view_literals;
using namespace ranges;

class LLVMIRBuilder {
private:
    deque<IRNodePtr> nodes;
    int indent = 0;

public:
    void increaseIndent()
    {
        indent++;
    }

    void decreaseIndent()
    {
        indent--;
    }

    void append(string_view str)
    {
        // if (nodes.empty() || !std::dynamic_pointer_cast<IRString>(nodes.back()))
        // {
        //     nodes.push_back(std::make_shared<IRString>());
        // }
        // auto textNode = std::dynamic_pointer_cast<IRString>(nodes.back());
        // if (textNode->refCount() == 0)
        //     textNode->append(str);
        // else
        // {
        //     nodes.push_back(IRString::create(str));
        // }
        nodes.push_back(IRString::create(str)->ref());
    }

    void append(IRNodePtr node)
    {
        nodes.push_back(node->ref());
    }

    void addIndent()
    {
        for (int i = 0; i < indent; i++) {
            append("    ");
        }
    }

    void addLine()
    {
        append("\n");
    }

    void addLine(string_view line, bool indent = true)
    {
        if (indent) {
            addIndent();
        }
        append(line);
        addLine();
    }

    void pushFuncDef(string_view funcName, string_view retType, vector<string> args = {})
    {
        auto argStr = args | views::join(", "s) | to<string>;
        addLine(fmt::format("define {} {}({}) {{", retType, funcName, argStr));
        increaseIndent();
    }

    void popFuncDef()
    {
        decreaseIndent();
        addLine("}");
        addLine();
    }

    string arrayType(string_view type, int size)
    {
        return fmt::format("[{} x {}]", size, type);
    }

    string arrayType(string_view type, string_view size)
    {
        return fmt::format("[{} x {}]", size, type);
    }

    string load(string_view type, string_view src)
    {
        return fmt::format("load {}, ptr {}", type, src);
    }

    string alloc(string_view type)
    {
        return fmt::format("alloca {}", type);
    }

    string call(string_view retType, string_view funcName, vector<string> args = {}) const
    {
        auto argStr = args | views::join(", "s) | to<string>;
        return fmt::format("call {} {}({})", retType, funcName, argStr);
    }

    string ptrIndex(string_view type, string_view src)
    {
        return fmt::format("{0} {1}", type, src);
    }

    string arrayIndex(string_view type, string_view src)
    {
        return fmt::format("{0} 0, {0} {1}", type, src);
    }

    string getelementptr(string_view type, string_view ptr, string_view index)
    {
        return fmt::format("getelementptr {}, ptr {}, {}", type, ptr, index);
    }

    void addGlobalVarDef(string_view name, string_view type, string_view value)
    {
        addLine(fmt::format("{} = global {} {}", name, type, value));
    }

    void addGlobalVarDef(string_view name, string_view type)
    {
        addGlobalVarDef(name, type, "zeroinitializer");
    }

    void addGlobalConstDef(string_view name, string_view type, string_view value)
    {
        addLine(fmt::format("{} = constant {} {}", name, type, value));
    }

    void addFuncDecl(string_view funcName, string_view retType, vector<string> args = {})
    {
        auto argStr = args | views::join(", "s) | to<string>;
        addLine(fmt::format("declare {} {}({})", retType, funcName, argStr));
    }

    void addRet()
    {
        addLine("ret void"sv);
    }

    void addRet(string_view type, string_view value)
    {
        addLine(fmt::format("ret {} {}", type, value));
    }

    void addLabel(string_view label)
    {
        addLine(fmt::format("{}:", label), false);
    }

    void addLabel(IRLabelPtr label)
    {
        append(label->refDecl());
    }

    void addBr(string_view labelName)
    {
        addLine(fmt::format("br label %{}", labelName));
    }

    void addBr(IRLabelPtr label)
    {
        // fmt::println("br label {}", label->refRef()->str());
        addIndent();
        append("br label ");
        append(label->refRef());
        addLine();
    }

    void addBr(string_view cond, string_view trueLabel, string_view falseLabel)
    {
        addLine(fmt::format("br i1 {}, label %{}, label %{}", cond, trueLabel, falseLabel));
    }

    void addBr(string_view cond, string_view trueLabel, IRLabelPtr falseLabel)
    {
        addIndent();
        append("br i1 ");
        append(cond);
        append(", label %");
        append(trueLabel);
        append(", label ");
        append(falseLabel->refRef());
        addLine();
    }

    void addStore(string_view type, string_view value, string_view ptr)
    {
        addLine(fmt::format("store {} {}, ptr {}", type, value, ptr));
    }

    void addAssign(string_view dest, string_view value)
    {
        addLine(fmt::format("{} = {}", dest, value));
    }

    void addBinary(string_view op, string_view dest, string_view type, string_view lhs, string_view rhs)
    {
        addLine(fmt::format("{} = {} {} {}, {}", dest, op, type, lhs, rhs));
    }

    void addUnary(string_view op, string_view dest, string_view type, string_view operand)
    {
        addLine(fmt::format("{} = {} {} {}", dest, op, type, operand));
    }

    bool empty() const
    {
        if (nodes.empty()) {
            return true;
        }
        return std::all_of(nodes.begin(), nodes.end(), [](auto node) { return node->empty(); });
    }

    string getIR() const
    {
        stringstream ss;
        for (auto node : nodes) {
            ss << node->str();
        }
        return ss.str();
    }

    string escapeStringLiteral(string_view str)
    {
        string result;
        for (char c : str) {
            switch (c) {
            case '\n':
                result += "\\0A";
                break;
            case '\r':
                result += "\\0D";
                break;
            case '\t':
                result += "\\09";
                break;
            case '\\':
                result += "\\\\";
                break;
            case '\"':
                result += "\\22";
                break;
            default:
                result += c;
                break;
            }
        }
        return result;
    }

    void clear()
    {
        nodes.clear();
    }
};

class WhileStack {
    struct WhileFrame {
        string condLabel, bodyLabel, endLabel;

        WhileFrame(string_view condLabel, string_view bodyLabel, string_view endLabel)
            : condLabel(condLabel)
            , bodyLabel(bodyLabel)
            , endLabel(endLabel)
        {
        }
    };

    std::stack<WhileFrame> stack;

public:
    void push(string_view condLabel, string_view bodyLabel, string_view endLabel)
    {
        stack.emplace(condLabel, bodyLabel, endLabel);
    }

    void pop()
    {
        stack.pop();
    }

    string getCondLabel() const
    {
        return stack.top().condLabel;
    }

    string getBodyLabel() const
    {
        return stack.top().bodyLabel;
    }

    string getEndLabel() const
    {
        return stack.top().endLabel;
    }

    bool empty() const
    {
        return stack.empty();
    }
};

template <typename Derived, typename Base>
inline bool is_ptr_type(shared_ptr<Base> ptr)
{
    return std::dynamic_pointer_cast<Derived>(ptr) != nullptr;
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

namespace sysy {
class NameManager {
private:
    std::unordered_map<string, int> identCount;
    int tempCount = 0;

    string genUniqueName(string_view ident)
    {
        auto it = identCount.find(ident.data());
        if (it == identCount.end()) {
            identCount[ident.data()] = 1;
            return std::format("{}", ident);
        }
        return std::format("{}.{}", ident, it->second++);
    }

public:
    void reset()
    {
        // nameCount.clear();
        tempCount = 0;
    }

    string genTempName()
    {
        return std::format("%t{}", ++tempCount);
    }

    string genUniqueLocalName(string_view ident)
    {
        return std::format("%{}", genUniqueName(ident));
    }

    string genUniqueGlobalName(string_view ident)
    {
        return std::format("@{}", genUniqueName(ident));
    }

    string genLabelName(string_view labelIdent)
    {
        auto it = identCount.find(labelIdent.data());
        if (it == identCount.end()) {
            identCount[labelIdent.data()] = 1;
            return std::format("{}", labelIdent);
        }
        return std::format("{}.{}", labelIdent, it->second++);
    }

    string getLabelRef(string_view labelName)
    {
        return std::format("%{}", labelName);
    }
};
}