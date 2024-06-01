#pragma once
#include <deque>
#include <fmt/format.h>
#include <iterator>
#include <llvm/IR/Instructions.h>
#include <memory>
#include <ranges>
#include <string>
#include <unordered_map>
#include <vector>

using std::deque;
using std::shared_ptr, std::enable_shared_from_this;
using std::string;
using std::string_view;
using std::vector;
using namespace std::string_view_literals;

namespace sysy {
enum class DFGNodeType {
    BinaryOp,
    Cast,
    Cmp,
    Load,
    Store,
    Call,
    Return,
    Branch,
    CondBranch,
    Const,
    Var,
    GetElementPtr,
};

struct DFGNode;

using DFGNodePtr = shared_ptr<DFGNode>;

struct DFGNode : public enable_shared_from_this<DFGNode> {
    int defLoc = -1;
    string name;
    DFGNodeType type;
    // 该节点依赖的节点
    vector<DFGNodePtr> dependencies {};
    // 依赖该节点的节点
    vector<DFGNodePtr> dependents {};

    DFGNode(int defLoc)
        : defLoc(defLoc)
    {
    }

    DFGNode(int defLoc, string_view name)
        : defLoc(defLoc)
        , name(name)
    {
    }

    virtual ~DFGNode() = default;

    void addDependency(DFGNodePtr node)
    {
        if (!node->isRealNode()) // 虚拟节点不添加依赖
            return;
        if (std::find(dependencies.begin(), dependencies.end(), node) != dependencies.end())
            return;
        dependencies.push_back(node);
        node->dependents.push_back(shared_from_this());
    }

    void removeDependency(DFGNodePtr node)
    {
        if (!node->isRealNode()) // 虚拟节点不添加依赖
            return;
        if (dependencies.empty())
            return;
        if (std::find(dependencies.begin(), dependencies.end(), node) == dependencies.end())
            return;
        dependencies.erase(std::remove(dependencies.begin(), dependencies.end(), node), dependencies.end());
        node->dependents.erase(std::remove(node->dependents.begin(), node->dependents.end(), shared_from_this()),
            node->dependents.end());
    }

    const auto getDependentNodes() const
    {
        return dependents;
    }

    auto countDependentNodes() const
    {
        return dependents.size();
    }

    bool hasName() const
    {
        return !name.empty();
    }

    virtual bool isLeaf() const
    {
        return false;
    }

    virtual string dump() const
    {
        return name;
    }

    virtual bool isRealNode() const
    {
        return true;
    }
};

class DFG {
    vector<DFGNodePtr> nodes;
    vector<int> visited;
    std::unordered_map<string, bool> visitedByName;

public:
    DFG() = default;

    DFG(vector<DFGNodePtr> nodes)
        : nodes(nodes)
    {
    }

    void addNode(DFGNodePtr node)
    {
        if (std::find(nodes.begin(), nodes.end(), node) != nodes.end())
            return;
        nodes.push_back(node);
    }

    void removeNode(DFGNodePtr node)
    {
        if (std::find(nodes.begin(), nodes.end(), node) == nodes.end())
            return;
        nodes.erase(std::remove(nodes.begin(), nodes.end(), node), nodes.end());
    }

    DFGNodePtr getNodeByName(string_view name)
    {
        for (auto& node : nodes) {
            if (node->name == name) {
                return node;
            }
        }
        return nullptr;
    }

    // 输出 Graphviz 格式的 DFG
    void print()
    {
        // 先输出节点
        fmt::println("digraph G {{");
        visitedByName.clear();
        for (size_t i = 0; i < nodes.size(); i++) {
            auto& node = nodes[i];
            auto ident { node->hasName() ? node->name : fmt::format("node{}", i) };
            if (visitedByName[ident])
                continue;
            fmt::print("  \"{}\" [label=\"{}\" shape={}];\n", ident, node->dump(), node->isLeaf() ? "box" : "ellipse");
            visitedByName[ident] = true;
        }
        for (size_t i = 0; i < nodes.size(); i++) {
            auto& node = nodes[i];
            auto ident { node->hasName() ? node->name : fmt::format("node{}", i) };
            for (auto& dep : node->dependents) {
                auto depIdent { dep->hasName()
                        ? dep->name
                        : fmt::format("node{}", std::distance(nodes.begin(), std::find(nodes.begin(), nodes.end(), dep))) };
                fmt::print("  \"{}\" -> \"{}\";\n", ident, depIdent);
            }
        }
        fmt::println("}}");
    }
};
} // namespace sysy