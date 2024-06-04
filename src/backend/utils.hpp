#pragma once
#include "registers.hpp"
#include <fmt/core.h>
#include <memory>
#include <range/v3/range/conversion.hpp>
#include <range/v3/view/join.hpp>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

using std::endl;
using std::shared_ptr;
using std::string;
using std::string_view;
using std::stringstream;
using std::unordered_set;
using std::vector;
using namespace std::string_literals;
using namespace std::string_view_literals;
using namespace ranges;

namespace sysy {
template <typename Derived, typename Base> inline bool ptrIs(shared_ptr<Base> ptr)
{
    return std::dynamic_pointer_cast<Derived>(ptr) != nullptr;
}

template <typename T>
unordered_set<T> unionSet(const unordered_set<T>& a, const unordered_set<T>& b)
{
    unordered_set<T> res = a;
    res.insert(b.begin(), b.end());
    return res;
}

template <typename T>
unordered_set<T> exceptSet(const unordered_set<T>& a, const unordered_set<T>& b)
{
    unordered_set<T> res;
    for (const auto& elem : a) {
        if (b.find(elem) == b.end())
            res.insert(elem);
    }
    return res;
}

template <typename T>
unordered_set<T> intersectSet(const unordered_set<T>& a, const unordered_set<T>& b)
{
    unordered_set<T> res;
    for (const auto& elem : a) {
        if (b.find(elem) != b.end())
            res.insert(elem);
    }
    return res;
}

template <typename T> std::set<T> intersectSet(const std::set<T>& a, const std::set<T>& b)
{
    std::set<T> res;
    for (const auto& elem : a) {
        if (b.find(elem) != b.end())
            res.insert(elem);
    }
    return res;
}

class AsmBuilder {
private:
    stringstream ss;

    template <typename... Operands> void printOperands(string_view operand, Operands... others)
    {
        ss << operand << ", ";
        printOperands(others...);
    }

    template <typename... Operands> void printOperands(Register operand, Operands... others)
    {
        ss << getRegisterName(operand) << ", ";
        printOperands(others...);
    }

public:
    template <typename... Operands> void addOp(string_view op, Operands... operands)
    {
        if (op.length() < 4) // align the op code
            ss << "\t" << op << "\t\t";
        else
            ss << '\t' << op << '\t';
        printOperands(operands...);
        ss << endl;
    }

    void addOp(string_view op) { ss << '\t' << op << endl; }

    void addLabel(string_view label) { ss << label << ':' << endl; }

    void addComment(string_view comment) { ss << "\t# " << comment << endl; }

    string getAsm() const { return ss.str(); }

    void clear() { ss.str(""); }

    void append(AsmBuilder& other) { ss << other.getAsm(); }
};

template <> inline void AsmBuilder::printOperands(string_view operand) { ss << operand; }

template <> inline void AsmBuilder::printOperands(Register operand)
{
    ss << sysy::getRegisterName(operand);
}

class AsmNameManager {
private:
    std::unordered_map<string, int> identCount;

public:
    void reset() { identCount.clear(); }

    string genUniqueName(string_view ident)
    {
        auto it = identCount.find(ident.data());
        if (it == identCount.end()) {
            identCount[ident.data()] = 1;
            return std::format("{}", ident);
        }
        return std::format("{}.{}", ident, it->second++);
    }

    string genLabelName(string_view labelIdent)
    {
        auto it = identCount.find(labelIdent.data());
        if (it == identCount.end()) {
            identCount[labelIdent.data()] = 1;
            return std::format(".{}", labelIdent);
        }
        return std::format(".{}.{}", labelIdent, it->second++);
    }
};
} // namespace sysy

class SegmentTree {
public:
    SegmentTree()
        : root(nullptr)
    {
    }

    // 区间插入
    void insert(int L, int R, bool val) { insert(root, -inf, inf, L, R, val); }

    // 单点查询
    bool query(int pos) { return query(root, -inf, inf, pos); }

    // 区间查询
    bool query(int L, int R) { return query(root, -inf, inf, L, R); }

private:
    struct Node {
        bool val;
        bool lazy;
        Node* left;
        Node* right;
        Node()
            : val(false)
            , lazy(false)
            , left(nullptr)
            , right(nullptr)
        {
        }
    };

    Node* root;
    const int inf = 1e9;

    // 区间插入
    void insert(Node*& node, int l, int r, int L, int R, bool val)
    {
        if (!node)
            node = new Node();
        if (L <= l && r <= R) {
            node->val = val;
            node->lazy = true;
            return;
        }
        int mid = l + (r - l) / 2;
        propagate(node);
        if (L <= mid)
            insert(node->left, l, mid, L, R, val);
        if (R > mid)
            insert(node->right, mid + 1, r, L, R, val);
        node->val
            = (node->left ? node->left->val : false) || (node->right ? node->right->val : false);
    }

    // 单点查询
    bool query(Node* node, int l, int r, int pos)
    {
        if (!node)
            return false;
        if (l == r)
            return node->val;
        int mid = l + (r - l) / 2;
        propagate(node);
        if (pos <= mid)
            return query(node->left, l, mid, pos);
        else
            return query(node->right, mid + 1, r, pos);
    }

    // 区间查询
    bool query(Node* node, int l, int r, int L, int R)
    {
        if (!node)
            return false;
        if (L <= l && r <= R)
            return node->val;
        int mid = l + (r - l) / 2;
        propagate(node);
        bool res = false;
        if (L <= mid)
            res |= query(node->left, l, mid, L, R);
        if (R > mid)
            res |= query(node->right, mid + 1, r, L, R);
        return res;
    }

    // 传播懒惰标记
    void propagate(Node* node)
    {
        if (node->lazy) {
            if (!node->left)
                node->left = new Node();
            if (!node->right)
                node->right = new Node();
            node->left->val = node->right->val = node->val;
            node->left->lazy = node->right->lazy = true;
            node->lazy = false;
        }
    }
};
