#pragma once
#include <deque>
#include <fmt/core.h>
#include <memory>
#include <sstream>
#include <string>
#include <string_view>
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

class IRNode;

using IRNodePtr = shared_ptr<IRNode>;

class IRNode : public std::enable_shared_from_this<IRNode> {
public:
    virtual ~IRNode() = default;
    virtual void append(string_view str) = 0;
    virtual void clear() = 0;
    virtual void set(string_view str) = 0;
    virtual bool empty() const = 0;
    virtual string str() const = 0;
    virtual IRNodePtr ref() = 0;

    template <typename T>
    shared_ptr<T> refAs()
    {
        return std::static_pointer_cast<T>(ref());
    }
};

class IRString : public IRNode {
    stringstream ss;

public:
    IRString() = default;

    IRString(string_view str)
    {
        ss << str;
    }

    void append(string_view str) override
    {
        ss << str;
    }

    void clear() override
    {
        ss.str("");
    }

    void set(string_view str) override
    {
        clear();
        ss << str;
    }

    bool empty() const override
    {
        return ss.str().empty();
    }

    string str() const override
    {
        return ss.str();
    }

    IRNodePtr ref() override;

    static shared_ptr<IRString> create()
    {
        return std::make_shared<IRString>();
    }

    static shared_ptr<IRString> create(string_view str)
    {
        return std::make_shared<IRString>(str);
    }
};

using IRStringPtr = shared_ptr<IRString>;

class IRStringRef : public IRNode {
    IRNodePtr node;

public:
    IRStringRef(IRNodePtr str)
        : node(str)
    {
    }

    void append(string_view str) override
    {
        this->node->append(str);
    }

    void clear() override
    {
        this->node->clear();
    }

    void set(string_view str) override
    {
        this->node->set(str);
    }

    bool empty() const override
    {
        return this->node->empty();
    }

    string str() const override
    {
        return this->node->str();
    }

    IRNodePtr ref() override
    {
        return make_shared<IRStringRef>(shared_from_this());
    }

    void merge(shared_ptr<IRStringRef> other)
    {
        this->node = other->node;
    }

    static shared_ptr<IRStringRef> create(IRStringPtr str)
    {
        return std::make_shared<IRStringRef>(str);
    }
};

class IRLabel : public IRNode {
    string label;
    shared_ptr<IRStringRef> labelRef, labelDecl;

public:
    IRLabel(string_view label)
        : label(label)
    {
        labelRef = IRString::create(fmt::format("%{}", label))->refAs<IRStringRef>();
        labelDecl = IRString::create(fmt::format("{}:\n", label))->refAs<IRStringRef>();
    }

    void append(string_view str) override
    {
        throw std::runtime_error("Cannot append to IRLabel");
    }

    void clear() override
    {
        throw std::runtime_error("Cannot clear IRLabel");
    }

    void set(string_view str) override
    {
        label = str;
        labelRef->set(fmt::format("%{}", str));
        labelDecl->set(fmt::format("{}:\n", str));
    }

    void setRef(string_view refStr)
    {
        labelRef->set(refStr);
    }

    void setRef(shared_ptr<IRStringRef> str)
    {
        labelRef->merge(str);
    }

    void setDecl(string_view declStr)
    {
        labelDecl->set(declStr);
    }

    void setDecl(shared_ptr<IRStringRef> str)
    {
        labelDecl->merge(str);
    }

    bool empty() const override
    {
        return label.empty();
    }

    string str() const override
    {
        return label;
    }

    IRNodePtr ref() override
    {
        return shared_from_this();
    }

    shared_ptr<IRStringRef> refRef() const
    {
        return labelRef->refAs<IRStringRef>();
    }

    shared_ptr<IRStringRef> refDecl() const
    {
        return labelDecl->refAs<IRStringRef>();
    }

    static shared_ptr<IRLabel> create(string_view label)
    {
        return std::make_shared<IRLabel>(label);
    }
};

using IRLabelPtr = shared_ptr<IRLabel>;
