//
// Created by pro on 2022/9/16.
//

#ifndef ISTOOL_INCRE_CONTEXT_H
#define ISTOOL_INCRE_CONTEXT_H

#include "incre_term.h"
#include "incre_type.h"
#include <unordered_map>

namespace incre {
    enum class BindingType {
        TYPE, TERM, VAR
    };

    class BindingData {
    public:
        BindingType type;
        BindingData(const BindingType& _type);
        virtual std::string toString() const = 0;
        BindingType getType() const;
        virtual ~BindingData() = default;
    };

    typedef std::shared_ptr<BindingData> Binding;

    class TypeBinding: public BindingData {
    public:
        Ty type;
        TypeBinding(const Ty& _type);
        virtual std::string toString() const;
        virtual ~TypeBinding() = default;
    };

    class TermBinding: public BindingData {
    public:
        Term term;
        Ty type;
        TermBinding(const Term& _term, const Ty& _ty=nullptr);
        virtual std::string toString() const;
        virtual ~TermBinding() = default;
    };

    class VarTypeBinding: public BindingData {
    public:
        Ty type;
        VarTypeBinding(const Ty& _type);
        virtual std::string toString() const;
        virtual ~VarTypeBinding() = default;
    };

    class Context {
    public:
        std::unordered_map<std::string, Binding> binding_map;
        void addBinding(const std::string& name, const Ty& type);
        void addBinding(const std::string& name, const Term& term, const Ty& type=nullptr);
        Term getTerm(const std::string& name);
        Ty getType(const std::string& name);
    };

    class TypeContext {
    public:
        struct BindLog {
            std::string name;
            Ty binding;
        };
        std::unordered_map<std::string, Ty> binding_map;
        TypeContext(Context* ctx);
        TypeContext() = default;
        virtual BindLog bind(const std::string& name, const Ty& type);
        Ty lookup(const std::string& name);
        virtual void cancelBind(const BindLog& log);
        ~TypeContext();
    };
}

#endif //ISTOOL_INCRE_CONTEXT_H
