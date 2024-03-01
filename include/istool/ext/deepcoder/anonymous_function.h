//
// Created by pro on 2022/1/15.
//

#ifndef ISTOOL_ANONYMOUS_FUNCTION_H
#define ISTOOL_ANONYMOUS_FUNCTION_H

#include "istool/basic/program.h"

typedef std::function<Data(const ProgramList&, ExecuteInfo*)> SemanticsFunction;
typedef std::function<Data(DataList&&, ExecuteInfo*)> FullSemanticsFunction;

class AnonymousSemantics: public Semantics {
public:
    SemanticsFunction f;
    AnonymousSemantics(const SemanticsFunction& f, const std::string& name="?");
    AnonymousSemantics(const FullSemanticsFunction& f, const std::string& name="?");
    virtual ~AnonymousSemantics() = default;
    virtual Data run(const std::vector<std::shared_ptr<Program>>& sub_list, ExecuteInfo* info);
};

class TypedAnonymousSemantics: public AnonymousSemantics, public TypedSemantics {
public:
    TypedAnonymousSemantics(const SemanticsFunction& f, const TypeList& inp_list, const PType& oup, const std::string& name="?");
    TypedAnonymousSemantics(const FullSemanticsFunction& f, const TypeList& inp_list, const PType& oup, const std::string& name="?");
    virtual ~TypedAnonymousSemantics() = default;
};

class SemanticsValue: public Value {
public:
    PSemantics sem;
    SemanticsValue(const PSemantics& sem);
    virtual ~SemanticsValue() = default;
    virtual std::string toString() const;
    virtual std::string toHaskell(bool in_result) const;
    virtual bool equal(Value* value) const;
};

namespace ext::ho {
    Data buildAnonymousData(const SemanticsFunction& f, const std::string& name="?");
    Data buildAnonymousData(const FullSemanticsFunction& f, const std::string& name="?");
    PSemantics getSemantics(const Data& data);
}

#endif //ISTOOL_ANONYMOUS_FUNCTION_H
