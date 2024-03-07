//
// Created by pro on 2022/9/17.
//

#ifndef ISTOOL_INCRE_H
#define ISTOOL_INCRE_H

#include "incre_term.h"
#include "incre_context.h"
#include "incre_value.h"
#include "incre_program.h"

namespace incre {
    bool isUsed(const Pattern& pt, const std::string& name);
    bool isMatch(const Data& data, const Pattern& pt);
    std::vector<std::pair<std::string, Term>> bindPattern(const Data& data, const Pattern& pt);
    std::vector<std::string> getPatternVars(const Pattern& pt);
    std::vector<std::string> getUnboundedVars(TermData* term);

    Term subst(const Term& x, const std::string& name, const Term& y);
    Data run(const Term& term, Context* ctx);
    void run(const Command& command, Context* ctx);
    void run(const IncreProgram& program, Context* ctx);
    Context* run(const IncreProgram& program);

    struct ExternalEnvRunRule {
        std::function<Data(const Term&, EnvAddress*, AddressHolder*, const std::unordered_map<TermType, ExternalEnvRunRule>&)> func;
    };
    typedef std::unordered_map<TermType, ExternalEnvRunRule> ExternalEnvRuleMap;
    Data runApp(const Data& func, const Data& param, AddressHolder* holder, const ExternalEnvRuleMap& map = {});
    Data envRun(const Term& term, EnvAddress* env, AddressHolder* holder, const ExternalEnvRuleMap& map = {});
    void envRun(const Command& command, EnvContext* ctx, const ExternalEnvRuleMap& map = {});
    void envRun(ProgramData* program, EnvContext* ctx, const ExternalEnvRuleMap& map = {});
    EnvContext* envRun(ProgramData* program, const ExternalEnvRuleMap& = {});

    struct ExternalTypeRule {
        std::function<Ty(const Term&, TypeContext*, const std::unordered_map<TermType, ExternalTypeRule>&)> func;
    };
    typedef std::unordered_map<TermType, ExternalTypeRule> ExternalTypeMap;

    Ty getType(const Term& x, Context* ctx, const ExternalTypeMap& ext = {});
    bool isTypeEqual(const Ty& x, const Ty& y, TypeContext* ctx);
    Ty getType(const Term& x, TypeContext* ctx, const ExternalTypeMap& ext = {});

    struct ExternalUnfoldRule {
        std::function<Ty(const Ty&, TypeContext*, std::vector<std::string>&, const std::unordered_map<TyType, ExternalUnfoldRule>&)> func;
    };
    typedef std::unordered_map<TyType, ExternalUnfoldRule> ExternalUnfoldMap;
    Ty unfoldType(const Ty& x, TypeContext* ctx, const std::vector<std::string>& tmp_names);
    Ty unfoldTypeAll(const Ty& x, TypeContext* ctx, std::vector<std::string>& tmps, const ExternalUnfoldMap& ext = {});
    Ty unfoldBasicType(const Ty& x, TypeContext* ctx);
    std::vector<TypeContext::BindLog> bindPattern(const Pattern& pt, const Ty& type, TypeContext* ctx);

    Ty clearCompress(const Ty& type);

    IncreProgram eliminateNestedAlign(ProgramData* program);
    IncreProgram eliminateUnusedLet(ProgramData* program);

    int getTypeSize(TyData* type);
    int getTermSize(TermData* term);
    int getCommandSize(CommandData* command);
    int getProgramSize(ProgramData* program);

    IncreProgram removeGlobal(ProgramData* program);
}

#endif //ISTOOL_INCRE_H
