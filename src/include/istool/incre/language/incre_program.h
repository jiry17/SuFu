//
// Created by pro on 2022/9/18.
//

#ifndef ISTOOL_INCRE_PROGRAM_H
#define ISTOOL_INCRE_PROGRAM_H

#include "istool/basic/env.h"
#include "incre_term.h"
#include "incre_type.h"
#include "incre_context.h"
#include <unordered_set>

namespace incre {
    enum class CommandType {
        IMPORT, BIND, DEF_IND
    };

    enum class CommandDecorate {
        INPUT, START, SYN_COMPRESS, SYN_COMBINE, SYN_ALIGN, SYN_NO_PARTIAL, TERM_NUM
    };

    enum class IncreConfig {
        COMPOSE_NUM, /*Max components in align, default 3*/
        VERIFY_BASE, /*Base number of examples in verification, default 1000*/
        SAMPLE_SIZE, /*Max size of random data structures, default 10*/
        SAMPLE_INT_MAX, /*Int Max of Sample, Default 5*/
        SAMPLE_INT_MIN, /*Int Min of Sample, Default -5*/
        NON_LINEAR, /*Whether consider * in synthesis, default false*/
        EXTRA_GRAMMAR, /*Extra grammar considered in synthesis, default Fold*/
        ENABLE_FOLD, /*Whether consider `fold` operator on data structures in synthesis, default false*/
        TERM_NUM, /* Number of terms considered by PolyGen*/
        CLAUSE_NUM, /* Number of terms considered by PolyGen*/
        PRINT_ALIGN /*Whether print align results to the result*/
    };

    typedef std::unordered_set<CommandDecorate> DecorateSet;

    class CommandData {
        CommandType type;
    public:
        DecorateSet decorate_set;
        CommandData(CommandType _type, const DecorateSet& _set);
        CommandType getType() const;
        bool isDecoratedWith(CommandDecorate deco) const;
        std::string toString();
        virtual std::string contentToString() const = 0;
        virtual ~CommandData() = default;
    };

    typedef std::shared_ptr<CommandData> Command;
    typedef std::vector<Command> CommandList;

    class CommandImport: public CommandData {
    public:
        std::string name;
        CommandList commands;
        CommandImport(const std::string& _name, const CommandList& _commands);
        virtual std::string contentToString() const;
        virtual ~CommandImport() = default;
    };

    class CommandBind: public CommandData {
    public:
        std::string name;
        Binding binding;
        CommandBind(const std::string& _name, const Binding& _binding, const DecorateSet& decorate_set);
        virtual std::string contentToString() const;
        virtual ~CommandBind() = default;
    };

    class CommandDefInductive: public CommandData {
    public:
        TyInductive* type;
        Ty _type;
        CommandDefInductive(const Ty& __type);
        virtual std::string contentToString() const;
        virtual ~CommandDefInductive() = default;
    };

    typedef std::unordered_map<IncreConfig, Data> IncreConfigMap;

    class ProgramData {
    public:
        IncreConfigMap config_map;
        CommandList commands;
        ProgramData(const CommandList& _commands, const IncreConfigMap& config_map);
        void print() const;
        virtual ~ProgramData() = default;
    };
    typedef std::shared_ptr<ProgramData> IncreProgram;

    CommandDecorate string2Decorate(const std::string& s);
    std::string decorate2String(CommandDecorate deco);
    IncreConfig string2ConfigType(const std::string& s);
    void applyConfig(IncreConfig config, const Data& config_value, Env* env);
    void applyConfig(ProgramData* program, Env* env);

    namespace config_name {
        extern const std::string KDataSizeLimitName;
        extern const std::string KIsNonLinearName;
        extern const std::string KExtraGrammarName;
        extern const std::string KIsEnableFoldName;
        extern const std::string KSampleIntMinName;
        extern const std::string KSampleIntMaxName;
        extern const std::string KPrintAlignName;
    }
}

#endif //ISTOOL_INCRE_PROGRAM_H
