//
// Created by pro on 2023/4/5.
//

#ifndef ISTOOL_INCRE_COMPONENT_COLLECTOR_H
#define ISTOOL_INCRE_COMPONENT_COLLECTOR_H

#include "istool/basic/grammar.h"
#include "istool/incre/language/incre.h"

namespace incre::grammar {
    typedef std::vector<PSemantics> SymbolContext;


    class SymbolInfo {
    public:
        SymbolContext context;
        PType type;
        NonTerminal* symbol;
        SymbolInfo(const SymbolContext& _context, const PType& _type, NonTerminal* _symbol);
        ~SymbolInfo() = default;
    };

    class GrammarBuilder {
    public:
        std::unordered_map<std::string, int> info_map;
        std::vector<SymbolInfo> info_list;
        std::vector<SymbolContext> contexts;
        void insertContext(const SymbolContext& context);
        void insertInfo(const SymbolContext& context, const PType& type);
        void insertTypeForAllContext(const PType& type);
        std::vector<SymbolInfo> getSymbols(const std::function<bool(const SymbolInfo&)>& filter) const;
        std::vector<SymbolInfo> getSymbols(const PType& type) const;
        NonTerminal* getSymbol(const SymbolContext& context, const PType& type) const;
        GrammarBuilder(const SymbolContext& init_context);
    };

    class SynthesisComponent {
    public:
        int command_id;
        std::string name;
        SynthesisComponent(int command_id, const std::string& _name);
        virtual void extendContext(GrammarBuilder& builder) = 0;
        virtual void insertComponent(const GrammarBuilder& builder) = 0;
        virtual void extendNTMap(GrammarBuilder& builder) = 0;
        virtual Term tryBuildTerm(const PSemantics& sem, const TermList& term_list) = 0;
        virtual ~SynthesisComponent() = default;
    };
    typedef std::shared_ptr<SynthesisComponent> PSynthesisComponent;
    typedef std::vector<PSynthesisComponent> SynthesisComponentList;

    class ContextFreeSynthesisComponent: public SynthesisComponent {
    public:
        ContextFreeSynthesisComponent(int command_id, const std::string& _name);
        virtual void extendContext(GrammarBuilder& builder);
        virtual ~ContextFreeSynthesisComponent() = default;
    };

    class IncreComponent: public ContextFreeSynthesisComponent {
    public:
        TypeList param_types;
        PType res_type;
        Data data;
        Term term;
        bool is_partial, is_parallel;
        IncreComponent(const std::string& _name, const PType& _type, const Data& _data, const Term& _term, int command_id, bool _is_partial, bool _is_parallel);
        virtual void insertComponent(const GrammarBuilder& symbol_map);
        virtual void extendNTMap(GrammarBuilder& symbol_map);
        virtual Term tryBuildTerm(const PSemantics& sem, const TermList& term_list);
        ~IncreComponent() = default;
    };

    class ConstComponent: public ContextFreeSynthesisComponent {
    public:
        PType type;
        DataList const_list;
        std::function<bool(Value*)> is_inside;
        ConstComponent(const PType& _type, const DataList& _const_list, const std::function<bool(Value*)>& _is_inside);
        virtual void insertComponent(const GrammarBuilder& symbol_map);
        virtual void extendNTMap(GrammarBuilder& symbol_map);
        virtual Term tryBuildTerm(const PSemantics& sem, const TermList& term_list);
        ~ConstComponent() = default;
    };

    class BasicOperatorComponent: public ContextFreeSynthesisComponent {
    public:
        TypedSemantics* sem;
        PSemantics _sem;
        BasicOperatorComponent(const std::string& _name, const PSemantics& __semantics);
        virtual Term tryBuildTerm(const PSemantics& sem, const TermList& term_list);
        virtual void insertComponent(const GrammarBuilder& symbol_map);
        virtual void extendNTMap(GrammarBuilder& symbol_map);
        ~BasicOperatorComponent() = default;
    };

#define LanguageComponent(name) \
    class name ## Component: public ContextFreeSynthesisComponent { \
    public: \
        name ## Component(); \
        virtual void insertComponent(const GrammarBuilder& builder); \
        virtual void extendNTMap(GrammarBuilder& builder); \
        virtual Term tryBuildTerm(const PSemantics& sem, const TermList& term_list); \
        ~name ## Component() = default;\
    }

    LanguageComponent(Ite);
    LanguageComponent(Tuple);
    LanguageComponent(Proj);

    class ApplyComponent: public ContextFreeSynthesisComponent {
    public:
        bool is_only_full;
        Context* ctx;
        ApplyComponent(Context* ctx, bool _is_only_full);
        virtual Term tryBuildTerm(const PSemantics& sem, const TermList& term_list);
        virtual void extendNTMap(GrammarBuilder &builder);
        virtual void insertComponent(const GrammarBuilder &builder);
        ~ApplyComponent() = default;
    };

    enum class GrammarType {
        ALIGN, COMPRESS, COMB
    };

    struct TypeLabeledDirectSemantics: public NormalSemantics {
    public:
        PType type;
        TypeLabeledDirectSemantics(const PType& _type);
        virtual Data run(DataList&& inp_list, ExecuteInfo* info);
        virtual ~TypeLabeledDirectSemantics() = default;
    };

    class ComponentPool {
    public:
        SynthesisComponentList align_list, compress_list, comb_list;
        ComponentPool(const SynthesisComponentList& _align_list, const SynthesisComponentList& _compress_list, const SynthesisComponentList& _comb_list);
        ComponentPool();
        void print() const;

        Grammar* buildAlignGrammar(const TypeList& inp_list, bool is_only_prime = true);
        Grammar* buildCompressGrammar(const TypeList& inp_list, int command_id);
        Grammar* buildCombinatorGrammar(const TypeList& inp_list, const PType& oup_type, int command_id);

        void merge(const ComponentPool& pool);
        ~ComponentPool() = default;
    };

    enum ComponentCollectorType {
        SOURCE = 0, LABEL = 1
    };

    namespace collector {
        ComponentPool collectComponentFromSource(EnvContext* env_ctx, TypeContext* ctx, ProgramData* program);
        ComponentPool collectComponentFromLabel(EnvContext* env_ctx, TypeContext* ctx, ProgramData* program);
        ComponentPool getBasicComponentPool(Env* env);
        ComponentPool collectExtraOperators(EnvContext* env_ctx, TypeContext* ctx, const std::string& extra_name);
        void loadExtraOperator(EnvContext* env_ctx, TypeContext* ctx, Env* env, const std::string& extra_name);
        CommandList extractExtraComponentInResult(const std::string& extra_name);
        extern const std::string KCollectMethodName;
    }
    namespace builder {
        Grammar *buildGrammar(const TypeList &inp_list, const SynthesisComponentList &component_list, const PType& oup);
    }
    ComponentPool collectComponent(EnvContext* env_ctx, TypeContext* ctx, Env* env, ProgramData* program);
}

#endif //ISTOOL_INCRE_COMPONENT_COLLECTOR_H
