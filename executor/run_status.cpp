//
// Created by pro on 2022/9/18.
//


#include "istool/basic/config.h"
#include "istool/incre/io/incre_from_json.h"
#include "istool/incre/io/incre_printer.h"
#include "istool/incre/autolabel/incre_autolabel_constraint_solver.h"
#include "istool/incre/analysis/incre_instru_info.h"
#include "istool/incre/autolifter/incre_autolifter_solver.h"
#include "istool/incre/grammar/incre_component_collector.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include <iostream>
#include "glog/logging.h"

using namespace incre;

typedef std::function<std::pair<int, bool>(TermData*)> TermMatcher;
typedef std::function<std::pair<int, bool> (TyData*)> TypeMatcher;

int countType(TyData* type, const TypeMatcher& matcher) {
    auto [res, tag] = matcher(type);
    if (tag) return res;
    switch (type->getType()) {
        case TyType::INT:
        case TyType::BOOL:
        case TyType::UNIT:
        case TyType::VAR:
            return res;
        case TyType::ARROW: {
            auto* ta = dynamic_cast<TyArrow*>(type);
            return res + countType(ta->source.get(), matcher) + countType(ta->target.get(), matcher);
        }
        case TyType::COMPRESS: {
            auto* tc = dynamic_cast<TyCompress*>(type);
            return res + countType(tc->content.get(), matcher);
        }
        case TyType::IND: {
            auto* ti = dynamic_cast<TyInductive*>(type);
            for (auto& [_, sub_type]: ti->constructors) {
                res += countType(sub_type.get(), matcher);
            }
            return res;
        }
        case TyType::TUPLE: {
            auto* tt = dynamic_cast<TyTuple*>(type);
            for (auto& sub_type: tt->fields) {
                res += countType(sub_type.get(), matcher);
            }
            return res;
        }
    }
}

int countTerm(TermData* term, const TypeMatcher& type_matcher, const TermMatcher& term_matcher) {
    auto sub_terms = incre::getSubTerms(term);
    auto [res, tag] = term_matcher(term);
    if (tag) return res;
    for (auto& sub_term: sub_terms) {
        res += countTerm(sub_term.get(), type_matcher, term_matcher);
    }
    auto* ta = dynamic_cast<TmAbs*>(term);
    if (ta) res += countType(ta->type.get(), type_matcher);
    return res;
}

int countProgram(ProgramData* program, const TypeMatcher& type_matcher, const TermMatcher& term_matcher) {
    auto res = 0;
    for (auto& command: program->commands) {
        switch (command->getType()) {
            case CommandType::IMPORT: break;
            case CommandType::DEF_IND: {
                auto* cd = dynamic_cast<CommandDefInductive*>(command.get());
                res += countType(cd->type, type_matcher);
                break;
            }
            case CommandType::BIND: {
                auto* cb = dynamic_cast<CommandBind*>(command.get());
                {
                    auto* bt = dynamic_cast<TypeBinding*>(cb->binding.get());
                    if (bt) res += countType(bt->type.get(), type_matcher);
                }
                {
                    auto* bt = dynamic_cast<TermBinding*>(cb->binding.get());
                    if (bt) res += countTerm(bt->term.get(), type_matcher, term_matcher);
                }
                break;
            }
        }
    }
    return res;
}

int main(int argv, char** argc) {
    std::string path;
    if (argv <= 1) {
        std::string name = "dp/15-4";
        path = config::KSourcePath + "incre-tests/" + name + ".f";
    } else {
        path = std::string(argc[1]);
    }

    auto env = std::make_shared<Env>();
    incre::prepareEnv(env.get());

    auto init_program = incre::parseFromF(path, true);

    auto false_type_matcher = [](TyData* type) -> std::pair<int, bool> {return {0, false};};
    auto false_term_matcher = [](TermData* term) -> std::pair<int, bool> {return {0, false};};
    auto compress_type_matcher = [](TyData* type) -> std::pair<int, bool> {
        if (type->getType() == TyType::COMPRESS) return {1, false};
        return {0, false};
    };
    auto label_term_matcher = [](TermData* term) -> std::pair<int, bool> {
        auto type = term->getType();
        auto w = type == TermType::LABEL || type == TermType::UNLABEL || type == TermType::ALIGN;
        if (w) return {1, false}; return {0, false};
    };
    auto align_size_matcher = [](TermData* term) -> std::pair<int, bool> {
        if (term->getType() == TermType::ALIGN) {
            return {incre::getTermSize(term) - 1, true};
        }
        return {0, false};
    };

    global::recorder.start("label");
    auto* label_solver = new autolabel::AutoLabelZ3Solver(init_program);
    auto res = label_solver->label();
    global::recorder.end("label");

    res = incre::eliminateNestedAlign(res.get());
    std::cout << "size: " << incre::getProgramSize(init_program.get()) << std::endl;
    std::cout << "compress-num: " << countProgram(init_program.get(), compress_type_matcher, false_term_matcher) << std::endl;
    std::cout << "label-num: " << countProgram(res.get(), false_type_matcher, label_term_matcher) << std::endl;
    std::cout << "rewrite-size: " << countProgram(res.get(), false_type_matcher, align_size_matcher) << std::endl;
    std::cout << global::recorder.query("label");
}