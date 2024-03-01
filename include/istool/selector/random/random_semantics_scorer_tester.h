//
// Created by pro on 2022/4/30.
//

#ifndef ISTOOL_RANDOM_SEMANTICS_SCORER_TESTER_H
#define ISTOOL_RANDOM_SEMANTICS_SCORER_TESTER_H

#include "istool/selector/random/grammar_flatter.h"
#include "istool/selector/random/random_semantics_scorer.h"
#include "istool/selector/random/learned_scorer.h"

namespace selector::test {
    RandomSemanticsScore getLearnedGroundTruth(Env* env, FlattenGrammar* graph, const PProgram& p,
            const std::vector<RandomSemanticsModel*>& model_list, LearnedScorerType type);
}

#endif //ISTOOL_RANDOM_SEMANTICS_SCORER_TESTER_H
