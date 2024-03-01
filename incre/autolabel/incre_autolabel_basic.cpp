//
// Created by pro on 2023/1/22.
//

#include "istool/incre/autolabel/incre_autolabel.h"
#include "glog/logging.h"

using namespace incre;
using namespace incre::autolabel;

AutoLabelSolver::AutoLabelSolver(const IncreProgram &_init_program): init_program(_init_program) {
}