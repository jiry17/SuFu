//
// Created by pro on 2022/10/8.
//

#ifndef ISTOOL_INCRE_PRINTER_H
#define ISTOOL_INCRE_PRINTER_H

#include "istool/incre/language/incre.h"

namespace incre {
    void printTy(const std::shared_ptr<TyData> &ty);
    void printPattern(const std::shared_ptr<PatternData> &pattern);
    void printTerm(const std::shared_ptr<TermData> &term);
    void printBinding(const std::shared_ptr<BindingData> &binding);
    void printCommand(const std::shared_ptr<CommandData> &command);
    void printProgram(const std::shared_ptr<ProgramData> &prog, const std::string &path = {}, bool is_align_mark = false);
}

#endif //ISTOOL_INCRE_PRINTER_H
