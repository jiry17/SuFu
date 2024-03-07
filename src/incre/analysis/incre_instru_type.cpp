//
// Created by pro on 2022/9/21.
//

#include "istool/incre/analysis/incre_instru_type.h"
#include "glog/logging.h"

using namespace incre;

VLabeledCompress::VLabeledCompress(const Data &_v, int _id):
    VCompress(_v), id(_id) {
}

std::string VLabeledCompress::toString() const {
    return "compress[" + std::to_string(id) + "] " + content.toString();
}

TyLabeledCompress::TyLabeledCompress(const Ty &_ty, int _id):
    TyCompress(_ty), id(_id) {
}
std::string TyLabeledCompress::toString() const {
    return "compress[" + std::to_string(id) + "] " + content->toString();
}

TmLabeledLabel::TmLabeledLabel(const Term &_content, int _id): TmLabel(_content), id(_id) {
}
std::string TmLabeledLabel::toString() const {
    return "create@" + std::to_string(id) + " " + content->toString();
}

TmLabeledAlign::TmLabeledAlign(const Term &_content, int _id, const std::unordered_map<std::string, Data>& info):
    TmAlign(_content), id(_id), subst_info(info) {
}
void TmLabeledAlign::addSubst(const std::string &name, const Data &v) {
    subst_info[name] = v;
}
std::string TmLabeledAlign::toString() const {
    return "align@" + std::to_string(id) + " " + content->toString();
}
