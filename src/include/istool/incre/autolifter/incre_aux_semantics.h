//
// Created by pro on 2022/11/16.
//

#ifndef ISTOOL_INCRE_SEMANTICS_H
#define ISTOOL_INCRE_SEMANTICS_H

#include "istool/basic/semantics.h"
#include "istool/incre/trans/incre_trans.h"
#include "istool/ext/deepcoder/data_type.h"

namespace incre::autolifter {

    namespace global {
        class IncreFold: public NormalSemantics {
            Data _run(TyData* current_type, const Data& data, const std::vector<PSemantics>& semantics_list, ExecuteInfo* info);
        public:
            TyInductive* type;
            std::unordered_map<std::string, int> cons_map;
            IncreFold(const PType& ind_type, const PType& oup_type);
            virtual Data run(DataList&& inp_list, ExecuteInfo* info);
            virtual ~IncreFold() = default;
        };
    }

    namespace list {
        bool isList(Type* type);

#define DefineIncreNormalSemantics(name) \
class Incre ## name ## Semantics: public NormalSemantics { \
public: \
    PType _type; \
    TyInductive* type; \
    Incre ## name ## Semantics(const PType& __type); \
    virtual Data run(DataList &&inp_list, ExecuteInfo* info); \
    ~Incre ## name ## Semantics() = default;\
};
        DefineIncreNormalSemantics(ListSum)
        DefineIncreNormalSemantics(ListLen)
        DefineIncreNormalSemantics(ListMax)
        DefineIncreNormalSemantics(ListMin)
        DefineIncreNormalSemantics(ListHead)
        DefineIncreNormalSemantics(ListLast)
        DefineIncreNormalSemantics(ListAccess)
        DefineIncreNormalSemantics(ListCount)
        DefineIncreNormalSemantics(ListMap)
        DefineIncreNormalSemantics(ListFilter)
        DefineIncreNormalSemantics(ListScanl)
        DefineIncreNormalSemantics(ListScanr)
        DefineIncreNormalSemantics(ListRev)
        DefineIncreNormalSemantics(ListSort)

        std::vector<std::pair<PType, Data>> getConstList(Env* env);
        std::vector<PSemantics> getSemanticsList(const PType& base_type);
    }
}

#endif //ISTOOL_INCRE_SEMANTICS_H
