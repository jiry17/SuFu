//
// Created by pro on 2022/1/3.
//

#ifndef ISTOOL_TOP_DOWN_MODEL_H
#define ISTOOL_TOP_DOWN_MODEL_H

#include "vsa.h"
#include "istool/basic/time_guard.h"

class TopDownContext {
public:
    virtual std::string toString() const = 0;
    virtual ~TopDownContext() = default;
};

typedef std::shared_ptr<TopDownContext> PTopDownContext;

enum class ProbModelType {
    NORMAL_PROB, NEG_LOG_PROB
};

class TopDownModel {
protected:
    virtual double getWeight(TopDownContext* ctx, Semantics* sem) const = 0;
    virtual std::vector<double> getWeightList(TopDownContext* ctx, const std::vector<Semantics*>& sem) const = 0;
public:
    PTopDownContext start;
    double default_weight;
    ProbModelType prob_type;
    TopDownModel(const PTopDownContext& start, double _inf, ProbModelType model_type=ProbModelType::NEG_LOG_PROB);
    virtual PTopDownContext move(TopDownContext* ctx, Semantics* sem, int pos) const = 0;
    double getWeight(TopDownContext* ctx, Semantics* sem, ProbModelType oup_type) const;
    std::vector<double> getWeightList(TopDownContext* ctx, const std::vector<Semantics*>& sem, ProbModelType) const;
    double getWeight(Program* program, ProbModelType oup_type) const;
    virtual ~TopDownModel() = default;
};

typedef std::function<std::string(Semantics*)> SemanticsAbstracter;

class NGramModel: public TopDownModel {
protected:
    virtual double getWeight(TopDownContext* ctx, Semantics* sem) const;
    virtual std::vector<double> getWeightList(TopDownContext* ctx, const std::vector<Semantics*>& sem) const;
public:
    unsigned int depth;
    SemanticsAbstracter sa;
    std::unordered_map<std::string, double> weight_map;
    NGramModel(unsigned int _depth, const SemanticsAbstracter &_sa, double _default_weight = 1e90);
    void set(const std::vector<std::pair<std::string, double>>& weight_list);
    virtual PTopDownContext move(TopDownContext* ctx, Semantics* sem, int pos) const;
    virtual ~NGramModel() = default;
};

namespace ext {
    namespace vsa {
        extern const SemanticsAbstracter KDefaultAbstracter;
        NGramModel *getSizeModel();
        void learn(NGramModel *model, const ProgramList &program_list);
        PProgram getBestProgram(VSANode* root, TopDownModel* model, TimeGuard* guard = nullptr);
        NGramModel* loadDefaultNGramModel(const std::string& model_file_path);
        void saveNGramModel(NGramModel* model, const std::string& model_file_path);
        double changeProbModel(double prob, ProbModelType source_type, ProbModelType target_type);
        double getTrueProb(double prob, ProbModelType type);
        double getRepresentedProb(double prob, ProbModelType type);
        double addProb(double prob_x, double prob_y, ProbModelType type);
        void learnNFoldModel(Env* env, const std::string& cache_path, const std::string& save_folder, int fold_num, const std::string& main_name="default");
        NGramModel* loadNFoldModel(const std::string& model_folder, const std::string& task_name);
    }
}

#endif //ISTOOL_TOP_DOWN_MODEL_H
