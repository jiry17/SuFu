//
// Created by pro on 2022/2/23.
//

#include "istool/dsl/autolifter/autolifter_dataset.h"
#include "istool/invoker/lifting_invoker.h"

int main(int argv, char** argc) {
    //auto* task = dsl::autolifter::getLiftingTask("lazy-tag", "3rd-min@neg");
    auto* task = dsl::autolifter::getLiftingTask("dad", "mss");
    //auto* task = dsl::autolifter::getLiftingTask("lsp", "page22-t2");
    auto res = invoker::single::invokeAutoLifter(task, nullptr, {});
    res.styledPrint();
    //std::cout << res.toString() << std::endl;
    return 0;
}