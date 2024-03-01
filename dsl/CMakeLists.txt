cmake_minimum_required(VERSION 3.5)
project(ISToolDSLs)

aux_source_directory(samplesy SY_DSL_SOURCE)
aux_source_directory(component COMP_DSL_SOURCE)
aux_source_directory(clia CLIA_DSL_SOURCE)
aux_source_directory(autolifter AUTOLIFTER_DSL_SOURCE)
aux_source_directory(deepcoder DEEPCODER_DSL_SOURCE)
add_library(dsl_lib STATIC ${SY_DSL_SOURCE} ${COMP_DSL_SOURCE} ${CLIA_DSL_SOURCE} ${DEEPCODER_DSL_SOURCE} ${AUTOLIFTER_DSL_SOURCE})

