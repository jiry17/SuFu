cmake_minimum_required(VERSION 3.5)
project(ISToolExecutor)
cmake_policy(SET CMP0079 NEW)
aux_source_directory(invoker INVOKER_SRC)
add_library(invoker_lib STATIC ${INVOKER_SRC} invoker/invoker.cpp)
set(ALL_LIBS ${SRC_LIBS} invoker_lib)

add_executable(run_incre_label run_incre_label.cpp)
target_link_libraries(run_incre_label ${ALL_LIBS} ${ALL_LIBS} ${THIRDPARTY_LIBS})

add_executable(run_incre_grisette run_incre_grisette.cpp)
target_link_libraries(run_incre_grisette ${ALL_LIBS} ${ALL_LIBS} ${THIRDPARTY_LIBS})