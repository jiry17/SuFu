cmake_minimum_required(VERSION 3.5)
project(ISToolSolver)

aux_source_directory(component CB_SRC)
aux_source_directory(enum ENUM_SRC)
aux_source_directory(stun STUN_SRC)
aux_source_directory(polygen POLY_SRC)
add_subdirectory(autolifter)
#add_subdirectory(recursive)
add_library(solver_lib STATIC solver.cpp iterative_solver.cpp ${CB_SRC} ${ENUM_SRC} ${VSA_SRC} ${STUN_SRC} ${POLY_SRC}
        ${TMP_SRC} ${MAX_SRC} ${AL_SRC} ${EXT_SOLVER_SRC})