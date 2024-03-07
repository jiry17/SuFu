cmake_minimum_required(VERSION 3.5)
project(ISToolSyGuSTheory)

aux_source_directory(basic/clia BASIC_CLIA_SRC)
aux_source_directory(basic/string BASIC_STRING_SRC)
set(BASIC_SRC ${BASIC_CLIA_SRC} ${BASIC_STRING_SRC} basic/theory_semantics.cpp)

add_library(sygus_theory_lib STATIC ${BASIC_SRC}  theory.cpp)