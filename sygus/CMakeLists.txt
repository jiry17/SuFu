cmake_minimum_required(VERSION 3.5)
project(ISToolSyGuS)

add_subdirectory(theory)
aux_source_directory(parser sygus_parser_src)
add_library(sygus_parser_lib ${sygus_parser_src} sygus.cpp)
set(SyGuS_LIBS sygus_parser_lib sygus_theory_lib  PARENT_SCOPE)