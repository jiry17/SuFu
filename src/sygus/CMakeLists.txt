cmake_minimum_required(VERSION 3.5)
project(ISToolSyGuS)

add_subdirectory(theory)
set(SyGuS_LIBS sygus_theory_lib  PARENT_SCOPE)