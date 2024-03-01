cmake_minimum_required(VERSION 3.5)
project(ISToolIncreLanguageAutoLabel)

file(GLOB SRC "*.cpp" "solver/*.cpp") # "greedy/*.cpp" )

set(AUTOLABEL_SRC ${SRC} PARENT_SCOPE)