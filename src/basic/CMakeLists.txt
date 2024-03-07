cmake_minimum_required(VERSION 3.5)
project(ISToolBasic)

file(GLOB basic_src  "*.cpp" "*.h")
add_library(basic_lib STATIC ${basic_src})