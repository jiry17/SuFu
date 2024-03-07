cmake_minimum_required(VERSION 3.5)
project(ISToolAutoLifter)

file(GLOB SRC "*.cpp" "basic/*.cpp")
set(AL_SRC ${SRC} PARENT_SCOPE)