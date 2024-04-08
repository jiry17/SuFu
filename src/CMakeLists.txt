cmake_minimum_required(VERSION 3.13)
project(ISTool)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/)
set(THREADS_PREFER_PTHREAD_FLAG ON)


SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c++17 -Ofast")

INCLUDE(FindPkgConfig)
include_directories(include)
find_package(Threads)

if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    add_definitions(-DLINUX)

    #Z3
    set(Z3_PATH Z3PATH)
    link_directories(${Z3_PATH}/build)
    set(Z3_FILE libz3.so)
    include_directories(${Z3_PATH}/src/api/c++)
    include_directories(${Z3_PATH}/src/api)

    #jsoncpp
    find_package(Jsoncpp)
    include_directories(${Jsoncpp_INCLUDE_DIR})

    #gurobi
    set(GUROBI_PATH GUROBIPATH)
    include_directories(${GUROBI_PATH}/include)
    link_directories(${GUROBI_PATH}/lib)
    set(GUROBI_FILE libgurobi_g++5.2.a libgurobi91.so)

    set(THIRDPARTY_LIBS glog gflags ${Jsoncpp_LIBRARY} ${Z3_FILE} ${GUROBI_FILE} ${PROFILER_FILE} ${CMAKE_THREAD_LIBS_INIT})
else()
    message(FATAL_ERROR "Unexceptional system ${CMAKE_SYSTEM_NAME}")
endif()

# subdic
add_subdirectory(basic)
add_subdirectory(ext)
add_subdirectory(sygus)
add_subdirectory(solver)
add_subdirectory(incre)
 
set(SRC_LIBS solver_lib  ${SyGuS_LIBS} ext_lib basic_lib incre_lib)

add_subdirectory(executor)
