cmake_minimum_required(VERSION 3.10)
project(cpprosalind)
set(CMAKE_CXX_STANDARD 17)

add_library(Problems
    dna.cpp
    rna.cpp
    revc.cpp
    fib.cpp
    gc.cpp
    hamm.cpp
    iprb.cpp
    prot.cpp
    subs.cpp
    cons.cpp
    common.cpp)
include_directories ("${PROJECT_SOURCE_DIR}")

add_executable(${PROJECT_NAME} "main.cpp")
target_link_libraries (${PROJECT_NAME} Problems)

