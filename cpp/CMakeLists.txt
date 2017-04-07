#############
## Library ##
#############

cmake_minimum_required  (VERSION 3.5.1)
project                 (problems)
set                     (CMAKE_CXX_STANDARD 14)
set                     (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pedantic -Wextra")
include_directories     (include)
file                    (GLOB SOURCES "src/*.cxx")
add_library             (problems SHARED ${SOURCES})
# Usage:
# /build $ cmake ..
# make


#############
## Testing ##
#############

set                     (GTEST_SOURCE_DIR external_lib/googletest/googletest)
add_subdirectory        (${GTEST_SOURCE_DIR})
set                     (JSON_SOURCE_DIR external_lib/json)
include_directories     (${JSON_SOURCE_DIR})

enable_testing          ()
include_directories     (${GTEST_SOURCE_DIR}/include ${GTEST_SOURCE_DIR})

################
## Unit Tests ##
################
add_executable          (runTests test/testproblems.cxx)

# Standard linking to gtest stuff.
target_link_libraries   (runTests gtest gtest_main)
# Extra linking for the project.
target_link_libraries   (runTests problems)
# Usage after make:
# /build $ ./runTests