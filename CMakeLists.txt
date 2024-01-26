cmake_minimum_required(VERSION 3.13)
project(compiler)

# settings
# set to OFF to enable C mode
set(CPP_MODE ON)
if(CPP_MODE)
  set(FB_EXT ".cpp")
else()
  set(FB_EXT ".c")
endif()
message(STATUS "Flex/Bison generated source file extension: ${FB_EXT}")

# enable all warnings
if(MSVC)
  add_compile_options(/W3)
else()
  # disable warnings caused by old version of Flex
  add_compile_options(-Wall -Wno-register)
endif()

# options about libraries and includes
# set(LIB_DIR "$ENV{CDE_LIBRARY_PATH}/native" CACHE STRING "directory of libraries")
# set(INC_DIR "$ENV{CDE_INCLUDE_PATH}" CACHE STRING "directory of includes")
# message(STATUS "Library directory: ${LIB_DIR}")
# message(STATUS "Include directory: ${INC_DIR}")

# find Flex/Bison
find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

# generate lexer/parser
file(GLOB_RECURSE L_SOURCES "parser/*.l")
file(GLOB_RECURSE Y_SOURCES "parser/*.y")
if(NOT (L_SOURCES STREQUAL "" AND Y_SOURCES STREQUAL ""))
  string(REGEX REPLACE ".*/(.*)\\.l" "${CMAKE_CURRENT_BINARY_DIR}/\\1.lex${FB_EXT}" L_OUTPUTS "${L_SOURCES}")
  string(REGEX REPLACE ".*/(.*)\\.y" "${CMAKE_CURRENT_BINARY_DIR}/\\1.tab${FB_EXT}" Y_OUTPUTS "${Y_SOURCES}")

  flex_target(Lexer ${L_SOURCES} ${L_OUTPUTS} )
  bison_target(Parser ${Y_SOURCES} ${Y_OUTPUTS} COMPILE_FLAGS "-v -Wcounterexamples")
  add_flex_bison_dependency(Lexer Parser)
endif()

# project link directories
link_directories(${LIB_DIR})

# project include directories
include_directories(src)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${INC_DIR})

# all of C/C++ source files
file(GLOB_RECURSE C_SOURCES "src/*.c")
file(GLOB_RECURSE CXX_SOURCES "src/*.cpp")
file(GLOB_RECURSE CC_SOURCES "src/*.cc")
file(GLOB BASE_SRC "*.cpp")
set(SOURCES ${C_SOURCES} ${CXX_SOURCES} ${CC_SOURCES} ${BASE_SRC}
            ${FLEX_Lexer_OUTPUTS} ${BISON_Parser_OUTPUT_SOURCE})
message(STATUS "FOUND SRC FILES ${SOURCES}")

# LLVM
find_package(LLVM REQUIRED CONFIG)


message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
add_definitions("-fno-rtti")

llvm_map_components_to_libnames(llvm_libs support core irreader analysis)
# executable
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -D_GLIBCXX_DEBUG")

add_executable(compiler ${SOURCES})
set_target_properties(compiler PROPERTIES C_STANDARD 11 CXX_STANDARD 17)
target_link_libraries(compiler pthread)
target_link_libraries(compiler ${llvm_libs})