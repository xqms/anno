
cmake_minimum_required(VERSION 3.11)
project(anno)

set(CMAKE_CXX_STANDARD 20)

include(cmake/CPM.cmake)

# Dependency: reflect
CPMAddPackage("gh:qlibs/reflect@1.2.4")

add_library(reflect INTERFACE)
target_include_directories(reflect SYSTEM INTERFACE ${reflect_SOURCE_DIR})
add_library(reflect::reflect ALIAS reflect)


add_library(anno INTERFACE)
target_include_directories(anno INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(anno INTERFACE
    reflect::reflect
)
add_library(anno::anno ALIAS anno)


add_subdirectory(examples)
