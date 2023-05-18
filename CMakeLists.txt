cmake_minimum_required(VERSION 3.20.0)
project(scriptlang-llvm)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)

if(CMAKE_BUILD_TYPE STREQUAL "")
    set(CMAKE_BUILD_TYPE Release)
endif()

add_subdirectory(third_party)
