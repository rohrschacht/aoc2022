cmake_minimum_required(VERSION 3.22)
project(part1)

set(CMAKE_CXX_STANDARD 20)

add_executable(part1 main.cpp)
add_custom_command(
        TARGET part1 POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_SOURCE_DIR}/input.txt
        ${CMAKE_CURRENT_BINARY_DIR}/input.txt)
