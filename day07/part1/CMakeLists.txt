cmake_minimum_required(VERSION 3.22)
project(part1 C)

set(CMAKE_C_STANDARD 17)

add_executable(part1 main.c)

add_custom_command(
        TARGET part1 POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_SOURCE_DIR}/input.txt
        ${CMAKE_CURRENT_BINARY_DIR}/input.txt)
