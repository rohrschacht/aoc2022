cmake_minimum_required(VERSION 3.22)
project(part2 C)

set(CMAKE_C_STANDARD 17)

add_executable(part2 main.c)

add_custom_command(
        TARGET part2 POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_SOURCE_DIR}/input.txt
        ${CMAKE_CURRENT_BINARY_DIR}/input.txt)
