cmake_minimum_required(VERSION 3.18)

project(turner
    VERSION         "1.0"
    DESCRIPTION     "C++20 JSON library"
    HOMEPAGE_URL    "https://github.com/punk-floyd/json-turner"
    LANGUAGES       CXX
)

# Are we the main CMake project? (vs included via add_subdirectory)
if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    message("${PROJECT_NAME} version: ${PROJECT_VERSION}")
    set_property(GLOBAL PROPERTY USE_FOLDERS ON)
    set(TURNER_IS_MASTER_PROJECT ON)
    set(CMAKE_CXX_FLAGS_RELEASE -O3)
    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
    set(CMAKE_CXX_CLANG_TIDY clang-tidy)
endif()

add_library(turner INTERFACE)
target_include_directories(turner INTERFACE inc)
set_target_properties(turner PROPERTIES CXX_STANDARD 20)

option(TURNER_TESTS     "Generate tests"        ${TURNER_IS_MASTER_PROJECT})
option(TURNER_EXAMPLES  "Generate examples"     ${TURNER_IS_MASTER_PROJECT})

if (TURNER_TESTS)
    enable_testing()
    add_subdirectory(test)
endif()

if (TURNER_EXAMPLES)
    add_subdirectory(examples)
endif()
