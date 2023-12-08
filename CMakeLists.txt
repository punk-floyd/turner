cmake_minimum_required(VERSION 3.18)

project(turner
    VERSION         "0.1"
    DESCRIPTION     "C++20 JSON library"
    HOMEPAGE_URL    "https://https://github.com/punk-floyd/json-turner"
    LANGUAGES       CXX
)

# Are we the main CMake project? (vs included via add_subdirectory)
if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    message("${PROJECT_NAME} version: ${PROJECT_VERSION}")
    set_property(GLOBAL PROPERTY USE_FOLDERS ON)
    set(TURNER_IS_MASTER_PROJECT ON)
endif()

add_library(turner INTERFACE)
target_include_directories(turner INTERFACE inc)
set_target_properties(turner PROPERTIES CXX_STANDARD 20)

option(TURNER_TESTS     "Generate tests"        ${TURNER_IS_MASTER_PROJECT})
option(TURNER_UTILS     "Generate utilities"    ${TURNER_IS_MASTER_PROJECT})
#ption(TURNER_MODULE    "Build as a module"     OFF)

if (TURNER_TESTS)
    enable_testing()
    add_subdirectory(test)
endif()

if (TURNER_UTILS)
    add_subdirectory(utils)
endif()
