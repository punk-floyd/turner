cmake_minimum_required(VERSION 3.18)

# Pull project/library version from main header (lib_version_* values)
file (READ "inc/turner/json.h" VER_SRC)
string(REGEX MATCH "lib_version_major[ \t]*=[ \t]*([0-9]*)" _${VER_SRC})
set(VER_MAJ ${CMAKE_MATCH_1})
string(REGEX MATCH "lib_version_minor[ \t]*=[ \t]*([0-9]*)" _${VER_SRC})
set(VER_MIN ${CMAKE_MATCH_1})
string(REGEX MATCH "lib_version_sub_minor[ \t]*=[ \t]*([0-9]*)" _${VER_SRC})
set(VER_SUB ${CMAKE_MATCH_1})
string(REGEX MATCH "lib_version_package[ \t]*=[ \t]*([0-9]*)" _${VER_SRC})
set(VER_PKG ${CMAKE_MATCH_1})

project(turner
    VERSION         "${VER_MAJ}.${VER_MIN}.${VER_SUB}.${VER_PKG}"
    DESCRIPTION     "C++20 JSON library"
    HOMEPAGE_URL    "https://github.com/punk-floyd/json-turner"
    LANGUAGES       CXX
)

# Are we the main CMake project? (vs included via add_subdirectory)
if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    message("${PROJECT_NAME} version: ${PROJECT_VERSION}")
    set_property(GLOBAL PROPERTY USE_FOLDERS ON)
    set(TURNER_IS_MASTER_PROJECT ON)
    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

    find_program(CLANG_TIDY_EXE NAMES clang-tidy)
    if (FALSE AND CLANG_TIDY_EXE)
        message("Found clang-tidy: ${CLANG_TIDY_EXE}")
        set(CMAKE_CXX_CLANG_TIDY clang-tidy)
    endif()

    # Set default compiler settings for examples and test code
    if (CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
        add_compile_options(-Wall -Wextra -Wshadow  -Wunused -Wpedantic)
        add_compile_options(-Wconversion -Wsign-conversion)
        add_compile_options(-Wnon-virtual-dtor -Wold-style-cast)

        if (TRUE AND CMAKE_BUILD_TYPE STREQUAL "Debug")
          add_compile_options(-fsanitize=undefined,address -fno-omit-frame-pointer)
          add_link_options   (-fsanitize=undefined,address)
        endif()
    elseif (MSVC)
        add_compile_options(/permissive- /W4)
        add_compile_definitions(NOMINMAX)
    endif()

    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
        # Ignore "note: parameter passing for argument of type [...] changed in
        # GCC X.Y" warnings.
        add_compile_options(-Wno-psabi)
    endif()
endif()

add_library(turner INTERFACE)
target_include_directories(turner INTERFACE inc)
set_target_properties(turner PROPERTIES CXX_STANDARD 20)
set_target_properties(turner PROPERTIES CXX_STANDARD_REQUIRED ON)

option(TURNER_TESTS     "Generate tests"        ${TURNER_IS_MASTER_PROJECT})
option(TURNER_EXAMPLES  "Generate examples"     ${TURNER_IS_MASTER_PROJECT})

if (TURNER_TESTS)
    find_package(Catch2 3)
    if (Catch2_FOUND)
        enable_testing()
        add_subdirectory(test)
    else()
        message("Catch2 NOT found; testing will be disabled")
    endif()
endif()

if (TURNER_EXAMPLES)
    add_subdirectory(examples)
endif()
