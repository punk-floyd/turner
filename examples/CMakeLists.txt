
if (MSVC)
    set(TURNER_UTILS_WARNING_OPTS /permissive- /W4)
endif()

if (UNIX)
    set(TURNER_UTILS_WARNING_OPTS
        -Wall -Wextra -Wshadow -Wold-style-cast -Wunused -Wpedantic
        -Wconversion -Wsign-conversion -Wnon-virtual-dtor
    )
endif()

add_executable(turner-print turner-print.cpp)
set_property(TARGET turner-print PROPERTY CXX_STANDARD 20)
set_property(TARGET turner-print PROPERTY CXX_STANDARD_REQUIRED ON)
target_link_libraries(turner-print PRIVATE turner)
target_compile_options(turner-print PRIVATE ${TURNER_UTILS_WARNING_OPTS})
