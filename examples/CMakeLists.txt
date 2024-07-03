
add_executable(turner-print turner-print.cpp)
set_property(TARGET turner-print PROPERTY CXX_STANDARD 20)
set_property(TARGET turner-print PROPERTY CXX_STANDARD_REQUIRED ON)
target_link_libraries(turner-print PRIVATE turner)
