
find_package(Catch2 3 REQUIRED)

add_executable(tests test.cpp)
set_target_properties(tests PROPERTIES CXX_STANDARD 20)
target_link_libraries(tests PRIVATE Catch2::Catch2WithMain)
target_link_libraries(tests PRIVATE turner)

# Bring the sample.json file into the test directory
configure_file(sample.json ${CMAKE_CURRENT_BINARY_DIR} COPYONLY)

include(CTest)
include(Catch)
catch_discover_tests(tests)
