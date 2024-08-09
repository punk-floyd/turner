include(CTest)
include(Catch)

macro(define_my_test test_name src_file)
    add_executable       (${test_name} ${src_file})
    set_target_properties(${test_name} PROPERTIES CXX_STANDARD 20)
    target_link_libraries(${test_name} PRIVATE Catch2::Catch2WithMain)
    target_link_libraries(${test_name} PRIVATE turner)
    catch_discover_tests (${test_name})
endmacro()

define_my_test(test-json      test-json.cpp)
define_my_test(test-expected  test-expected.cpp)

# Bring the sample.json file into the test directory
configure_file(sample.json ${CMAKE_CURRENT_BINARY_DIR} COPYONLY)
