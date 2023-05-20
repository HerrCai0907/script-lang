set(LLVM_LINK_COMPONENTS
    Support
)

add_scriptlang_tool(scriptlang
    driver.cpp
)

target_link_libraries(scriptlang
    PRIVATE
    scriptlang_basic
    scriptlang_parser
    scriptlang_sematic
)

set_target_properties(scriptlang PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
)
