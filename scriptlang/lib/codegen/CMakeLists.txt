aux_source_directory(${CMAKE_CURRENT_SOURCE_DIR} scriptlang_codegen_srcs)

add_scriptlang_library(scriptlang_codegen
    ${scriptlang_codegen_srcs}

    LINK_LIBS

    scriptlang_sematic
    LLVMCodeGen
)