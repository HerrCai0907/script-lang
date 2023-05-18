set(ANTLR_BUILD_CPP_TESTS off)
add_subdirectory(antlr4/runtime/Cpp)

set(LLVM_ENABLE_RTTI on)
set(LLVM_ENABLE_EH on)
set(LLVM_EXTERNAL_PROJECTS "scriptlang")
set(LLVM_EXTERNAL_SCRIPTLANG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../scriptlang)
add_subdirectory(llvm-project/llvm)
