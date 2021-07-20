set(QUARK_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(QUARK_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

set(LLVM_TARGET_DEFINITIONS Options.td)
tablegen(LLVM Options.inc -gen-opt-parser-defs -I ${QUARK_SOURCE_DIR}/include)
add_public_tablegen_target(QuarkOptions)

add_llvm_executable(quark
  quark.cpp
)

target_link_libraries(quark
  PRIVATE
  QuarkCodeGen
  QuarkAST
  QuarkParsing
  QuarkSupport
  QuarkSema

  LLVMSupport
  LLVMCore
  LLVMOption
)
