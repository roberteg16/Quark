llvm_add_library(QuarkCodeGen
  CodeGen.cpp
  Mangler.cpp
  LLVMTypeTranslation.cpp
  RecoverVarsUsed.cpp

  LINK_LIBS
  QuarkAST
  QuarkParsing

  LINK_COMPONENTS
  Core
  Support
)
