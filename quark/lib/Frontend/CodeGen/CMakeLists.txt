llvm_add_library(QuarkCodeGen
  CodeGen.cpp
  Mangler.cpp
  LLVMTypeTranslation.cpp
  RecoverVarsUsed.cpp
  OMPTypes.cpp
  OMPFunctions.cpp

  LINK_LIBS
  QuarkAST
  QuarkParsing

  LINK_COMPONENTS
  Core
  Support
)
