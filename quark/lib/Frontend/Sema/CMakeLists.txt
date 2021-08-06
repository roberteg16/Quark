llvm_add_library(QuarkSema
  SemaAnal.cpp

  LINK_LIBS
  QuarkAST
  QuarkParsing

  LINK_COMPONENTS
  Support
  Core
)