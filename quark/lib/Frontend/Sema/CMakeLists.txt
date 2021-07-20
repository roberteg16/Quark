llvm_add_library(QuarkSema
  SemaAnal.cpp

  LINK_LIBS
  QuarkAST

  LINK_COMPONENTS
  Support
  Core
)