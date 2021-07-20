llvm_add_library(QuarkAST
  Expr.cpp
  Decl.cpp
  Stmt.cpp
  Type.cpp
  ASTDumper.cpp

  LINK_LIBS
  QuarkSupport

  LINK_COMPONENTS
  Support
)
