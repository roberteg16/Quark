#include <quark/Frontend/AST/Stmt.h>

#include <quark/Frontend/AST/ASTDumper.h>
#include <quark/Frontend/AST/Decl.h>
#include <quark/Frontend/AST/Expr.h>

#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>

using namespace quark;

Stmt::~Stmt() {}
BlockStmt::~BlockStmt() {}
ForStmt::~ForStmt() {}
IfStmt::~IfStmt() {}
WhileStmt::~WhileStmt() {}
ReturnStmt::~ReturnStmt() {}
DeferStmt::~DeferStmt() {}
DeallocStmt::~DeallocStmt() {}
VarDeclStmt::~VarDeclStmt() {}
ExprStmt::~ExprStmt() {}
PrintStmt::~PrintStmt() {}

void Stmt::print(llvm::raw_ostream &out) const { ASTDumper{out}.dump(*this); }

void Stmt::dump() const { print(llvm::dbgs()); }

IfStmt::CondAndStmt::CondAndStmt(std::unique_ptr<Expr> cond,
                                 std::unique_ptr<::Stmt> stmt)
    : Cond(std::move(cond)), Stmt(std::move(stmt)) {}
