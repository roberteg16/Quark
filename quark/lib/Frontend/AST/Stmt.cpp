#include <quark/Frontend/AST/Stmt.h>

#include <quark/Frontend/AST/ASTDumper.h>

#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>

using namespace quark;

Stmt::~Stmt() {}
#define QK_STMT(NODE)                                                          \
  NODE::~NODE() {}
#include <quark/Frontend/AST/ASTNodes.def>

void Stmt::print(llvm::raw_ostream &out) const { ASTDumper{out}.dump(*this); }
void Stmt::dump() const { print(llvm::dbgs()); }

IfStmt::CondAndStmt::CondAndStmt(location loc, std::unique_ptr<Expr> cond,
                                 std::unique_ptr<::Stmt> stmt)
    : Location(loc), Cond(std::move(cond)), Stmt(std::move(stmt)) {}
