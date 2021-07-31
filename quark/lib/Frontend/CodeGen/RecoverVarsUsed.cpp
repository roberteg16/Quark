#include "RecoverVarsUsed.h"
#include "llvm/ADT/SetVector.h"

using namespace quark;

void RecoverVarsUsed::recover(const Stmt &stmt) { visit(stmt); }

void RecoverVarsUsed::VisitVarDecl(const VarDecl &decl) {
  VarDecls.insert(&decl);
}

void RecoverVarsUsed::VisitVarRefExpr(const VarRefExpr &expr) {
  VarUsed.insert(&expr.RefVar);
}
