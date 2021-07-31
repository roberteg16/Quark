#ifndef __QUARK_LIB_FRONTEND_CODEGEN_RECOVERVARSUSED_H__
#define __QUARK_LIB_FRONTEND_CODEGEN_RECOVERVARSUSED_H__

#include <quark/Frontend/AST/ASTVisitor.h>

#include <llvm/ADT/SetVector.h>

namespace quark {

struct RecoverVarsUsed : public ASTVisitor {
  RecoverVarsUsed() {}
  virtual ~RecoverVarsUsed() {}

  void recover(const Stmt &);

  llvm::SetVector<const VarDecl *> takeVarDecls() {
    return std::move(VarDecls);
  };
  llvm::SetVector<const VarDecl *> takeVarUsed() { return std::move(VarUsed); };

private:
  void VisitVarDecl(const VarDecl &decl) override;
  void VisitVarRefExpr(const VarRefExpr &expr) override;

  llvm::SetVector<const VarDecl *> VarDecls;
  llvm::SetVector<const VarDecl *> VarUsed;
};

} // namespace quark

#endif // __QUARK_LIB_FRONTEND_CODEGEN_RECOVERVARSUSED_H__
