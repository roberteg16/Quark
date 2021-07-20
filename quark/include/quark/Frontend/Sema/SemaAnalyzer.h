#ifndef __QUARK_FRONTEND_SEMAANALYZER_H__
#define __QUARK_FRONTEND_SEMAANALYZER_H__

#include <quark/Frontend/AST/ASTVisitor.h>

namespace quark {

struct SemaAnalyzer : public ASTVisitor {
  SemaAnalyzer() {}
  virtual ~SemaAnalyzer() {}

  bool analyze(const SourceModule &);

  void VisitFunctionCallExpr(const FunctionCallExpr &) override;
  void VisitMemberCallExpr(const MemberCallExpr &) override;
  void VisitMemberExpr(const MemberExpr &) override;
  void VisitVarRefExpr(const VarRefExpr &) override;
  void VisitBinaryExpr(const BinaryExpr &) override;
  void VisitUnaryExpr(const UnaryExpr &) override;
  void VisitDereferenceExpr(const DereferenceExpr &) override;
  void VisitArrayAccessExpr(const ArrayAccessExpr &) override;

  void VisitDeallocStmt(const DeallocStmt &) override;
  void VisitForStmt(const ForStmt &) override;
  void VisitIfStmt(const IfStmt &) override;
  void VisitReturnStmt(const ReturnStmt &) override;
  void VisitVarDeclStmt(const VarDeclStmt &) override;
  void VisitWhileStmt(const WhileStmt &) override;

  void VisitFuncDecl(const FuncDecl &) override;

  const FuncDecl *CurrentFunc;
};

} // namespace quark

#endif // __QUARK_FRONTEND_SEMAANALYZER_H__
