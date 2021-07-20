#ifndef __QUARK_FRONTEND_AST_ASTDUMPER_H__
#define __QUARK_FRONTEND_AST_ASTDUMPER_H__

#include <quark/Frontend/AST/ASTVisitor.h>
#include <quark/Support/IdentationFormat.h>

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/raw_ostream.h>

#include <vector>

namespace quark {

struct ASTDumper : public ASTVisitor {
  ASTDumper(llvm::raw_ostream &out)
      : Out(out), Holder(), RSO(Holder), Depth(0) {}
  virtual ~ASTDumper() {}

  void dump(const SourceModule &);
  void dump(const Decl &);
  void dump(const Stmt &);
  void dump(const Expr &);
  void dump(const Type &);

  void PreStmt() override;
  void PostStmt() override;
  void PreExpr() override;
  void PostExpr() override;
  void PreDecl() override;
  void PostDecl() override;

  void VisitSourceModule() override;
  void VisitExitSourceModule() override;
  void VisitExportModule(llvm::StringRef) override;
  void VisitImportModule(llvm::StringRef) override;

  void VisitAllocExpr(const AllocExpr &) override;
  void VisitFunctionCallExpr(const FunctionCallExpr &) override;
  void VisitMemberCallExpr(const MemberCallExpr &) override;
  void VisitMemberExpr(const MemberExpr &) override;
  void VisitStringExpr(const StringExpr &) override;
  void VisitIntegerExpr(const IntegerExpr &) override;
  void VisitFloatingExpr(const FloatingExpr &) override;
  void VisitCharExpr(const CharExpr &) override;
  void VisitVarRefExpr(const VarRefExpr &) override;
  void VisitBinaryExpr(const BinaryExpr &) override;
  void VisitUnaryExpr(const UnaryExpr &) override;
  void VisitDereferenceExpr(const DereferenceExpr &) override;
  void VisitAddressofExpr(const AddressofExpr &) override;
  void VisitArrayAccessExpr(const ArrayAccessExpr &) override;
  void VisitExplicitCastExpr(const ExplicitCastExpr &) override;
  void VisitImplicitCastExpr(const ImplicitCastExpr &) override;
  void VisitBooleanExpr(const BooleanExpr &) override;

  void VisitBlockStmt(const BlockStmt &) override;
  void VisitDeallocStmt(const DeallocStmt &) override;
  void VisitDeferStmt(const DeferStmt &) override;
  void VisitExprStmt(const ExprStmt &) override;
  void VisitForStmt(const ForStmt &) override;
  void VisitIfStmt(const IfStmt &) override;
  void VisitReturnStmt(const ReturnStmt &) override;
  void VisitVarDeclStmt(const VarDeclStmt &) override;
  void VisitWhileStmt(const WhileStmt &) override;
  void VisitPrintStmt(const PrintStmt &) override;

  void VisitVarDecl(const VarDecl &) override;
  void VisitTypeDecl(const TypeDecl &) override;
  void VisitTypeFieldDecl(const TypeFieldDecl &) override;
  void VisitFuncDecl(const FuncDecl &) override;
  void VisitAliasTypeDecl(const AliasTypeDecl &) override;

  void VisitBuiltinType(const BuiltinType &) override;
  void VisitAliasType(const AliasType &) override;
  void VisitCompoundType(const CompoundType &) override;
  void VisitPtrType(const PtrType &) override;
  void VisitArrayType(const ArrayType &) override;
  void VisitFuncType(const FuncType &) override;

  llvm::raw_ostream &Out;
  std::string Holder;
  llvm::raw_string_ostream RSO;
  std::vector<IdentationElement> Elements;
  std::size_t Depth;

  bool PrintVerboseStmtHeaders = false;
};

} // namespace quark

#endif // __QUARK_FRONTEND_AST_ASTDUMPER_H__