#ifndef __QUARK_FRONTEND_AST_ASTVISITOR_H__
#define __QUARK_FRONTEND_AST_ASTVISITOR_H__

#include <quark/Frontend/AST/Decl.h>
#include <quark/Frontend/AST/Expr.h>
#include <quark/Frontend/AST/Stmt.h>
#include <quark/Frontend/AST/Type.h>
#include <quark/Frontend/Parsing/SourceModule.h>

#include <llvm/ADT/ScopeExit.h>
#include <llvm/Support/Casting.h>

namespace quark {

struct ASTVisitor {
  ASTVisitor() {}
  virtual ~ASTVisitor() {}

protected:
  void visit(const SourceModule &sm) {
    VisitSourceModule();

    if (!sm.ExportedModule.empty())
      VisitExportModule(sm.ExportedModule);

    for (llvm::StringRef imported : sm.ImportedModules) {
      VisitImportModule(imported);
    }

    for (const std::unique_ptr<Decl> &decl : sm.Declarations) {
      visit(*decl);
    }

    VisitExitSourceModule();
  }

  void visit(const Decl &decl) {
    PreDecl();
    auto onExit = llvm::make_scope_exit([&] { PostDecl(); });
    switch (decl.Kind) {
    case DeclKind::VarDecl:
      VisitVarDecl(*llvm::cast<VarDecl>(&decl));
      return;
    case DeclKind::TypeDecl: {
      const auto &typeDecl = *llvm::cast<TypeDecl>(&decl);
      VisitTypeDecl(typeDecl);
      for (auto &fields : typeDecl.FieldDecls) {
        visit(*fields);
      }
      return;
    }
    case DeclKind::TypeFieldDecl:
      VisitTypeFieldDecl(*llvm::cast<TypeFieldDecl>(&decl));
      return;
    case DeclKind::FuncDecl: {
      const auto &funcDecl = *llvm::cast<FuncDecl>(&decl);
      VisitFuncDecl(funcDecl);
      if (funcDecl.Reciver)
        visit(*funcDecl.Reciver);
      for (auto &param : funcDecl.Params) {
        visit(*param);
      }
      for (auto &stmt : funcDecl.Body) {
        visit(*stmt);
      }
      return;
    }
    case DeclKind::AliasTypeDecl:
      VisitAliasTypeDecl(*llvm::cast<AliasTypeDecl>(&decl));
      return;
    }
  }

  void visit(const Stmt &stmt) {
    PreStmt();
    auto onExit = llvm::make_scope_exit([&] { PostStmt(); });
    switch (stmt.Kind) {
    case StmtKind::BlockStmt: {
      const auto &blockStmt = *llvm::cast<BlockStmt>(&stmt);
      VisitBlockStmt(blockStmt);
      for (auto &stmt : blockStmt.Stmts) {
        visit(*stmt);
      }
      return;
    }
    case StmtKind::DeallocStmt: {
      const auto &deallocStmt = *llvm::cast<DeallocStmt>(&stmt);
      VisitDeallocStmt(deallocStmt);
      visit(*deallocStmt.ExprToDealloc);
      return;
    }
    case StmtKind::DeferStmt: {
      const auto &deferStmt = *llvm::cast<DeferStmt>(&stmt);
      VisitDeferStmt(deferStmt);
      visit(*deferStmt.ExprToDefer);
      return;
    }
    case StmtKind::ExprStmt: {
      const auto &exprStmt = *llvm::cast<ExprStmt>(&stmt);
      VisitExprStmt(exprStmt);
      visit(*exprStmt.Expr);
      return;
    }
    case StmtKind::ForStmt: {
      const auto &forStmt = *llvm::cast<ForStmt>(&stmt);
      VisitForStmt(forStmt);
      visit(*forStmt.VarDecl);
      visit(*forStmt.Cond);
      visit(*forStmt.Inc);
      visit(*forStmt.Body);
      return;
    }
    case StmtKind::IfStmt: {
      const auto &ifStmt = *llvm::cast<IfStmt>(&stmt);
      VisitIfStmt(ifStmt);
      visit(*ifStmt.Cond);
      visit(*ifStmt.Code);
      for (auto &elsif : ifStmt.Elsifs) {
        visit(*elsif.Cond);
        visit(*elsif.Stmt);
      }
      if (ifStmt.ElseCode)
        visit(*ifStmt.ElseCode);
      return;
    }
    case StmtKind::ReturnStmt: {
      const auto &retStmt = *llvm::cast<ReturnStmt>(&stmt);
      VisitReturnStmt(retStmt);
      if (retStmt.ReturnValue)
        visit(*retStmt.ReturnValue);
      return;
    }
    case StmtKind::VarDeclStmt: {
      const auto &varDeclStmt = *llvm::cast<VarDeclStmt>(&stmt);
      VisitVarDeclStmt(varDeclStmt);
      visit(*varDeclStmt.VarDecl);
      if (varDeclStmt.InitExpr)
        visit(*varDeclStmt.InitExpr);
      return;
    }
    case StmtKind::WhileStmt: {
      const auto &whileStmt = *llvm::cast<WhileStmt>(&stmt);
      VisitWhileStmt(whileStmt);
      visit(*whileStmt.Cond);
      visit(*whileStmt.Code);
      return;
    }
    case StmtKind::PrintStmt: {
      const auto &printStmt = *llvm::cast<PrintStmt>(&stmt);
      VisitPrintStmt(printStmt);
      visit(*printStmt.String);
      for (const std::unique_ptr<Expr> &arg : printStmt.Args) {
        visit(*arg);
      }
      return;
    }
    }
  }

  void visit(const Expr &expr) {
    PreExpr();
    auto onExit = llvm::make_scope_exit([&] { PostExpr(); });
    switch (expr.Kind) {
    case ExprKind::AllocExpr:
      VisitAllocExpr(*llvm::cast<AllocExpr>(&expr));
      return;
    case ExprKind::BinaryExpr: {
      const auto &binExpr = *llvm::cast<BinaryExpr>(&expr);
      VisitBinaryExpr(binExpr);
      visit(*binExpr.Lhs);
      visit(*binExpr.Rhs);
      return;
    }
    case ExprKind::CharExpr:
      VisitCharExpr(*llvm::cast<CharExpr>(&expr));
      return;
    case ExprKind::FloatingExpr:
      VisitFloatingExpr(*llvm::cast<FloatingExpr>(&expr));
      return;
    case ExprKind::FunctionCallExpr: {
      const auto &funcCallExpr = *llvm::cast<FunctionCallExpr>(&expr);
      VisitFunctionCallExpr(funcCallExpr);
      for (auto &param : funcCallExpr.Params) {
        visit(*param);
      }
      return;
    }
    case ExprKind::IntegerExpr:
      VisitIntegerExpr(*llvm::cast<IntegerExpr>(&expr));
      return;
    case ExprKind::MemberCallExpr: {
      const auto &memberCallExpr = *llvm::cast<MemberCallExpr>(&expr);
      VisitMemberCallExpr(memberCallExpr);
      visit(*memberCallExpr.Accessor);
      for (auto &param : memberCallExpr.Params) {
        visit(*param);
      }
      return;
    }
    case ExprKind::MemberExpr: {
      const auto &memberExpr = *llvm::cast<MemberExpr>(&expr);
      VisitMemberExpr(memberExpr);
      visit(*memberExpr.Accessed);
      return;
    }
    case ExprKind::StringExpr:
      VisitStringExpr(*llvm::cast<StringExpr>(&expr));
      return;
    case ExprKind::UnaryExpr: {
      const auto &unaryExpr = *llvm::cast<UnaryExpr>(&expr);
      VisitUnaryExpr(unaryExpr);
      visit(*unaryExpr.Lhs);
      return;
    }
    case ExprKind::VarRefExpr:
      VisitVarRefExpr(*llvm::cast<VarRefExpr>(&expr));
      return;
    case ExprKind::DereferenceExpr: {
      const auto &dereferenceExpr = *llvm::cast<DereferenceExpr>(&expr);
      VisitDereferenceExpr(dereferenceExpr);
      visit(*dereferenceExpr.DereferencingExpr);
      return;
    }
    case ExprKind::AddressofExpr: {
      const auto &addressofExpr = *llvm::cast<AddressofExpr>(&expr);
      VisitAddressofExpr(addressofExpr);
      visit(*addressofExpr.AdressOfExpr);
      return;
    }
    case ExprKind::ArrayAccessExpr: {
      const auto &arrayAccessExpr = *llvm::cast<ArrayAccessExpr>(&expr);
      VisitArrayAccessExpr(arrayAccessExpr);
      visit(*arrayAccessExpr.RefVar);
      visit(*arrayAccessExpr.Idx);
      return;
    }
    case ExprKind::ExplicitCastExpr: {
      const auto &explicitCastExpr = *llvm::cast<ExplicitCastExpr>(&expr);
      VisitExplicitCastExpr(explicitCastExpr);
      visit(*explicitCastExpr.ConvertingExpr);
      return;
    }
    case ExprKind::ImplicitCastExpr: {
      const auto &implicitCastExpr = *llvm::cast<ImplicitCastExpr>(&expr);
      VisitImplicitCastExpr(implicitCastExpr);
      visit(*implicitCastExpr.CastedExpr);
      return;
    }
    case ExprKind::BooleanExpr: {
      const auto &booleanExpr = *llvm::cast<BooleanExpr>(&expr);
      VisitBooleanExpr(booleanExpr);
      return;
    }
    }
  }

  void visit(const Type &type) {
    switch (type.TKind) {
    case TypeKind::AliasType:
      return VisitAliasType(*llvm::cast<AliasType>(&type));
    case TypeKind::ArrayType:
      return VisitArrayType(*llvm::cast<ArrayType>(&type));
    case TypeKind::BuiltinType:
      return VisitBuiltinType(*llvm::cast<BuiltinType>(&type));
    case TypeKind::CompoundType:
      return VisitCompoundType(*llvm::cast<CompoundType>(&type));
    case TypeKind::PtrType:
      return VisitPtrType(*llvm::cast<PtrType>(&type));
    case TypeKind::FuncType:
      return VisitFuncType(*llvm::cast<FuncType>(&type));
    }
  };

public:
  virtual void PreStmt() {}
  virtual void PostStmt() {}
  virtual void PreExpr() {}
  virtual void PostExpr() {}
  virtual void PreDecl() {}
  virtual void PostDecl() {}
  virtual void VisitSourceModule() {}
  virtual void VisitExitSourceModule() {}
  virtual void VisitExportModule(llvm::StringRef) {}
  virtual void VisitImportModule(llvm::StringRef) {}

  virtual void VisitAllocExpr(const AllocExpr &expr) {}
  virtual void VisitFunctionCallExpr(const FunctionCallExpr &expr) {}
  virtual void VisitMemberCallExpr(const MemberCallExpr &expr) {}
  virtual void VisitMemberExpr(const MemberExpr &expr) {}
  virtual void VisitStringExpr(const StringExpr &expr) {}
  virtual void VisitIntegerExpr(const IntegerExpr &expr) {}
  virtual void VisitFloatingExpr(const FloatingExpr &expr) {}
  virtual void VisitCharExpr(const CharExpr &expr) {}
  virtual void VisitVarRefExpr(const VarRefExpr &expr) {}
  virtual void VisitBinaryExpr(const BinaryExpr &expr) {}
  virtual void VisitUnaryExpr(const UnaryExpr &expr) {}
  virtual void VisitDereferenceExpr(const DereferenceExpr &expr) {}
  virtual void VisitAddressofExpr(const AddressofExpr &expr) {}
  virtual void VisitArrayAccessExpr(const ArrayAccessExpr &expr) {}
  virtual void VisitExplicitCastExpr(const ExplicitCastExpr &expr) {}
  virtual void VisitImplicitCastExpr(const ImplicitCastExpr &expr) {}
  virtual void VisitBooleanExpr(const BooleanExpr &expr) {}

  virtual void VisitBlockStmt(const BlockStmt &stmt) {}
  virtual void VisitDeallocStmt(const DeallocStmt &stmt) {}
  virtual void VisitDeferStmt(const DeferStmt &stmt) {}
  virtual void VisitExprStmt(const ExprStmt &stmt) {}
  virtual void VisitForStmt(const ForStmt &stmt) {}
  virtual void VisitIfStmt(const IfStmt &stmt) {}
  virtual void VisitReturnStmt(const ReturnStmt &stmt) {}
  virtual void VisitVarDeclStmt(const VarDeclStmt &stmt) {}
  virtual void VisitWhileStmt(const WhileStmt &stmt) {}
  virtual void VisitPrintStmt(const PrintStmt &stmt) {}

  virtual void VisitVarDecl(const VarDecl &) {}
  virtual void VisitTypeDecl(const TypeDecl &decl) {}
  virtual void VisitTypeFieldDecl(const TypeFieldDecl &) {}
  virtual void VisitFuncDecl(const FuncDecl &decl) {}
  virtual void VisitAliasTypeDecl(const AliasTypeDecl &) {}

  virtual void VisitBuiltinType(const BuiltinType &) {}
  virtual void VisitAliasType(const AliasType &) {}
  virtual void VisitCompoundType(const CompoundType &) {}
  virtual void VisitPtrType(const PtrType &) {}
  virtual void VisitArrayType(const ArrayType &) {}
  virtual void VisitFuncType(const FuncType &) {}
};

} // namespace quark

#endif // __QUARK_FRONTEND_AST_ASTVISITOR_H__
