#include <quark/Frontend/Sema/SemaAnalyzer.h>

#include <quark/Frontend/AST/Expr.h>
#include <quark/Frontend/AST/Stmt.h>
#include <quark/Frontend/AST/Type.h>

#include <llvm/Support/raw_ostream.h>

using namespace quark;

bool SemaAnalyzer::analyze(const SourceModule &sm) {
  visit(sm);
  // TODO: returning inconditionally true, this should return depending on the
  //       result of the analysis
  return true;
}

void SemaAnalyzer::VisitFunctionCallExpr(const FunctionCallExpr &expr) {
  // TODO: Check params
}
void SemaAnalyzer::VisitMemberCallExpr(const MemberCallExpr &expr) {
  // TODO: Check params y reciever
}

void SemaAnalyzer::VisitMemberExpr(const MemberExpr &expr) {
  // TODO: Would be nice to check here if '.' or '->' matches
}

void SemaAnalyzer::VisitVarRefExpr(const VarRefExpr &expr) {
  if (expr.getType() != *expr.RefVar.Type) {
    llvm::outs() << "Error: Mistmatch type on varRefExpr";
  }
}

void SemaAnalyzer::VisitBinaryExpr(const BinaryExpr &expr) {
  if (expr.Lhs->getType() != expr.Rhs->getType()) {
    llvm::outs() << "Error on binary expr";
    expr.Lhs->dump();
    expr.Rhs->dump();
  }
}

void SemaAnalyzer::VisitUnaryExpr(const UnaryExpr &expr) {
  // TODO: Handle all cases of UnaryExpr
}

void SemaAnalyzer::VisitDereferenceExpr(const DereferenceExpr &expr) {
  // TODO: Refactor to handle all dereferences here
}

void SemaAnalyzer::VisitArrayAccessExpr(const ArrayAccessExpr &expr) {
  // TODO: Refactor to handle all array access here
}

void SemaAnalyzer::VisitDeallocStmt(const DeallocStmt &stmt) {
  // TODO: Refactor to handle dealloc here
}

static void IsIntegerBuiltin(const Type &type) {
  const auto *condType = llvm::dyn_cast<BuiltinType>(&type);
  if (!condType) {
    llvm::outs() << "Condition is not builtin type\n";
  }

  if (!condType->isBoolean()) {
    llvm::outs() << "Condition is not boolean";
  }
}

void SemaAnalyzer::VisitForStmt(const ForStmt &stmt) {
  IsIntegerBuiltin(stmt.Cond->getType().desugar());
}

void SemaAnalyzer::VisitIfStmt(const IfStmt &stmt) {
  IsIntegerBuiltin(stmt.Cond->getType().desugar());
  for (auto &elsif : stmt.Elsifs) {
    IsIntegerBuiltin(elsif.Cond->getType().desugar());
  }
}

void SemaAnalyzer::VisitReturnStmt(const ReturnStmt &retStmt) {
  if (!retStmt.ReturnValue) {
    auto *type =
        llvm::dyn_cast<BuiltinType>(CurrentFunc->FuncType.RetType.get());
    if (type && !type->isVoid()) {
      llvm::outs() << "Returning non void in void function";
    }
  } else {
    if (!retStmt.ReturnValue) {
      llvm::outs() << "Return in non void function must returna a value";
      return;
    }

    if (retStmt.ReturnValue->getType() != *CurrentFunc->FuncType.RetType) {
      llvm::outs() << "Return does not match returning type of function";
    }
  }
}

void SemaAnalyzer::VisitVarDeclStmt(const VarDeclStmt &stmt) {
  if (stmt.InitExpr && *stmt.VarDecl->Type != stmt.InitExpr->getType()) {
    llvm::outs() << "Error VarDeclStmt\n";
    stmt.VarDecl->dump();
    stmt.InitExpr->dump();
  }
}

void SemaAnalyzer::VisitWhileStmt(const WhileStmt &stmt) {
  IsIntegerBuiltin(stmt.Cond->getType().desugar());
}

void SemaAnalyzer::VisitFuncDecl(const FuncDecl &funcDecl) {
  CurrentFunc = &funcDecl;
}
