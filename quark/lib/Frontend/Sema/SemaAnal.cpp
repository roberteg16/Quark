#include <quark/Frontend/Sema/SemaAnalyzer.h>

#include <quark/Frontend/AST/Expr.h>
#include <quark/Frontend/AST/Stmt.h>
#include <quark/Frontend/AST/Type.h>
#include <quark/Frontend/Parsing/ParserUtils.h>

#include <llvm/Support/raw_ostream.h>

using namespace quark;

bool SemaAnalyzer::analyze(const SourceModule &sm) {
  visit(sm);
  return Success;
}

void SemaAnalyzer::VisitFunctionCallExpr(const FunctionCallExpr &expr) {
  auto &name = expr.FunctionDecl.Name;
  auto &params = expr.Params;

  FuncDecl::FuncSignature funcSignature(name, params);
  if (!(funcSignature == expr.FunctionDecl.Signature)) {
    PrintLocation(llvm::outs(), expr.Location);
    llvm::outs() << "Error: FuncCall does not match with params";
    Success = false;
  }
}

void SemaAnalyzer::VisitMemberCallExpr(const MemberCallExpr &expr) {
  auto &name = expr.FunctionDecl.Name;
  auto &params = expr.Params;
  auto reciever = expr.Accessor->getType().clone();

  FuncDecl::FuncSignature funcSignature(name, params, std::move(reciever));
  if (!(funcSignature == expr.FunctionDecl.Signature)) {
    PrintLocation(llvm::outs(), expr.Location);
    llvm::outs() << "Error: MemberCall does not match with params/reciver";
    Success = false;
  }
}

void SemaAnalyzer::VisitMemberExpr(const MemberExpr &expr) {
  // TODO: Would be nice to check here if '.' or '->' matches
}

void SemaAnalyzer::VisitVarRefExpr(const VarRefExpr &expr) {
  if (expr.getType() != *expr.RefVar.Type) {
    PrintLocation(llvm::outs(), expr.Location);
    llvm::outs() << "Error: Mistmatch type on varRefExpr";
    Success = false;
  }
}

void SemaAnalyzer::VisitBinaryExpr(const BinaryExpr &expr) {
  if (expr.Lhs->getType() != expr.Rhs->getType()) {
    PrintLocation(llvm::outs(), expr.Location);
    llvm::outs() << "Error: Binary exprs with mistmatched types\n";
    Success = false;
  }
}

void SemaAnalyzer::VisitUnaryExpr(const UnaryExpr &expr) {
  // TODO: Handle all cases of UnaryExpr
  switch (expr.Op) {
  case UnaryOperatorKind::AddressOf:
  case UnaryOperatorKind::Dereference:
    // Handed specifically on their callbacks
    break;
  case UnaryOperatorKind::ArithmeticNegation: {
    auto *builtinType = llvm::dyn_cast<BuiltinType>(&*expr.ExprType);
    if (!builtinType ||
        (!IsInteger(builtinType->Kind) && IsFloatingPoint(builtinType->Kind))) {
      PrintLocation(llvm::outs(), expr.Location);
      llvm::outs() << "Error: Not possible to arithmetically negate a non "
                      "integer nor float\n";
      Success = false;
    }
    break;
  }
  case UnaryOperatorKind::LogicalNegation: {
    auto *builtinType = llvm::dyn_cast<BuiltinType>(&*expr.ExprType);
    if (!builtinType || !IsBoolean(builtinType->Kind)) {
      PrintLocation(llvm::outs(), expr.Location);
      llvm::outs() << "Error: Not possible to logically negate a non boolean\n";
      Success = false;
    }
  }
  }
}

void SemaAnalyzer::VisitDereferenceExpr(const DereferenceExpr &expr) {
  auto *arrayType = llvm::dyn_cast<ArrayType>(&expr.ExprType->desugar());
  auto *ptrType = llvm::dyn_cast<PtrType>(&expr.ExprType->desugar());
  if (!arrayType && !ptrType) {
    PrintLocation(llvm::outs(), expr.Location);
    llvm::outs() << "Error: Deferencing non array nor ptr type\n";
    Success = false;
  }
}

void SemaAnalyzer::VisitAddressofExpr(const AddressofExpr &addressOfExpr) {
  if (addressOfExpr.isRValue()) {
    llvm::outs() << "Error: Taking address of value without memory address\n";
    Success = false;
  }
}

void SemaAnalyzer::VisitArrayAccessExpr(const ArrayAccessExpr &expr) {
  auto *arrayType =
      llvm::dyn_cast<ArrayType>(&expr.RefVar->getType().desugar());
  auto *ptrType = llvm::dyn_cast<PtrType>(&expr.RefVar->getType().desugar());
  if (!arrayType && !ptrType) {
    PrintLocation(llvm::outs(), expr.Location);
    llvm::outs() << "Error: Array access on non array type\n";
    Success = false;
  }
}

void SemaAnalyzer::VisitExplicitCastExpr(const ExplicitCastExpr &explicitCast) {
  TypeCasting castResult =
      CastType(*explicitCast.ExprType, explicitCast.ConvertingExpr->getType());
  if (castResult == TypeCasting::Unknown) {
    PrintLocation(llvm::outs(), explicitCast.Location);
    llvm::outs() << "Error: invalid casting from '";
    explicitCast.ConvertingExpr->getType().print(llvm::outs());
    llvm::outs() << "' to '";
    explicitCast.ExprType->print(llvm::outs());
    llvm::outs() << "'\n";
    Success = false;
  }
}

void SemaAnalyzer::VisitDeallocStmt(const DeallocStmt &stmt) {
  auto *ptrType =
      llvm::dyn_cast<PtrType>(&stmt.ExprToDealloc->getType().desugar());
  if (!ptrType) {
    PrintLocation(llvm::outs(), stmt.Location);
    llvm::outs() << "Error: Deallocing non pointer\n";
    Success = false;
  }
}

static bool IsBooleanBuiltin(const Type &type, location loc) {
  const auto *condType = llvm::dyn_cast<BuiltinType>(&type);
  if (!condType || !condType->isBoolean()) {
    PrintLocation(llvm::outs(), loc);
    llvm::outs() << "Error: Condition is not boolean\n";
    return false;
  }
  return true;
}

void SemaAnalyzer::VisitForStmt(const ForStmt &stmt) {
  Success &= IsBooleanBuiltin(stmt.Cond->getType().desugar(), stmt.Location);
}

void SemaAnalyzer::VisitIfStmt(const IfStmt &stmt) {
  Success &= IsBooleanBuiltin(stmt.Cond->getType().desugar(), stmt.Location);
  for (auto &elsif : stmt.Elsifs) {
    Success &= IsBooleanBuiltin(elsif.Cond->getType().desugar(), stmt.Location);
  }
}

void SemaAnalyzer::VisitReturnStmt(const ReturnStmt &retStmt) {
  const bool isVoidReturn = !retStmt.ReturnValue;
  if (isVoidReturn) {
    auto *type =
        llvm::dyn_cast<BuiltinType>(CurrentFunc->FuncType.RetType.get());
    if (type && !type->isVoid()) {
      PrintLocation(llvm::outs(), retStmt.Location);
      llvm::outs()
          << "Error: Return in non void function must returna a value\n";
      Success = false;
    }
  } else {
    auto *type =
        llvm::dyn_cast<BuiltinType>(CurrentFunc->FuncType.RetType.get());
    if (type && type->isVoid()) {
      PrintLocation(llvm::outs(), retStmt.Location);
      llvm::outs() << "Error: Returning value in void function\n";
      Success = false;
    }

    if (retStmt.ReturnValue->getType() != *CurrentFunc->FuncType.RetType) {
      PrintLocation(llvm::outs(), retStmt.Location);
      llvm::outs()
          << "Error: Return does not match returning type of function\n";
      Success = false;
    }
  }
}

void SemaAnalyzer::VisitVarDeclStmt(const VarDeclStmt &stmt) {
  if (stmt.InitExpr && *stmt.VarDecl->Type != stmt.InitExpr->getType()) {
    PrintLocation(llvm::outs(), stmt.Location);
    llvm::outs()
        << "Error: VarDeclStmt type and init expr are from different type\n";
    Success = false;
  }
}

void SemaAnalyzer::VisitWhileStmt(const WhileStmt &stmt) {
  Success &= IsBooleanBuiltin(stmt.Cond->getType().desugar(), stmt.Location);
}

void SemaAnalyzer::VisitFuncDecl(const FuncDecl &funcDecl) {
  CurrentFunc = &funcDecl;
}
