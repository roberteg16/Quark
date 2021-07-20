#include <quark/Frontend/Parsing/ParserUtils.h>

#include <quark/Frontend/AST/Expr.h>

#include "../../build/tools/quark/lib/Frontend/QuarkParser.hpp"

#include <llvm/Support/Casting.h>

using namespace quark;

void quark::CheckPtrToValueOrValue(const Type &type) {
  const Type *tmpType = &type;

  if (auto *ptrType = llvm::dyn_cast<PtrType>(tmpType)) {
    tmpType = ptrType->PointeeType.get();
  }

  if (!llvm::isa<CompoundType>(tmpType)) {
    throw quark::QuarkParser::syntax_error(
        "Trying to call a method with non ptr to value nor value");
  }
}

std::unique_ptr<quark::Expr>
quark::DerefererenceIfNeeded(std::unique_ptr<quark::Expr> expr,
                             const quark::TypeAccess &access) {
  const PtrType *ptrType = llvm::dyn_cast<PtrType>(&expr->getType());
  if ((ptrType && access.Kind != TypeAccessKind::Pointer) ||
      (!ptrType && access.Kind != TypeAccessKind::Value)) {
    if (ptrType) {
      throw quark::QuarkParser::syntax_error("Did you mean '->'?");
    }
    throw quark::QuarkParser::syntax_error("Did you mean '.'?");
  }

  if (ptrType) {
    expr = ImplicitCastExpr::Create(ImplicitCastKind::LValueToRValue,
                                    std::move(expr));
  }

  return expr;
}

std::unique_ptr<quark::Expr>
quark::GetMemberAccess(std::unique_ptr<quark::Expr> expr,
                       quark::TypeAccess &access) {
  // Must be CompoundType or PtrType to CompoundType
  const auto *compType = CheckCompoundOrTypeToCompound(&expr->getType());
  if (!compType) {
    throw quark::QuarkParser::syntax_error("Not a compound type");
  }

  auto *field = compType->Decl.findField(access.Name);
  if (!field) {
    throw quark::QuarkParser::syntax_error("Field does not exists");
  }

  expr = DerefererenceIfNeeded(std::move(expr), access);
  expr = std::make_unique<quark::MemberExpr>(std::move(expr), *field);

  for (std::unique_ptr<Expr> &arrayAccess : access.ArrayAccesses) {
    std::unique_ptr<Type> innerType = GetArrayAccessType(*expr);
    expr = std::make_unique<ArrayAccessExpr>(
        std::move(expr), std::move(innerType), std::move(arrayAccess));
  }

  return expr;
}

std::unique_ptr<quark::Type>
quark::GetArrayAccessType(const quark::Expr &expr) {
  const Type &type = expr.getType();
  const auto *ptrType = llvm::dyn_cast<PtrType>(&type);
  const auto *arrayType = llvm::dyn_cast<ArrayType>(&type);

  if (ptrType) {
    return ptrType->PointeeType->clone();
  }

  if (arrayType) {
    return arrayType->RealType->clone();
  }

  throw quark::QuarkParser::syntax_error(
      "Accessing with [] non pointer nor array");
}

static bool IsDefUnary(const Expr &expr) {
  if (auto *unary = llvm::dyn_cast<UnaryExpr>(&expr)) {
    if (unary->Op == UnaryOperatorKind::Dereference) {
      return true;
    }
  }
  return false;
}

static bool IsRefUnary(const Expr &expr) {
  if (auto *unary = llvm::dyn_cast<UnaryExpr>(&expr)) {
    if (unary->Op == UnaryOperatorKind::AddressOf) {
      return true;
    }
  }
  return false;
}

std::unique_ptr<Expr>
quark::AddCastIfNeededAndVarRefExpr(std::unique_ptr<Expr> expr) {
  if (IsRefUnary(*expr)) {
    return expr;
  }

  if ((llvm::isa<VarRefExpr>(expr.get()) || llvm::isa<MemberExpr>(expr.get()) ||
       IsDefUnary(*expr) || llvm::isa<ArrayAccessExpr>(expr.get())) &&
      expr->isLValue()) {
    return ImplicitCastExpr::Create(ImplicitCastKind::LValueToRValue,
                                    std::move(expr));
  }
  return expr;
}

std::unique_ptr<Expr> quark::AddCastIfNeeded(std::unique_ptr<Expr> expr) {
  if (IsRefUnary(*expr)) {
    return expr;
  }

  if (expr->isLValue()) {
    return ImplicitCastExpr::Create(ImplicitCastKind::LValueToRValue,
                                    std::move(expr));
  }
  return expr;
}
