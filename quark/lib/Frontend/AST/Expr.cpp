#include <quark/Frontend/AST/Expr.h>

#include <quark/Frontend/AST/ASTDumper.h>

#include <llvm/Support/Casting.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>

using namespace quark;

void Expr::print(llvm::raw_ostream &out) const { ASTDumper{out}.dump(*this); }
void Expr::dump() const { print(llvm::dbgs()); }

// ==== Destructors ==== //
Expr::~Expr() {}
#define QK_EXPR(NODE)                                                          \
  NODE::~NODE() {}
#include <quark/Frontend/AST/ASTNodes.def>

// ==== Constructors ==== //
BinaryExpr::BinaryExpr(location loc, BinaryOperatorKind op,
                       std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs,
                       ValueTypeKind valueKind)
    : Expr(loc, ExprKind::BinaryExpr, valueKind, lhs->ExprType->clone()),
      Lhs(std::move(lhs)), Rhs(std::move(rhs)), Op(op) {}
BinaryExpr::BinaryExpr(location loc, BinaryOperatorKind op,
                       std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs,
                       std::unique_ptr<Type> newType, ValueTypeKind valueKind)
    : Expr(loc, ExprKind::BinaryExpr, valueKind, std::move(newType)),
      Lhs(std::move(lhs)), Rhs(std::move(rhs)), Op(op) {}

UnaryExpr::UnaryExpr(location loc, UnaryOperatorKind kind,
                     std::unique_ptr<Expr> lhs, ValueTypeKind valueKind)
    : Expr(loc, ExprKind::UnaryExpr, valueKind, lhs->ExprType->clone()),
      Lhs(std::move(lhs)), Op(kind) {}
UnaryExpr::UnaryExpr(location loc, UnaryOperatorKind kind,
                     std::unique_ptr<Expr> lhs, std::unique_ptr<Type> newType,
                     ValueTypeKind valueKind)
    : Expr(loc, ExprKind::UnaryExpr, valueKind, std::move(newType)),
      Lhs(std::move(lhs)), Op(kind) {}

FunctionCallExpr::FunctionCallExpr(
    location loc, const FuncDecl &funcDecl,
    llvm::SmallVector<std::unique_ptr<Expr>, 4> params)
    : Expr(loc, ExprKind::FunctionCallExpr, ValueTypeKind::RightValue,
           funcDecl.FuncType.RetType->clone()),
      FunctionDecl(funcDecl), Params(std::move(params)) {}

MemberCallExpr::MemberCallExpr(
    location loc, const FuncDecl &funcDecl, std::unique_ptr<Expr> expr,
    llvm::SmallVectorImpl<std::unique_ptr<Expr>> &params)
    : Expr(loc, ExprKind::MemberCallExpr, ValueTypeKind::RightValue,
           funcDecl.FuncType.RetType->clone()),
      FunctionDecl(funcDecl), Accessor(std::move(expr)),
      Params(std::move(params)) {}

MemberExpr::MemberExpr(location loc, std::unique_ptr<Expr> expr,
                       const TypeFieldDecl &decl)
    : Expr(loc, ExprKind::MemberExpr, ValueTypeKind::LeftValue,
           decl.Type->clone()),
      Field(decl), Accessed(std::move(expr)) {}

VarRefExpr::VarRefExpr(location loc, const VarDecl &var)
    : Expr(loc, ExprKind::VarRefExpr, ValueTypeKind::LeftValue,
           var.Type->clone()),
      RefVar(var) {}

ArrayAccessExpr::ArrayAccessExpr(location loc, std::unique_ptr<Expr> refVar,
                                 std::unique_ptr<Type> type,
                                 std::unique_ptr<Expr> idx)
    : Expr(loc, ExprKind::ArrayAccessExpr, ValueTypeKind::LeftValue,
           std::move(type)),
      RefVar(std::move(refVar)), Idx(std::move(idx)) {}

AllocExpr::AllocExpr(location loc, std::unique_ptr<Type> type,
                     std::unique_ptr<Expr> size)
    : Expr(loc, ExprKind::AllocExpr, ValueTypeKind::RightValue, type->clone()),
      AllocType(std::move(type)), SizeToAlloc(std::move(size)) {}

StringExpr::StringExpr(location loc, llvm::SmallString<40> v)
    : Expr(loc, ExprKind::StringExpr, ValueTypeKind::RightValue,
           std::make_unique<PtrType>(
               std::make_unique<BuiltinType>(BuiltinTypeKind::u8))),
      Value(v) {}

IntegerExpr::IntegerExpr(location loc, long long v)
    : Expr(loc, ExprKind::IntegerExpr, ValueTypeKind::RightValue,
           std::make_unique<BuiltinType>(BuiltinTypeKind::i32)),
      Value(v) {}

CharExpr::CharExpr(location loc, char v)
    : Expr(loc, ExprKind::CharExpr, ValueTypeKind::RightValue,
           std::make_unique<BuiltinType>(BuiltinTypeKind::u8)),
      Value(v) {}

FloatingExpr::FloatingExpr(location loc, long double v)
    : Expr(loc, ExprKind::FloatingExpr, ValueTypeKind::RightValue,
           std::make_unique<BuiltinType>(BuiltinTypeKind::f32)),
      Value(v) {}

BooleanExpr::BooleanExpr(location loc, bool v)
    : Expr(loc, ExprKind::BooleanExpr, ValueTypeKind::RightValue,
           std::make_unique<BuiltinType>(BuiltinTypeKind::b1)),
      Value(v) {}

DereferenceExpr::DereferenceExpr(location loc, std::unique_ptr<Expr> expr)
    : Expr(loc, ExprKind::DereferenceExpr, expr->getValueKind(),
           llvm::cast<PtrType>(&expr->getType())->PointeeType->clone()),
      DereferencingExpr(std::move(expr)) {}

AddressofExpr::AddressofExpr(location loc, std::unique_ptr<Expr> expr)
    : Expr(loc, ExprKind::AddressofExpr, expr->getValueKind(),
           std::make_unique<PtrType>(expr->getType().clone())),
      AdressOfExpr(std::move(expr)) {}

ImplicitCastExpr::ImplicitCastExpr(location loc, std::unique_ptr<Expr> expr,
                                   ImplicitCastKind kind,
                                   ValueTypeKind valueKind)
    : Expr(loc, ExprKind::ImplicitCastExpr, valueKind,
           (kind == ImplicitCastKind::ToBool
                ? std::make_unique<BuiltinType>(BuiltinTypeKind::b1)
                : expr->ExprType->clone())),
      CastedExpr(std::move(expr)), CastKind(kind) {}

ExplicitCastExpr::ExplicitCastExpr(location loc, std::unique_ptr<Type> toType,
                                   std::unique_ptr<Expr> expr)
    : Expr(loc, ExprKind::ExplicitCastExpr, expr->getValueKind(),
           std::move(toType)),
      ConvertingExpr(std::move(expr)) {}

std::unique_ptr<Expr> ImplicitCastExpr::Create(location loc,
                                               ImplicitCastKind kind,
                                               std::unique_ptr<Expr> expr) {
  if (kind == ImplicitCastKind::LValueToRValue) {
    ValueTypeKind vkind = ValueTypeKind::RightValue;

    if (auto *ptr = llvm::dyn_cast<PtrType>(&expr->getType())) {
      if (llvm::isa<PtrType>(ptr->PointeeType) ||
          llvm::isa<ArrayType>(ptr->PointeeType)) {
        vkind = ValueTypeKind::LeftValue;
      }
    } else if (auto *array = llvm::dyn_cast<ArrayType>(&expr->getType())) {
      if (llvm::isa<PtrType>(array->RealType) ||
          llvm::isa<ArrayType>(array->RealType)) {
        vkind = ValueTypeKind::LeftValue;
      }
    }

    return std::make_unique<ImplicitCastExpr>(loc, std::move(expr), kind,
                                              vkind);
  }
  return std::make_unique<ImplicitCastExpr>(loc, std::move(expr), kind,
                                            expr->ValueKind);
}

llvm::StringRef quark::ToString(BinaryOperatorKind binOp) {
  switch (binOp) {
  case BinaryOperatorKind::Add:
    return "+";
  case BinaryOperatorKind::Minus:
    return "-";
  case BinaryOperatorKind::Mul:
    return "*";
  case BinaryOperatorKind::Div:
    return "/";
  case BinaryOperatorKind::Mod:
    return "%";
  case BinaryOperatorKind::Assign:
    return "=";
  case BinaryOperatorKind::LogicalNotEquals:
    return "!=";
  case BinaryOperatorKind::LogicalEquals:
    return "==";
  case BinaryOperatorKind::LogicalAnd:
    return "&&";
  case BinaryOperatorKind::LogicalOr:
    return "||";
  case BinaryOperatorKind::LogicalLess:
    return "<";
  case BinaryOperatorKind::LogicalLessEqual:
    return "<=";
  case BinaryOperatorKind::LogicalGreater:
    return ">";
  case BinaryOperatorKind::LogicalGreaterEqual:
    return ">=";
  }
}

llvm::StringRef quark::ToString(UnaryOperatorKind unaryOp) {
  switch (unaryOp) {
  case UnaryOperatorKind::LogicalNegation:
    return "!";
  case UnaryOperatorKind::ArithmeticNegation:
    return "-";
  case UnaryOperatorKind::Dereference:
    return "*";
  case UnaryOperatorKind::AddressOf:
    return "&";
  }
}

unsigned MemberExpr::getIdxAccesses() const {
  auto *compoundType = CheckCompoundOrTypeToCompound(Accessed->ExprType.get());
  assert(compoundType);

  for (unsigned i = 0; i < compoundType->Decl.FieldDecls.size(); i++) {
    if (compoundType->Decl.FieldDecls[i].get() == &Field) {
      return i;
    }
  }

  llvm::llvm_unreachable_internal("The type field must be found");
}
