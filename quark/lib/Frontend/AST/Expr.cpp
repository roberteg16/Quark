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
BinaryExpr::BinaryExpr(BinaryOperatorKind op, std::unique_ptr<Expr> lhs,
                       std::unique_ptr<Expr> rhs, ValueTypeKind valueKind)
    : Expr(ExprKind::BinaryExpr, valueKind, lhs->ExprType->clone()),
      Lhs(std::move(lhs)), Rhs(std::move(rhs)), Op(op) {}
BinaryExpr::BinaryExpr(BinaryOperatorKind op, std::unique_ptr<Expr> lhs,
                       std::unique_ptr<Expr> rhs, std::unique_ptr<Type> newType,
                       ValueTypeKind valueKind)
    : Expr(ExprKind::BinaryExpr, valueKind, std::move(newType)),
      Lhs(std::move(lhs)), Rhs(std::move(rhs)), Op(op) {}

UnaryExpr::UnaryExpr(UnaryOperatorKind kind, std::unique_ptr<Expr> lhs,
                     ValueTypeKind valueKind)
    : Expr(ExprKind::UnaryExpr, valueKind, lhs->ExprType->clone()),
      Lhs(std::move(lhs)), Op(kind) {}
UnaryExpr::UnaryExpr(UnaryOperatorKind kind, std::unique_ptr<Expr> lhs,
                     std::unique_ptr<Type> newType, ValueTypeKind valueKind)
    : Expr(ExprKind::UnaryExpr, valueKind, std::move(newType)),
      Lhs(std::move(lhs)), Op(kind) {}

FunctionCallExpr::FunctionCallExpr(
    const FuncDecl &funcDecl,
    llvm::SmallVector<std::unique_ptr<Expr>, 4> params)
    : Expr(ExprKind::FunctionCallExpr, ValueTypeKind::RightValue,
           funcDecl.FuncType.RetType->clone()),
      FunctionDecl(funcDecl), Params(std::move(params)) {}

MemberCallExpr::MemberCallExpr(
    const FuncDecl &funcDecl, std::unique_ptr<Expr> expr,
    llvm::SmallVectorImpl<std::unique_ptr<Expr>> &params)
    : Expr(ExprKind::MemberCallExpr, ValueTypeKind::RightValue,
           funcDecl.FuncType.RetType->clone()),
      FunctionDecl(funcDecl), Accessor(std::move(expr)),
      Params(std::move(params)) {}

MemberExpr::MemberExpr(std::unique_ptr<Expr> expr, const TypeFieldDecl &decl)
    : Expr(ExprKind::MemberExpr, ValueTypeKind::LeftValue, decl.Type->clone()),
      Field(decl), Accessed(std::move(expr)) {}

VarRefExpr::VarRefExpr(const VarDecl &var)
    : Expr(ExprKind::VarRefExpr, ValueTypeKind::LeftValue, var.Type->clone()),
      RefVar(var) {}

ArrayAccessExpr::ArrayAccessExpr(std::unique_ptr<Expr> refVar,
                                 std::unique_ptr<Type> type,
                                 std::unique_ptr<Expr> idx)
    : Expr(ExprKind::ArrayAccessExpr, ValueTypeKind::LeftValue,
           std::move(type)),
      RefVar(std::move(refVar)), Idx(std::move(idx)) {}

AllocExpr::AllocExpr(std::unique_ptr<Type> type, std::unique_ptr<Expr> size)
    : Expr(ExprKind::AllocExpr, ValueTypeKind::RightValue, type->clone()),
      AllocType(std::move(type)), SizeToAlloc(std::move(size)) {}

StringExpr::StringExpr(llvm::SmallString<40> v)
    : Expr(ExprKind::StringExpr, ValueTypeKind::RightValue,
           std::make_unique<PtrType>(
               std::make_unique<BuiltinType>(BuiltinTypeKind::u8))),
      Value(v) {}

IntegerExpr::IntegerExpr(long long v)
    : Expr(ExprKind::IntegerExpr, ValueTypeKind::RightValue,
           std::make_unique<BuiltinType>(BuiltinTypeKind::i32)),
      Value(v) {}

CharExpr::CharExpr(char v)
    : Expr(ExprKind::CharExpr, ValueTypeKind::RightValue,
           std::make_unique<BuiltinType>(BuiltinTypeKind::u8)),
      Value(v) {}

FloatingExpr::FloatingExpr(long double v)
    : Expr(ExprKind::FloatingExpr, ValueTypeKind::RightValue,
           std::make_unique<BuiltinType>(BuiltinTypeKind::f32)),
      Value(v) {}

BooleanExpr::BooleanExpr(bool v)
    : Expr(ExprKind::BooleanExpr, ValueTypeKind::RightValue,
           std::make_unique<BuiltinType>(BuiltinTypeKind::b1)),
      Value(v) {}

DereferenceExpr::DereferenceExpr(std::unique_ptr<Expr> expr)
    : Expr(ExprKind::DereferenceExpr, expr->getValueKind(),
           llvm::cast<PtrType>(&expr->getType())->PointeeType->clone()),
      DereferencingExpr(std::move(expr)) {}

AddressofExpr::AddressofExpr(std::unique_ptr<Expr> expr)
    : Expr(ExprKind::AddressofExpr, expr->getValueKind(),
           std::make_unique<PtrType>(expr->getType().clone())),
      AdressOfExpr(std::move(expr)) {}

ImplicitCastExpr::ImplicitCastExpr(std::unique_ptr<Expr> expr,
                                   ImplicitCastKind kind,
                                   ValueTypeKind valueKind)
    : Expr(ExprKind::ImplicitCastExpr, valueKind,
           (kind == ImplicitCastKind::ToBool
                ? std::make_unique<BuiltinType>(BuiltinTypeKind::b1)
                : expr->ExprType->clone())),
      CastedExpr(std::move(expr)), CastKind(kind) {}

ExplicitCastExpr::ExplicitCastExpr(std::unique_ptr<Type> toType,
                                   std::unique_ptr<Expr> expr)
    : Expr(ExprKind::ExplicitCastExpr, expr->getValueKind(), std::move(toType)),
      ConvertingExpr(std::move(expr)) {}

std::unique_ptr<Expr> ImplicitCastExpr::Create(ImplicitCastKind kind,
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

    return std::make_unique<ImplicitCastExpr>(std::move(expr), kind, vkind);
  }
  return std::make_unique<ImplicitCastExpr>(std::move(expr), kind,
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
