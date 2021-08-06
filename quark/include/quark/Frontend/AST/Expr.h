#ifndef __QUARK_FRONTEND_AST_EXPR_H__
#define __QUARK_FRONTEND_AST_EXPR_H__

#include <quark/Frontend/AST/Node.h>
#include <quark/Frontend/AST/Type.h>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/raw_ostream.h>

namespace quark {

struct VarDecl;
struct TypeFieldDecl;
struct FuncDecl;
struct Decl;
struct LexContext;

enum class ExprKind {
#define QK_EXPR(ID) ID,
#include "ASTNodes.def"
};

enum ValueTypeKind { LeftValue = 0, RightValue };

struct Expr : Node {
  Expr(location loc, ExprKind kind, ValueTypeKind valueKind,
       std::unique_ptr<Type> exprType)
      : Node(loc), Kind(kind), ValueKind(valueKind),
        ExprType(std::move(exprType)) {}
  virtual ~Expr() = 0;

  virtual void print(llvm::raw_ostream &) const;
  virtual void dump() const;

  bool isLValue() const { return ValueKind == ValueTypeKind::LeftValue; }
  bool isRValue() const { return ValueKind == ValueTypeKind::RightValue; }

  const Type &getType() const { return *ExprType; }
  virtual ExprKind getKind() const { return Kind; }
  ValueTypeKind getValueKind() const { return ValueKind; }

  ExprKind Kind;
  ValueTypeKind ValueKind;
  std::unique_ptr<Type> ExprType;
};

enum class BinaryOperatorKind {
  Add,
  Minus,
  Mul,
  Div,
  Mod,
  Assign,
  LogicalStart,
  LogicalNotEquals = LogicalStart,
  LogicalEquals,
  LogicalAnd,
  LogicalOr,
  LogicalLess,
  LogicalLessEqual,
  LogicalGreater,
  LogicalGreaterEqual,
  LogicalEnd = LogicalGreaterEqual,
};

llvm::StringRef ToString(BinaryOperatorKind);

struct BinaryExpr : public Expr {
  BinaryExpr(location loc, BinaryOperatorKind op, std::unique_ptr<Expr> lhs,
             std::unique_ptr<Expr> rhs, ValueTypeKind valueKind);
  BinaryExpr(location loc, BinaryOperatorKind op, std::unique_ptr<Expr> lhs,
             std::unique_ptr<Expr> rhs, std::unique_ptr<Type> newType,
             ValueTypeKind valueKind);
  virtual ~BinaryExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::BinaryExpr;
  }

  std::unique_ptr<Expr> Lhs;
  std::unique_ptr<Expr> Rhs;
  BinaryOperatorKind Op;
};

enum class UnaryOperatorKind {
  LogicalNegation,
  ArithmeticNegation,
  Dereference,
  AddressOf
};

llvm::StringRef ToString(UnaryOperatorKind);

struct UnaryExpr : public Expr {
  UnaryExpr(location loc, UnaryOperatorKind kind, std::unique_ptr<Expr> lhs,
            ValueTypeKind valueKind);
  UnaryExpr(location loc, UnaryOperatorKind kind, std::unique_ptr<Expr> lhs,
            std::unique_ptr<Type> newType, ValueTypeKind valueKind);
  virtual ~UnaryExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::UnaryExpr;
  }

  std::unique_ptr<Expr> Lhs;
  UnaryOperatorKind Op;
};

struct ExplicitCastExpr : public Expr {
  ExplicitCastExpr(location loc, std::unique_ptr<Type> toType,
                   std::unique_ptr<Expr> expr);
  virtual ~ExplicitCastExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::ExplicitCastExpr;
  }

  std::unique_ptr<Expr> ConvertingExpr;
};

enum ImplicitCastKind { LValueToRValue = 0, ToBool };

struct ImplicitCastExpr : public Expr {
  ImplicitCastExpr(location loc, std::unique_ptr<Expr> expr,
                   ImplicitCastKind kind, ValueTypeKind valueKind);
  virtual ~ImplicitCastExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::ImplicitCastExpr;
  }

  static std::unique_ptr<Expr> Create(location loc, ImplicitCastKind kind,
                                      std::unique_ptr<Expr> expr);

  std::unique_ptr<Expr> CastedExpr;
  ImplicitCastKind CastKind;
};

struct FunctionCallExpr : public Expr {
  FunctionCallExpr(location loc, const FuncDecl &funcDecl,
                   llvm::SmallVector<std::unique_ptr<Expr>, 4> params);
  virtual ~FunctionCallExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::FunctionCallExpr;
  }

  const FuncDecl &FunctionDecl;
  llvm::SmallVector<std::unique_ptr<Expr>, 6> Params;
};

struct MemberExpr : public Expr {
  MemberExpr(location loc, std::unique_ptr<Expr> expr,
             const TypeFieldDecl &decl);
  virtual ~MemberExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::MemberExpr;
  }

  unsigned getIdxAccesses() const;

  const TypeFieldDecl &Field;
  const std::unique_ptr<Expr> Accessed;
};

enum class TypeAccessKind { Unknown, Value, Pointer };
struct TypeAccess {
  TypeAccess() : Kind(TypeAccessKind::Unknown), Name("") {}

  TypeAccess(location loc, TypeAccessKind k, llvm::SmallString<10> name,
             std::vector<std::unique_ptr<Expr>> arrayAccesses)
      : Location(loc), Kind(k), Name(std::move(name)),
        ArrayAccesses(std::move(arrayAccesses)) {}

  location Location;
  TypeAccessKind Kind;
  llvm::SmallString<10> Name;
  std::vector<std::unique_ptr<Expr>> ArrayAccesses;
};

struct VarRefExpr : public Expr {
  VarRefExpr(location loc, const VarDecl &var);
  virtual ~VarRefExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::VarRefExpr;
  }

  const VarDecl &RefVar;
};

struct ArrayAccessExpr : public Expr {
  ArrayAccessExpr(location loc, std::unique_ptr<Expr> refVar,
                  std::unique_ptr<Type> type, std::unique_ptr<Expr> idx);
  virtual ~ArrayAccessExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::ArrayAccessExpr;
  }

  std::unique_ptr<Expr> RefVar;
  std::unique_ptr<Expr> Idx;
};

struct MemberCallExpr : public Expr {
  MemberCallExpr(location loc, const FuncDecl &funcDecl,
                 std::unique_ptr<Expr> memberExpr,
                 llvm::SmallVectorImpl<std::unique_ptr<Expr>> &params);
  virtual ~MemberCallExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::MemberCallExpr;
  }

  const FuncDecl &FunctionDecl;
  std::unique_ptr<Expr> Accessor;
  llvm::SmallVector<std::unique_ptr<Expr>, 6> Params;
};

struct AllocExpr : public Expr {
  AllocExpr(location loc, std::unique_ptr<Type> type,
            std::unique_ptr<Expr> size);
  virtual ~AllocExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::AllocExpr;
  }

  const std::unique_ptr<Type> AllocType;
  const std::unique_ptr<Expr> SizeToAlloc;
};

struct StringExpr : public Expr {
  StringExpr(location loc, llvm::SmallString<40> v);
  virtual ~StringExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::StringExpr;
  }

  llvm::SmallString<40> Value;
};

struct IntegerExpr : public Expr {
  IntegerExpr(location loc, long long v);
  virtual ~IntegerExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::IntegerExpr;
  }

  long long Value;
};

struct CharExpr : public Expr {
  CharExpr(location loc, char v);
  virtual ~CharExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::CharExpr;
  }

  char Value;
};

struct FloatingExpr : public Expr {
  FloatingExpr(location loc, long double v);
  virtual ~FloatingExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::FloatingExpr;
  }

  long double Value;
};

struct BooleanExpr : public Expr {
  BooleanExpr(location loc, bool v);
  virtual ~BooleanExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::BooleanExpr;
  }

  bool Value;
};

struct DereferenceExpr : public Expr {
  DereferenceExpr(location loc, std::unique_ptr<Expr> expr);
  virtual ~DereferenceExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::DereferenceExpr;
  }

  std::unique_ptr<Expr> DereferencingExpr;
};

struct AddressofExpr : public Expr {
  AddressofExpr(location loc, std::unique_ptr<Expr> expr);
  virtual ~AddressofExpr();

  static bool classof(const Expr *expr) {
    return expr->getKind() == ExprKind::AddressofExpr;
  }

  std::unique_ptr<Expr> AdressOfExpr;
};

} // namespace quark

#endif // __QUARK_FRONTEND_AST_EXPR_H__
