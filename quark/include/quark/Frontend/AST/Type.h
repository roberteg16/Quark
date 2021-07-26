#ifndef __QUARK_FRONTEND_AST_TYPE_H__
#define __QUARK_FRONTEND_AST_TYPE_H__

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>

namespace quark {

struct TypeDecl;
struct Expr;

enum class TypeKind {
#define QK_TYPE(ID) ID,
#include "ASTNodes.def"
};

struct Type {
  Type(TypeKind kind) : TKind(kind) {}
  virtual ~Type() = 0;

  virtual void print(llvm::raw_ostream &) const = 0;
  virtual void dump() const = 0;

  virtual std::unique_ptr<Type> clone() const = 0;
  virtual const Type &desugar() const = 0;
  virtual const Type &innerType() const = 0;

  virtual TypeKind getKind() const { return TKind; }

  virtual bool operator==(const Type &) const = 0;
  virtual bool operator!=(const Type &rhs) const { return !(*this == rhs); }

  TypeKind TKind;
};

struct NamedType : Type {
  virtual ~NamedType() = 0;
  llvm::SmallString<20> Name;

protected:
  NamedType(TypeKind kind, llvm::StringRef name) : Type(kind), Name(name) {}
};

enum class BuiltinTypeKind {
#define QK_BUILTIN_TYPE(TYPE, NAME, BYTES, SIGNED) TYPE,
#include "BuiltinTypes.def"
};

auto GetTypeName(BuiltinTypeKind type) -> llvm::StringRef;
auto GetBytes(BuiltinTypeKind type) -> std::size_t;
auto IsSigned(BuiltinTypeKind type) -> bool;
auto IsUnsigned(BuiltinTypeKind type) -> bool;
auto IsFloatingPoint(BuiltinTypeKind type) -> bool;
auto IsInteger(BuiltinTypeKind type) -> bool;
auto IsVector(BuiltinTypeKind type) -> bool;
auto IsBoolean(BuiltinTypeKind type) -> bool;

struct BuiltinType : public NamedType {
  BuiltinType(BuiltinTypeKind kind)
      : NamedType(TypeKind::BuiltinType, GetTypeName(kind)), Kind(kind) {}
  virtual ~BuiltinType() {}

  auto bytes() const -> std::size_t;
  auto isSigned() const -> bool;
  auto isUnsigned() const -> bool;
  auto isFloatingPoint() const -> bool;
  auto isInteger() const -> bool;
  auto isVoid() const -> bool;
  auto isVector() const -> bool;
  auto isBoolean() const -> bool;

  std::unique_ptr<Type> clone() const override;
  const Type &desugar() const override;
  const Type &innerType() const override;

  void print(llvm::raw_ostream &) const override;
  void dump() const override;

  bool operator==(const Type &) const override;

  static bool classof(const Type *t) { // NOLINT
    return t->getKind() == TypeKind::BuiltinType;
  }

  BuiltinTypeKind Kind;
};

struct AliasType : public NamedType {
  AliasType(llvm::StringRef name, const Type &type)
      : NamedType(TypeKind::AliasType, name), RealType(type) {}
  virtual ~AliasType() {}

  std::unique_ptr<Type> getRealType() const;

  std::unique_ptr<Type> clone() const override;
  const Type &desugar() const override;
  const Type &innerType() const override;

  void print(llvm::raw_ostream &) const override;
  void dump() const override;

  bool operator==(const Type &) const override;

  static bool classof(const Type *t) { // NOLINT
    return t->getKind() == TypeKind::AliasType;
  }

  const Type &RealType;
};

struct CompoundType : public NamedType {
  CompoundType(const TypeDecl &typeDecl);
  virtual ~CompoundType() {}

  std::unique_ptr<Type> clone() const override;
  const Type &desugar() const override;
  const Type &innerType() const override;

  void print(llvm::raw_ostream &) const override;
  void dump() const override;

  bool operator==(const Type &) const override;

  static bool classof(const Type *t) { // NOLINT
    return t->getKind() == TypeKind::CompoundType;
  }

  llvm::SmallVector<std::unique_ptr<Type>, 4> Types;
  const TypeDecl &Decl;
};

struct PtrType : Type {
  PtrType(std::unique_ptr<Type> pointeeType)
      : Type(TypeKind::PtrType), PointeeType(std::move(pointeeType)) {}
  virtual ~PtrType() {}

  std::unique_ptr<Type> clone() const override;
  const Type &desugar() const override;
  const Type &innerType() const override;

  void print(llvm::raw_ostream &) const override;
  void dump() const override;

  bool operator==(const Type &) const override;

  static bool classof(const Type *t) { // NOLINT
    return t->getKind() == TypeKind::PtrType;
  }

  const std::unique_ptr<Type> PointeeType;
};

struct ArrayType : Type {
  ArrayType(std::unique_ptr<Type> type, std::size_t n)
      : Type(TypeKind::ArrayType), RealType(std::move(type)), Elements(n) {}
  virtual ~ArrayType() {}

  std::unique_ptr<Type> clone() const override;
  const Type &desugar() const override;
  const Type &innerType() const override;

  void print(llvm::raw_ostream &) const override;
  void dump() const override;

  bool operator==(const Type &) const override;

  static bool classof(const Type *t) { // NOLINT
    return t->getKind() == TypeKind::ArrayType;
  }

  std::unique_ptr<Type> RealType;
  const std::size_t Elements;
};

struct FuncType : Type {
  FuncType() : Type(TypeKind::FuncType) {}
  FuncType(std::unique_ptr<Type> retType,
           llvm::SmallVectorImpl<std::unique_ptr<Type>> &params,
           std::unique_ptr<Type> reciver = nullptr)
      : Type(TypeKind::FuncType), RetType(std::move(retType)),
        Reciver(std::move(reciver)), Params(std::move(params)) {}
  virtual ~FuncType() {}

  std::unique_ptr<Type> clone() const override;
  const Type &desugar() const override;
  const Type &innerType() const override;

  void print(llvm::raw_ostream &) const override;
  void dump() const override;

  bool operator==(const Type &) const override;

  static bool classof(const Type *t) { // NOLINT
    return t->getKind() == TypeKind::FuncType;
  }

  static std::unique_ptr<FuncType>
  create(llvm::ArrayRef<std::unique_ptr<Expr>>, std::unique_ptr<Type> retType,
         std::unique_ptr<Type> reciver = nullptr);

  std::unique_ptr<Type> RetType;
  std::unique_ptr<Type> Reciver;
  llvm::SmallVector<std::unique_ptr<Type>> Params;
};

const CompoundType *CheckCompoundOrTypeToCompound(const Type *type);

} // namespace quark

#endif // __QUARK_FRONTEND_AST_TYPE_H__
