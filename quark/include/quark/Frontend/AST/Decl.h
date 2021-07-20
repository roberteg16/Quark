#ifndef __QUARK_FRONTEND_AST_DECL_H__
#define __QUARK_FRONTEND_AST_DECL_H__

#include <quark/Frontend/AST/Type.h>

#include <llvm/ADT/Optional.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/SmallVector.h>

#include <vector>

namespace quark {

/// Forward declarations
struct Stmt;
struct BlockStmt;

enum class DeclKind {
  VarDecl = 0,
  FuncDecl,
  TypeFieldDecl,
  TypeDecl,
  AliasTypeDecl
};

struct Decl {
  Decl(DeclKind kind, llvm::StringRef name)
      : Kind(kind), Name(std::move(name)) {}
  virtual ~Decl() = 0;

  virtual void print(llvm::raw_ostream &) const;
  virtual void dump() const;

  virtual DeclKind getKind() const { return Kind; }

  DeclKind Kind;
  llvm::SmallString<10> Name;
};

enum class QualifierKind { Mut };
enum class VarDeclKind { None, LocalVar, ParamVar, RecieverVar };

llvm::StringRef ToString(VarDeclKind kind);

struct VarDecl : public Decl {
  VarDecl(llvm::StringRef name, std::unique_ptr<Type> type)
      : Decl(DeclKind::VarDecl, name), Type(std::move(type)) {}
  virtual ~VarDecl();

  bool operator==(const VarDecl &) const;
  VarDeclKind getVarDeclKind() const {
    assert(Kind != VarDeclKind::None);
    return Kind;
  }

  static bool classof(const Decl *decl) {
    return decl->getKind() == DeclKind::VarDecl;
  }

  std::unique_ptr<Type> Type;

  friend class QuarkParser;

private:
  VarDeclKind Kind = VarDeclKind::None;
  void setKind(VarDeclKind kind) { Kind = kind; };
};

struct FuncDecl : public Decl {
  FuncDecl(llvm::StringRef name);
  virtual ~FuncDecl();

  struct FuncSignature {
    llvm::SmallString<20> Name;
    llvm::SmallVector<std::unique_ptr<Type>, 4> ParamTypes;
    std::unique_ptr<Type> Reciver;

    FuncSignature() = default;
    ~FuncSignature() = default;
    FuncSignature(llvm::StringRef name,
                  llvm::ArrayRef<std::unique_ptr<Expr>> params,
                  std::unique_ptr<Type> reciver = nullptr);

    bool operator==(const FuncSignature &) const;

    void print(llvm::raw_ostream &) const;
    void dump() const;
  };

  bool isMethod() { return Reciver.get(); }

  void fillFunction(llvm::SmallVector<std::unique_ptr<VarDecl>, 4> params,
                    std::unique_ptr<Type> returnType,
                    std::vector<std::unique_ptr<Stmt>> stmts,
                    std::unique_ptr<VarDecl> reciver = nullptr);

  bool operator==(const FuncDecl &) const;

  static bool classof(const Decl *decl) {
    return decl->getKind() == DeclKind::FuncDecl;
  }

  /// Params of the function
  llvm::SmallVector<std::unique_ptr<VarDecl>, 4> Params;

  /// Reciver, if any, of the function
  std::unique_ptr<VarDecl> Reciver;

  /// Body of the function
  std::vector<std::unique_ptr<Stmt>> Body;

  // Signature of the function
  FuncSignature Signature;

  // Type of the function
  FuncType FuncType;
};

struct TypeFieldDecl : public Decl {
  TypeFieldDecl(llvm::StringRef name, std::unique_ptr<Type> type)
      : Decl(DeclKind::TypeFieldDecl, name), Type(std::move(type)) {}
  TypeFieldDecl(TypeFieldDecl &&typeFieldDel) = default;
  TypeFieldDecl &operator=(TypeFieldDecl &&typeFieldDel) = default;
  virtual ~TypeFieldDecl();

  static bool classof(const Decl *decl) {
    return decl->getKind() == DeclKind::TypeFieldDecl;
  }

  std::unique_ptr<Type> Type;
};

struct TypeDecl : public Decl {
  TypeDecl(llvm::StringRef name,
           llvm::SmallVectorImpl<std::unique_ptr<TypeFieldDecl>> &&type)
      : Decl(DeclKind::TypeDecl, name), FieldDecls(std::move(type)),
        Type(*this) {}
  virtual ~TypeDecl();

  static bool classof(const Decl *decl) {
    return decl->getKind() == DeclKind::TypeDecl;
  }

  const TypeFieldDecl *findField(llvm::StringRef name) const;

  llvm::SmallVector<std::unique_ptr<TypeFieldDecl>, 4> FieldDecls;
  CompoundType Type;
};

struct AliasTypeDecl : public Decl {
  AliasTypeDecl(llvm::StringRef name, const Type &type)
      : Decl(DeclKind::AliasTypeDecl, name), RealType(type) {}
  virtual ~AliasTypeDecl();

  static bool classof(const Decl *decl) {
    return decl->getKind() == DeclKind::AliasTypeDecl;
  }

  const Type &RealType;
};

} // namespace quark

#endif // __QUARK_FRONTEND_AST_DECL_H__
