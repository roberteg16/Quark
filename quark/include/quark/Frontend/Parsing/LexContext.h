#ifndef __QUARK_FRONTEND_PARSING_LEXCONTEXT_H__
#define __QUARK_FRONTEND_PARSING_LEXCONTEXT_H__

#include <quark/Frontend/AST/Decl.h>
#include <quark/Frontend/AST/Expr.h>
#include <quark/Frontend/AST/Type.h>
#include <quark/Frontend/Parsing/SourceModule.h>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/raw_ostream.h>

#include <../../build/tools/quark/lib/Frontend/Parsing/SourceLoc.h>

namespace quark {

struct NamedType;
struct Type;

using Scope = llvm::SmallVector<const VarDecl *, 5>;

struct LexContext {
  quark::location Loc;
  std::size_t line = 1;
  std::size_t col = 1;

  LexContext(std::string &file);
  ~LexContext();

  const FuncDecl *findFunctionDecl(const FuncDecl &funcDecl) const;
  const FuncDecl *
  findFunctionDecl(const FuncDecl::FuncSignature &funcSig) const;
  const Type *findBuiltinType(llvm::StringRef id) const;
  const Type *findType(llvm::StringRef id) const;
  const TypeDecl *findTypeDecl(llvm::StringRef id) const;
  const VarDecl *findVarCurrentLevel(llvm::StringRef id) const;
  const VarDecl *findVar(llvm::StringRef id) const;

  const FuncDecl *getFunctionDecl(const FuncDecl &funcDecl) const;
  const FuncDecl *getFunctionDecl(const FuncDecl::FuncSignature &funcSig) const;
  const Type *getType(llvm::StringRef id) const;
  const VarDecl *getVarCurrentLevel(llvm::StringRef id) const;
  const VarDecl *getVar(llvm::StringRef id) const;

  void checkNonExistenceOfFunction(const FuncDecl &funcDecl);
  void checkNonExistenceOfVar(llvm::StringRef id);
  void checkNonExistenceOfVarCurrentLevel(llvm::StringRef id);
  void checkNonExistenceOfType(llvm::StringRef id);

  const FuncDecl *addFunctionDecl(const FuncDecl &);
  const TypeDecl *addTypeDecl(const TypeDecl &);
  const VarDecl *addVar(const VarDecl &var);

  void enterFunction(location loc, llvm::StringRef name);
  std::unique_ptr<FuncDecl>
  exitFunction(location loc,
               llvm::SmallVector<std::unique_ptr<VarDecl>, 4> params,
               std::unique_ptr<Type> returnType,
               std::vector<std::unique_ptr<Stmt>> stmts,
               std::unique_ptr<VarDecl> reciver);

  void enterScope();
  void exitScope();

  std::unique_ptr<BinaryExpr>
  createLogicalBinaryExpr(location loc, BinaryOperatorKind op,
                          std::unique_ptr<Expr> lhs,
                          std::unique_ptr<Expr> rhs) const;
  std::unique_ptr<BinaryExpr>
  createArithmeticBinaryExpr(location loc, BinaryOperatorKind op,
                             std::unique_ptr<Expr> lhs,
                             std::unique_ptr<Expr> rhs) const;
  std::unique_ptr<ReturnStmt> makeReturnStmt(location loc,
                                             std::unique_ptr<Expr> expr);
  std::unique_ptr<Expr> castToBoolIfNeeded(std::unique_ptr<Expr> expr);

private:
  /// References to real storage
  std::vector<std::reference_wrapper<const TypeDecl>> TypeDecls;
  std::vector<std::reference_wrapper<const FuncDecl>> FunctionDecls;

  /// Builtin types
  llvm::ArrayRef<BuiltinType> BuiltinTypes;

  /// Scopes
  llvm::SmallVector<Scope, 10> Scopes;

  /// Current function being built
  std::unique_ptr<FuncDecl> CurrentFunc;

  friend class SourceModule;
  /// Cache with usefull information, forwarded to SourceModule
  std::unique_ptr<SourceModuleCache> PImplCache;
};

} // namespace quark

#endif // __QUARK_FRONTEND_PARSING_LEXCONTEXT_H__
