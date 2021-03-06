#ifndef __QUARK_FRONTEND_AST_STMT_H__
#define __QUARK_FRONTEND_AST_STMT_H__

#include <quark/Frontend/AST/Expr.h>
#include <quark/Frontend/AST/Node.h>

#include <llvm/ADT/Optional.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Casting.h>

#include <vector>

namespace quark {

/// Forward declarations
struct Decl;
struct VarDecl;
struct Expr;
struct VarRefExpr;

enum class StmtKind {
#define QK_STMT(ID) ID,
#include "ASTNodes.def"
};

struct Stmt : Node {
  Stmt(location loc, StmtKind kind) : Node(loc), Kind(kind) {}
  virtual ~Stmt() = 0;

  virtual void print(llvm::raw_ostream &out) const;
  virtual void dump() const;

  virtual StmtKind getKind() const { return Kind; }

  StmtKind Kind;
};

struct BlockStmt : public Stmt {
  BlockStmt(location loc, std::vector<std::unique_ptr<Stmt>> stmts)
      : Stmt(loc, StmtKind::BlockStmt), Stmts(std::move(stmts)) {}
  virtual ~BlockStmt();

  static bool classof(const Stmt *stmt) {
    return stmt->getKind() == StmtKind::BlockStmt;
  }

  std::vector<std::unique_ptr<Stmt>> Stmts;
};

struct VarDeclStmt : public Stmt {
  VarDeclStmt(location loc, std::unique_ptr<VarDecl> varDecl,
              std::unique_ptr<Expr> initExpr)
      : Stmt(loc, StmtKind::VarDeclStmt), VarDecl(std::move(varDecl)),
        InitExpr(std::move(initExpr)) {}
  virtual ~VarDeclStmt();

  static bool classof(const Stmt *stmt) {
    return stmt->getKind() == StmtKind::VarDeclStmt;
  }

  std::unique_ptr<VarDecl> VarDecl;
  std::unique_ptr<Expr> InitExpr;
};

struct ForStmt : public Stmt {
  ForStmt(location loc, std::unique_ptr<VarDeclStmt> varDecl,
          std::unique_ptr<Expr> cond, std::unique_ptr<Expr> inc,
          std::unique_ptr<Stmt> body, bool isParallel)
      : Stmt(loc, StmtKind::ForStmt), VarDecl(std::move(varDecl)),
        Cond(std::move(cond)), Inc(std::move(inc)), Body(std::move(body)),
        IsParallel(isParallel) {}
  virtual ~ForStmt();

  static bool classof(const Stmt *stmt) {
    return stmt->getKind() == StmtKind::ForStmt;
  }

  std::unique_ptr<VarDeclStmt> VarDecl;
  std::unique_ptr<Expr> Cond;
  std::unique_ptr<Expr> Inc;
  std::unique_ptr<Stmt> Body;
  bool IsParallel = false;
};

struct IfStmt : public Stmt {
  struct CondAndStmt {
    CondAndStmt() = default;
    CondAndStmt(CondAndStmt &&) = default;
    CondAndStmt &operator=(CondAndStmt &&) = default;
    CondAndStmt(location loc, std::unique_ptr<Expr> cond,
                std::unique_ptr<Stmt> stmt);

    location Location;
    std::unique_ptr<Expr> Cond;
    std::unique_ptr<Stmt> Stmt;
  };

  IfStmt(location loc, std::unique_ptr<Expr> cond, std::unique_ptr<Stmt> code,
         llvm::SmallVectorImpl<CondAndStmt> &&elsifs,
         std::unique_ptr<Stmt> elseCode = nullptr)
      : Stmt(loc, StmtKind::IfStmt), Cond(std::move(cond)),
        Code(std::move(code)), Elsifs(std::move(elsifs)),
        ElseCode(std::move(elseCode)) {}

  virtual ~IfStmt();

  static bool classof(const Stmt *stmt) {
    return stmt->getKind() == StmtKind::IfStmt;
  }

  std::unique_ptr<Expr> Cond;
  std::unique_ptr<Stmt> Code;
  llvm::SmallVector<CondAndStmt, 4> Elsifs;
  std::unique_ptr<Stmt> ElseCode;
};

struct WhileStmt : public Stmt {
  WhileStmt(location loc, std::unique_ptr<Expr> cond,
            std::unique_ptr<Stmt> code)
      : Stmt(loc, StmtKind::WhileStmt), Cond(std::move(cond)),
        Code(std::move(code)) {}
  virtual ~WhileStmt();

  static bool classof(const Stmt *stmt) {
    return stmt->getKind() == StmtKind::WhileStmt;
  }

  std::unique_ptr<Expr> Cond;
  std::unique_ptr<Stmt> Code;
};

struct ReturnStmt : public Stmt {
  ReturnStmt(location loc, std::unique_ptr<Expr> retExpr)
      : Stmt(loc, StmtKind::ReturnStmt), ReturnValue(std::move(retExpr)) {}
  ReturnStmt(location loc)
      : Stmt(loc, StmtKind::ReturnStmt), ReturnValue(nullptr) {}
  virtual ~ReturnStmt();

  static bool classof(const Stmt *stmt) {
    return stmt->getKind() == StmtKind::ReturnStmt;
  }

  std::unique_ptr<Expr> ReturnValue;
};

struct DeferStmt : public Stmt {
  DeferStmt(location loc, std::unique_ptr<Expr> exprToDefer)
      : Stmt(loc, StmtKind::DeferStmt), ExprToDefer(std::move(exprToDefer)) {}
  virtual ~DeferStmt();

  static bool classof(const Stmt *stmt) {
    return stmt->getKind() == StmtKind::DeferStmt;
  }

  std::unique_ptr<Expr> ExprToDefer;
};

struct DeallocStmt : public Stmt {
  DeallocStmt(location loc, std::unique_ptr<Expr> exprToDealloc)
      : Stmt(loc, StmtKind::DeallocStmt),
        ExprToDealloc(std::move(exprToDealloc)) {}
  virtual ~DeallocStmt();

  static bool classof(const Stmt *stmt) {
    return stmt->getKind() == StmtKind::DeallocStmt;
  }

  std::unique_ptr<Expr> ExprToDealloc;
};

struct ExprStmt : public Stmt {
  ExprStmt(location loc, std::unique_ptr<Expr> expr)
      : Stmt(loc, StmtKind::ExprStmt), Expr(std::move(expr)) {}
  virtual ~ExprStmt();

  static bool classof(const Stmt *stmt) {
    return stmt->getKind() == StmtKind::ExprStmt;
  }

  std::unique_ptr<Expr> Expr;
};

struct PrintStmt : public Stmt {
  PrintStmt(location loc, std::unique_ptr<Expr> string,
            llvm::SmallVector<std::unique_ptr<Expr>, 4> args)
      : Stmt(loc, StmtKind::PrintStmt), String(std::move(string)),
        Args(std::move(args)) {}
  virtual ~PrintStmt();

  static bool classof(const Stmt *stmt) {
    return stmt->getKind() == StmtKind::PrintStmt;
  }

  std::unique_ptr<Expr> String;
  llvm::SmallVector<std::unique_ptr<Expr>, 4> Args;
};

} // namespace quark

#endif // __QUARK_FRONTEND_AST_STMT_H__
