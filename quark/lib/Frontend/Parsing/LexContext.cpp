#include <quark/Frontend/Parsing/LexContext.h>

#include "SourceModuleCache.h"
#include <quark/Frontend/AST/Decl.h>
#include <quark/Frontend/AST/Expr.h>
#include <quark/Frontend/AST/Stmt.h>
#include <quark/Frontend/AST/Type.h>
#include <quark/Frontend/Parsing/ParserUtils.h>

#include "../../build/tools/quark/lib/Frontend/Parsing/QuarkParser.hpp"

#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>

#include <memory>

using namespace quark;

LexContext::LexContext(std::string &file)
    : Loc(&file), BuiltinTypes(SourceModule::BuiltinTypes),
      PImplCache(std::make_unique<SourceModuleCache>()) {}
LexContext::~LexContext() {}

static void ThrowSyntaxError(llvm::Twine twine, quark::location location) {
  throw quark::QuarkParser::syntax_error(location, twine.str());
}

const FuncDecl *LexContext::findFunctionDecl(const FuncDecl &funcDecl) const {
  for (const FuncDecl &rhsFunc : FunctionDecls) {
    if (rhsFunc == funcDecl) {
      return &rhsFunc;
    }
  }
  return nullptr;
}

const FuncDecl *LexContext::findFunctionDecl(
    const FuncDecl::FuncSignature &funcSignature) const {
  for (const FuncDecl &rhsFunc : FunctionDecls) {
    if (rhsFunc.Signature == funcSignature) {
      return &rhsFunc;
    }
  }
  return nullptr;
}

void LexContext::checkNonExistenceOfFunction(const FuncDecl &funcDecl) {
  if (findFunctionDecl(funcDecl))
    ThrowSyntaxError(
        llvm::Twine("function '") + funcDecl.Name + "' already defined!", Loc);
}

void LexContext::checkNonExistenceOfVar(llvm::StringRef id) {
  if (findVar(id))
    ThrowSyntaxError(llvm::Twine("var '") + id + "' already defined!", Loc);
}

void LexContext::checkNonExistenceOfVarCurrentLevel(llvm::StringRef id) {
  if (findVarCurrentLevel(id))
    ThrowSyntaxError(llvm::Twine("var '") + id + "' already defined!", Loc);
}

void LexContext::checkNonExistenceOfType(llvm::StringRef id) {
  if (findType(id))
    ThrowSyntaxError(llvm::Twine("type '") + id + "' already defined!", Loc);
}

const Type *LexContext::findBuiltinType(llvm::StringRef id) const {
  for (const BuiltinType &type : BuiltinTypes) {
    if (type.Name == id) {
      return &type;
    }
  }
  return nullptr;
}

const Type *LexContext::findType(llvm::StringRef id) const {
  if (const Type *type = findBuiltinType(id)) {
    return type;
  }

  for (const TypeDecl &type : TypeDecls) {
    if (type.Name == id) {
      return &type.Type;
    }
  }

  return nullptr;
}

const TypeDecl *LexContext::findTypeDecl(llvm::StringRef id) const {
  for (const TypeDecl &typeDecl : TypeDecls) {
    if (typeDecl.Name == id) {
      return &typeDecl;
    }
  }

  return nullptr;
}

const VarDecl *LexContext::findVarCurrentLevel(llvm::StringRef id) const {
  for (const VarDecl *varDecl : Scopes.back()) {
    if (varDecl->Name == id) {
      return varDecl;
    }
  }

  return nullptr;
}

const VarDecl *LexContext::findVar(llvm::StringRef id) const {
  for (const Scope &scope : llvm::reverse(Scopes)) {
    for (const VarDecl *varDecl : scope) {
      if (varDecl->Name == id) {
        return varDecl;
      }
    }
  }

  return nullptr;
}

const FuncDecl *LexContext::getFunctionDecl(const FuncDecl &funcDecl) const {
  const FuncDecl *funcDeclPtr = findFunctionDecl(funcDecl);
  if (!funcDeclPtr) {
    ThrowSyntaxError(
        llvm::Twine("function '") + funcDecl.Name + "' not defined!", Loc);
  }

  return funcDeclPtr;
}

const FuncDecl *LexContext::getFunctionDecl(
    const FuncDecl::FuncSignature &funcSignature) const {
  const FuncDecl *funcDecl = findFunctionDecl(funcSignature);
  if (!funcDecl) {
    std::string errorMsg;
    llvm::raw_string_ostream rso(errorMsg);
    funcSignature.print(rso);
    if (funcSignature.Reciver) {
      ThrowSyntaxError(llvm::Twine("method: '") + rso.str() + "' not defined!",
                       Loc);
    } else {
      ThrowSyntaxError(
          llvm::Twine("function: '") + rso.str() + "' not defined!", Loc);
    }
  }

  return funcDecl;
}

const Type *LexContext::getType(llvm::StringRef id) const {
  const Type *type = findType(id);
  if (!type) {
    ThrowSyntaxError(llvm::Twine("type '") + id + "' not found!", Loc);
  }

  return type;
}

const VarDecl *LexContext::getVarCurrentLevel(llvm::StringRef id) const {
  const VarDecl *varDecl = findVarCurrentLevel(id);
  if (!varDecl) {
    ThrowSyntaxError(llvm::Twine("variable '") + id + "' not defined!", Loc);
  }

  return varDecl;
}

const VarDecl *LexContext::getVar(llvm::StringRef id) const {
  const VarDecl *varDecl = findVar(id);
  if (!varDecl) {
    ThrowSyntaxError(llvm::Twine("variable '") + id + "' not defined!", Loc);
  }

  return varDecl;
}

const FuncDecl *
LexContext::LexContext::addFunctionDecl(const FuncDecl &funcDecl) {
  checkNonExistenceOfFunction(funcDecl);

  FunctionDecls.push_back(funcDecl);
  return &FunctionDecls.back().get();
}

const TypeDecl *LexContext::addTypeDecl(const TypeDecl &typeDecl) {
  checkNonExistenceOfType(typeDecl.Name);

  TypeDecls.push_back(typeDecl);
  return &TypeDecls.back().get();
}

const VarDecl *LexContext::addVar(const VarDecl &var) {
  checkNonExistenceOfVarCurrentLevel(var.Name);

  PImplCache->VarDeclStmtByFuncMap[CurrentFunc.get()].push_back(&var);
  Scopes.back().push_back(&var);
  return &var;
}

void LexContext::enterFunction(location loc, llvm::StringRef name) {
  CurrentFunc = std::make_unique<FuncDecl>(loc, name);
}

std::unique_ptr<FuncDecl> LexContext::exitFunction(
    location loc, llvm::SmallVector<std::unique_ptr<VarDecl>, 4> params,
    std::unique_ptr<Type> returnType, std::vector<std::unique_ptr<Stmt>> stmts,
    std::unique_ptr<VarDecl> reciver) {
  CurrentFunc->fillFunction(loc, std::move(params), std::move(returnType),
                            std::move(stmts), std::move(reciver));
  return std::move(CurrentFunc);
}

void LexContext::enterScope() { Scopes.emplace_back(); }

void LexContext::exitScope() { Scopes.pop_back(); }

std::unique_ptr<BinaryExpr>
LexContext::createLogicalBinaryExpr(location loc, BinaryOperatorKind op,
                                    std::unique_ptr<Expr> lhs,
                                    std::unique_ptr<Expr> rhs) const {
  return std::make_unique<BinaryExpr>(
      loc, op, AddCastIfNeeded(std::move(lhs)), AddCastIfNeeded(std::move(rhs)),
      std::make_unique<BuiltinType>(BuiltinTypeKind::b1),
      ValueTypeKind::RightValue);
}

std::unique_ptr<BinaryExpr>
LexContext::createArithmeticBinaryExpr(location loc, BinaryOperatorKind op,
                                       std::unique_ptr<Expr> lhs,
                                       std::unique_ptr<Expr> rhs) const {
  return std::make_unique<BinaryExpr>(loc, op, AddCastIfNeeded(std::move(lhs)),
                                      AddCastIfNeeded(std::move(rhs)),
                                      ValueTypeKind::RightValue);
}

std::unique_ptr<ReturnStmt>
LexContext::makeReturnStmt(location loc, std::unique_ptr<Expr> expr) {
  std::unique_ptr<ReturnStmt> retStmt;
  if (expr) {
    retStmt =
        std::make_unique<ReturnStmt>(loc, AddCastIfNeeded(std::move(expr)));
  } else {
    retStmt = std::make_unique<ReturnStmt>(loc);
  }
  PImplCache->RetStmtByFuncMap[CurrentFunc.get()].push_back(retStmt.get());
  return retStmt;
}

std::unique_ptr<Expr>
LexContext::castToBoolIfNeeded(std::unique_ptr<Expr> expr) {
  if (auto *builtin = llvm::dyn_cast<BuiltinType>(&expr->getType())) {
    if (!builtin->isBoolean()) {
      return ImplicitCastExpr::Create(expr->Location, ImplicitCastKind::ToBool,
                                      AddCastIfNeeded(std::move(expr)));
    }
  }
  return expr;
}