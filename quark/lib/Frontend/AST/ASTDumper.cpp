#include "quark/Frontend/AST/Expr.h"
#include "quark/Frontend/AST/Type.h"
#include <quark/Frontend/AST/ASTDumper.h>

using namespace quark;

static void printAll(llvm::ArrayRef<IdentationElement> elems,
                     llvm::raw_ostream &out) {
  ElementTreeIndentationFormat formater(elems);

  for (unsigned i = 0; i < elems.size(); i++) {
    formater.printFormat(out, i);
    out << elems[i].Str << "\n";
  }
}

void ASTDumper::dump(const SourceModule &sm) {
  visit(sm);
  printAll(Elements, Out);
}

void ASTDumper::dump(const Decl &decl) {
  visit(decl);
  printAll(Elements, Out);
}

void ASTDumper::dump(const Stmt &stmt) {
  visit(stmt);
  printAll(Elements, Out);
}

void ASTDumper::dump(const Expr &expr) {
  visit(expr);
  printAll(Elements, Out);
}

void ASTDumper::dump(const Type &type) {
  visit(type);
  Elements.emplace_back(RSO.str(), Depth);
  printAll(Elements, Out);
}

void ASTDumper::PreStmt() { Depth++; }
void ASTDumper::PostStmt() { Depth--; }
void ASTDumper::PreExpr() { Depth++; }
void ASTDumper::PostExpr() { Depth--; }
void ASTDumper::PreDecl() { Depth++; }
void ASTDumper::PostDecl() { Depth--; }

void ASTDumper::VisitSourceModule() {
  RSO << "SourceModule: ";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitExitSourceModule() { Depth--; }

void ASTDumper::VisitExportModule(llvm::StringRef exported) {
  Depth++;
  RSO << "export: " << exported;
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
  Depth--;
}

void ASTDumper::VisitImportModule(llvm::StringRef import) {
  Depth++;
  RSO << "import: " << import;
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
  Depth--;
}

void ASTDumper::VisitVarDecl(const VarDecl &decl) {
  RSO << "VarDecl: (" << ToString(decl.getVarDeclKind()) << ") " << decl.Name
      << ' ';
  visit(*decl.Type);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitTypeDecl(const TypeDecl &decl) {
  RSO << "TypeDecl: " << decl.Name;
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitTypeFieldDecl(const TypeFieldDecl &decl) {
  RSO << "TypeFieldDecl: " << decl.Name << ' ';
  visit(*decl.Type);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitFuncDecl(const FuncDecl &decl) {
  if (decl.Reciver) {
    RSO << "MethodDecl: (";
    visit(*decl.Reciver->Type);
    RSO << ") " << decl.Name << ": ( ";
  } else {
    RSO << "FuncDecl: " << decl.Name << ": ( ";
  }
  for (unsigned i = 0; i < decl.Params.size(); i++) {
    RSO << decl.Params[i]->Name << " ";
    visit(*decl.Params[i]->Type);
    RSO << (i == decl.Params.size() - 1 ? "" : ", ");
  }
  RSO << " ) -> ";
  visit(*decl.FuncType.RetType);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitAliasTypeDecl(const AliasTypeDecl &decl) {}

void ASTDumper::VisitBlockStmt(const BlockStmt &blockStmt) {
  RSO << "BlockStmt:";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitDeallocStmt(const DeallocStmt &stmt) {
  RSO << "DeallocStmt:";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitDeferStmt(const DeferStmt &stmt) {
  RSO << "DeferStmt:";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitExprStmt(const ExprStmt &stmt) {
  RSO << "ExprStmt:";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitForStmt(const ForStmt &stmt) {
  RSO << "ForStmt:";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitIfStmt(const IfStmt &stmt) {
  RSO << "IfStmt:";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitReturnStmt(const ReturnStmt &stmt) {
  RSO << "RetStmt:";
  if (!stmt.ReturnValue) {
    RSO << " void";
  }
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitVarDeclStmt(const VarDeclStmt &stmt) {
  RSO << "VarDeclStmt:";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitWhileStmt(const WhileStmt &stmt) {
  RSO << "WhileStmt:";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitPrintStmt(const PrintStmt &stmt) {
  RSO << "PrintStmt:";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitAllocExpr(const AllocExpr &expr) {
  RSO << "AllocExpr: ";
  visit(*expr.AllocType);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
  visit(*expr.SizeToAlloc);
}

void ASTDumper::VisitFunctionCallExpr(const FunctionCallExpr &expr) {
  RSO << "FunctionCallExpr: " << expr.FunctionDecl.Name << " (";
  for (std::size_t i = 0; i < expr.FunctionDecl.Params.size(); i++) {
    if (i == expr.FunctionDecl.Params.size() - 1) {
      visit(*expr.FunctionDecl.Params[i]->Type);
    } else {
      visit(*expr.FunctionDecl.Params[i]->Type);
      RSO << ", ";
    }
  }
  RSO << ") -> ";
  visit(*expr.ExprType);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitMemberCallExpr(const MemberCallExpr &expr) {
  RSO << "MemberCallExpr: (rec: ";
  visit(expr.Accessor->getType());
  RSO << ") " << expr.FunctionDecl.Name << " (";
  for (std::size_t i = 0; i < expr.FunctionDecl.Params.size(); i++) {
    if (i == expr.FunctionDecl.Params.size() - 1) {
      visit(*expr.FunctionDecl.Params[i]->Type);
    } else {
      visit(*expr.FunctionDecl.Params[i]->Type);
      RSO << ", ";
    }
  }
  RSO << ") -> ";
  visit(*expr.ExprType);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitMemberExpr(const MemberExpr &expr) {
  RSO << "MemberExpr: '";
  RSO << expr.Field.Name << "' ";
  visit(*expr.ExprType);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitStringExpr(const StringExpr &expr) {
  RSO << "StringExpr: ";
  visit(*expr.ExprType);
  RSO << " \"" << expr.Value << "\"";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitIntegerExpr(const IntegerExpr &expr) {
  RSO << "IntegerExpr: ";
  visit(*expr.ExprType);
  RSO << " " << expr.Value;
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitFloatingExpr(const FloatingExpr &expr) {
  RSO << "FloatingExpr: ";
  visit(*expr.ExprType);
  RSO << " " << (double)expr.Value;
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitCharExpr(const CharExpr &expr) {
  RSO << "CharExpr: ";
  visit(*expr.ExprType);
  RSO << " '" << expr.Value << "'";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitVarRefExpr(const VarRefExpr &expr) {
  RSO << "VarRefExpr: ";
  visit(*expr.ExprType);
  RSO << " " << expr.RefVar.Name;
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitBinaryExpr(const BinaryExpr &expr) {
  RSO << "BinaryExpr: ";
  visit(*expr.ExprType);
  RSO << " (" << ToString(expr.Op) << ")";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitUnaryExpr(const UnaryExpr &expr) {
  RSO << "UnaryExpr: ";
  visit(*expr.ExprType);
  RSO << " (" << ToString(expr.Op) << ")";
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitAddressofExpr(const AddressofExpr &expr) {
  RSO << "AddressofExpr: ";
  visit(*expr.ExprType);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitDereferenceExpr(const DereferenceExpr &expr) {
  RSO << "DereferenceExpr: ";
  visit(*expr.ExprType);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitArrayAccessExpr(const ArrayAccessExpr &expr) {
  RSO << "ArrayAccessExpr: ";
  visit(*expr.ExprType);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitExplicitCastExpr(const ExplicitCastExpr &expr) {
  RSO << "ExplicitCastExpr: ";
  visit(*expr.ExprType);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitImplicitCastExpr(const ImplicitCastExpr &expr) {
  RSO << "ImplicitCastExpr: ";
  RSO << (expr.CastKind == ImplicitCastKind::LValueToRValue ? "LValueToRValue"
                                                            : "BoolCasting");
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitBooleanExpr(const BooleanExpr &expr) {
  RSO << "BooleanExpr: ";
  visit(*expr.ExprType);
  RSO << " " << (expr.Value ? "true" : "false");
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::VisitBuiltinType(const BuiltinType &builtinType) {
  builtinType.print(RSO);
}

void ASTDumper::VisitAliasType(const AliasType &aliasType) {
  aliasType.print(RSO);
}

void ASTDumper::VisitCompoundType(const CompoundType &compoundType) {
  compoundType.print(RSO);
}

void ASTDumper::VisitPtrType(const PtrType &ptrType) { ptrType.print(RSO); }

void ASTDumper::VisitArrayType(const ArrayType &arrayType) {
  arrayType.print(RSO);
}

void ASTDumper::VisitFuncType(const FuncType &funcType) { funcType.print(RSO); }
