#include <quark/Frontend/AST/ASTDumper.h>

#include <llvm/Support/raw_ostream.h>

using namespace quark;

static void PrintAll(llvm::ArrayRef<IdentationElement> elems,
                     llvm::raw_ostream &out) {
  ElementTreeIndentationFormat formater(elems);

  for (unsigned i = 0; i < elems.size(); i++) {
    formater.printFormat(out, i);
    out << elems[i].Str << "\n";
  }
}

void ASTDumper::printLocation(const location &loc) {
  if ((!loc.begin.filename || !loc.end.filename) ||
      (*loc.begin.filename != *loc.end.filename)) {
    RSO << "<invalid file>";
    return;
  }

  bool changedCurrentFile = false;
  std::string &currentFile = *loc.begin.filename;
  if (CurrentFile != currentFile) {
    CurrentFile = currentFile;
    changedCurrentFile = true;
    RSO << "<file:'" << CurrentFile << "' ";
  }

  const position &begin = loc.begin;
  const position &end = loc.end;

  const bool sameLine = begin.line == end.line;
  const bool sameCol = begin.column == end.column;
  const bool differentColByOne = begin.column == (end.column - 1);

  const bool newLine =
      (!sameLine) || (sameLine && static_cast<int>(CurrentLine) != begin.line);

  if (newLine) {
    if (sameLine) {
      RSO << "<line:" << begin.line;
      if (sameCol || differentColByOne) {
        RSO << " <col:" << begin.column << ">>";
      } else {
        RSO << " <col:" << begin.column << " col:" << end.column << ">>";
      }
    } else {
      RSO << "<line:" << begin.line << " col:" << begin.column << ">-";
      RSO << "<line:" << end.line << " col:" << end.column << ">";
    }
    CurrentLine = begin.line;
  } else {
    if (sameLine) {
      if (sameCol || differentColByOne) {
        RSO << "<col:" << begin.column << ">";
      } else {
        RSO << "<col:" << begin.column << " col:" << end.column << ">";
      }
    }
  }

  if (changedCurrentFile) {
    RSO << ">";
  }
}

void ASTDumper::printLocationAndAddNodeToTree(const location &loc) {
  RSO << ' ';
  printLocation(loc);
  Elements.emplace_back(RSO.str(), Depth);
  RSO.str().clear();
}

void ASTDumper::dump(const SourceModule &sm) {
  visit(sm);
  PrintAll(Elements, Out);
}

void ASTDumper::dump(const Decl &decl) {
  visit(decl);
  PrintAll(Elements, Out);
}

void ASTDumper::dump(const Stmt &stmt) {
  visit(stmt);
  PrintAll(Elements, Out);
}

void ASTDumper::dump(const Expr &expr) {
  visit(expr);
  PrintAll(Elements, Out);
}

void ASTDumper::dump(const Type &type) {
  visit(type);
  Elements.emplace_back(RSO.str(), Depth);
  PrintAll(Elements, Out);
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
  printLocationAndAddNodeToTree(decl.Location);
}

void ASTDumper::VisitTypeDecl(const TypeDecl &decl) {
  RSO << "TypeDecl: " << decl.Name;
  printLocationAndAddNodeToTree(decl.Location);
}

void ASTDumper::VisitTypeFieldDecl(const TypeFieldDecl &decl) {
  RSO << "TypeFieldDecl: " << decl.Name << ' ';
  visit(*decl.Type);
  printLocationAndAddNodeToTree(decl.Location);
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
  printLocationAndAddNodeToTree(decl.Location);
}

void ASTDumper::VisitAliasTypeDecl(const AliasTypeDecl &decl) {}

void ASTDumper::VisitBlockStmt(const BlockStmt &stmt) {
  RSO << "BlockStmt:";
  printLocationAndAddNodeToTree(stmt.Location);
}

void ASTDumper::VisitDeallocStmt(const DeallocStmt &stmt) {
  RSO << "DeallocStmt:";
  printLocationAndAddNodeToTree(stmt.Location);
}

void ASTDumper::VisitDeferStmt(const DeferStmt &stmt) {
  RSO << "DeferStmt:";
  printLocationAndAddNodeToTree(stmt.Location);
}

void ASTDumper::VisitExprStmt(const ExprStmt &stmt) {
  RSO << "ExprStmt:";
  printLocationAndAddNodeToTree(stmt.Location);
}

void ASTDumper::VisitForStmt(const ForStmt &stmt) {
  RSO << "ForStmt: " << (stmt.IsParallel ? "ParLoop" : "SeqLoop");
  printLocationAndAddNodeToTree(stmt.Location);
}

void ASTDumper::VisitIfStmt(const IfStmt &stmt) {
  RSO << "IfStmt:";
  printLocationAndAddNodeToTree(stmt.Location);
}

void ASTDumper::VisitReturnStmt(const ReturnStmt &stmt) {
  RSO << "RetStmt:";
  if (!stmt.ReturnValue) {
    RSO << " void";
  }
  printLocationAndAddNodeToTree(stmt.Location);
}

void ASTDumper::VisitVarDeclStmt(const VarDeclStmt &stmt) {
  RSO << "VarDeclStmt:";
  printLocationAndAddNodeToTree(stmt.Location);
}

void ASTDumper::VisitWhileStmt(const WhileStmt &stmt) {
  RSO << "WhileStmt:";
  printLocationAndAddNodeToTree(stmt.Location);
}

void ASTDumper::VisitPrintStmt(const PrintStmt &stmt) {
  RSO << "PrintStmt:";
  printLocationAndAddNodeToTree(stmt.Location);
}

void ASTDumper::VisitAllocExpr(const AllocExpr &expr) {
  RSO << "AllocExpr: ";
  visit(*expr.AllocType);
  printLocationAndAddNodeToTree(expr.Location);
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
  printLocationAndAddNodeToTree(expr.Location);
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
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitMemberExpr(const MemberExpr &expr) {
  RSO << "MemberExpr: '";
  RSO << expr.Field.Name << "' ";
  visit(*expr.ExprType);
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitStringExpr(const StringExpr &expr) {
  RSO << "StringExpr: ";
  visit(*expr.ExprType);
  RSO << " \"" << expr.Value << "\"";
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitIntegerExpr(const IntegerExpr &expr) {
  RSO << "IntegerExpr: ";
  visit(*expr.ExprType);
  RSO << " " << expr.Value;
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitFloatingExpr(const FloatingExpr &expr) {
  RSO << "FloatingExpr: ";
  visit(*expr.ExprType);
  RSO << " " << (double)expr.Value;
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitCharExpr(const CharExpr &expr) {
  RSO << "CharExpr: ";
  visit(*expr.ExprType);
  RSO << " '" << expr.Value << "'";
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitVarRefExpr(const VarRefExpr &expr) {
  RSO << "VarRefExpr: ";
  visit(*expr.ExprType);
  RSO << " " << expr.RefVar.Name;
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitBinaryExpr(const BinaryExpr &expr) {
  RSO << "BinaryExpr: ";
  visit(*expr.ExprType);
  RSO << " (" << ToString(expr.Op) << ")";
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitUnaryExpr(const UnaryExpr &expr) {
  RSO << "UnaryExpr: ";
  visit(*expr.ExprType);
  RSO << " (" << ToString(expr.Op) << ")";
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitAddressofExpr(const AddressofExpr &expr) {
  RSO << "AddressofExpr: ";
  visit(*expr.ExprType);
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitDereferenceExpr(const DereferenceExpr &expr) {
  RSO << "DereferenceExpr: ";
  visit(*expr.ExprType);
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitArrayAccessExpr(const ArrayAccessExpr &expr) {
  RSO << "ArrayAccessExpr: ";
  visit(*expr.ExprType);
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitExplicitCastExpr(const ExplicitCastExpr &expr) {
  RSO << "ExplicitCastExpr: ";
  visit(*expr.ExprType);
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitImplicitCastExpr(const ImplicitCastExpr &expr) {
  RSO << "ImplicitCastExpr: ";
  RSO << (expr.CastKind == ImplicitCastKind::LValueToRValue ? "LValueToRValue"
                                                            : "BoolCasting");
  printLocationAndAddNodeToTree(expr.Location);
}

void ASTDumper::VisitBooleanExpr(const BooleanExpr &expr) {
  RSO << "BooleanExpr: ";
  visit(*expr.ExprType);
  RSO << " " << (expr.Value ? "true" : "false");
  printLocationAndAddNodeToTree(expr.Location);
}

#define QK_TYPE(ID)                                                            \
  void ASTDumper::Visit##ID(const ID &ID) { ID.print(RSO); }
#include <quark/Frontend/AST/ASTNodes.def>
