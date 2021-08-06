#ifndef __QUARK_FRONTEND_AST_ASTDUMPER_H__
#define __QUARK_FRONTEND_AST_ASTDUMPER_H__

#include <quark/Frontend/AST/ASTVisitor.h>
#include <quark/Support/IdentationFormat.h>

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/raw_ostream.h>

#include <vector>

namespace quark {

struct ASTDumper : public ASTVisitor {
  ASTDumper(llvm::raw_ostream &out)
      : Out(out), Holder(), RSO(Holder), Depth(0) {}
  virtual ~ASTDumper() {}

  void dump(const SourceModule &);
  void dump(const Decl &);
  void dump(const Stmt &);
  void dump(const Expr &);
  void dump(const Type &);

  void PreStmt() override;
  void PostStmt() override;
  void PreExpr() override;
  void PostExpr() override;
  void PreDecl() override;
  void PostDecl() override;

  void VisitSourceModule() override;
  void VisitExitSourceModule() override;
  void VisitExportModule(llvm::StringRef) override;
  void VisitImportModule(llvm::StringRef) override;

  void printLocation(const location &loc);
  void printLocationAndAddNodeToTree(const location &loc);

#define QK_ASTNODE(NodeKind)                                                   \
  virtual void Visit##NodeKind(const NodeKind &) override;
#include "ASTNodes.def"

  llvm::raw_ostream &Out;
  std::string Holder;
  llvm::raw_string_ostream RSO;
  std::vector<IdentationElement> Elements;
  std::size_t Depth;

  std::size_t CurrentLine = 1;
  std::string CurrentFile = "";
  bool PrintVerboseStmtHeaders = false;
};

} // namespace quark

#endif // __QUARK_FRONTEND_AST_ASTDUMPER_H__