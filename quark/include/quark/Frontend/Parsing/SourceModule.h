#ifndef __QUARK_FRONTEND_SOURCEMODULE_H__
#define __QUARK_FRONTEND_SOURCEMODULE_H__

#include <quark/Frontend/AST/Decl.h>
#include <quark/Frontend/AST/Expr.h>
#include <quark/Frontend/AST/Stmt.h>
#include <quark/Frontend/AST/Type.h>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/raw_ostream.h>

#include <deque>

namespace llvm {
class AllocaInst;
}

namespace quark {

struct SourceModuleCache;

class SourceModule {
public:
  SourceModule();
  ~SourceModule();
  SourceModule(SourceModule &&);
  SourceModule &operator=(SourceModule &&);
  SourceModule(const SourceModule &) = delete;
  SourceModule &operator=(const SourceModule &) = delete;

  void print(llvm::raw_ostream &) const;
  void dump() const;

  llvm::ArrayRef<const VarDecl *> getVarDeclStmtsForFunc(const FuncDecl &);
  llvm::ArrayRef<const ReturnStmt *> getReturnStmtsForFunc(const FuncDecl &);
  llvm::AllocaInst &getAllocaForVarDecl(const VarDecl &);
  llvm::StructType &getLLVMTypeForCompoundType(llvm::StringRef id);

  void addAllocaForVarDecl(const VarDecl &, llvm::AllocaInst &);
  void addLLVMTypeForCompoundType(llvm::StructType &, llvm::StringRef);

  void fill(LexContext &, llvm::SmallString<10>,
            llvm::SmallVector<llvm::SmallString<10>, 10>,
            std::deque<std::unique_ptr<Decl>>);

public:
  /// Exported module
  llvm::SmallString<10> ExportedModule;

  /// Imported module
  llvm::SmallVector<llvm::SmallString<10>, 10> ImportedModules;

  /// Declarations
  std::deque<std::unique_ptr<Decl>> Declarations;

  /// Builtin types
  static const BuiltinType BuiltinTypes[19];

  /// Builtin functions
  // static const FuncDecl BuiltinFuncs[1];

  /// Cache with usefull information
  std::unique_ptr<SourceModuleCache> PImplCache;
};

} // namespace quark

#endif // __QUARK_FRONTEND_SOURCEMODULE_H__
