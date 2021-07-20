#ifndef __QUARK_LIB_FRONTEND_SOURCEMODULECACHE_H__
#define __QUARK_LIB_FRONTEND_SOURCEMODULECACHE_H__

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringMap.h>

namespace llvm {
class AllocaInst;
class StructType;
} // namespace llvm

namespace quark {

struct VarDecl;
struct ReturnStmt;
struct FuncDecl;

struct SourceModuleCache {
  SourceModuleCache() = default;
  ~SourceModuleCache() = default;

  llvm::DenseMap<const FuncDecl *, llvm::SmallVector<const VarDecl *, 8>>
      VarDeclStmtByFuncMap;

  llvm::DenseMap<const FuncDecl *, llvm::SmallVector<const ReturnStmt *, 8>>
      RetStmtByFuncMap;

  llvm::DenseMap<const VarDecl *, llvm::AllocaInst *> VarDeclToAllocaMap;

  llvm::StringMap<llvm::StructType *> CompoundTypeToLLVMTypeMap;
};

} // namespace quark

#endif // __QUARK_LIB_FRONTEND_SOURCEMODULECACHE_H__
