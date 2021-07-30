#ifndef __QUARK_FRONTEND_CODEGEN_CODEGEN_H__
#define __QUARK_FRONTEND_CODEGEN_CODEGEN_H__

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#include <quark/Frontend/CodeGen/QuarkContext.h>
#include <quark/Frontend/Parsing/SourceModule.h>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

namespace llvm {
class Function;
} // namespace llvm

namespace quark {

struct CodeGen {
  CodeGen(llvm::StringRef file, SourceModule &, QuarkContext &ctx);
  virtual ~CodeGen();

  std::unique_ptr<llvm::Module> generate();

  struct CodeGenContext {
    llvm::SmallVector<bool> StopEmmiting = {false};
    llvm::BasicBlock *RetsBB;
    llvm::AllocaInst *RetAlloca;
    llvm::Function *CurrFuncLLVM;
    llvm::DenseMap<const FuncDecl *, llvm::FunctionCallee> DeclToFuncCalleeMap;
    bool IsInOutlineFunction = {false};
    llvm::DenseMap<const VarDecl *, llvm::AllocaInst *> OutlinedVarDeclToAlloca;
    llvm::SmallVector<const llvm::Value *, 4> DeferedValues;

    void reset();
  };

private:
  void emitBlock(llvm::BasicBlock &bb);
  void emitConditionalBlock(llvm::BasicBlock &condBB, llvm::BasicBlock &trueBB,
                            llvm::BasicBlock &falseBB, quark::Expr &cond);
  void emitPossiblyRetStmtBlock(llvm::BasicBlock &offenderBB,
                                llvm::BasicBlock &normalFlowBB,
                                llvm::BasicBlock &retFlowBB, quark::Stmt &stmt);

  void emitStmt(const Stmt &);
  void emitDecl(const Decl &);

  void emitFuncDecl(const FuncDecl &);
  void emitTypeDecl(const TypeDecl &);
  void emitForLoop(const ForStmt &);
  void emitParallelForLoop(const ForStmt &);

  void
  emitOutlinedParallelLoopFunc(const ForStmt &parallelFor,
                               llvm::ArrayRef<const VarDecl *> varsUsed,
                               llvm::ArrayRef<const VarDecl *> varDeclared);

  llvm::Value *getExpr(const Expr &);

  QuarkContext &Ctx;
  llvm::IRBuilder<> IRBuilder;
  SourceModule &SM;
  std::unique_ptr<llvm::Module> Mod;
  CodeGenContext CGCtx;
};

} // namespace quark

#endif // __QUARK_FRONTEND_CODEGEN_CODEGEN_H__