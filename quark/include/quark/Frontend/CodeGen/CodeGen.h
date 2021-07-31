#ifndef __QUARK_FRONTEND_CODEGEN_CODEGEN_H__
#define __QUARK_FRONTEND_CODEGEN_CODEGEN_H__

#include "quark/Frontend/AST/Expr.h"
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

struct ParallelForAuxVars {
  llvm::AllocaInst *GTID;
  llvm::AllocaInst *BTID;
  llvm::AllocaInst *IterationVariable;
  llvm::AllocaInst *LbOMP;
  llvm::AllocaInst *UbOMP;
  llvm::AllocaInst *StrideOMP;
  llvm::AllocaInst *IsLastOMP;
  llvm::AllocaInst *CaptureExpr;
  llvm::AllocaInst *CaptureExpr1;

  llvm::AllocaInst *ArtificialVarDeclIterator;

  ParallelForAuxVars(llvm::IRBuilder<> &irBuilder, llvm::Type &itVarType) {
    llvm::LLVMContext &llvmContext = irBuilder.getContext();
    auto *i32PtrType = llvm::Type::getInt32Ty(llvmContext);
    GTID = irBuilder.CreateAlloca(i32PtrType->getPointerTo(), nullptr,
                                  "globalThreadID.addr");
    BTID = irBuilder.CreateAlloca(i32PtrType->getPointerTo(), nullptr,
                                  "boundThreadID.addr");

    IterationVariable = irBuilder.CreateAlloca(&itVarType, nullptr, "omp.iv");
    LbOMP = irBuilder.CreateAlloca(&itVarType, nullptr, "omp.lowerBound");
    UbOMP = irBuilder.CreateAlloca(&itVarType, nullptr, "omp.upperBound");
    StrideOMP = irBuilder.CreateAlloca(&itVarType, nullptr, "omp.stride");
    IsLastOMP = irBuilder.CreateAlloca(i32PtrType, nullptr, "omp.isLastFlag");

    CaptureExpr =
        irBuilder.CreateAlloca(&itVarType, nullptr, "omp.capture_expr");
    CaptureExpr1 =
        irBuilder.CreateAlloca(&itVarType, nullptr, "omp.capture_expr1");

    ArtificialVarDeclIterator =
        irBuilder.CreateAlloca(&itVarType, nullptr, "artificial.var");
    llvm::MDNode *node =
        llvm::MDNode::get(llvmContext, llvm::MDString::get(llvmContext, "yes"));
    ArtificialVarDeclIterator->setMetadata("par-local-del", node);
  }
};

struct ParallelForAuxData {
  VarDecl &ItVarDecl;
  llvm::Value *InitValue;
  llvm::Value *IncValue;
  llvm::Value *CmpValue;
  llvm::Value *ItVarValue;

  ParallelForAuxData(VarDecl &var) : ItVarDecl(var) {}
};

struct ParallelForBBs {
  llvm::BasicBlock *OMPPrecond;
  llvm::BasicBlock *OMPPrecondThen;
  llvm::BasicBlock *OMPPrecondEnd;
  llvm::BasicBlock *OMPCondTrue;
  llvm::BasicBlock *OMPCondFalse;
  llvm::BasicBlock *OMPCondEnd;
  llvm::BasicBlock *OMPLoopExit;

  ParallelForBBs(llvm::LLVMContext &ctx) {
    OMPPrecond = llvm::BasicBlock::Create(ctx, "omp.precond");
    OMPPrecondThen = llvm::BasicBlock::Create(ctx, "omp.precond.then");
    OMPPrecondEnd = llvm::BasicBlock::Create(ctx, "omp.precond.end");
    OMPCondTrue = llvm::BasicBlock::Create(ctx, "omp.cond.true");
    OMPCondFalse = llvm::BasicBlock::Create(ctx, "omp.cond.false");
    OMPCondEnd = llvm::BasicBlock::Create(ctx, "omp.cond.end");
    OMPLoopExit = llvm::BasicBlock::Create(ctx, "omp.loop.exit");
  }
};

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

  void emitOMPPreCondThen();

  void
  emitOutlinedParallelLoopFunc(const ForStmt &parallelFor,
                               llvm::ArrayRef<const VarDecl *> varsUsed,
                               llvm::ArrayRef<const VarDecl *> varDeclared);
  void emitInnerParallelInParallelLoop(ParallelForAuxVars &pfav,
                                       ParallelForAuxData &pfad, Stmt &body);
  llvm::Value *emitInnerParallelCond(ParallelForAuxVars &pfav,
                                     ParallelForAuxData &pfad);
  void emitInnerParallelBody(ParallelForAuxVars &pfav, ParallelForAuxData &pfad,
                             Stmt &body);
  void emitInnerParallelInc(ParallelForAuxVars &pfav, ParallelForAuxData &pfad);

  llvm::Value *getExpr(const Expr &);

  QuarkContext &Ctx;
  llvm::IRBuilder<> IRBuilder;
  SourceModule &SM;
  std::unique_ptr<llvm::Module> Mod;
  CodeGenContext CGCtx;
};

} // namespace quark

#endif // __QUARK_FRONTEND_CODEGEN_CODEGEN_H__