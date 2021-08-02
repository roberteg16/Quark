#include <quark/Frontend/CodeGen/CodeGen.h>

#include <quark/Frontend/CodeGen/QuarkContext.h>
#include <quark/Frontend/Parsing/SourceModule.h>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/ScopeExit.h>
#include <llvm/ADT/SetOperations.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/raw_ostream.h>

#include "LLVMTypeTranslation.h"
#include "Mangler.h"
#include "OMPCodeGen.h"
#include "OMPFunctions.h"
#include "OMPTypes.h"
#include "RecoverVarsUsed.h"

using namespace quark;

CodeGen::CodeGen(llvm::StringRef file, SourceModule &sm, QuarkContext &ctx)
    : Ctx(ctx), IRBuilder(Ctx.LLVMCtx), SM(sm),
      Mod(std::make_unique<llvm::Module>(file, Ctx.LLVMCtx)) {}

CodeGen::~CodeGen() {}

std::unique_ptr<llvm::Module> CodeGen::generate() {
  for (auto &decl : SM.Declarations) {
    emitDecl(*decl);
  }

  return std::move(Mod);
}

// Only compound and alias types
void CodeGen::emitTypeDecl(const TypeDecl &decl) {
  if (auto *compound = llvm::dyn_cast<CompoundType>(&decl.Type)) {
    llvm::SmallVector<llvm::Type *, 5> types;
    types.reserve(compound->Types.size());
    for (auto &type : compound->Types) {
      if (auto *innerCompoundType = llvm::dyn_cast<CompoundType>(type.get())) {
        emitTypeDecl(innerCompoundType->Decl);
      }
      types.push_back(TranslateType(Ctx.LLVMCtx, *type, SM));
    }
    auto *newStruct =
        llvm::StructType::create(Ctx.LLVMCtx, types, compound->Name);
    assert(newStruct);
    SM.addLLVMTypeForCompoundType(*newStruct, compound->Name);
  }
  // TODO: implement AliasTypes
}

void CodeGen::emitForLoop(const ForStmt &forStmt) {
  auto *initBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "for.init");
  auto *condBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "for.cond");
  auto *bodyBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "for.body");
  auto *incBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "for.inc");
  auto *endBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "for.end");

  IRBuilder.CreateBr(initBB);
  emitBlock(*initBB);
  emitStmt(*forStmt.VarDecl);

  IRBuilder.CreateBr(condBB);
  emitConditionalBlock(*condBB, *bodyBB, *endBB, *forStmt.Cond);
  emitPossiblyRetStmtBlock(*bodyBB, *CGCtx.RetsBB, *incBB, *forStmt.Body);

  emitBlock(*incBB);
  getExpr(*forStmt.Inc);
  IRBuilder.CreateBr(condBB);

  emitBlock(*endBB);
}

static llvm::GlobalVariable *
CreateOMPIndentGlobalVariable(llvm::LLVMContext &ctx,
                              llvm::IRBuilder<> &irBuilder, llvm::Module &mod,
                              IdentType identTypeKind) {
  llvm::Constant *cnts[] = {
      llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(ctx), 0),
      llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(ctx),
                             static_cast<std::uint64_t>(identTypeKind)),
      llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(ctx), 0),
      llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(ctx), 0),
      irBuilder.CreateGlobalStringPtr("path/to/this/stmt"),
  };

  llvm::StructType *identType =
      llvm::cast<llvm::StructType>(GetOMPType(OMPType::ident_t, ctx));

  llvm::Constant *constantStruct =
      llvm::ConstantStruct::get(identType, llvm::makeArrayRef(cnts));

  return new llvm::GlobalVariable(
      mod, identType, /*isConst*/ true,
      llvm::GlobalValue::LinkageTypes::PrivateLinkage, constantStruct);
}

static std::string GenerateUniqueOutlinedParLoopFunc() {
  static unsigned counter = 0;
  std::string str;
  llvm::raw_string_ostream rso(str);
  rso << "outline.par.for." << counter++;
  return rso.str();
}

static llvm::FunctionType *
CreateFunctionTypeForBitCast(llvm::LLVMContext &ctx) {
  std::vector<llvm::Type *> paramTypes = {
      llvm::IntegerType::getInt32PtrTy(ctx),
      llvm::IntegerType::getInt32PtrTy(ctx)};

  return llvm::FunctionType::get(llvm::Type::getVoidTy(ctx),
                                 llvm::makeArrayRef(paramTypes),
                                 /*isVarArg*/ true);
}

static llvm::FunctionType *
CreateFunctionTypeForOutlinedParLoop(llvm::LLVMContext &ctx,
                                     llvm::ArrayRef<const VarDecl *> varsUsed,
                                     SourceModule &sm) {
  std::vector<llvm::Type *> paramTypes = {
      llvm::IntegerType::getInt32PtrTy(ctx),
      llvm::IntegerType::getInt32PtrTy(ctx)};

  for (const VarDecl *varDecl : varsUsed) {
    paramTypes.push_back(
        TranslateType(ctx, *varDecl->Type, sm)->getPointerTo());
  }

  return llvm::FunctionType::get(llvm::Type::getVoidTy(ctx),
                                 llvm::makeArrayRef(paramTypes),
                                 /*isVarArg*/ false);
}

static Expr &GetIncrement(VarDecl &itVarDecl, Expr &expr) {
  auto *binExpr = llvm::dyn_cast<BinaryExpr>(&expr);
  assert(binExpr);

  auto *varRefExpr = llvm::dyn_cast<VarRefExpr>(binExpr->Lhs.get());
  assert(varRefExpr->RefVar == itVarDecl);

  auto *rhsBinExpr = llvm::dyn_cast<BinaryExpr>(binExpr->Rhs.get());
  assert(rhsBinExpr && rhsBinExpr->Op == BinaryOperatorKind::Add);
  auto *implicitCast = llvm::dyn_cast<ImplicitCastExpr>(rhsBinExpr->Lhs.get());
  assert(implicitCast &&
         implicitCast->CastKind == ImplicitCastKind::LValueToRValue);

  auto *rhsVarRefExpr =
      llvm::dyn_cast<VarRefExpr>(implicitCast->CastedExpr.get());
  assert(rhsVarRefExpr->RefVar == itVarDecl);

  return *rhsBinExpr->Rhs;
}

static bool IsLogicalOperator(BinaryOperatorKind op) {
  return op >= BinaryOperatorKind::LogicalStart &&
         op <= BinaryOperatorKind::LogicalEnd;
}

static std::pair<Expr &, BinaryOperatorKind> GetCmpExprAndOp(VarDecl &itVarDecl,
                                                             Expr &expr) {
  auto *binExpr = llvm::dyn_cast<BinaryExpr>(&expr);
  assert(binExpr);

  assert(IsLogicalOperator(binExpr->Op));
  auto *implicitCast = llvm::dyn_cast<ImplicitCastExpr>(binExpr->Lhs.get());
  assert(implicitCast &&
         implicitCast->CastKind == ImplicitCastKind::LValueToRValue);
  auto *rhsVarRefExpr =
      llvm::dyn_cast<VarRefExpr>(implicitCast->CastedExpr.get());
  assert(rhsVarRefExpr->RefVar == itVarDecl);

  return {*binExpr->Rhs, binExpr->Op};
}

llvm::Value *CodeGen::emitOMPPrecond(const ForStmt &parallelFor,
                                     ParallelForBBs &pfb,
                                     ParallelForAuxVars &pfav,
                                     ParallelForAuxData &pfad) {
  emitBlock(*pfb.OMPPrecond);

  auto [cmpExpr, op] =
      GetCmpExprAndOp(*parallelFor.VarDecl->VarDecl, *parallelFor.Cond);
  pfad.CmpValue = getExpr(cmpExpr);

  // TODO handle cmp and, inc value those affect the creation of the following
  // insts
  IRBuilder.CreateStore(pfad.CmpValue, pfav.CaptureExpr);
  auto *captureExpr1Val = IRBuilder.CreateSub(
      IRBuilder.CreateSDiv(
          IRBuilder.CreateSub(IRBuilder.CreateLoad(pfav.CaptureExpr),
                              pfad.InitValue),
          pfad.IncValue),
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 1));
  IRBuilder.CreateStore(captureExpr1Val, pfav.CaptureExpr1);

  llvm::Instruction *itVar = IRBuilder.CreateLoad(
      CGCtx.OutlinedVarDeclToAlloca[parallelFor.VarDecl->VarDecl.get()]);

  auto *captureExpr = IRBuilder.CreateLoad(pfav.CaptureExpr);
  return IRBuilder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLT, itVar,
                             captureExpr);
}

llvm::Value *CodeGen::emitOMPPrecondThen(const ForStmt &parallelFor,
                                         ParallelForBBs &pfb,
                                         ParallelForAuxVars &pfav,
                                         ParallelForAuxData &pfad) {
  emitBlock(*pfb.OMPPrecondThen);

  IRBuilder.CreateStore(
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 0),
      pfav.LbOMP);
  IRBuilder.CreateStore(IRBuilder.CreateLoad(pfav.CaptureExpr1), pfav.UbOMP);
  IRBuilder.CreateStore(
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 1),
      pfav.StrideOMP);
  IRBuilder.CreateStore(
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 0),
      pfav.IsLastOMP);

  auto *gtid = IRBuilder.CreateLoad(IRBuilder.CreateLoad(pfav.GTID));

  llvm::FunctionCallee func =
      GetOMPFunction(OMPFunction::for_static_init_4, *Mod);

  llvm::Value *paramsStaticInitFunc[] = {
      CreateOMPIndentGlobalVariable(Ctx.LLVMCtx, IRBuilder, *Mod,
                                    IdentType::KMP_IDENT_KMPC),
      gtid,
      llvm::ConstantInt::get(
          llvm::Type::getInt32Ty(Ctx.LLVMCtx),
          static_cast<std::int32_t>(sched_type::kmp_sch_static)),
      pfav.IsLastOMP,
      pfav.LbOMP,
      pfav.UbOMP,
      pfav.StrideOMP,
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 1),
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 1)};

  IRBuilder.CreateCall(func, paramsStaticInitFunc);
  return IRBuilder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGT,
                             IRBuilder.CreateLoad(pfav.UbOMP),
                             IRBuilder.CreateLoad(pfav.CaptureExpr1));
}

llvm::Value *CodeGen::emitOMPPrecondTrue(const ForStmt &parallelFor,
                                         ParallelForBBs &pfb,
                                         ParallelForAuxVars &pfav,
                                         ParallelForAuxData &pfad) {
  emitBlock(*pfb.OMPCondTrue);
  return IRBuilder.CreateLoad(pfav.CaptureExpr1);
}
llvm::Value *CodeGen::emitOMPPrecondFalse(const ForStmt &parallelFor,
                                          ParallelForBBs &pfb,
                                          ParallelForAuxVars &pfav,
                                          ParallelForAuxData &pfad) {
  emitBlock(*pfb.OMPCondFalse);
  return IRBuilder.CreateLoad(pfav.UbOMP);
}

void CodeGen::emitOMPPrecondEnd(const ForStmt &parallelFor, ParallelForBBs &pfb,
                                ParallelForAuxVars &pfav,
                                ParallelForAuxData &pfad,
                                llvm::Value &captureExpr1,
                                llvm::Value &upperBound) {
  emitBlock(*pfb.OMPCondEnd);
  auto *phi = IRBuilder.CreatePHI(captureExpr1.getType(), 2);
  phi->addIncoming(&captureExpr1, pfb.OMPCondTrue);
  phi->addIncoming(&upperBound, pfb.OMPCondFalse);
  IRBuilder.CreateStore(phi, pfav.UbOMP);
  IRBuilder.CreateStore(IRBuilder.CreateLoad(pfav.LbOMP),
                        pfav.IterationVariable);
}

void CodeGen::emitOMPLoopExit(const ForStmt &parallelFor, ParallelForBBs &pfb,
                              ParallelForAuxVars &pfav,
                              ParallelForAuxData &pfad) {
  emitBlock(*pfb.OMPLoopExit);

  auto *gtid = IRBuilder.CreateLoad(IRBuilder.CreateLoad(pfav.GTID));
  // Get function call to omp library, fork func
  llvm::FunctionCallee funcStaticFIni =
      GetOMPFunction(OMPFunction::for_static_fini, *Mod);

  llvm::Value *paramsFIniFunc[] = {
      CreateOMPIndentGlobalVariable(Ctx.LLVMCtx, IRBuilder, *Mod,
                                    IdentType::KMP_IDENT_WORK_LOOP),
      gtid};
  IRBuilder.CreateCall(funcStaticFIni, paramsFIniFunc);
}

void CodeGen::emitOutlinedParallelLoopFunc(
    const ForStmt &parallelFor, llvm::ArrayRef<const VarDecl *> varsUsed,
    llvm::ArrayRef<const VarDecl *> varsDeclared) {
  emitBlock(*llvm::BasicBlock::Create(Ctx.LLVMCtx, "entry"));

  llvm::Type *itVarType =
      TranslateType(Ctx.LLVMCtx, *parallelFor.VarDecl->VarDecl->Type, SM);

  // Auxiliar variables needed for the parallel for region
  ParallelForAuxVars pfav(IRBuilder, *itVarType);

  // Save old ret block to restore after the outlined loop function
  llvm::BasicBlock *prevRetBB = CGCtx.RetsBB;
  auto recoverRetBB = llvm::make_scope_exit([=] { CGCtx.RetsBB = prevRetBB; });

  // Create return block for outlined loop function
  CGCtx.RetsBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "func.end");

  // Emit all allocas needed for variables used inside the paralell loop
  for (const VarDecl *varDecl : varsUsed) {
    llvm::AllocaInst *alloca = IRBuilder.CreateAlloca(
        TranslateType(Ctx.LLVMCtx, *varDecl->Type, SM)->getPointerTo(), nullptr,
        varDecl->Name + ".var");
    CGCtx.OutlinedVarDeclToAlloca[varDecl] = alloca;
  }

  // Also emit allocas allocas for each local declaration inside the loop region
  for (const VarDecl *varDecl : varsDeclared) {
    llvm::AllocaInst *alloca =
        IRBuilder.CreateAlloca(TranslateType(Ctx.LLVMCtx, *varDecl->Type, SM),
                               nullptr, varDecl->Name + ".var");
    // Addding metadata so on the emit of the VarRefExpr we can distinguish
    // whether we have to load a local declaration or a variable used in the
    // loop region that is getting passed by address to the outlined function
    llvm::MDNode *node =
        llvm::MDNode::get(Ctx.LLVMCtx, llvm::MDString::get(Ctx.LLVMCtx, "yes"));
    alloca->setMetadata("par-local-del", node);
    CGCtx.OutlinedVarDeclToAlloca[varDecl] = alloca;
  }

  // Create store for params of omp fork call
  llvm::Argument *globalThreadID = CGCtx.CurrFuncLLVM->getArg(0);
  llvm::Argument *boundThreadID = CGCtx.CurrFuncLLVM->getArg(1);
  IRBuilder.CreateStore(globalThreadID, pfav.GTID);
  IRBuilder.CreateStore(boundThreadID, pfav.BTID);

  // Create store for each param
  for (unsigned i = 0; i < varsUsed.size(); ++i) {
    IRBuilder.CreateStore(CGCtx.CurrFuncLLVM->getArg(i + 2),
                          CGCtx.OutlinedVarDeclToAlloca[varsUsed[i]]);
  }

  // Auxiliar data needed for the codegen of parallel for
  ParallelForAuxData pfad(*parallelFor.VarDecl->VarDecl);

  // TODO: emmit error instead of hard asserting it
  assert(parallelFor.VarDecl->InitExpr && "VarDecl must have init value");
  pfad.InitValue = getExpr(*parallelFor.VarDecl->InitExpr);
  pfad.IncValue =
      getExpr(GetIncrement(*parallelFor.VarDecl->VarDecl, *parallelFor.Inc));

  // Auxiliar get blocks needed for the parallel for logic
  ParallelForBBs pfb(Ctx.LLVMCtx);

  IRBuilder.CreateBr(pfb.OMPPrecond);

  llvm::Value *precond = emitOMPPrecond(parallelFor, pfb, pfav, pfad);
  IRBuilder.CreateCondBr(precond, pfb.OMPPrecondThen, CGCtx.RetsBB);

  llvm::Value *cond = emitOMPPrecondThen(parallelFor, pfb, pfav, pfad);
  IRBuilder.CreateCondBr(cond, pfb.OMPCondTrue, pfb.OMPCondFalse);

  auto *captureExpr1 = emitOMPPrecondTrue(parallelFor, pfb, pfav, pfad);
  IRBuilder.CreateBr(pfb.OMPCondEnd);

  auto *upperBound = emitOMPPrecondFalse(parallelFor, pfb, pfav, pfad);
  IRBuilder.CreateBr(pfb.OMPCondEnd);

  emitOMPPrecondEnd(parallelFor, pfb, pfav, pfad, *captureExpr1, *upperBound);

  emitInnerParallelInParallelLoop(pfav, pfad, *parallelFor.Body);
  IRBuilder.CreateBr(pfb.OMPLoopExit);

  emitOMPLoopExit(parallelFor, pfb, pfav, pfad);
  IRBuilder.CreateBr(CGCtx.RetsBB);

  emitBlock(*CGCtx.RetsBB);

  // Outlined functions always returns void
  IRBuilder.CreateRetVoid();
}

llvm::Value *CodeGen::emitInnerParallelCond(ParallelForAuxVars &pfav,
                                            ParallelForAuxData &pfad) {
  auto *itVar = IRBuilder.CreateLoad(pfav.IterationVariable);
  auto *upperBoundPlusOne = IRBuilder.CreateAdd(
      IRBuilder.CreateLoad(pfav.UbOMP),
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 1));
  return IRBuilder.CreateCmp(llvm::CmpInst::Predicate::ICMP_ULT, itVar,
                             upperBoundPlusOne);
}

void CodeGen::emitInnerParallelBody(ParallelForAuxVars &pfav,
                                    ParallelForAuxData &pfad, Stmt &body) {
  // Update real iteration var of loop (artificial copy of induction variable)
  auto *mul = IRBuilder.CreateMul(IRBuilder.CreateLoad(pfav.IterationVariable),
                                  pfad.IncValue);
  IRBuilder.CreateStore(IRBuilder.CreateAdd(mul, pfad.InitValue),
                        pfav.ArtificialVarDeclIterator);
  emitStmt(body);
}

void CodeGen::emitInnerParallelInc(ParallelForAuxVars &pfav,
                                   ParallelForAuxData &pfad) {
  // Increment aux iteration variable
  IRBuilder.CreateStore(
      IRBuilder.CreateAdd(
          IRBuilder.CreateLoad(pfav.IterationVariable),
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 1)),
      pfav.IterationVariable);
}

void CodeGen::emitInnerParallelInParallelLoop(ParallelForAuxVars &pfav,
                                              ParallelForAuxData &pfad,
                                              Stmt &body) {
  auto *ompInnerForCond =
      llvm::BasicBlock::Create(Ctx.LLVMCtx, "omp.inner.for.cond");
  auto *ompInnerForBody =
      llvm::BasicBlock::Create(Ctx.LLVMCtx, "omp.inner.for.body");
  auto *ompInnerForBodyContinue =
      llvm::BasicBlock::Create(Ctx.LLVMCtx, "omp.body.continue");
  auto *ompInnerForInc =
      llvm::BasicBlock::Create(Ctx.LLVMCtx, "omp.inner.for.inc");
  auto *ompInnerForEnd =
      llvm::BasicBlock::Create(Ctx.LLVMCtx, "omp.inner.for.end");

  CGCtx.OutlinedVarDeclToAlloca[&pfad.ItVarDecl] =
      pfav.ArtificialVarDeclIterator;

  IRBuilder.CreateBr(ompInnerForCond);

  emitBlock(*ompInnerForCond);
  auto *cond = emitInnerParallelCond(pfav, pfad);
  IRBuilder.CreateCondBr(cond, ompInnerForBody, ompInnerForEnd);

  emitBlock(*ompInnerForBody);
  emitInnerParallelBody(pfav, pfad, body);
  IRBuilder.CreateBr(ompInnerForBodyContinue);

  emitBlock(*ompInnerForBodyContinue);
  IRBuilder.CreateBr(ompInnerForInc);

  emitBlock(*ompInnerForInc);
  emitInnerParallelInc(pfav, pfad);
  IRBuilder.CreateBr(ompInnerForCond);

  emitBlock(*ompInnerForEnd);
}

void CodeGen::emitParallelForLoop(const ForStmt &forStmt) {
  // Recover all used vars inside parallel for loop
  RecoverVarsUsed varUsedVisitor;
  varUsedVisitor.recover(forStmt);

  auto varDecls = varUsedVisitor.takeVarDecls();
  auto varsUsed = varUsedVisitor.takeVarUsed();
  auto varUsedNotDeclared =
      llvm::set_difference(varsUsed, varDecls).takeVector();

  std::string outlinedFunctionName = GenerateUniqueOutlinedParLoopFunc();
  // Create outlined function
  llvm::FunctionCallee outlinedFunc = Mod->getOrInsertFunction(
      outlinedFunctionName, CreateFunctionTypeForOutlinedParLoop(
                                Ctx.LLVMCtx, varUsedNotDeclared, SM));

  llvm::SmallVector<llvm::Value *, 5> params;
  // Add Ident_t struct required by OMP fork
  params.push_back(CreateOMPIndentGlobalVariable(Ctx.LLVMCtx, IRBuilder, *Mod,
                                                 IdentType::KMP_IDENT_KMPC));
  // Add num of extra params (vars used inside the outline function)
  params.push_back(llvm::ConstantInt::get(
      llvm::IntegerType::getInt32Ty(Ctx.LLVMCtx), varUsedNotDeclared.size()));
  // Add real function to parallelize, the outlined one
  params.push_back(IRBuilder.CreateBitCast(
      outlinedFunc.getCallee(),
      CreateFunctionTypeForBitCast(Ctx.LLVMCtx)->getPointerTo()));
  // Add extra params, vars used inside for
  for (auto *varUsed : varUsedNotDeclared) {
    params.push_back(&SM.getAllocaForVarDecl(*varUsed));
  }

  // Get function call to omp library, fork func
  llvm::FunctionCallee func = GetOMPFunction(OMPFunction::fork_call, *Mod);

  // Finally emit the call
  IRBuilder.CreateCall(func, params);

  // Save for later the current fuction we are as we are going to enter
  // in the outlined function
  auto *currentFunc = CGCtx.CurrFuncLLVM;
  auto recoverFucn =
      llvm::make_scope_exit([=] { CGCtx.CurrFuncLLVM = currentFunc; });

  // Implement outline function
  CGCtx.CurrFuncLLVM = Mod->getFunction(outlinedFunctionName);

  // Activate flags of outlined function emiting
  CGCtx.IsInOutlineFunction = true;
  emitOutlinedParallelLoopFunc(forStmt, varUsedNotDeclared,
                               varDecls.takeVector());
  CGCtx.IsInOutlineFunction = false;
  CGCtx.OutlinedVarDeclToAlloca.clear();
}

void CodeGen::emitBlock(llvm::BasicBlock &bb) {
  IRBuilder.SetInsertPoint(&bb);
  bb.insertInto(CGCtx.CurrFuncLLVM);
}

void CodeGen::emitConditionalBlock(llvm::BasicBlock &condBB,
                                   llvm::BasicBlock &trueBB,
                                   llvm::BasicBlock &falseBB,
                                   quark::Expr &condExpr) {
  emitBlock(condBB);
  IRBuilder.CreateCondBr(getExpr(condExpr), &trueBB, &falseBB);
}

void CodeGen::emitPossiblyRetStmtBlock(llvm::BasicBlock &offenderBB,
                                       llvm::BasicBlock &retFlowBB,
                                       llvm::BasicBlock &normalFlowBB,
                                       quark::Stmt &stmt) {
  emitBlock(offenderBB);
  CGCtx.StopEmmiting.push_back(false);
  emitStmt(stmt);
  const bool didReturn = CGCtx.StopEmmiting.back();
  IRBuilder.CreateBr(didReturn ? &retFlowBB : &normalFlowBB);
  CGCtx.StopEmmiting.pop_back();
}

static llvm::SmallString<40> StrReplace(llvm::StringRef sHaystack,
                                        std::string sNeedle,
                                        std::string sReplace,
                                        size_t nTimes = 0) {
  size_t found = 0, pos = 0, c = 0;
  size_t len = sNeedle.size();
  size_t replen = sReplace.size();
  std::string input(sHaystack);

  do {
    found = input.find(sNeedle, pos);
    if (found == std::string::npos) {
      break;
    }
    input.replace(found, len, sReplace);
    pos = found + replen;
    ++c;
  } while (!nTimes || c < nTimes);

  return {input};
}

llvm::Value *CodeGen::getExpr(const Expr &expr) {
  if (!CGCtx.StopEmmiting.empty() && CGCtx.StopEmmiting.back()) {
    return nullptr;
  }

  switch (expr.Kind) {
  case ExprKind::AllocExpr: {
    const auto &allocExpr = *llvm::cast<AllocExpr>(&expr);
    // Allocated type always is a pointer because otherwise in the program it
    // would be accesses with '.' instead of '->'. LLVM adds a pointer, so we
    // remove it
    auto *ptr = llvm::cast<PtrType>(allocExpr.AllocType.get());
    auto *type = TranslateType(Ctx.LLVMCtx, *ptr->PointeeType, SM);

    llvm::Type *ity = llvm::Type::getInt32Ty(Ctx.LLVMCtx);
    llvm::Constant *allocSize = llvm::ConstantExpr::getSizeOf(type);
    allocSize = llvm::ConstantExpr::getTruncOrBitCast(allocSize, ity);

    auto *malloc = llvm::CallInst::CreateMalloc(
        IRBuilder.GetInsertBlock(), ity, type, allocSize,
        allocExpr.SizeToAlloc ? getExpr(*allocExpr.SizeToAlloc) : nullptr,
        nullptr);
    IRBuilder.Insert(malloc);
    return malloc;
  }
  case ExprKind::BinaryExpr: {
    const auto &binExpr = *llvm::cast<BinaryExpr>(&expr);
    llvm::Value *lhs = getExpr(*binExpr.Lhs);
    llvm::Value *rhs = getExpr(*binExpr.Rhs);

    // Handle special cases first
    switch (binExpr.Op) {
    case BinaryOperatorKind::Assign:
      return IRBuilder.CreateStore(rhs, lhs);
    case BinaryOperatorKind::LogicalOr:
      llvm::llvm_unreachable_internal("Logical or not implmented yet");
    case BinaryOperatorKind::LogicalAnd:
      llvm::llvm_unreachable_internal("Logical and not implmented yet");
    default:;
    }

    BuiltinTypeKind kind = llvm::cast<BuiltinType>(binExpr.Lhs->getType()).Kind;
    bool isFloatingCompatible =
        quark::IsFloatingPoint(kind) || quark::IsVector(kind);
    bool isSigned =
        quark::IsSigned(llvm::cast<BuiltinType>(binExpr.Lhs->getType()).Kind);

    if (auto optBinaryOp = quark::TranslateArithmeticBinaryKind(
            binExpr.Op, isFloatingCompatible, isSigned)) {
      return IRBuilder.CreateBinOp(*optBinaryOp, lhs, rhs);
    }

    if (auto optLogicalOp = quark::TranslateLogicalBinaryKind(
            binExpr.Op, isFloatingCompatible, /*ordered*/ false, isSigned)) {
      return IRBuilder.CreateCmp(*optLogicalOp, lhs, rhs);
    }

    llvm::llvm_unreachable_internal("Should never arrive a this point");
  }
  case ExprKind::UnaryExpr: {
    const auto &unaryExpr = *llvm::cast<UnaryExpr>(&expr);
    llvm::Value *lhs = getExpr(*unaryExpr.Lhs);

    if (unaryExpr.Op == UnaryOperatorKind::AddressOf ||
        unaryExpr.Op == UnaryOperatorKind::Dereference) {
      return lhs;
    }

    BuiltinTypeKind kind =
        llvm::cast<BuiltinType>(unaryExpr.Lhs->getType()).Kind;
    bool isFloatingCompatible =
        quark::IsFloatingPoint(kind) || quark::IsVector(kind);
    bool isSigned = quark::IsSigned(kind);

    switch (unaryExpr.Op) {
    case UnaryOperatorKind::ArithmeticNegation:
      return isFloatingCompatible ? IRBuilder.CreateFNeg(lhs)
                                  : IRBuilder.CreateNeg(lhs);
    case UnaryOperatorKind::LogicalNegation: {
      llvm::Value *expr = lhs;
      if (lhs->getType()->getIntegerBitWidth() != 32) {
        if (isSigned) {
          expr = IRBuilder.CreateSExtOrTrunc(
              lhs, llvm::Type::getInt32Ty(Ctx.LLVMCtx));
        } else {
          expr = IRBuilder.CreateZExtOrTrunc(
              lhs, llvm::Type::getInt32Ty(Ctx.LLVMCtx));
        }
      }
      return IRBuilder.CreateXor(
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 0,
                                 isSigned),
          expr);
    }
    default:
      llvm::llvm_unreachable_internal();
    }
  }
  case ExprKind::CharExpr: {
    return llvm::ConstantInt::get(
        llvm::Type::getInt8Ty(Ctx.LLVMCtx),
        llvm::APInt(8, llvm::cast<CharExpr>(&expr)->Value));
  }
  case ExprKind::FloatingExpr: {
    return llvm::ConstantFP::get(llvm::Type::getFloatTy(Ctx.LLVMCtx),
                                 llvm::cast<FloatingExpr>(&expr)->Value);
  }
  case ExprKind::IntegerExpr: {
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx),
                                  llvm::cast<IntegerExpr>(&expr)->Value);
  }
  case ExprKind::StringExpr: {
    return IRBuilder.CreateBitCast(
        IRBuilder.CreateGlobalStringPtr(
            StrReplace(llvm::cast<StringExpr>(&expr)->Value, "\\n", "\n")),
        llvm::Type::getInt8PtrTy(Ctx.LLVMCtx));
  }
  case ExprKind::VarRefExpr: {
    const VarRefExpr &varRefExpr = *llvm::cast<VarRefExpr>(&expr);
    if (CGCtx.IsInOutlineFunction) {
      // If we are in a outlined context extract the alloca from the temporal
      // map
      auto *alloca = CGCtx.OutlinedVarDeclToAlloca[&varRefExpr.RefVar];
      if (alloca->hasMetadata()) {
        return alloca;
      }
      return IRBuilder.CreateLoad(alloca);
    }
    // Otherwise from the SourceModule cache
    return &SM.getAllocaForVarDecl(varRefExpr.RefVar);
  }
  case ExprKind::BooleanExpr: {
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(Ctx.LLVMCtx),
                                  llvm::cast<BooleanExpr>(&expr)->Value);
  }
  case ExprKind::FunctionCallExpr: {
    const auto &funcCallExpr = *llvm::cast<FunctionCallExpr>(&expr);
    llvm::SmallVector<llvm::Value *, 8> params;
    params.reserve(funcCallExpr.Params.size());
    for (auto &param : funcCallExpr.Params) {
      params.push_back(getExpr(*param));
    }

    llvm::FunctionCallee &callee =
        CGCtx.DeclToFuncCalleeMap[&funcCallExpr.FunctionDecl];
    return IRBuilder.CreateCall(
        callee, params,
        (!callee.getFunctionType()->getReturnType()->isVoidTy()
             ? funcCallExpr.FunctionDecl.Name.data()
             : ""));
  }
  case ExprKind::MemberCallExpr: {
    const auto &memberCallExpr = *llvm::cast<MemberCallExpr>(&expr);
    llvm::SmallVector<llvm::Value *, 8> params;
    // + 1 because the reciever
    params.reserve(memberCallExpr.Params.size() + 1);
    params.push_back(getExpr(*memberCallExpr.Accessor));
    for (auto &param : memberCallExpr.Params) {
      params.push_back(getExpr(*param));
    }
    return IRBuilder.CreateCall(
        llvm::cast<llvm::FunctionType>(TranslateType(
            Ctx.LLVMCtx, memberCallExpr.FunctionDecl.FuncType, SM)),
        CGCtx.DeclToFuncCalleeMap[&memberCallExpr.FunctionDecl].getCallee(),
        params);
  }
  case ExprKind::MemberExpr: {
    const auto &memberExpr = *llvm::cast<MemberExpr>(&expr);
    return IRBuilder.CreateStructGEP(getExpr(*memberExpr.Accessed),
                                     memberExpr.getIdxAccesses());
  }
  case ExprKind::DereferenceExpr: {
    const auto &dereferenceExpr = *llvm::cast<DereferenceExpr>(&expr);
    return getExpr(*dereferenceExpr.DereferencingExpr);
  }
  case ExprKind::AddressofExpr: {
    const auto &addressofExpr = *llvm::cast<AddressofExpr>(&expr);
    return getExpr(*addressofExpr.AdressOfExpr);
  }
  case ExprKind::ArrayAccessExpr: {
    const auto &arrayAccessExpr = *llvm::cast<ArrayAccessExpr>(&expr);
    auto *llvmExpr = getExpr(*arrayAccessExpr.RefVar);

    if (llvm::isa<PtrType>(arrayAccessExpr.RefVar->getType())) {
      llvm::Value *idx[] = {getExpr(*arrayAccessExpr.Idx)};
      return IRBuilder.CreateInBoundsGEP(llvmExpr, idx);
    }

    llvm::Value *idx[] = {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 0),
        getExpr(*arrayAccessExpr.Idx)};

    return IRBuilder.CreateGEP(llvmExpr, idx);
  }
  case ExprKind::ExplicitCastExpr: {
    const auto &explicitCastExpr = *llvm::cast<ExplicitCastExpr>(&expr);
    TypeCasting castType = CastType(explicitCastExpr.ConvertingExpr->getType(),
                                    *explicitCastExpr.ExprType);
    llvm::Type *llvmType =
        TranslateType(Ctx.LLVMCtx, *explicitCastExpr.ExprType, SM);
    llvm::Value *llvmVal = getExpr(*explicitCastExpr.ConvertingExpr);
    switch (castType) {
    case TypeCasting::Unknown:
      llvm::llvm_unreachable_internal("This should be catched but the Sema");
    case TypeCasting::Same:
      return llvmVal;
    case TypeCasting::Trunc:
      return IRBuilder.CreateTrunc(llvmVal, llvmType);
    case TypeCasting::ZExt:
      return IRBuilder.CreateZExt(llvmVal, llvmType);
    case TypeCasting::SExt:
      return IRBuilder.CreateSExt(llvmVal, llvmType);
    case TypeCasting::FPTrunc:
      return IRBuilder.CreateFPTrunc(llvmVal, llvmType);
    case TypeCasting::PFExt:
      return IRBuilder.CreateFPExt(llvmVal, llvmType);
    case TypeCasting::FPToInt:
      return IRBuilder.CreateFPToSI(llvmVal, llvmType);
    case TypeCasting::FPToUInt:
      return IRBuilder.CreateFPToUI(llvmVal, llvmType);
    case TypeCasting::IntToFP:
      return IRBuilder.CreateSIToFP(llvmVal, llvmType);
    case TypeCasting::UIntToFP:
      return IRBuilder.CreateUIToFP(llvmVal, llvmType);
    }
  }
  case ExprKind::ImplicitCastExpr: {
    const auto &implicitCastExpr = *llvm::cast<ImplicitCastExpr>(&expr);
    switch (implicitCastExpr.CastKind) {
    case ImplicitCastKind::LValueToRValue: {
      auto *castedValue = getExpr(*implicitCastExpr.CastedExpr);
      return IRBuilder.CreateLoad(castedValue);
    }
    case ImplicitCastKind::ToBool:
      // TODO: create real casting
      llvm::Value *expr = getExpr(*implicitCastExpr.CastedExpr);
      if (expr->getType()->isIntegerTy()) {
        return IRBuilder.CreateICmpNE(
            expr,
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 0));
      }
      if (expr->getType()->isFloatingPointTy()) {
        return IRBuilder.CreateFCmpUNE(
            expr,
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx.LLVMCtx), 0));
      }
      llvm::llvm_unreachable_internal("Not implemented yet");
    }
  }
  }
}

static bool DoesReturnInAllPaths(const Stmt &stmt) {
  switch (stmt.getKind()) {
  case StmtKind::BlockStmt:
    return llvm::any_of(llvm::cast<BlockStmt>(&stmt)->Stmts,
                        [&](const std::unique_ptr<Stmt> &stmt) {
                          return DoesReturnInAllPaths(*stmt);
                        });
  case StmtKind::IfStmt: {
    const auto &ifStmt = *llvm::cast<IfStmt>(&stmt);
    if (!ifStmt.ElseCode) {
      return false;
    }

    bool mustAllReturn = DoesReturnInAllPaths(*ifStmt.Code);
    for (auto &elsif : ifStmt.Elsifs) {
      mustAllReturn &= DoesReturnInAllPaths(*elsif.Stmt);
    }

    mustAllReturn &= DoesReturnInAllPaths(*ifStmt.ElseCode);

    return mustAllReturn;
  }
  case StmtKind::DeallocStmt:
  case StmtKind::DeferStmt:
  case StmtKind::ExprStmt:
  case StmtKind::ForStmt:
  case StmtKind::VarDeclStmt:
  case StmtKind::WhileStmt:
  case StmtKind::PrintStmt:
    return false;
  case StmtKind::ReturnStmt:
    return true;
  }
}

static bool DoesReturnInAllPaths(const FuncDecl &func) {
  return llvm::any_of(func.Body, [&](const std::unique_ptr<Stmt> &stmt) {
    return DoesReturnInAllPaths(*stmt);
  });
}

void CodeGen::emitStmt(const Stmt &stmt) {
  if (!CGCtx.StopEmmiting.empty() && CGCtx.StopEmmiting.back()) {
    return;
  }

  switch (stmt.getKind()) {
  case StmtKind::BlockStmt:
    for (auto &stmt : llvm::cast<BlockStmt>(&stmt)->Stmts) {
      emitStmt(*stmt);
    }
    break;
  case StmtKind::DeallocStmt: {
    const auto &deallocStmt = *llvm::cast<DeallocStmt>(&stmt);
    auto *toDealloc = getExpr(*deallocStmt.ExprToDealloc);
    IRBuilder.Insert(llvm::CallInst::CreateFree(IRBuilder.CreateLoad(toDealloc),
                                                IRBuilder.GetInsertBlock()));
    break;
  }
  case StmtKind::DeferStmt: {
    const auto &deferStmt = *llvm::cast<DeferStmt>(&stmt);
    CGCtx.DeferedValues.push_back(getExpr(*deferStmt.ExprToDefer));
    break;
  }
  case StmtKind::ExprStmt: {
    const auto &exprStmt = *llvm::cast<ExprStmt>(&stmt);
    getExpr(*exprStmt.Expr);
    break;
  }
  case StmtKind::ForStmt: {
    const auto &forStmt = *llvm::cast<ForStmt>(&stmt);
    if (forStmt.IsParallel) {
      llvm::BasicBlock *prev = IRBuilder.GetInsertBlock();
      auto restorePrevBB = llvm::make_scope_exit([=] {
        IRBuilder.SetInsertPoint(prev);
        CGCtx.OutlinedVarDeclToAlloca.clear();
      });

      emitParallelForLoop(forStmt);
    } else {
      emitForLoop(forStmt);
    }
    break;
  }
  case StmtKind::IfStmt: {
    const auto &ifStmt = *llvm::cast<IfStmt>(&stmt);

    auto *ifCondBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "if.cond");
    auto *ifCodeBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "if.code");

    std::vector<std::pair<llvm::BasicBlock *, llvm::BasicBlock *>> elsifsBB;
    elsifsBB.reserve(ifStmt.Elsifs.size());
    for (unsigned i = 0; i < ifStmt.Elsifs.size(); i++) {
      elsifsBB.push_back({llvm::BasicBlock::Create(Ctx.LLVMCtx, "elsif.cond"),
                          llvm::BasicBlock::Create(Ctx.LLVMCtx, "elsif.code")});
    }

    auto *elseBB = ifStmt.ElseCode
                       ? llvm::BasicBlock::Create(Ctx.LLVMCtx, "if.else")
                       : nullptr;
    auto *endIf = llvm::BasicBlock::Create(Ctx.LLVMCtx, "if.end");

    unsigned currentElsifsIdx = 0;
    auto getEndBB = [&]() -> llvm::BasicBlock * {
      if (currentElsifsIdx < elsifsBB.size()) {
        return elsifsBB[currentElsifsIdx++].first;
      }
      return elseBB ? elseBB : endIf;
    };

    IRBuilder.CreateBr(ifCondBB);
    emitConditionalBlock(*ifCondBB, *ifCodeBB, *getEndBB(), *ifStmt.Cond);
    emitPossiblyRetStmtBlock(*ifCodeBB, *CGCtx.RetsBB, *endIf, *ifStmt.Code);

    // Emmit all elsifs
    for (auto elsifPair : llvm::zip(elsifsBB, ifStmt.Elsifs)) {
      llvm::BasicBlock *elsifCondBB, *elsifCodeBB;
      std::tie(elsifCondBB, elsifCodeBB) = std::get<0>(elsifPair);
      quark::Expr &elsifCond = *std::get<1>(elsifPair).Cond;
      quark::Stmt &elsifCode = *std::get<1>(elsifPair).Stmt;

      emitConditionalBlock(*elsifCondBB, *elsifCodeBB, *getEndBB(), elsifCond);
      emitPossiblyRetStmtBlock(*elsifCodeBB, *CGCtx.RetsBB, *endIf, elsifCode);
    }

    // Emmit else code if any
    if (ifStmt.ElseCode) {
      emitPossiblyRetStmtBlock(*elseBB, *CGCtx.RetsBB, *endIf,
                               *ifStmt.ElseCode);
    }

    emitBlock(*endIf);
    break;
  }
  case StmtKind::ReturnStmt: {
    const auto &retStmt = *llvm::cast<ReturnStmt>(&stmt);
    auto onExit =
        llvm::make_scope_exit([&]() { CGCtx.StopEmmiting.back() = true; });

    // Void return
    if (!retStmt.ReturnValue) {
      break;
    }

    // More than one return
    IRBuilder.CreateStore(getExpr(*retStmt.ReturnValue), CGCtx.RetAlloca);
    break;
  }
  case StmtKind::VarDeclStmt: {
    const auto &varDeclStmt = *llvm::cast<VarDeclStmt>(&stmt);
    if (varDeclStmt.InitExpr) {
      llvm::Value *val = &SM.getAllocaForVarDecl(*varDeclStmt.VarDecl);
      if (CGCtx.IsInOutlineFunction) {
        // If we are in a outlined context extract the alloca from the temporal
        // map
        auto *alloca = CGCtx.OutlinedVarDeclToAlloca[varDeclStmt.VarDecl.get()];
        if (alloca->hasMetadata()) {
          val = alloca;
        } else {
          val = IRBuilder.CreateLoad(alloca);
        }
      }

      // Otherwise from the SourceModule cache
      llvm::Value *varInitExpr = getExpr(*varDeclStmt.InitExpr);
      IRBuilder.CreateStore(varInitExpr, val);
    }
    return;
  }
  case StmtKind::WhileStmt: {
    const auto &whileStmt = *llvm::cast<WhileStmt>(&stmt);
    auto *condBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "while.cond");
    auto *bodyBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "while.body");
    auto *endBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "while.end");

    IRBuilder.CreateBr(condBB);
    emitConditionalBlock(*condBB, *bodyBB, *endBB, *whileStmt.Cond);
    emitPossiblyRetStmtBlock(*bodyBB, *CGCtx.RetsBB, *condBB, *whileStmt.Code);
    emitBlock(*endBB);
    break;
  }
  case StmtKind::PrintStmt: {
    const auto &printStmt = *llvm::cast<PrintStmt>(&stmt);

    llvm::SmallVector<llvm::Value *> values;
    values.push_back(getExpr(*printStmt.String));
    for (auto &arg : printStmt.Args) {
      values.push_back(getExpr(*arg));
    }

    IRBuilder.CreateCall(
        Mod->getOrInsertFunction(
            "printf",
            llvm::FunctionType::get(
                llvm::IntegerType::getInt32Ty(Ctx.LLVMCtx),
                llvm::PointerType::get(llvm::Type::getInt8Ty(Ctx.LLVMCtx), 0),
                /*varArg*/ true)),
        values);

    break;
  }
  }
}

static llvm::FunctionType *GetFunctionType(llvm::LLVMContext &ctx,
                                           SourceModule &sm,
                                           const FuncDecl &decl) {
  llvm::SmallVector<llvm::Type *, 6> paramTypes;
  paramTypes.reserve(!!decl.Reciver + decl.Params.size());

  // First is going to be the reciever as a pointer in case of existing one
  if (decl.Reciver) {
    paramTypes.push_back(TranslateType(ctx, *decl.Reciver->Type, sm));
  }

  // Emit normal params
  for (unsigned i = 0; i < decl.Params.size(); ++i) {
    paramTypes.push_back(TranslateType(ctx, *decl.Params[i]->Type, sm));
  }

  llvm::Type *retType = TranslateType(ctx, *decl.FuncType.RetType, sm);

  return llvm::FunctionType::get(retType, paramTypes, /*isVarArg*/ false);
}

void CodeGen::emitFuncDecl(const FuncDecl &decl) {
  // Type of function
  llvm::FunctionType *funcType = GetFunctionType(Ctx.LLVMCtx, SM, decl);

  // Get mangled function's name
  std::string mangledName = decl.Name.data();
  if (decl.Name != "main") {
    mangledName = mangleFunction(decl.Name, decl.FuncType);
  }

  assert(!Mod->getFunction(mangledName));
  CGCtx.DeclToFuncCalleeMap[&decl] =
      Mod->getOrInsertFunction(mangledName, funcType);
  CGCtx.CurrFuncLLVM = Mod->getFunction(mangledName);
  assert(CGCtx.CurrFuncLLVM);

  // Save in codegen context for later
  emitBlock(*llvm::BasicBlock::Create(Ctx.LLVMCtx, "entry"));

  // Emit all allocas needed for vars, reciever and params
  for (const VarDecl *varDecl : SM.getVarDeclStmtsForFunc(decl)) {
    llvm::AllocaInst *alloca =
        IRBuilder.CreateAlloca(TranslateType(Ctx.LLVMCtx, *varDecl->Type, SM),
                               nullptr, varDecl->Name + ".var");
    SM.addAllocaForVarDecl(*varDecl, *alloca);
  }

  // Create alloca for return
  if (!CGCtx.CurrFuncLLVM->getReturnType()->isVoidTy()) {
    CGCtx.RetAlloca = IRBuilder.CreateAlloca(
        TranslateType(Ctx.LLVMCtx, *decl.FuncType.RetType, SM), nullptr,
        "ret.var");
  }

  // Create store for reciver if it exists
  if (decl.Reciver) {
    IRBuilder.CreateStore(CGCtx.CurrFuncLLVM->getArg(0),
                          &SM.getAllocaForVarDecl(*decl.Reciver));
  }

  // Create store for each param
  for (unsigned i = 0; i < decl.Params.size(); ++i) {
    IRBuilder.CreateStore(CGCtx.CurrFuncLLVM->getArg(i + !!decl.Reciver),
                          &SM.getAllocaForVarDecl(*decl.Params[i]));
  }

  // Create return block
  CGCtx.RetsBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "func.end");
  for (const std::unique_ptr<Stmt> &stmt : decl.Body) {
    emitStmt(*stmt);
  }

  // Create unreachable if function returns but not all paths have a return,
  // otherwise just jump to the return block
  if (!CGCtx.CurrFuncLLVM->getReturnType()->isVoidTy() &&
      !DoesReturnInAllPaths(decl)) {
    IRBuilder.CreateIntrinsic(llvm::Intrinsic::IndependentIntrinsics::trap, {},
                              {});
    IRBuilder.CreateUnreachable();
  } else {
    IRBuilder.CreateBr(CGCtx.RetsBB);
  }

  emitBlock(*CGCtx.RetsBB);

  if (CGCtx.CurrFuncLLVM->getReturnType()->isVoidTy()) {
    IRBuilder.CreateRetVoid();
  } else if (SM.getReturnStmtsForFunc(decl).size() != 0) {
    IRBuilder.CreateRet(IRBuilder.CreateLoad(CGCtx.RetAlloca));
  }
}

void CodeGen::CodeGenContext::reset() {
  StopEmmiting = {false};
  RetsBB = nullptr;
  RetAlloca = nullptr;
  CurrFuncLLVM = nullptr;
  DeferedValues.clear();
  IsInOutlineFunction = false;
  OutlinedVarDeclToAlloca.clear();
}

void CodeGen::emitDecl(const Decl &decl) {
  if (auto *funcDecl = llvm::dyn_cast<FuncDecl>(&decl)) {
    emitFuncDecl(*funcDecl);
    CGCtx.reset();
  } else if (auto *typeDecl = llvm::dyn_cast<TypeDecl>(&decl)) {
    emitTypeDecl(*typeDecl);
  }
}
