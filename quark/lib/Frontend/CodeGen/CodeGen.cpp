#include <quark/Frontend/CodeGen/CodeGen.h>

#include <quark/Frontend/CodeGen/QuarkContext.h>
#include <quark/Frontend/Parsing/SourceModule.h>

#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/ScopeExit.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/raw_ostream.h>

#include "LLVMTypeTranslation.h"
#include "Mangler.h"

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
    BuiltinTypeKind kind =
        llvm::cast<BuiltinType>(unaryExpr.Lhs->getType()).Kind;
    bool isFloatingCompatible =
        quark::IsFloatingPoint(kind) || quark::IsVector(kind);
    bool isSigned = quark::IsSigned(kind);

    switch (unaryExpr.Op) {
    case UnaryOperatorKind::AddressOf:
    case UnaryOperatorKind::Dereference:
      return lhs;
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
    return &SM.getAllocaForVarDecl(llvm::cast<VarRefExpr>(&expr)->RefVar);
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
    return IRBuilder.CreateCall(
        CGCtx.CurrFuncLLVM->getFunctionType(),
        CGCtx.DeclToFuncCalleeMap[&funcCallExpr.FunctionDecl].getCallee(),
        params, funcCallExpr.FunctionDecl.Name);
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
    llvm::llvm_unreachable_internal("ExplicitCastExpr not implemented yet");
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
      llvm::Value *alloca = &SM.getAllocaForVarDecl(*varDeclStmt.VarDecl);
      llvm::Value *varInitExpr = getExpr(*varDeclStmt.InitExpr);
      IRBuilder.CreateStore(varInitExpr, alloca);
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

void CodeGen::emitFuncDecl(const FuncDecl &decl) {
  llvm::DenseMap<const Decl *, unsigned> idx;

  // Calculate params types
  llvm::SmallVector<llvm::Type *, 6> paramTypes;
  paramTypes.reserve(!!decl.Reciver + decl.Params.size());

  unsigned idxOfParam = 0;
  // First is going to be the reciever as a pointer in case of existing one
  if (decl.Reciver) {
    paramTypes.push_back(TranslateType(Ctx.LLVMCtx, *decl.Reciver->Type, SM));
    idx[&decl] = idxOfParam++;
  }

  // Emit normal params
  for (unsigned i = 0; i < decl.Params.size(); ++i) {
    paramTypes.push_back(TranslateType(Ctx.LLVMCtx, *decl.Params[i]->Type, SM));
    idx[decl.Params[i].get()] = idxOfParam++;
  }

  llvm::Type *retType = TranslateType(Ctx.LLVMCtx, *decl.FuncType.RetType, SM);
  llvm::FunctionType *funcType =
      llvm::FunctionType::get(retType, paramTypes, /*isVarArg*/ false);

  auto isMain = decl.Name == "main";

  assert(!Mod->getFunction(decl.Name));
  std::string mangledName =
      isMain ? decl.Name.data() : mangleFunction(decl.Name, decl.FuncType);
  CGCtx.DeclToFuncCalleeMap[&decl] =
      Mod->getOrInsertFunction(mangledName, funcType);
  llvm::Function *func = Mod->getFunction(mangledName);
  assert(func);

  // Save in codegen context for later
  CGCtx.CurrFuncLLVM = func;

  emitBlock(*llvm::BasicBlock::Create(Ctx.LLVMCtx, "entry"));

  // Emit all allocas needed for vars, reciever and params
  for (const VarDecl *varDecl : SM.getVarDeclStmtsForFunc(decl)) {
    llvm::AllocaInst *alloca =
        IRBuilder.CreateAlloca(TranslateType(Ctx.LLVMCtx, *varDecl->Type, SM),
                               nullptr, varDecl->Name + ".var");
    SM.addAllocaForVarDecl(*varDecl, *alloca);
  }

  // Create alloca and final block for multiple returns
  auto returns = SM.getReturnStmtsForFunc(decl);

  CGCtx.RetsBB = llvm::BasicBlock::Create(Ctx.LLVMCtx, "func.end");

  if (!CGCtx.CurrFuncLLVM->getReturnType()->isVoidTy()) {
    CGCtx.RetAlloca = IRBuilder.CreateAlloca(
        TranslateType(Ctx.LLVMCtx, *decl.FuncType.RetType, SM), nullptr,
        "ret.var");
  }

  for (const VarDecl *varDecl : SM.getVarDeclStmtsForFunc(decl)) {
    if (varDecl->getVarDeclKind() == VarDeclKind::RecieverVar ||
        varDecl->getVarDeclKind() == VarDeclKind::ParamVar) {
      IRBuilder.CreateStore(func->getArg(idx[varDecl]),
                            &SM.getAllocaForVarDecl(*varDecl));
    }
  }

  for (const std::unique_ptr<Stmt> &stmt : decl.Body) {
    emitStmt(*stmt);
  }

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
  } else if (returns.size() != 0) {
    IRBuilder.CreateRet(IRBuilder.CreateLoad(CGCtx.RetAlloca));
  }
}

void CodeGen::CodeGenContext::reset() {
  StopEmmiting = {false};
  RetsBB = nullptr;
  RetAlloca = nullptr;
  CurrFuncLLVM = nullptr;
  DeferedValues.clear();
}

void CodeGen::emitDecl(const Decl &decl) {
  if (auto *funcDecl = llvm::dyn_cast<FuncDecl>(&decl)) {
    emitFuncDecl(*funcDecl);
    CGCtx.reset();
  } else if (auto *typeDecl = llvm::dyn_cast<TypeDecl>(&decl)) {
    emitTypeDecl(*typeDecl);
  }
}
