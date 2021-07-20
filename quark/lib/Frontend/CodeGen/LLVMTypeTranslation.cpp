#include "LLVMTypeTranslation.h"
#include "llvm/ADT/None.h"
#include "llvm/IR/Instruction.h"

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/ErrorHandling.h>

#include <quark/Frontend/AST/Type.h>
#include <quark/Frontend/Parsing/SourceModule.h>

using namespace quark;

llvm::Type *quark::TranslateType(llvm::LLVMContext &ctx, const Type &type,
                                 SourceModule &sm) {
  switch (type.getKind()) {
  case TypeKind::ArrayType: {
    const ArrayType &arrayType = *llvm::cast<ArrayType>(&type);
    return llvm::ArrayType::get(TranslateType(ctx, *arrayType.RealType, sm),
                                arrayType.Elements);
  }
  case TypeKind::BuiltinType:
    switch (llvm::cast<BuiltinType>(&type)->Kind) {
    case BuiltinTypeKind::Void:
      return llvm::Type::getVoidTy(ctx);
    case BuiltinTypeKind::b1:
      return llvm::Type::getInt1Ty(ctx);
    case BuiltinTypeKind::i8:
    case BuiltinTypeKind::u8:
      return llvm::Type::getInt8Ty(ctx);
    case BuiltinTypeKind::i16:
    case BuiltinTypeKind::u16:
      return llvm::Type::getInt16Ty(ctx);
    case BuiltinTypeKind::i32:
    case BuiltinTypeKind::u32:
      return llvm::Type::getInt32Ty(ctx);
    case BuiltinTypeKind::i64:
    case BuiltinTypeKind::u64:
      return llvm::Type::getInt64Ty(ctx);
    case BuiltinTypeKind::i128:
    case BuiltinTypeKind::u128:
      return llvm::Type::getInt128Ty(ctx);
    case BuiltinTypeKind::f32:
      return llvm::Type::getFloatTy(ctx);
    case BuiltinTypeKind::f64:
      return llvm::Type::getDoubleTy(ctx);
    case BuiltinTypeKind::f80:
      return llvm::Type::getX86_FP80Ty(ctx);
    default:
      llvm::llvm_unreachable_internal("Type not yet supported");
      return nullptr;
    }
    break;
  case TypeKind::PtrType: {
    return llvm::PointerType::get(
        TranslateType(ctx, *llvm::cast<PtrType>(&type)->PointeeType, sm), 0);
  }
  case TypeKind::CompoundType: {
    auto &compoundType = *llvm::cast<CompoundType>(&type);
    return &sm.getLLVMTypeForCompoundType(compoundType.Name);
  }
  case TypeKind::AliasType:
    return TranslateType(ctx, llvm::cast<AliasType>(&type)->RealType, sm);
  case TypeKind::FuncType: {
    auto &funcType = *llvm::cast<FuncType>(&type);
    llvm::SmallVector<llvm::Type *, 4> paramTypes;
    if (funcType.Reciver) {
      paramTypes.push_back(TranslateType(ctx, funcType.Reciver->desugar(), sm));
    }
    for (auto &param : funcType.Params) {
      paramTypes.push_back(TranslateType(ctx, param->desugar(), sm));
    }
    return llvm::FunctionType::get(TranslateType(ctx, *funcType.RetType, sm),
                                   paramTypes, false);
  }
  }
}

llvm::Optional<llvm::Instruction::BinaryOps>
quark::TranslateArithmeticBinaryKind(BinaryOperatorKind kind,
                                     bool isFloatingCompatible, bool isSigned) {
  using ops = llvm::Instruction::BinaryOps;
  switch (kind) {
  case BinaryOperatorKind::Add:
    return isFloatingCompatible ? ops::FAdd : ops::Add;
  case BinaryOperatorKind::Div:
    return isFloatingCompatible ? ops::FDiv
                                : (isSigned ? ops::SDiv : ops::UDiv);
  case BinaryOperatorKind::Minus:
    return isFloatingCompatible ? ops::FSub : ops::Sub;
  case BinaryOperatorKind::Mod:
    return isFloatingCompatible ? ops::FRem
                                : (isSigned ? ops::SRem : ops::URem);
  case BinaryOperatorKind::Mul:
    return isFloatingCompatible ? ops::FMul : ops::Mul;
  default:
    return llvm::None;
  }
}

llvm::Optional<llvm::CmpInst::Predicate>
quark::TranslateLogicalBinaryKind(BinaryOperatorKind kind,
                                  bool isFloatingCompatible, bool ordered,
                                  bool isSigned) {
  using preds = llvm::CmpInst::Predicate;
  switch (kind) {
  case BinaryOperatorKind::LogicalEquals:
    return !isFloatingCompatible
               ? preds::ICMP_EQ
               : (ordered ? preds::FCMP_OEQ : preds::FCMP_UEQ);
  case BinaryOperatorKind::LogicalNotEquals:
    return !isFloatingCompatible
               ? preds::ICMP_NE
               : (ordered ? preds::FCMP_ONE : preds::FCMP_UNE);
  case BinaryOperatorKind::LogicalGreater:
    return !isFloatingCompatible
               ? (isSigned ? preds::ICMP_SGT : preds::ICMP_UGT)
               : (ordered ? preds::FCMP_OGT : preds::FCMP_UGT);
  case BinaryOperatorKind::LogicalGreaterEqual:
    return !isFloatingCompatible
               ? (isSigned ? preds::ICMP_SGE : preds::ICMP_UGE)
               : (ordered ? preds::FCMP_OGE : preds::FCMP_UGE);
  case BinaryOperatorKind::LogicalLess:
    return !isFloatingCompatible
               ? (isSigned ? preds::ICMP_SLT : preds::ICMP_ULT)
               : (ordered ? preds::FCMP_OLT : preds::FCMP_ULT);
  case BinaryOperatorKind::LogicalLessEqual:
    return !isFloatingCompatible
               ? (isSigned ? preds::ICMP_SLE : preds::ICMP_ULE)
               : (ordered ? preds::FCMP_OLE : preds::FCMP_ULE);
  default:
    return llvm::None;
  }
}
