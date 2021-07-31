#include "OMPTypes.h"

#include <llvm/IR/LLVMContext.h>

namespace {

llvm::StructType *GetOMPident_t(llvm::LLVMContext &ctxt) {
  llvm::Type *types[] = {
      llvm::Type::getInt32Ty(ctxt),          llvm::Type::getInt32Ty(ctxt),
      llvm::Type::getInt32Ty(ctxt),          llvm::Type::getInt32Ty(ctxt),
      llvm::PointerType::getInt8PtrTy(ctxt),
  };
  static llvm::StructType *structType = nullptr;
  if (!structType) {
    structType =
        llvm::StructType::create(llvm::makeArrayRef(types), "struct.ident_t");
  }
  return structType;
}

llvm::Type *GetOMPVoid(llvm::LLVMContext &ctxt) {
  return llvm::Type::getVoidTy(ctxt);
}

llvm::Type *GetOMPkmp_int32(llvm::LLVMContext &ctxt) {
  return llvm::Type::getInt32Ty(ctxt);
}

llvm::Type *GetOMPkmp_uint32(llvm::LLVMContext &ctxt) {
  return llvm::Type::getInt32Ty(ctxt);
}

llvm::Type *GetOMPkmp_int64(llvm::LLVMContext &ctxt) {
  return llvm::Type::getInt64Ty(ctxt);
}

llvm::Type *GetOMPkmp_uint64(llvm::LLVMContext &ctxt) {
  return llvm::Type::getInt64Ty(ctxt);
}

llvm::Type *GetOMPkmpc_micro(llvm::LLVMContext &ctxt) {
  llvm::Type *types[] = {llvm::Type::getInt32PtrTy(ctxt),
                         llvm::Type::getInt32PtrTy(ctxt)};

  return llvm::FunctionType::get(llvm::Type::getVoidTy(ctxt), types,
                                 /*isVarArg*/ true)
      ->getPointerTo();
}

llvm::Type *GetOMPkmp_critical_name(llvm::LLVMContext &ctxt) {
  llvm::llvm_unreachable_internal("Not implemented yet");
}

llvm::Type *GetOMPsched_type(llvm::LLVMContext &ctxt) {
  llvm::llvm_unreachable_internal("Not implemented yet");
}

llvm::Type *GetOMPreduce_func(llvm::LLVMContext &ctxt) {
  llvm::llvm_unreachable_internal("Not implemented yet");
}

} // namespace

using namespace quark;

llvm::Type *quark::GetOMPType(OMPType type, llvm::LLVMContext &ctx) {
  switch (type) {
#define QK_OMPType(ID)                                                         \
  case OMPType::ID:                                                            \
    return GetOMP##ID(ctx);
#include "OMPTypes.def"
  }
}
