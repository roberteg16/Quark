#ifndef __QUARK_LIB_FRONTEND_CODEGEN_OMPTYPES_H__
#define __QUARK_LIB_FRONTEND_CODEGEN_OMPTYPES_H__

#include "llvm/IR/LLVMContext.h"
#include <llvm/IR/DerivedTypes.h>

// LLVM OpenMP 1.8.3.1 functions
// Doc: https://openmp.llvm.org/Reference.pdf

namespace quark {

enum class OMPType {
#define QK_OMPType(ID) ID,
#include "OMPTypes.def"
};

llvm::Type *GetOMPType(OMPType type, llvm::LLVMContext &ctx);

} // namespace quark

#endif // __QUARK_LIB_FRONTEND_CODEGEN_OMPTYPES_H__
