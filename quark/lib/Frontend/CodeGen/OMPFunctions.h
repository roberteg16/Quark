#ifndef __QUARK_LIB_FRONTEND_CODEGEN_OMPFUNCTIONS_H__
#define __QUARK_LIB_FRONTEND_CODEGEN_OMPFUNCTIONS_H__

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>

// LLVM OpenMP 1.8.3.1 functions
// Doc: https://openmp.llvm.org/Reference.pdf

namespace quark {

enum class OMPFunction {
#define QK_OMPFunction(ID, RetType, IsVarArgs, ...) ID,
#include "OMPFunctions.def"
};

llvm::FunctionCallee GetOMPFunction(OMPFunction, llvm::Module &);

} // namespace quark

#endif // __QUARK_LIB_FRONTEND_CODEGEN_OMPFUNCTIONS_H__
