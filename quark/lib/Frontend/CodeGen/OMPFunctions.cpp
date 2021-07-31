#include "OMPFunctions.h"

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>

#include "OMPTypes.h"

llvm::FunctionCallee quark::GetOMPFunction(quark::OMPFunction function,
                                           llvm::Module &mod) {
  switch (function) {
#define QK_OMPFunction(ID, RetType, IsVarArgs, ...)                            \
  case OMPFunction::ID: {                                                      \
    llvm::Type *params[] = {__VA_ARGS__};                                      \
    auto *funcType = llvm::FunctionType::get(RetType, params, IsVarArgs);      \
    return mod.getOrInsertFunction("__kmpc_" #ID, funcType);                   \
  }
#include "OMPFunctions.def"
  }
}
