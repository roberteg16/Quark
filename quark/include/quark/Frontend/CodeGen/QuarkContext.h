#ifndef __QUARK_FRONTEND_CODEGEN_QUARKCONTEXT_H__
#define __QUARK_FRONTEND_CODEGEN_QUARKCONTEXT_H__

#include <llvm/IR/LLVMContext.h>

namespace quark {

struct QuarkContext {
  static QuarkContext &buildContext() {
    static QuarkContext ctx;
    return ctx;
  }

  llvm::LLVMContext LLVMCtx;
};

} // namespace quark

#endif // __QUARK_FRONTEND_CODEGEN_QUARKCONTEXT_H__