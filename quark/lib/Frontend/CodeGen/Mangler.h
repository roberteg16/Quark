#ifndef __QUARK_LIB_FRONTEND_MANGLER_H__
#define __QUARK_LIB_FRONTEND_MANGLER_H__

#include <llvm/ADT/StringRef.h>

namespace quark {

struct FuncType;

std::string mangleFunction(llvm::StringRef, const FuncType &);

} // namespace quark

#endif // __QUARK_LIB_FRONTEND_MANGLER_H__
