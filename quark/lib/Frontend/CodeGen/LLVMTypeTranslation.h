#ifndef __QUARK_LIB_FRONTEND_LLVMTYPETRASNLATION_H__
#define __QUARK_LIB_FRONTEND_LLVMTYPETRASNLATION_H__

#include <llvm/IR/Instructions.h>
#include <quark/Frontend/AST/Expr.h>

namespace llvm {
class LLVMContext;
class Type;
} // namespace llvm

namespace quark {
struct Type;
class SourceModule;

llvm::Type *TranslateType(llvm::LLVMContext &, const Type &, SourceModule &);

llvm::Optional<llvm::Instruction::BinaryOps>
TranslateArithmeticBinaryKind(BinaryOperatorKind kind,
                              bool isFloatingCompatible = false,
                              bool isSigned = false);

llvm::Optional<llvm::CmpInst::Predicate>
TranslateLogicalBinaryKind(BinaryOperatorKind kind,
                           bool isFloatingCompatible = false,
                           bool ordered = false, bool isSigned = false);

} // namespace quark

#endif // __QUARK_LIB_FRONTEND_LLVMTYPETRANSLATION_H__
