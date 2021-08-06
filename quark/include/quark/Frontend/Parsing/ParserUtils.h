#ifndef __QUARK_FRONTEND_PARSING_PARSERUTILS_H__
#define __QUARK_FRONTEND_PARSING_PARSERUTILS_H__

#include <quark/Frontend/Parsing/LexContext.h>

namespace quark {

void PrintLocation(llvm::raw_ostream &out, const location loc);

void CheckPtrToValueOrValue(const Expr &expr);

std::unique_ptr<quark::Expr>
DerefererenceIfNeeded(std::unique_ptr<quark::Expr> expr,
                      const quark::TypeAccess &access);

const CompoundType *CheckCompoundOrTypeToCompound(const Type *type);

std::unique_ptr<quark::Expr> GetMemberAccess(std::unique_ptr<quark::Expr> expr,
                                             quark::TypeAccess &access);

std::unique_ptr<quark::Type> GetArrayAccessType(const quark::Expr &expr);

std::unique_ptr<Expr> AddCastIfNeededAndVarRefExpr(std::unique_ptr<Expr> expr);

std::unique_ptr<Expr> AddCastIfNeeded(std::unique_ptr<Expr> expr);

} // namespace quark

#endif // __QUARK_FRONTEND_PARSING_PARSERUTILS_H__
