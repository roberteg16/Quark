#include <quark/Frontend/Parsing/SourceModule.h>

#include "SourceModuleCache.h"
#include <quark/Frontend/AST/ASTDumper.h>
#include <quark/Frontend/AST/Decl.h>
#include <quark/Frontend/AST/Stmt.h>
#include <quark/Frontend/AST/Type.h>
#include <quark/Frontend/Parsing/LexContext.h>

#include <llvm/ADT/DenseMap.h>
#include <llvm/Support/Debug.h>

using namespace quark;

const BuiltinType SourceModule::BuiltinTypes[19] = {
    {BuiltinTypeKind::Void}, {BuiltinTypeKind::b1},   {BuiltinTypeKind::i8},
    {BuiltinTypeKind::u8},   {BuiltinTypeKind::i16},  {BuiltinTypeKind::u16},
    {BuiltinTypeKind::i32},  {BuiltinTypeKind::u32},  {BuiltinTypeKind::i64},
    {BuiltinTypeKind::u64},  {BuiltinTypeKind::f32},  {BuiltinTypeKind::f64},
    {BuiltinTypeKind::f80},  {BuiltinTypeKind::i128}, {BuiltinTypeKind::u128},
    {BuiltinTypeKind::u256}, {BuiltinTypeKind::i256}, {BuiltinTypeKind::i512},
    {BuiltinTypeKind::u512}};

// const FuncDecl SourceModule::BuiltinFuncs[1] = {}

void SourceModule::print(llvm::raw_ostream &out) const {
  ASTDumper{out}.dump(*this);
}

SourceModule::SourceModule() {}
SourceModule::~SourceModule() {}
SourceModule::SourceModule(SourceModule &&sm) { *this = std::move(sm); }
SourceModule &SourceModule::operator=(SourceModule &&sm) {
  ExportedModule = std::move(sm.ExportedModule);
  ImportedModules = std::move(sm.ImportedModules);
  Declarations = std::move(sm.Declarations);
  PImplCache = std::move(sm.PImplCache);
  return *this;
}

void SourceModule::dump() const { print(llvm::dbgs()); }

llvm::ArrayRef<const VarDecl *>
SourceModule::getVarDeclStmtsForFunc(const FuncDecl &funcDecl) {
  return PImplCache->VarDeclStmtByFuncMap[&funcDecl];
}

llvm::ArrayRef<const ReturnStmt *>
SourceModule::getReturnStmtsForFunc(const FuncDecl &funcDecl) {
  return PImplCache->RetStmtByFuncMap[&funcDecl];
}

llvm::AllocaInst &SourceModule::getAllocaForVarDecl(const VarDecl &varDecl) {
  return *PImplCache->VarDeclToAllocaMap[&varDecl];
}

llvm::StructType &SourceModule::getLLVMTypeForCompoundType(llvm::StringRef id) {
  return *PImplCache->CompoundTypeToLLVMTypeMap[id.str()];
}

void SourceModule::addAllocaForVarDecl(const VarDecl &varDecl,
                                       llvm::AllocaInst &alloca) {
  PImplCache->VarDeclToAllocaMap[&varDecl] = &alloca;
}

void SourceModule::addLLVMTypeForCompoundType(llvm::StructType &structType,
                                              const llvm::StringRef id) {
  PImplCache->CompoundTypeToLLVMTypeMap[id.str()] = &structType;
}

void SourceModule::fill(LexContext &ctx, llvm::SmallString<10> exported,
                        llvm::SmallVector<llvm::SmallString<10>, 10> imports,
                        std::deque<std::unique_ptr<Decl>> decls) {
  ExportedModule = std::move(exported);
  ImportedModules = std::move(imports);
  Declarations = std::move(decls);
  PImplCache = std::move(ctx.PImplCache);
}
