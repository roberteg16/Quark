#include <quark/Frontend/AST/Decl.h>

#include <quark/Frontend/AST/ASTDumper.h>

#include <llvm/Support/Debug.h>

using namespace quark;

void Decl::print(llvm::raw_ostream &out) const { ASTDumper{out}.dump(*this); }
void Decl::dump() const { print(llvm::dbgs()); }

Decl::~Decl() {}
#define QK_DECL(NODE)                                                          \
  NODE::~NODE() {}
#include <quark/Frontend/AST/ASTNodes.def>

llvm::StringRef quark::ToString(VarDeclKind kind) {
  switch (kind) {
  case VarDeclKind::LocalVar:
    return "local";
  case VarDeclKind::ParamVar:
    return "param";
  case VarDeclKind::RecieverVar:
    return "reciv";
  case VarDeclKind::None:
    llvm::llvm_unreachable_internal("VarDeclKind not setted correctly");
  }
}

bool VarDecl::operator==(const VarDecl &rhs) const {
  return this->Kind == rhs.Kind && this->Name == rhs.Name &&
         &this->Type == &rhs.Type;
}

void FuncDecl::fillFunction(
    llvm::SmallVector<std::unique_ptr<VarDecl>, 4> params,
    std::unique_ptr<Type> returnType, std::vector<std::unique_ptr<Stmt>> stmts,
    std::unique_ptr<VarDecl> reciver) {
  Params = std::move(params);
  Body = std::move(stmts);
  Reciver = std::move(reciver);

  llvm::SmallVector<std::unique_ptr<Type>, 4> paramTypes;
  llvm::SmallVector<std::unique_ptr<Type>, 4> paramTypes2;
  paramTypes.reserve(params.size());
  paramTypes2.reserve(params.size());
  for (auto &param : Params) {
    paramTypes.push_back(param->Type->clone());
    paramTypes2.push_back(param->Type->clone());
  }

  Signature.Name = Name;
  Signature.Reciver = Reciver ? Reciver->Type->clone() : nullptr;
  Signature.ParamTypes = std::move(paramTypes);

  FuncType.RetType = std::move(returnType);
  FuncType.Reciver = Reciver ? Reciver->Type->clone() : nullptr;
  FuncType.Params = std::move(paramTypes2);
}

bool FuncDecl::operator==(const FuncDecl &rhs) const {
  return Signature == rhs.Signature;
}

const TypeFieldDecl *TypeDecl::findField(llvm::StringRef name) const {
  for (const std::unique_ptr<TypeFieldDecl> &field : FieldDecls) {
    if (field->Name == name) {
      return field.get();
    }
  }
  return nullptr;
}

FuncDecl::FuncSignature::FuncSignature(
    llvm::StringRef name, llvm::ArrayRef<std::unique_ptr<Expr>> params,
    std::unique_ptr<Type> reciver)
    : Name(name), Reciver(std::move(reciver)) {
  llvm::SmallVector<std::unique_ptr<Type>, 4> paramTypes;
  for (auto &expr : params) {
    paramTypes.push_back(expr->getType().clone());
  }
  ParamTypes = std::move(paramTypes);
}

static bool AreReciversEqual(const Type *lhs, const Type *rhs) {
  if ((lhs && !rhs) || (!lhs && rhs)) {
    return false;
  }
  if (!lhs && !rhs) {
    return true;
  }
  return lhs->desugar().innerType() == rhs->desugar().innerType();
}

bool FuncDecl::FuncSignature::operator==(const FuncSignature &rhs) const {
  if (Name != rhs.Name || ParamTypes.size() != rhs.ParamTypes.size()) {
    return false;
  }
  if (!AreReciversEqual(Reciver.get(), rhs.Reciver.get())) {
    return false;
  }
  for (const auto pair : llvm::zip(ParamTypes, rhs.ParamTypes)) {
    if (!(*std::get<0>(pair) == *std::get<1>(pair))) {
      return false;
    }
  }
  return true;
}

void FuncDecl::FuncSignature::print(llvm::raw_ostream &out) const {
  if (Reciver) {
    out << "(";
    Reciver->print(out);
    out << ") ";
  }
  out << Name << ":";
  for (unsigned i = 0; i < ParamTypes.size(); i++) {
    ParamTypes[i]->print(out);
    out << (i == ParamTypes.size() - 1 ? "" : ", ");
  }
}
void FuncDecl::FuncSignature::dump() const { print(llvm::dbgs()); }
