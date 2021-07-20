#include <quark/Frontend/AST/Type.h>

#include <quark/Frontend/AST/ASTDumper.h>
#include <quark/Frontend/AST/Decl.h>

#include <llvm/Support/Casting.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>

using namespace quark;

llvm::StringRef quark::GetTypeName(BuiltinTypeKind type) {
  switch (type) {
  case BuiltinTypeKind::Void:
    return "void";
  case BuiltinTypeKind::b1:
    return "b1";
  case BuiltinTypeKind::i8:
    return "i8";
  case BuiltinTypeKind::u8:
    return "u8";
  case BuiltinTypeKind::i16:
    return "i16";
  case BuiltinTypeKind::u16:
    return "u16";
  case BuiltinTypeKind::i32:
    return "i32";
  case BuiltinTypeKind::u32:
    return "u32";
  case BuiltinTypeKind::i64:
    return "i64";
  case BuiltinTypeKind::u64:
    return "u64";
  case BuiltinTypeKind::f32:
    return "f32";
  case BuiltinTypeKind::f64:
    return "f64";
  case BuiltinTypeKind::f80:
    return "f80";
  case BuiltinTypeKind::i128:
    return "i128";
  case BuiltinTypeKind::u128:
    return "u128";
  case BuiltinTypeKind::i256:
    return "i256";
  case BuiltinTypeKind::u256:
    return "u256";
  case BuiltinTypeKind::i512:
    return "i512";
  case BuiltinTypeKind::u512:
    return "u512";
  }
}

std::size_t quark::GetBytes(BuiltinTypeKind type) {
  switch (type) {
  case BuiltinTypeKind::Void:
    return 0;
  case BuiltinTypeKind::b1:
  case BuiltinTypeKind::i8:
  case BuiltinTypeKind::u8:
    return 1;
  case BuiltinTypeKind::i16:
  case BuiltinTypeKind::u16:
    return 2;
  case BuiltinTypeKind::i32:
  case BuiltinTypeKind::u32:
    return 4;
  case BuiltinTypeKind::i64:
  case BuiltinTypeKind::u64:
    return 8;
  case BuiltinTypeKind::f32:
    return 4;
  case BuiltinTypeKind::f64:
    return 8;
  case BuiltinTypeKind::f80:
    return 16;
  case BuiltinTypeKind::i128:
  case BuiltinTypeKind::u128:
    return 16;
  case BuiltinTypeKind::i256:
  case BuiltinTypeKind::u256:
    return 32;
  case BuiltinTypeKind::i512:
  case BuiltinTypeKind::u512:
    return 64;
  }
}

bool quark::IsSigned(BuiltinTypeKind type) {
  switch (type) {
  case BuiltinTypeKind::i8:
  case BuiltinTypeKind::i16:
  case BuiltinTypeKind::i32:
  case BuiltinTypeKind::i64:
  case BuiltinTypeKind::i128:
  case BuiltinTypeKind::i256:
  case BuiltinTypeKind::i512:
  case BuiltinTypeKind::f32:
  case BuiltinTypeKind::f64:
  case BuiltinTypeKind::f80:
    return true;
  case BuiltinTypeKind::Void:
  case BuiltinTypeKind::b1:
  case BuiltinTypeKind::u8:
  case BuiltinTypeKind::u16:
  case BuiltinTypeKind::u32:
  case BuiltinTypeKind::u64:
  case BuiltinTypeKind::u128:
  case BuiltinTypeKind::u256:
  case BuiltinTypeKind::u512:
    return false;
  }
}

bool quark::IsUnsigned(BuiltinTypeKind type) {
  switch (type) {
  case BuiltinTypeKind::Void:
  case BuiltinTypeKind::b1:
  case BuiltinTypeKind::i8:
  case BuiltinTypeKind::i16:
  case BuiltinTypeKind::i32:
  case BuiltinTypeKind::i64:
  case BuiltinTypeKind::i128:
  case BuiltinTypeKind::i256:
  case BuiltinTypeKind::i512:
  case BuiltinTypeKind::f32:
  case BuiltinTypeKind::f64:
  case BuiltinTypeKind::f80:
    return false;
  case BuiltinTypeKind::u8:
  case BuiltinTypeKind::u16:
  case BuiltinTypeKind::u32:
  case BuiltinTypeKind::u64:
  case BuiltinTypeKind::u128:
  case BuiltinTypeKind::u256:
  case BuiltinTypeKind::u512:
    return true;
  }
}

bool quark::IsFloatingPoint(BuiltinTypeKind type) {
  switch (type) {
  case BuiltinTypeKind::Void:
  case BuiltinTypeKind::b1:
  case BuiltinTypeKind::i8:
  case BuiltinTypeKind::u8:
  case BuiltinTypeKind::i16:
  case BuiltinTypeKind::u16:
  case BuiltinTypeKind::i32:
  case BuiltinTypeKind::u32:
  case BuiltinTypeKind::i64:
  case BuiltinTypeKind::u64:
  case BuiltinTypeKind::i128:
  case BuiltinTypeKind::u128:
  case BuiltinTypeKind::i256:
  case BuiltinTypeKind::u256:
  case BuiltinTypeKind::i512:
  case BuiltinTypeKind::u512:
    return false;
  case BuiltinTypeKind::f32:
  case BuiltinTypeKind::f64:
  case BuiltinTypeKind::f80:
    return true;
  }
}

bool quark::IsInteger(BuiltinTypeKind type) {
  switch (type) {
  case BuiltinTypeKind::i8:
  case BuiltinTypeKind::u8:
  case BuiltinTypeKind::i16:
  case BuiltinTypeKind::u16:
  case BuiltinTypeKind::i32:
  case BuiltinTypeKind::u32:
  case BuiltinTypeKind::i64:
  case BuiltinTypeKind::u64:
    return true;
  case BuiltinTypeKind::b1:
  case BuiltinTypeKind::Void:
  case BuiltinTypeKind::i128:
  case BuiltinTypeKind::u128:
  case BuiltinTypeKind::i256:
  case BuiltinTypeKind::u256:
  case BuiltinTypeKind::i512:
  case BuiltinTypeKind::u512:
  case BuiltinTypeKind::f32:
  case BuiltinTypeKind::f64:
  case BuiltinTypeKind::f80:
    return false;
  }
}

bool quark::IsVector(BuiltinTypeKind type) {
  return !IsInteger(type) && !IsFloatingPoint(type) &&
         type != BuiltinTypeKind::Void && !IsBoolean(type);
}

bool quark::IsBoolean(BuiltinTypeKind type) {
  return type == BuiltinTypeKind::b1;
}

Type::~Type() {}

void Type::print(llvm::raw_ostream &out) const {}
void Type::dump() const { print(llvm::dbgs()); }

NamedType::~NamedType() {}

std::size_t BuiltinType::bytes() const { return GetBytes(Kind); }
bool BuiltinType::isSigned() const { return IsSigned(Kind); }
bool BuiltinType::isUnsigned() const { return IsUnsigned(Kind); }
bool BuiltinType::isFloatingPoint() const { return IsFloatingPoint(Kind); }
bool BuiltinType::isInteger() const { return IsInteger(Kind); }
bool BuiltinType::isVoid() const { return Kind == BuiltinTypeKind::Void; }
bool BuiltinType::isVector() const { return IsVector(Kind); }
bool BuiltinType::isBoolean() const { return IsBoolean(Kind); }

std::unique_ptr<Type> AliasType::getRealType() const {
  if (auto *type = llvm::dyn_cast<AliasType>(&RealType)) {
    return type->getRealType();
  }
  return RealType.clone();
}

CompoundType::CompoundType(const TypeDecl &typeDecl)
    : NamedType(TypeKind::CompoundType, typeDecl.Name), Decl(typeDecl) {
  for (auto &decl : typeDecl.FieldDecls) {
    Types.push_back(decl->Type->clone());
  }
}

const Type &BuiltinType::desugar() const { return *this; }
const Type &AliasType::desugar() const { return RealType; }
const Type &CompoundType::desugar() const { return *this; }
const Type &PtrType::desugar() const { return *PointeeType; }
const Type &ArrayType::desugar() const { return *RealType; }
const Type &FuncType::desugar() const { return *this; }

std::unique_ptr<Type> BuiltinType::clone() const {
  return std::make_unique<BuiltinType>(Kind);
}

std::unique_ptr<Type> CompoundType::clone() const {
  return std::make_unique<CompoundType>(this->Decl);
}

std::unique_ptr<Type> AliasType::clone() const {
  return std::make_unique<AliasType>(Name, RealType);
}

std::unique_ptr<Type> PtrType::clone() const {
  return std::make_unique<PtrType>(this->PointeeType->clone());
}

std::unique_ptr<Type> ArrayType::clone() const {
  return std::make_unique<ArrayType>(this->RealType->clone(), this->Elements);
}

std::unique_ptr<Type> FuncType::clone() const {
  llvm::SmallVector<std::unique_ptr<Type>, 4> paramsTypes;
  paramsTypes.reserve(Params.size());
  for (auto &param : Params) {
    paramsTypes.push_back(param->clone());
  }

  return std::make_unique<FuncType>(RetType->clone(), paramsTypes,
                                    Reciver ? Reciver->clone() : nullptr);
}

// ==== Type comparations ==== //
bool BuiltinType::operator==(const Type &rhs) const {
  auto *builtin = llvm::dyn_cast<BuiltinType>(&rhs);
  if (!builtin) {
    return false;
  }
  return this->Kind == builtin->Kind;
}

bool CompoundType::operator==(const Type &rhs) const {
  auto *compound = llvm::dyn_cast<CompoundType>(&rhs);
  if (!compound) {
    return false;
  }
  return this->Name == compound->Name;
}

bool AliasType::operator==(const Type &rhs) const {
  auto *alias = llvm::dyn_cast<AliasType>(&rhs);
  if (!alias) {
    return false;
  }
  // TODO: add method to get the real value under an alias
  return this->Name == alias->Name;
}

bool PtrType::operator==(const Type &rhs) const {
  auto *ptr = llvm::dyn_cast<PtrType>(&rhs);
  if (!ptr) {
    return false;
  }
  return *this->PointeeType == *ptr->PointeeType;
}

bool ArrayType::operator==(const Type &rhs) const {
  auto *array = llvm::dyn_cast<ArrayType>(&rhs);
  if (!array) {
    return false;
  }
  return this->Elements == array->Elements &&
         *this->RealType == *array->RealType;
}

static bool AreReciversEqual(const Type *lhs, const Type *rhs) {
  if ((lhs && !rhs) || (!lhs && rhs)) {
    return false;
  }
  if (!lhs && !rhs) {
    return true;
  }
  return *lhs == *rhs;
}

bool FuncType::operator==(const Type &rhs) const {
  auto *funcType = llvm::dyn_cast<FuncType>(&rhs);
  if (!funcType) {
    return false;
  }

  // Discard by number of params or name
  if (Params.size() != funcType->Params.size()) {
    return false;
  }
  // Discard by recivers
  if (!AreReciversEqual(funcType->Reciver.get(), Reciver.get())) {
    return false;
  }

  // Discard by types of params
  for (const auto pair : llvm::zip(funcType->Params, Params)) {
    if (!(std::get<0>(pair)->desugar() == std::get<1>(pair)->desugar())) {
      return false;
    }
  }

  return true;
}
// ==== End of comparations ==== //

//== Start of dumpings ==//
void BuiltinType::print(llvm::raw_ostream &out) const { out << Name; }
void BuiltinType::dump() const { print(llvm::dbgs()); }

void AliasType::print(llvm::raw_ostream &out) const { out << Name; }
void AliasType::dump() const { print(llvm::dbgs()); }

void CompoundType::print(llvm::raw_ostream &out) const { out << Name; }
void CompoundType::dump() const { print(llvm::dbgs()); }

void PtrType::print(llvm::raw_ostream &out) const {
  out << "*";
  PointeeType->print(out);
}
void PtrType::dump() const { print(llvm::dbgs()); }

void ArrayType::print(llvm::raw_ostream &out) const {
  out << "[" << Elements << "]";
  RealType->print(out);
}
void ArrayType::dump() const { print(llvm::dbgs()); }

void FuncType::print(llvm::raw_ostream &out) const {
  if (Reciver) {
    out << "( ";
    Reciver->print(out);
    out << " ) ";
  }
  out << "(*) ( ";

  for (unsigned i = 0; i < Params.size(); i++) {
    Params[i]->print(out);
    out << (i == Params.size() - 1 ? "" : ", ");
  }
  out << " ) -> ";
  RetType->print(out);
}

void FuncType::dump() const { print(llvm::dbgs()); }

const CompoundType *quark::CheckCompoundOrTypeToCompound(const Type *type) {
  if (const auto *ptrType = llvm::dyn_cast<quark::PtrType>(type)) {
    type = ptrType->PointeeType.get();
  }

  auto *compType = llvm::dyn_cast<quark::CompoundType>(type);
  if (!compType) {
    return nullptr;
  }

  return compType;
}
