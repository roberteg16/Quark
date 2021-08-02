#include <quark/Frontend/AST/Type.h>

#include <quark/Frontend/AST/Decl.h>

#include <llvm/Support/Casting.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/raw_ostream.h>

using namespace quark;

template <class T, std::size_t N>
constexpr std::size_t Size(const T (&array)[N]) noexcept {
  return N;
}

constexpr std::size_t quark::GetNumOfBuiltinTypes() {
  constexpr BuiltinTypeKind kinds[] = {
#define QK_BUILTIN_TYPE(TYPE, NAME, BYTES, SIGNED) BuiltinTypeKind::TYPE,
#include <quark/Frontend/AST/BuiltinTypes.def>
  };
  return Size(kinds);
}

llvm::StringRef quark::ToString(BuiltinTypeKind type) {
  switch (type) {
#define QK_BUILTIN_TYPE(TYPE, NAME, BYTES, SIGNED)                             \
  case BuiltinTypeKind::TYPE:                                                  \
    return #NAME;
#include <quark/Frontend/AST/BuiltinTypes.def>
  }
}

std::size_t quark::GetBytes(BuiltinTypeKind type) {
  switch (type) {
#define QK_BUILTIN_TYPE(TYPE, NAME, BYTES, SIGNED)                             \
  case BuiltinTypeKind::TYPE:                                                  \
    return BYTES;
#include <quark/Frontend/AST/BuiltinTypes.def>
  }
}

bool quark::IsSigned(BuiltinTypeKind type) {
  switch (type) {
#define QK_BUILTIN_TYPE(TYPE, NAME, BYTES, SIGNED)                             \
  case BuiltinTypeKind::TYPE:                                                  \
    return SIGNED;
#include <quark/Frontend/AST/BuiltinTypes.def>
  }
}

bool quark::IsUnsigned(BuiltinTypeKind type) { return !IsSigned(type); }

bool quark::IsFloatingPoint(BuiltinTypeKind type) {
#define QK_BUILTIN_FLOATING_TYPE(TYPE, BYTES, SIGNED)                          \
  if (type == BuiltinTypeKind::TYPE) {                                         \
    return true;                                                               \
  }
#include <quark/Frontend/AST/BuiltinTypes.def>
  return false;
}

bool quark::IsInteger(BuiltinTypeKind type) {
#define QK_BUILTIN_INTEGER_TYPE(TYPE, BYTES, SIGNED)                           \
  if (type == BuiltinTypeKind::TYPE) {                                         \
    return true;                                                               \
  }
#include <quark/Frontend/AST/BuiltinTypes.def>
  return false;
}

bool quark::IsVector(BuiltinTypeKind type) {
#define QK_BUILTIN_VECTOR_TYPE(TYPE, BYTES, SIGNED)                            \
  if (type == BuiltinTypeKind::TYPE) {                                         \
    return true;                                                               \
  }
#include <quark/Frontend/AST/BuiltinTypes.def>
  return false;
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
const Type &PtrType::desugar() const { return *this; }
const Type &ArrayType::desugar() const { return *this; }
const Type &FuncType::desugar() const { return *this; }

const Type &BuiltinType::innerType() const { return *this; }
const Type &AliasType::innerType() const { return *this; }
const Type &CompoundType::innerType() const { return *this; }
const Type &PtrType::innerType() const { return *PointeeType; }
const Type &ArrayType::innerType() const { return *RealType; }
const Type &FuncType::innerType() const { return *this; }

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

llvm::StringRef quark::ToString(TypeCasting typeCasting) {
  switch (typeCasting) {
  case TypeCasting::Unknown:
    return "Unknown";
  case TypeCasting::Same:
    return "Same";
  case TypeCasting::Trunc:
    return "Trunc";
  case TypeCasting::ZExt:
    return "ZExt";
  case TypeCasting::SExt:
    return "SExt";
  case TypeCasting::FPTrunc:
    return "FPTrunc";
  case TypeCasting::PFExt:
    return "PFExt";
  case TypeCasting::FPToInt:
    return "FPToInt";
  case TypeCasting::FPToUInt:
    return "FPToUInt";
  case TypeCasting::IntToFP:
    return "IntToFP";
  case TypeCasting::UIntToFP:
    return "UIntToFp";
  }
}

static TypeCasting CastBuiltinType(const BuiltinType &fromType,
                                   const BuiltinType &toType) {
  using tc = TypeCasting;
  constexpr std::size_t numOfBuiltinTypes = GetNumOfBuiltinTypes();
  constexpr TypeCasting castings[numOfBuiltinTypes][numOfBuiltinTypes] = {
      // clang-format off
//            Void        b1           u8           i8          u16          i16         u32          i32         u64          i64         f32          f64          f80          u128        i128        i256        u256        i512    u512
/*Void*/ {tc::Same,   tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*b1*/   {tc::Unknown,tc::Same,    tc::ZExt,    tc::ZExt,   tc::ZExt,    tc::ZExt,   tc::ZExt,    tc::ZExt,   tc::ZExt,    tc::ZExt,   tc::UIntToFP,tc::UIntToFP,tc::UIntToFP,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*u8*/   {tc::Unknown,tc::ZExt,    tc::Same,    tc::Same,   tc::ZExt,    tc::ZExt,   tc::ZExt,    tc::ZExt,   tc::ZExt,    tc::ZExt,   tc::UIntToFP,tc::UIntToFP,tc::UIntToFP,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*i8*/   {tc::Unknown,tc::ZExt,    tc::Same,    tc::Same,   tc::ZExt,    tc::ZExt,   tc::ZExt,    tc::SExt,   tc::ZExt,    tc::SExt,   tc::IntToFP, tc::IntToFP, tc::IntToFP, tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*u16*/  {tc::Unknown,tc::ZExt,    tc::Trunc,   tc::Trunc,  tc::Same,    tc::Same,   tc::ZExt,    tc::ZExt,   tc::ZExt,    tc::ZExt,   tc::UIntToFP,tc::UIntToFP,tc::UIntToFP,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*i16*/  {tc::Unknown,tc::ZExt,    tc::Trunc,   tc::Trunc,  tc::Same,    tc::Same,   tc::ZExt,    tc::SExt,   tc::ZExt,    tc::SExt,   tc::IntToFP, tc::IntToFP, tc::IntToFP, tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*u32*/  {tc::Unknown,tc::ZExt,    tc::Trunc,   tc::Trunc,  tc::Trunc,   tc::Trunc,  tc::Same,    tc::Same,   tc::ZExt,    tc::ZExt,   tc::UIntToFP,tc::UIntToFP,tc::UIntToFP,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*i32*/  {tc::Unknown,tc::ZExt,    tc::Trunc,   tc::Trunc,  tc::Trunc,   tc::Trunc,  tc::Same,    tc::Same,   tc::ZExt,    tc::SExt,   tc::IntToFP, tc::IntToFP, tc::IntToFP, tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*u64*/  {tc::Unknown,tc::ZExt,    tc::Trunc,   tc::Trunc,  tc::Trunc,   tc::Trunc,  tc::Trunc,   tc::Trunc,  tc::Same,    tc::Same,   tc::UIntToFP,tc::UIntToFP,tc::UIntToFP,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*i64*/  {tc::Unknown,tc::ZExt,    tc::Trunc,   tc::Trunc,  tc::Trunc,   tc::Trunc,  tc::Trunc,   tc::Trunc,  tc::Same,    tc::Same,   tc::IntToFP, tc::IntToFP, tc::IntToFP, tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*f32*/  {tc::Unknown,tc::UIntToFP,tc::FPToUInt,tc::FPToInt,tc::FPToUInt,tc::FPToInt,tc::FPToUInt,tc::FPToInt,tc::FPToUInt,tc::FPToInt,tc::Same,    tc::PFExt,   tc::PFExt,   tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*f64*/  {tc::Unknown,tc::UIntToFP,tc::FPToUInt,tc::FPToInt,tc::FPToUInt,tc::FPToInt,tc::FPToUInt,tc::FPToInt,tc::FPToUInt,tc::FPToInt,tc::FPTrunc, tc::Same,    tc::PFExt,   tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*f80*/  {tc::Unknown,tc::UIntToFP,tc::FPToUInt,tc::FPToInt,tc::FPToUInt,tc::FPToInt,tc::FPToUInt,tc::FPToInt,tc::FPToUInt,tc::FPToInt,tc::FPTrunc, tc::FPTrunc, tc::Same,    tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*u128*/ {tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown, tc::Same,   tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*i128*/ {tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown, tc::Unknown,tc::Same,   tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown},
/*i256*/ {tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown,tc::Same,   tc::Unknown,tc::Unknown,tc::Unknown},
/*u256*/ {tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown,tc::Unknown,tc::Same,   tc::Unknown,tc::Unknown},
/*i512*/ {tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Same,   tc::Unknown},
/*u512*/ {tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown,tc::Unknown, tc::Unknown, tc::Unknown, tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Unknown,tc::Same},
      // clang-format on
  };
  return castings[static_cast<std::size_t>(fromType.Kind)]
                 [static_cast<std::size_t>(toType.Kind)];
}

static TypeCasting CastBuiltinType(const BuiltinType &from, const Type &to) {
  switch (to.getKind()) {
  case TypeKind::ArrayType:
  case TypeKind::CompoundType:
  case TypeKind::FuncType:
  case TypeKind::PtrType:
    return TypeCasting::Unknown;
  case TypeKind::AliasType: {
    return CastBuiltinType(from, to.desugar());
  }
  case TypeKind::BuiltinType: {
    return CastBuiltinType(from, *llvm::cast<BuiltinType>(&to));
  }
  }
}

TypeCasting quark::CastType(const Type &from, const Type &to) {
  switch (from.getKind()) {
  case TypeKind::ArrayType:
  case TypeKind::CompoundType:
  case TypeKind::FuncType:
  case TypeKind::PtrType:
    return TypeCasting::Unknown;
  case TypeKind::AliasType: {
    return CastType(from.desugar(), to);
  }
  case TypeKind::BuiltinType: {
    return CastBuiltinType(*llvm::cast<BuiltinType>(&from), to);
  }
  }
}
