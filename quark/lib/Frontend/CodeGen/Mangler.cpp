#include "Mangler.h"

#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <quark/Frontend/AST/Type.h>
#include <string>

using namespace quark;

static void mangleType(const Type &type, llvm::raw_ostream &out) {
  switch (type.getKind()) {
  case TypeKind::AliasType: {
    auto &aliasType = *llvm::cast<AliasType>(&type);
    out << '_' << aliasType.Name;
    break;
  }
  case TypeKind::CompoundType: {
    auto &compoundType = *llvm::cast<CompoundType>(&type);
    out << '_' << compoundType.Name;
    break;
  }
  case TypeKind::FuncType: {
    auto &funcType = *llvm::cast<FuncType>(&type);
    if (funcType.Reciver) {
      out << "_rec_";
      mangleType(*funcType.Reciver, out);
    }
    for (auto &type : funcType.Params) {
      mangleType(*type, out);
    }
    break;
  }
  case TypeKind::ArrayType: {
    auto &arrayType = *llvm::cast<ArrayType>(&type);
    mangleType(*arrayType.RealType, out);
    out << "_arr_" << arrayType.Elements << '_';
    break;
  }
  case TypeKind::BuiltinType: {
    auto &builtinType = *llvm::cast<BuiltinType>(&type);
    out << builtinType.Name;
    break;
  }
  case TypeKind::PtrType: {
    auto &ptrType = *llvm::cast<PtrType>(&type);
    out << "_ptr_";
    mangleType(*ptrType.PointeeType, out);
    break;
  }
  }
}

std::string quark::mangleFunction(llvm::StringRef name,
                                  const FuncType &funcType) {
  std::string mangledName;
  llvm::raw_string_ostream rso(mangledName);

  rso << name << '_';
  mangleType(funcType, rso);

  return rso.str();
}