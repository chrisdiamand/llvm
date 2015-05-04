#include <iostream>

#include "llvm/Transforms/AllocSites/Composite.h"

namespace Composite {

const Type Type::add(const Type &T2) const {
  check();
  T2.check();
  return Type(UniqtypeStr + "_plus_" + T2.UniqtypeStr);
}

const Type Type::mul(const Type &T2) const {
  check();
  T2.check();
  return Type(UniqtypeStr + "_mul_" + T2.UniqtypeStr);
}

bool Type::operator==(const Type &T) const {
  check();
  T.check();
  return UniqtypeStr == T.UniqtypeStr;
}

Type &Type::operator=(const Type &Other) {
  check();
  Other.check();

  if (this == &Other) {
    return *this;
  }
  UniqtypeStr = Other.UniqtypeStr;
  valid = Other.valid;
  return *this;
}

const std::string Type::str() const {
  check();

  if (!valid) {
    return "void";
  }
  return UniqtypeStr;
}

std::ostream &operator<<(std::ostream &OS, const Type &T) {
  OS << T.str();
  return OS;
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, const Type &T) {
  OS << T.str();
  return OS;
}

} // namespace Composite
