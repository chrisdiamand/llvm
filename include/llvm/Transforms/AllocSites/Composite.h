#ifndef LLVM_TRANSFORM_ALLOCSITES_COMPOSITE_H
#define LLVM_TRANSFORM_ALLOCSITES_COMPOSITE_H

#include <llvm/Support/raw_ostream.h>

#include <iostream>
#include <cstring>

namespace Composite {

class Type {
private:
  std::string UniqtypeStr;
  bool valid;

public:
  inline void check(void) const {
    assert((valid && UniqtypeStr.size() > 0) ||
           (!valid && UniqtypeStr.size() == 0));
  }

  Type(std::string _UniqtypeStr) :
    UniqtypeStr(_UniqtypeStr), valid(true) {
    check();
  }

  Type(const Type &T) : UniqtypeStr(T.UniqtypeStr), valid(T.valid) {
    check();
  }

  // An empty type to show that no type was found.
  Type() : UniqtypeStr(""), valid(false) {};

  const Type add(const Type &) const;
  const Type sub(const Type &) const;
  const Type mul(const Type &) const;
  const Type div(const Type &) const;

  inline bool isValid() const {
    return valid;
  }

  bool operator==(const Type &) const;

  inline bool operator!=(const Type &Other) const {
    return !(*this == Other);
  }

  Type &operator=(const Type &);

  const std::string str() const;
  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &, const Type &);
  friend std::ostream &operator<<(std::ostream &, const Type &);
};

} // namespace Composite

#endif // LLVM_TRANSFORM_ALLOCSITES_COMPOSITE_H
