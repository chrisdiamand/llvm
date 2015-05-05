#ifndef LLVM_TRANSFORM_ALLOCSITES_COMPOSITE_H
#define LLVM_TRANSFORM_ALLOCSITES_COMPOSITE_H

#include <llvm/Support/raw_ostream.h>

#include <cstring>
#include <iostream>
#include <set>

namespace Composite {

/* In order to do arithmetic on `sizeof' types, represent them as sums of
 * fractions. This is _not_ recursive - there is no point having a full tree
 * representation since most things don't make sense with types anyway. */

class BaseTypePtr {
public:
  const std::string Base;
  const int PointerDegree;

  const std::string str(bool) const;
  const std::string strComposite() const;
  const std::string strUniqtype() const;

  inline bool operator==(const BaseTypePtr &Other) const {
    return Base == Other.Base;
  }

  inline bool operator!=(const BaseTypePtr &Other) const {
    return !(*this == Other);
  }

  inline bool operator<(const BaseTypePtr &Other) const {
    return Base < Other.Base;
  }

  BaseTypePtr(const std::string &B, const int P) :
    Base(B), PointerDegree(P) {};
};

class TypeTerm {
private:
  std::set<BaseTypePtr> Top, Bottom;

  void mulBase(const BaseTypePtr &);
  void divBase(const BaseTypePtr &);

public:

  inline TypeTerm(BaseTypePtr Base) {
    Top.insert(Base);
  }
  inline TypeTerm() {}

  inline bool operator==(const TypeTerm &Other) const {
    return Top == Other.Top && Bottom == Other.Bottom;
  }

  inline bool operator<(const TypeTerm &Other) const {
    return str(false) < Other.str(false);
  }

  const std::string str(bool) const;
  const TypeTerm mul(const TypeTerm &) const;
  const TypeTerm recip() const;

  inline bool isVoid() const {
    return Top.size() == 0 && Bottom.size() == 0;
  }
};

class Type {
private:

  // The order matters for addition.
  std::vector<TypeTerm> Pos;
  std::set<TypeTerm> Neg;

  void addTerm(const TypeTerm &);
  void subTerm(const TypeTerm &);
  const Type recip() const;

public:

  Type(std::string &);
  Type(const Type &T);
  // An empty type shows that no type was found.
  inline Type(void) {};

  const Type add(const Type &) const;
  const Type sub(const Type &) const;
  const Type mul(const Type &) const;
  const Type div(const Type &) const;

  bool isVoid() const;
  bool isComposite() const;

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
