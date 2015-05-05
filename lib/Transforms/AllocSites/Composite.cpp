#include <iostream>

#include "llvm/Transforms/AllocSites/Composite.h"

namespace Composite {

const std::string BaseTypePtr::strComposite() const {
  std::string Ret = "";

  for (int i = 0; i < PointerDegree; ++i) {
    Ret += "(pointer_type [type = ";
  }

  // Replace underscores with spaces
  for (auto it = Base.begin(); it != Base.end(); ++it) {
    if (*it == '_') {
      Ret += "\\ ";
    } else {
      Ret += *it;
    }
  }

  // Close the brackets
  for (int i = 0; i < PointerDegree; ++i) {
    Ret += "];)";
  }

  return Ret;
}

const std::string BaseTypePtr::strUniqtype() const {
  std::string Ret = "";
  for (int i = 0; i < PointerDegree; ++i) {
    Ret += "__PTR_";
  }
  Ret += Base;
  return Ret;
}

const std::string BaseTypePtr::str(bool Composite) const {
  if (Composite) {
    return strComposite();
  } else {
    return strUniqtype();
  }
}

const std::string TypeTerm::str(bool Composite) const {
  std::string Ret = "";
  size_t Count = 0;

  for (auto it = Top.begin(); it != Top.end(); ++it) {
    Ret += it->str(Composite);
    if (Count++ < Top.size() - 1) {
      Ret += "_mul_";
    }
  }

  for (auto it = Bottom.begin(); it != Bottom.end(); ++it) {
    Ret += "_div_";
    Ret += it->str(Composite);
  }
  return Ret;
}

void TypeTerm::mulBase(const BaseTypePtr &Base) {
  // If it already exists on the bottom, cancel it.
  auto IsInBottom = std::find(Bottom.begin(), Bottom.end(), Base);
  if (IsInBottom != Bottom.end()) {
    Bottom.erase(IsInBottom);
  } else {
    Top.insert(Base);
  }
}

void TypeTerm::divBase(const BaseTypePtr &Base) {
  auto IsInTop = std::find(Top.begin(), Top.end(), Base);
  if (IsInTop != Top.end()) {
    Top.erase(IsInTop);
  } else {
    Bottom.insert(Base);
  }
}

const TypeTerm TypeTerm::mul(const TypeTerm &T2) const {
  TypeTerm Ret(*this);

  for (auto it = T2.Top.begin(); it != T2.Top.end(); ++it) {
    Ret.mulBase(*it);
  }
  for (auto it = T2.Bottom.begin(); it != T2.Bottom.end(); ++it) {
    Ret.divBase(*it);
  }

  return Ret;
}

const TypeTerm TypeTerm::recip() const {
  TypeTerm Ret;

  Ret.Top = Bottom;
  Ret.Bottom = Top;

  return Ret;
}

const std::string ArithType::str() const {
  if (isVoid()) {
    return "void";
  }

  std::string Ret;
  // The common case.
  if (Pos.size() == 1) {
    Ret = Pos.front().str(false);
  } else {
    Ret = "{ ";
    int Index = 0;
    for (auto it = Pos.begin(); it != Pos.end(); ++it, ++Index) {
      Ret += "member : ";
      if (Index != 1) {
        Ret += it->str(true);
      } else {
        Ret += "(array_type [type = " + it->str(true) + "] { })";
      }
      Ret += "; ";
    }
    Ret += "};";
  }

  for (auto it = Neg.begin(); it != Neg.end(); ++it) {
    Ret += "_sub_" + it->str(false);
  }
  return Ret;
}

bool ArithType::isVoid() const {
  /* Only need to check Pos and Neg, if they are not empty then they still
   * should not contain any empty TypeFractions. */
  if (Pos.size() == 0 && Neg.size() == 0) {
    return true;
  }
  return false;
}

bool ArithType::isComposite() const {
  if (Pos.size() > 1) {
    return true;
  }
  return false;
}

ArithType::ArithType(std::string &UniqtypeStr) {
  int PointerDegree = 0;
  while (UniqtypeStr.find("__PTR_") == 0) {
    PointerDegree++;
    UniqtypeStr.erase(0, 6);
  }
  BaseTypePtr Base(UniqtypeStr, PointerDegree);
  TypeTerm Frac(Base);
  Pos.push_back(Frac);
}

ArithType::ArithType(const ArithType &T) {
  Pos = T.Pos;
  Neg = T.Neg;
}

// Warning: This function operates in place!
void ArithType::addTerm(const TypeTerm &Term) {
  if (Term.isVoid()) {
    return;
  }

  // If the same fraction exists on the negative side, cancel them.
  auto IsInNeg = std::find(Neg.begin(), Neg.end(), Term);
  if (IsInNeg != Neg.end()) {
    Neg.erase(IsInNeg);
  } else {
    Pos.push_back(Term);
  }
}

void ArithType::subTerm(const TypeTerm &Term) {
  if (Term.isVoid()) {
    return;
  }

  auto IsInPos = std::find(Pos.begin(), Pos.end(), Term);
  if (IsInPos != Pos.end()) {
    Pos.erase(IsInPos);
  } else {
    Neg.insert(Term);
  }
}

const ArithType ArithType::add(const ArithType &T2) const {
  ArithType Ret(*this);

  for (auto it = T2.Pos.begin(); it != T2.Pos.end(); ++it) {
    Ret.addTerm(*it);
  }
  for (auto it = T2.Neg.begin(); it != T2.Neg.end(); ++it) {
    Ret.subTerm(*it);
  }

  return Ret;
}

const ArithType ArithType::sub(const ArithType &T2) const {
  ArithType Ret(*this);

  for (auto it = T2.Pos.begin(); it != T2.Pos.end(); ++it) {
    Ret.subTerm(*it);
  }
  for (auto it = T2.Neg.begin(); it != T2.Neg.end(); ++it) {
    Ret.addTerm(*it);
  }

  return Ret;
}

const ArithType ArithType::mul(const ArithType &T2) const {
  ArithType Ret;

  // If one operand void, just copy the other operand's type.
  if (isVoid()) {
    return T2;
  } else if (T2.isVoid()) {
    return *this;
  }

  for (auto it = Pos.begin(); it != Pos.end(); ++it) {
    for (auto jt = T2.Pos.begin(); jt != T2.Pos.end(); ++jt) {
      Ret.addTerm(it->mul(*jt));
    }
    for (auto jt = T2.Neg.begin(); jt != T2.Neg.end(); ++jt) {
      Ret.subTerm(it->mul(*jt));
    }
  }

  for (auto it = Neg.begin(); it != Neg.end(); ++it) {
    for (auto jt = T2.Pos.begin(); jt != T2.Pos.end(); ++jt) {
      Ret.subTerm(it->mul(*jt));
    }
    for (auto jt = T2.Neg.begin(); jt != T2.Neg.end(); ++jt) {
      Ret.addTerm(it->mul(*jt));
    }
  }

  return Ret;
}

const ArithType ArithType::recip() const {
  ArithType Ret;

  for (auto it = Pos.begin(); it != Pos.end(); ++it) {
    Ret.Pos.push_back(it->recip());
  }
  for (auto it = Neg.begin(); it != Neg.end(); ++it) {
    Ret.Neg.insert(it->recip());
  }

  return Ret;
}

const ArithType ArithType::div(const ArithType &T2) const {
  ArithType Recip = T2.recip();

  if (isVoid()) {
    return Recip;
  } else if (T2.isVoid()) {
    return *this;
  }

  return mul(Recip);
}

bool ArithType::operator==(const ArithType &T) const {
  return Pos == T.Pos && Neg == T.Neg;
}

ArithType &ArithType::operator=(const ArithType &Other) {
  if (this == &Other) {
    return *this;
  }
  Pos = Other.Pos;
  Neg = Other.Neg;
  return *this;
}

std::ostream &operator<<(std::ostream &OS, const ArithType &T) {
  OS << T.str();
  return OS;
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, const ArithType &T) {
  OS << T.str();
  return OS;
}

} // namespace Composite
