#ifndef LLVM_TRANSFORM_ALLOCSITES_COMPOSITE_H
#define LLVM_TRANSFORM_ALLOCSITES_COMPOSITE_H

#include <cstring>

namespace Composite {

typedef std::string Type;

const std::string TypeNotInferred = "void";

// TODO: Do this properly.
extern inline Type add(Type T1, Type T2) {
    return T1 + "_plus_" + T2;
}

extern inline Type mul(Type T1, Type T2) {
    return T1 + "_mul_" + T2;
}

} // namespace Composite

#endif // LLVM_TRANSFORM_ALLOCSITES_COMPOSITE_H
