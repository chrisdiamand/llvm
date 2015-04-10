#ifndef LLVM_CLANG_LIB_CODEGEN_CRUNCH_ALLOCFUNCTION_H
#define LLVM_CLANG_LIB_CODEGEN_CRUNCH_ALLOCFUNCTION_H

#include <map>
#include <string>

namespace Crunch {

class AllocFunction {
private:
  unsigned int SizeArgIndex = 0;
  std::string Name = "", Args = "", Return = "";
  bool valid = false;

  void parseDescr(const std::string &);
  AllocFunction(const std::string &);

  static std::map<std::string, AllocFunction *> Functions;
  static void add(const std::string &);
  static void addFromEnvironment(const std::string &);
  static void addFunctions();

public:
  unsigned int getSizeArg();
  static AllocFunction *get(const std::string &);
};

} // namespace Crunch

#endif
