#ifndef LLVM_TRANSFORM_ALLOCSITES_ALLOCFUNCTION_H
#define LLVM_TRANSFORM_ALLOCSITES_ALLOCFUNCTION_H

#include <map>
#include <string>

namespace Crunch {

class AllocFunction;

typedef std::map<std::string, AllocFunction *> AllocFunctionMap;

class AllocFunction {
private:
  unsigned int SizeArgIndex = 0;
  std::string Name = "", Args = "", Return = "";
  bool valid = false;

  void parseDescr(const std::string &);
  AllocFunction(const std::string &);

  static AllocFunctionMap Functions;

  static void add(const std::string &);
  static void addFromEnvironment(const std::string &);
  static void addFunctions();

public:

  unsigned int getSizeArg();
  std::string getName();

  static AllocFunctionMap &getAll();
  static AllocFunction *get(const std::string &);
};

} // namespace Crunch

#endif // LLVM_TRANSFORM_ALLOCSITES_ALLOCFUNCTION_H
