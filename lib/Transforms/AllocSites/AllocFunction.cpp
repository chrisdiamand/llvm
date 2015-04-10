#include <assert.h>
#include <iostream>
#include <regex>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>

#include "llvm/Transforms/AllocSites/AllocFunction.h"

namespace Crunch {

std::map<std::string, AllocFunction *> AllocFunction::Functions;

// Parse a libcrunch-style allocation function description.
void AllocFunction::parseDescr(const std::string &Descr) {
  std::smatch sm;
  const std::regex Expr("([a-zA-Z_][a-zA-Z0-9_]*)\\(([a-zA-Z]*)\\)([a-zA-Z])");
  std::regex_match(Descr, sm, Expr);
  if (sm.size() == 0) {
    std::cerr << "Error: '" << Descr
              << "' not a valid allocation function descriptor.\n";
    valid = false;
    return;
  }

  assert(sm.size() == 4);

  Name = sm[1];
  Args = sm[2];
  Return = sm[3];

  SizeArgIndex = 0;
  for (size_t i = 0; i < Args.size(); ++i) {
    if (Args.at(i) == 'Z') {
      SizeArgIndex = i;
    }
  }
}

AllocFunction::AllocFunction(const std::string &Descr) {
  valid = true;
  parseDescr(Descr);
}

void AllocFunction::add(const std::string &Descr) {
  AllocFunction *AF = new AllocFunction(Descr);
  if (AF->valid) {
    Functions[AF->Name] = AF;
  } else {
    delete AF;
  }
}

void AllocFunction::addFromEnvironment(const std::string &EnvVar) {
  char *EnvValue = getenv(EnvVar.c_str());
  char *saveptr;
  if (EnvValue == nullptr) {
    return;
  }

  // Split into whitespace-separated tokens
  char *Token = strtok_r(EnvValue, " \t,", &saveptr);
  while (Token != NULL) {
    std::string Descr(Token);
    std::cout << "!!!!!" << Descr << "\n";
    add(Descr);
    Token = strtok_r(NULL, " \t,", &saveptr);
  }
}

void AllocFunction::addFunctions() {
  if (Functions.size() > 0) {
    return;
  }

  add("alloca(Z)p");
  add("malloc(Z)p");
  add("calloc(zZ)p");
  add("realloc(pZ)p");
  add("memalign(zZ)p");

  addFromEnvironment("LIBALLOCS_ALLOC_FNS");
  addFromEnvironment("LIBALLOCS_SUBALLOC_FNS");
  addFromEnvironment("LIBALLOCS_ALLOCSZ_FNS");
}

AllocFunction *AllocFunction::get(const std::string &Name) {
  addFunctions();

  auto it = Functions.find(Name);

  if (it == Functions.end()) {
    return nullptr;
  }
  return it->second;
}

unsigned int AllocFunction::getSizeArg() {
  return SizeArgIndex;
}

} // namespace Crunch
