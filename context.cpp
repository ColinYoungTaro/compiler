#include "context.h"
#include "llvm/Support/raw_ostream.h"

CodeGenContext *ctx;

void logError(const std::string &logInfo) {
  // llvm::errs() << logInfo << "\n";
  throw logInfo;
}

void logWarning(const std::string &logInfo) { errs() << logInfo << "\n"; }
