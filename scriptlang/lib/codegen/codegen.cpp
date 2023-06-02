#include "scriptlang/lib/codegen/codegen.hpp"
#include "scriptlang/lib/codegen/ir_coverter.hpp"

namespace scriptlang {

class CodeGenImpl {
public:
  void compile(std::shared_ptr<hir::Statement> statement, std::shared_ptr<TypeSystem> typeSystem) {
    llvm::LLVMContext ctx;
    auto m = new llvm::Module("demo", ctx);
    ToIRVisitor visitor{m, typeSystem.get()};
    visitor.run(statement.get());
    m->print(llvm::outs(), nullptr);
  }
};

void CodeGen::compile(std::shared_ptr<hir::Statement> statement,
                      std::shared_ptr<TypeSystem> typeSystem) {
  impl_->compile(statement, typeSystem);
}

} // namespace scriptlang
