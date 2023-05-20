#ifndef SCRIPTLANG_LIB_BASIC_PRINTER_HPP
#define SCRIPTLANG_LIB_BASIC_PRINTER_HPP

#include <cstdint>
#include <string>

namespace scriptlang {

class PrinterBase {
protected:
  std::string space() { return std::string(indent_, ' '); }

  class RttiIndent {
  public:
    static constexpr const uint32_t INDENT_SIZE = 2U;
    explicit RttiIndent(PrinterBase *printer) : printer_(printer) {
      printer_->indent_ += INDENT_SIZE;
    }
    ~RttiIndent() { printer_->indent_ -= INDENT_SIZE; }

  private:
    PrinterBase *printer_;
  };

private:
  uint32_t indent_ = 0;
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_BASIC_PRINTER_HPP */
