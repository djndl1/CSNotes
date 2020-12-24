#include <exception>
#include <string>

namespace utf_cvt {
    class utf_conversion_error;
    std::wstring utf8_to_utf16(const std::string &utf8_string);
}
