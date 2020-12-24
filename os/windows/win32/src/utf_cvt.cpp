#include <windows.h>

#include <string>
#include <cwchar>
#include <exception>

#include <locale>

namespace utf_cvt {
    class utf_conversion_error : public std::exception
    {
    private:
        std::string what_msg;
    public:
        utf_conversion_error(const std::string &err_msg)
            {
                what_msg = err_msg;
            }

        utf_conversion_error(std::string &&err_msg)
            {
                what_msg = err_msg;
            }

        utf_conversion_error(const char *err_msg)
            {
                what_msg = std::string{err_msg};
            }

        const char *what() const noexcept override
            {
                return what_msg.c_str();
            }
    };

    std::wstring utf8_to_utf16(const std::string &utf8_string)
    {
        int requiredBufLen = MultiByteToWideChar(CP_UTF8, 0, utf8_string.c_str(), utf8_string.size(), nullptr, 0);

        if (requiredBufLen == 0)
            throw utf_conversion_error{"Failed to obtain the length of a UTF-16 buffer"};

        std::wstring utf16_buf{};
        utf16_buf.reserve(requiredBufLen);
        int cnt_written = MultiByteToWideChar(CP_UTF8, 0, utf8_string.c_str(), utf8_string.size(),
                                              &utf16_buf[0], requiredBufLen);

        if (cnt_written == 0)
            throw utf_conversion_error{"Failed to Convert a UTF-8 string to UTF16"};

        return utf16_buf;
    }
    
}
