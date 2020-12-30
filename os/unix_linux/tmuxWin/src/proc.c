#include <tmuxWin.h>

#include <unistd.h>
#include <limits.h>

#include <string>


namespace tmuxWin {
    std::string get_exe_path()
    {
        std::string buf(PATH_MAX, '\0');

        errno = 0;
        if ((readlink("/proc/self/exe", &buf[0], PATH_MAX)) == -1) {
            throw std::system_error(errno, std::system_category(),
                                    "failed to obtain the executable path");
        }

        return buf;
    }
}

extern "C" char *GetExeFileName()
{
    std::string path = tmuxWin::get_exe_path();
}
