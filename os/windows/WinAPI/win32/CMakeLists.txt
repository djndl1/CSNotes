cmake_minimum_required(VERSION 3.12)

project(Win32API)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_SYSTEM_NAME WINDOWS)
include_directories(lib/include)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(CreateProcess
  src/CreateProcess.cpp
  lib/src/errors.cpp
  lib/src/processes.cpp)
target_compile_definitions(CreateProcess
  PRIVATE UNICODE _UNICODE _WIN32_WINNT=0x6000)
target_link_options(CreateProcess
  PRIVATE -municode)

add_executable(ProcessIdentity
  src/ProcessIdentity.cpp
  lib/src/errors.cpp
  lib/src/processes.cpp)
target_compile_definitions(ProcessIdentity
  PRIVATE UNICODE _UNICODE _WIN32_WINNT=0x6000)
target_link_options(ProcessIdentity
  PRIVATE -municode)
