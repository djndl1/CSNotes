cmake_minimum_required(VERSION 3.16)
project(gtk_c)

find_package(PkgConfig REQUIRED)
pkg_check_modules(GLIB2 REQUIRED glib-2.0)
pkg_check_modules(GTK2 REQUIRED gtk+-2.0)
pkg_check_modules(GTK3 REQUIRED gtk+-3.0)

add_compile_options(${GLIB2_CFLAGS})
link_libraries(${GLIB2_LIBRARIES})

add_executable(helloworld_gtk2 helloworld.c)
target_compile_options(helloworld_gtk2
  PRIVATE ${GTK2_CFLAGS})
target_link_libraries(helloworld_gtk2
  PRIVATE ${GTK2_LIBRARIES})

add_executable(helloworld_gtk3 helloworld.c)
target_compile_options(helloworld_gtk3
   PRIVATE ${GTK3_CFLAGS})
target_link_libraries(helloworld_gtk3
  PRIVATE ${GTK3_LIBRARIES})

