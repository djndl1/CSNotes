cmake_minimum_required(VERSION 3.10)
project(gtk_c)

find_package(PkgConfig REQUIRED)
pkg_check_modules(GLIB2 REQUIRED glib-2.0)
pkg_check_modules(GTK2 REQUIRED gtk+-2.0)
pkg_check_modules(GTK3 REQUIRED gtk+-3.0)

if (USE_GTK2)
  add_compile_options(${GTK2_CFLAGS})
  link_libraries(${GTK2_LIBRARIES})
else()
  add_compile_options(${GTK3_CFLAGS})
  link_libraries(${GTK3_LIBRARIES})
endif()

add_compile_options(${GLIB2_CFLAGS})
link_libraries(${GLIB2_LIBRARIES})

add_executable(helloworld_gtk helloworld.c)
add_executable(button_gtk button.c)
add_executable(exercise2.2 exercise2.2.c)
add_executable(gtkbox gtkbox.c)
add_executable(gtkpane panes.c)
add_executable(gtkgrid tables.c)
add_executable(gtkfixed fixed.c)
add_executable(gtkexpander expander.c)
add_executable(gtknotebook notebook.c)
add_executable(exercise3.1 exercise3.1.c)

add_executable(gtkradiobutton radiobuttons.c)
add_executable(gtkentry entry.c)
add_executable(gtkdialog SimpleDialog.c)
add_executable(gtkmsg MessageDialog.c)

add_executable(gtimeout timeouts.c)

add_executable(exercise6.1 exercise6.1.c)
add_executable(exercise6.2 exercise6.2.c)

add_executable(scrolledwindow scrolledwindows.c)
add_executable(textview textview2.c)
add_executable(iterator iterator.c)
