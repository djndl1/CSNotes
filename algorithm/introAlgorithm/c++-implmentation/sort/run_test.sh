clang++ sort_test.cc merge_sort.cc insertion_sort.cc -g $(pkg-config --cflags --libs gtest) -I. && ./a.out
