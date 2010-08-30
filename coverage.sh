#!/bin/sh
make clean
autoreconf
CFLAGS="-g3 -gdwarf-2 -O0 -Wall -Wextra -fprofile-arcs -ftest-coverage" \
  ./configure --disable-shared --enable-static
make
make check
#cd src
#gcov *.c
#cd libserveez
#gcov *.c
#cd ../..
# cd src
lcov --capture --compat-libtool --directory . --output-file serveez.info-file
genhtml serveez.info-file -o lcov
# cd ..
