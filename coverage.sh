autoreconf
CFLAGS="-g3 -gdwarf-2 -O0 -Wall -Wextra -fprofile-arcs -ftest-coverage" \
./configure --disable-shared --enable-static
make clean
make
make check
#cd src
#gcov *.c
#cd libserveez
#gcov *.c
#cd ../..
lcov -c -d . -o serveez.info-file
genhtml serveez.info-file -o lcov
