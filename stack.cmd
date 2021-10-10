echo off
setlocal

set PATH=C:\msys64\mingw64\bin;C:\msys64\usr\local\bin;C:\msys64\usr\bin;C:\msys64\bin;%PATH%

stack.exe --skip-msys %1 %2 %3 %4 %5 %6 %7 %8 %9

endlocal
