echo off
rem This compliation script is for Windows 10 x64 with Linux Subsystem^
rem installed. "mingw-w64" should be installed in windows, and "gfortran"
rem should be installed in bash.
echo on
cd generated
gfortran -c ../ansicolors.f08
gfortran -o ../../build/wrtfe.exe ansicolors.o ../wrtfe.f95 -static
bash -c "gfortran -c ../ansicolors.f08"
bash -c "gfortran -o ../../build/wrtfe ansicolors.o ../wrtfe.f95 -static"
cd ..
