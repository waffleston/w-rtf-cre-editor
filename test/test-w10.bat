gfortran -c ..\src\ansicolors.f08
gfortran -o test test.f08 ansicolors.o
bash -c "gfortran -c ../src/ansicolors.f08"
bash -c "gfortran -o test test.f08 ansicolors.o"
pause
test
bash -c "./test"
pause
