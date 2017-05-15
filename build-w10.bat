rem This compliation script is for Windows 10 x64 with Linux Subsystem
rem installed. "mingw-w64" should be installed in windows, and "gfortran"
rem should be installed in bash.
gfortran -o build/wrtfe.exe wrtfe.f95 -static
bash -c "gfortran -o build/wrtfe wrtfe.f95 -static"
pause
