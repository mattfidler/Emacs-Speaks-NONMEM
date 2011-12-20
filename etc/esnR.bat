@echo off
rem Create esnR packages
del esnR*.zip
del esnR*.gz
rem build esnR_xxx.zip
R CMD build --binary esnR
rem build esnR_xxx.tar.gz
R CMD build esnR