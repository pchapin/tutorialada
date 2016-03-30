@echo off

rem TODO: Also have to clean out the build folders in subdirectories.
rem TODO: Also have to remove the gnatprove folders in the build folders.
rem TODO: Write a script that takes care of both of the above items.
rem 
cd samples\build
move .gitkeep ..
del *.*
move ..\.gitkeep .
cd ..\..

del samples.zip
zip -r samples.zip samples
