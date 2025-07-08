@echo off
@setlocal

SET HMGPATH=c:\minigui

SET PATH=%HMGPATH%\harbour\bin;c:\borland\bcc58\bin;%PATH%

hbmk2 Otis.hbp > build.log 2>&1

@endlocal
