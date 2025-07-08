@echo off
@setlocal

SET HMGPATH=\minigui

SET PATH=%HMGPATH%\harbour\bin;\borland\bcc58\bin;%PATH%

hbmk2 tsb.hbp > build.log 2>&1

@endlocal
