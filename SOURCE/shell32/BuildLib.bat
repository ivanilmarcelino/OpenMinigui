@echo off

SET HMGPATH=c:\minigui

SET PATH=%HMGPATH%\harbour\bin;c:\borland\bcc58\bin;%PATH%

hbmk2 Shell32.hbp > Shell32.log
