REM @ECHO OFF

REM Create IRS standalone executables.

if exist make-travel.bat cd ..

set lisp="C:\Program Files\LispWorks\lispworks-5-1-0-x86-win32.exe"
if not exist %lisp% set lisp="C:\Program Files\LispWorks\lispworks-5-0-0-x86-win32.exe"
if not exist %lisp% set lisp="C:\Program Files\LispWorks\lispworks-4450.exe"

call %lisp% -init scripts/make-travel-publisher.lisp -siteinit -

:END
