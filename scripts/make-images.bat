REM @ECHO OFF

REM Create various IRS lisp images.

if exist make-images.bat cd ..

set lisp="C:\Program Files\LispWorks\lispworks-5-1-0-x86-win32.exe"
if not exist %lisp% set lisp="C:\Program Files\LispWorks\lispworks-5-0-0-x86-win32.exe"
if not exist %lisp% set lisp="C:\Program Files\LispWorks\lispworks-4450.exe"

call %lisp% -init scripts/make-irs-image.lisp -siteinit -

:END
