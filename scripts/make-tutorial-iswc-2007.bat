REM @ECHO OFF

REM Create ISWC 2007 tutorial executables.

if exist make-tutorial-iswc-2007.bat cd ..

set lisp="C:\Program Files\LispWorks\lispworks-5-1-0-x86-win32.exe"
if not exist %lisp% set lisp="C:\Program Files\LispWorks\lispworks-5-0-0-x86-win32.exe"
if not exist %lisp% set lisp="C:\Program Files\LispWorks\lispworks-4450.exe"

call %lisp% -init scripts/make-tutorial-iswc-2007-server.lisp -siteinit -
call %lisp% -init scripts/make-tutorial-iswc-2007-publisher.lisp -siteinit -

:END
