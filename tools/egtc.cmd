@setlocal enabledelayedexpansion enableextensions
@set ROOT=%~dp0~~~
@set ROOT=%ROOT:\~~~=%
@set ROOT=%ROOT:\=/%
@call make -f "%~dp0Makefile" ROOT="%ROOT%" %*
