.paket\paket.bootstrapper.exe

if errorlevel 1 (
  exit /b %errorlevel%
)

SET TOOL_PATH=.fake

IF NOT EXIST "%TOOL_PATH%\fake.exe" (
  dotnet tool install fake-cli --tool-path ./%TOOL_PATH% --version 5.188.1 paket
)

"%TOOL_PATH%/fake.exe" run build.fsx %*