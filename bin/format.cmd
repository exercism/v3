if "%~1"=="" (set PATTERN="**/*.md") else (set PATTERN="%1")
npx prettier@2.0.4 --write %PATTERN%