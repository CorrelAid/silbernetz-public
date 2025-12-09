@echo off
set /p commit_hash=ID eingeben aus pulled_versions.txt
git checkout %commit_hash%
pause