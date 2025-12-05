@echo off
cd /d "c:\Users\pc\IdeaProjects\PprojetErlang"

echo Compiling Erlang modules...
erl -compile transaction.erl
erl -compile merkle_tree.erl
erl -compile block.erl
erl -compile node.erl
erl -compile part2.erl

echo.
echo Running Part 2: Election Process Testing...
erl -noshell -s part2 test

pause
