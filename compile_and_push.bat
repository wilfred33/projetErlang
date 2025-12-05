@echo off
cd /d "c:\Users\pc\IdeaProjects\PprojetErlang"

echo ========================================
echo Compiling all Erlang modules...
echo ========================================

erl -compile transaction.erl
erl -compile merkle_tree.erl
erl -compile block.erl
erl -compile node.erl
erl -compile builder.erl
erl -compile verify_genesis.erl
erl -compile part1.erl
erl -compile part2.erl
erl -compile part3.erl

echo.
echo ========================================
echo Compilation complete!
echo ========================================
================================

pause
