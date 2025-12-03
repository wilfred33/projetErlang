@echo off
echo Compiling Erlang files...
erl -compile transaction.erl
erl -compile merkle_tree.erl
erl -compile block.erl
erl -compile node.erl
erl -compile builder.erl
erl -compile test_simple.erl

echo.
echo Running test...
erl -noshell -eval "test_simple:test_builder_create_block(), init:stop()."

pause
