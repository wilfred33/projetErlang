@echo off
echo Compiling Erlang files...
erl -compile transaction.erl
erl -compile merkle_tree.erl
erl -compile block.erl
erl -compile node.erl
erl -compile builder.erl
erl -compile test_csv_transactions.erl

echo.
echo Running CSV transactions test...
erl -noshell -eval "test_csv_transactions:test(), init:stop()."

pause
