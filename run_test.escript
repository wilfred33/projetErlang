#!/usr/bin/env escript

main(_Args) ->
    %% Compile tous les modules
    io:format("Compiling modules...~n"),
    compile:file(transaction, [verbose, report_errors]),
    compile:file(merkle_tree, [verbose, report_errors]),
    compile:file(block, [verbose, report_errors]),
    compile:file(node, [verbose, report_errors]),
    compile:file(builder, [verbose, report_errors]),
    compile:file(test_simple, [verbose, report_errors]),

    io:format("~nRunning test...~n~n"),

    %% Exécute le test
    test_simple:test_builder_create_block(),

    io:format("~nTest completed!~n").
