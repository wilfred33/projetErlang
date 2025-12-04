-module(test_validation_flow).
-export([test/0]).

%% Test du flow de validation: Builder -> ProposerGroup -> Broadcast
test() ->
    io:format("~n=== Test: Validation Flow avec ProposerGroup ===~n~n"),

    %% Configuration
    AllValidators = ["Validator_1", "Validator_2", "Validator_3"],
    Builders = ["Builder_1"],
    AllNodes = AllValidators ++ Builders,

    io:format("Step 1: Starting 3 validators...~n"),
    {ok, _Pid1, _Name1} = node:start_validator(1, AllValidators, Builders),
    {ok, _Pid2, _Name2} = node:start_validator(2, AllValidators, Builders),
    {ok, _Pid3, _Name3} = node:start_validator(3, AllValidators, Builders),
    timer:sleep(500),

    io:format("Step 2: Setting ProposerGroup (Validator_1 and Validator_2)...~n"),
    ProposerGroup = ["Validator_1", "Validator_2"],
    node:set_proposer_group("Validator_1", ProposerGroup),
    node:set_proposer_group("Validator_2", ProposerGroup),
    node:set_proposer_group("Validator_3", ProposerGroup),
    timer:sleep(200),

    io:format("Step 3: Starting builder...~n"),
    {ok, _PidB, _NameB} = node:start(builder, 1, AllNodes),
    timer:sleep(200),

    io:format("Step 4: Setting ProposerGroup for builder...~n"),
    Builder_1 = list_to_atom("Builder_1"),
    Builder_1 ! {set_proposer_group, ProposerGroup},
    timer:sleep(200),

    io:format("Step 5: Loading transactions...~n"),
    node:load_transactions("Builder_1", "transactions.csv"),
    timer:sleep(500),

    io:format("Step 6: Starting block creation (will send to ProposerGroup)...~n"),
    node:start_block_creation("Builder_1"),

    io:format("Step 7: Waiting for all transactions to be processed...~n"),
    wait_for_transactions_processed("Builder_1", 100),

    io:format("~n=== Checking results ===~n"),
    {ok, BC1} = node:get_blockchain("Builder_1"),
    {ok, BCV1} = node:get_blockchain("Validator_1"),
    {ok, BCV2} = node:get_blockchain("Validator_2"),

    io:format("  Builder_1 blockchain: ~p blocks~n", [length(BC1)]),
    io:format("  Validator_1 blockchain: ~p blocks~n", [length(BCV1)]),
    io:format("  Validator_2 blockchain: ~p blocks~n", [length(BCV2)]),

    io:format("~n=== Test Complete ===~n"),
    ok.

%% Attend que toutes les transactions soient traitées (pool vide)
wait_for_transactions_processed(NodeName, MaxRetries) ->
    wait_for_transactions_processed(NodeName, MaxRetries, 0).

wait_for_transactions_processed(_NodeName, MaxRetries, Retries) when Retries >= MaxRetries ->
    io:format("  Timeout: Max retries reached~n"),
    timeout;
wait_for_transactions_processed(NodeName, MaxRetries, Retries) ->
    case node:get_transaction_pool_size(NodeName) of
        {ok, 0} ->
            io:format("  All transactions processed! Pool is empty.~n"),
            %% Attendre encore 1 seconde pour que le dernier bloc se propage
            timer:sleep(1000),
            ok;
        {ok, Size} ->
            io:format("  Waiting... (~p transactions remaining)~n", [Size]),
            timer:sleep(500),
            wait_for_transactions_processed(NodeName, MaxRetries, Retries + 1);
        {error, Reason} ->
            io:format("  Error getting pool size: ~p~n", [Reason]),
            {error, Reason}
    end.
