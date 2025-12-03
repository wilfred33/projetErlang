-module(test_csv_transactions).
-export([test/0]).

%% Test : création automatique de blocs avec transactions depuis un fichier CSV
test() ->
    io:format("~n=== Test: Automatic Block Creation with CSV Transactions ===~n~n"),

    %% Étape 1 : Créer les listes de nœuds
    io:format("Step 1: Creating node lists...~n"),
    AllValidators = ["Validator_1", "Validator_2", "Validator_3"],
    Builders = ["Builder_1"],
    AllNodes = AllValidators ++ Builders,
    io:format("  All validators: ~p~n", [AllValidators]),
    io:format("  Builders: ~p~n", [Builders]),
    io:format("  All nodes: ~p~n~n", [AllNodes]),

    %% Étape 2 : Démarrer le builder
    io:format("Step 2: Starting builder...~n"),
    {ok, Pid1, Name1} = node:start(builder, 1, AllNodes),
    io:format("  Started ~s (PID: ~p)~n", [Name1, Pid1]),

    timer:sleep(200),

    %% Étape 3 : Charger les transactions depuis un fichier CSV
    io:format("~nStep 3: Loading transactions from CSV file...~n"),
    TransactionFile = "transactions.csv",
    io:format("  Transaction file: ~s~n", [TransactionFile]),

    %% Charger les transactions dans le builder
    node:load_transactions("Builder_1", TransactionFile),

    timer:sleep(500),

    %% Étape 4 : Démarrer la création automatique de blocs
    io:format("~nStep 4: Starting automatic block creation...~n"),
    node:start_block_creation("Builder_1"),
    io:format("  Builder_1 block creation started~n"),
    io:format("  Blocks will be created every 0.5 seconds...~n"),

    %% Étape 5 : Attendre que des blocs soient créés
    io:format("~nStep 5: Waiting for blocks to be created (5 seconds)...~n"),
    timer:sleep(5000),

    %% Étape 6 : Vérifier les blocs créés
    io:format("~nStep 6: Checking created blocks...~n"),
    {ok, BC1} = node:get_blockchain("Builder_1"),
    io:format("  Builder_1 blockchain length: ~p blocks~n", [length(BC1)]),

    %% Afficher les détails du dernier bloc créé
    case length(BC1) > 0 of
        true ->
            LastBlock1 = lists:last(BC1),
            Txs1 = block:get_transactions(LastBlock1),
            io:format("  Builder_1 last block: #~p with ~p transactions~n",
                      [block:get_number(LastBlock1), length(Txs1)]);
        false ->
            io:format("  Builder_1: No blocks created~n")
    end,

    %% Vérifier si on a bien 10 blocs
    case length(BC1) of
        10 ->
            io:format("  ✓ SUCCESS: Expected 10 blocks, got ~p blocks!~n", [length(BC1)]);
        N ->
            io:format("  ✗ WARNING: Expected 10 blocks, got ~p blocks~n", [N])
    end,

    io:format("~n=== Test Complete ===~n~n"),
    ok.
