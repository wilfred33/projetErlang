-module(test_full_election).
-export([test/0]).

%% Test complet : élection avec builders et validateurs
test() ->
    io:format("~n=== Test: Full Election with Builders and Validators ===~n~n"),

    %% Étape 1 : Créer les listes de nœuds
    io:format("Step 1: Creating node lists...~n"),
    AllValidators = ["Validator_1", "Validator_2", "Validator_3","Validator_4","Validator_5"],
    Builders = ["Builder_1"],
    AllNodes = AllValidators ++ Builders,
    io:format("  All validators: ~p~n", [AllValidators]),
    io:format("  Builders: ~p~n", [Builders]),
    io:format("  All nodes: ~p~n~n", [AllNodes]),

    %% Étape 2 : Démarrer les builders
    io:format("Step 2: Starting builders...~n"),
    lists:foreach(fun(Index) ->
        {ok, Pid, Name} = node:start(builder, Index, AllNodes),
        io:format("  Started ~s (PID: ~p)~n", [Name, Pid])
    end, lists:seq(1, 1)),

    timer:sleep(200),

    %% Étape 3 : Démarrer les validateurs
    io:format("~nStep 3: Starting validators...~n"),
    lists:foreach(fun(Index) ->
        %% KnownNodes = tous les autres nœuds (validateurs + builders)
        {ok, Pid, Name} = node:start_validator(Index, AllValidators, Builders),
        io:format("  Started ~s (PID: ~p)~n", [Name, Pid])
    end, lists:seq(1, 5)),

    timer:sleep(500),

    %% Étape 4 : Définir un proposer group initial
    io:format("~nStep 4: Setting initial proposer group...~n"),
    InitialProposerGroup = ["Validator_1"],
    io:format("  Initial proposer group: ~p~n", [InitialProposerGroup]),

    lists:foreach(fun(ValidatorName) ->
        node:set_proposer_group(ValidatorName, InitialProposerGroup)
    end, AllValidators),

    timer:sleep(200),

    %% Étape 5 : Lancer l'élection
    io:format("~nStep 5: Starting election from head validator...~n"),
    HeadValidator = "Validator_1",
    io:format("  Head validator: ~s~n", [HeadValidator]),
    io:format("  Broadcasting start_election to all nodes...~n"),

    node:start_election(HeadValidator, AllValidators, AllNodes),

    %% Attendre que l'élection se termine
    timer:sleep(6000),

    %% Étape 6 : Vérifier le nouveau proposer group
    io:format("~nStep 6: Checking new proposer group...~n"),
    lists:foreach(fun(ValidatorName) ->
        {ok, ProposerGroup} = node:get_proposer_group(ValidatorName),
        io:format("  ~s proposer group: ~p~n", [ValidatorName, ProposerGroup])
    end, AllValidators),

    %% Étape 7 : Vérifier que le nouveau head a broadcast start_new_epoch
    io:format("~nStep 7: Verifying that start_new_epoch was broadcasted...~n"),
    io:format("  Check the logs above - you should see:~n"),
    io:format("    - [Builder_1] Election started - will finish current block then pause~n"),
    io:format("    - [Builder_2] Election started - will finish current block then pause~n"),
    io:format("    - [Builder_1] New epoch - block creation can resume~n"),
    io:format("    - [Builder_2] New epoch - block creation can resume~n"),
    io:format("    - [Validator_X] New epoch started! Block creation RESUMED.~n"),

    io:format("~n=== Test Complete ===~n~n"),
    ok.
