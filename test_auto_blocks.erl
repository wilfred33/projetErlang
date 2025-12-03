-module(test_auto_blocks).
-export([test/0]).

%% Test complet : builders créent des blocs automatiquement et s'arrêtent pendant l'élection
test() ->
    io:format("~n=== Test: Automatic Block Creation with Election ===~n~n"),

    %% Étape 1 : Créer les listes de nœuds
    io:format("Step 1: Creating node lists...~n"),
    AllValidators = ["Validator_1", "Validator_2", "Validator_3"],
    Builders = ["Builder_1", "Builder_2"],
    AllNodes = AllValidators ++ Builders,
    io:format("  All validators: ~p~n", [AllValidators]),
    io:format("  Builders: ~p~n", [Builders]),
    io:format("  All nodes: ~p~n~n", [AllNodes]),

    %% Étape 2 : Démarrer les builders
    io:format("Step 2: Starting builders...~n"),
    lists:foreach(fun(Index) ->
        {ok, Pid, Name} = node:start(builder, Index, AllNodes),
        io:format("  Started ~s (PID: ~p)~n", [Name, Pid])
    end, lists:seq(1, 2)),

    timer:sleep(200),

    %% Étape 3 : Démarrer les validateurs
    io:format("~nStep 3: Starting validators...~n"),
    lists:foreach(fun(Index) ->
        {ok, Pid, Name} = node:start_validator(Index, AllValidators, Builders),
        io:format("  Started ~s (PID: ~p)~n", [Name, Pid])
    end, lists:seq(1, 3)),

    timer:sleep(500),

    %% Étape 4 : Définir un proposer group initial
    io:format("~nStep 4: Setting initial proposer group...~n"),
    InitialProposerGroup = ["Validator_1"],
    lists:foreach(fun(ValidatorName) ->
        node:set_proposer_group(ValidatorName, InitialProposerGroup)
    end, AllValidators),
    io:format("  Proposer group: ~p~n", [InitialProposerGroup]),

    timer:sleep(200),

    %% Étape 5 : Démarrer la création automatique de blocs pour les builders
    io:format("~nStep 5: Starting automatic block creation for builders...~n"),
    CreationPid1 = node:start_block_creation("Builder_1"),
    CreationPid2 = node:start_block_creation("Builder_2"),
    io:format("  Builder_1 creation process: ~p~n", [CreationPid1]),
    io:format("  Builder_2 creation process: ~p~n", [CreationPid2]),
    io:format("  Blocks will be created every 2 seconds...~n"),

    %% Attendre que quelques blocs soient créés
    io:format("~nStep 6: Waiting for blocks to be created (6 seconds)...~n"),
    timer:sleep(6000),

    %% Vérifier combien de blocs ont été créés
    {ok, BC1} = node:get_blockchain("Builder_1"),
    {ok, BC2} = node:get_blockchain("Builder_2"),
    io:format("  Builder_1 blockchain length: ~p blocks~n", [length(BC1)]),
    io:format("  Builder_2 blockchain length: ~p blocks~n", [length(BC2)]),

    %% Étape 7 : Lancer l'élection (les builders devraient s'arrêter)
    io:format("~nStep 7: Starting election (builders should PAUSE)...~n"),
    node:start_election("Validator_1", AllValidators, AllNodes),

    timer:sleep(1000),

    %% Étape 8 : Attendre pendant l'élection (aucun nouveau bloc ne devrait être créé)
    io:format("~nStep 8: Waiting during election (5 seconds, NO new blocks)...~n"),
    {ok, BC1_before} = node:get_blockchain("Builder_1"),
    {ok, BC2_before} = node:get_blockchain("Builder_2"),
    io:format("  Before wait - Builder_1: ~p blocks, Builder_2: ~p blocks~n",
              [length(BC1_before), length(BC2_before)]),

    timer:sleep(5000),

    {ok, BC1_after} = node:get_blockchain("Builder_1"),
    {ok, BC2_after} = node:get_blockchain("Builder_2"),
    io:format("  After wait - Builder_1: ~p blocks, Builder_2: ~p blocks~n",
              [length(BC1_after), length(BC2_after)]),

    case (length(BC1_after) =:= length(BC1_before)) and
         (length(BC2_after) =:= length(BC2_before)) of
        true ->
            io:format("  ✓ SUCCESS: No new blocks created during election!~n");
        false ->
            io:format("  ✗ FAILED: Blocks were created during election~n")
    end,

    %% Étape 9 : Attendre que la nouvelle epoch commence (les builders reprennent)
    io:format("~nStep 9: Waiting for new epoch to start...~n"),
    timer:sleep(2000),

    %% Étape 10 : Vérifier que les builders ont repris la création de blocs
    io:format("~nStep 10: Checking that block creation resumed (4 seconds)...~n"),
    {ok, BC1_resume_before} = node:get_blockchain("Builder_1"),
    {ok, BC2_resume_before} = node:get_blockchain("Builder_2"),
    io:format("  Before - Builder_1: ~p blocks, Builder_2: ~p blocks~n",
              [length(BC1_resume_before), length(BC2_resume_before)]),

    timer:sleep(4000),

    {ok, BC1_resume_after} = node:get_blockchain("Builder_1"),
    {ok, BC2_resume_after} = node:get_blockchain("Builder_2"),
    io:format("  After - Builder_1: ~p blocks, Builder_2: ~p blocks~n",
              [length(BC1_resume_after), length(BC2_resume_after)]),

    case (length(BC1_resume_after) > length(BC1_resume_before)) or
         (length(BC2_resume_after) > length(BC2_resume_before)) of
        true ->
            io:format("  ✓ SUCCESS: Block creation resumed after election!~n");
        false ->
            io:format("  ✗ FAILED: Block creation did not resume~n")
    end,

    %% Étape 11 : Arrêter la création automatique
    io:format("~nStep 11: Stopping automatic block creation...~n"),
    node:stop_block_creation(CreationPid1),
    node:stop_block_creation(CreationPid2),
    io:format("  Block creation stopped~n"),

    io:format("~n=== Test Complete ===~n~n"),
    ok.
