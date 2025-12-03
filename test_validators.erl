-module(test_validators).
-export([test/0]).

%% Test basique : démarrer des validateurs et gérer le proposer group
test() ->
    io:format("~n=== Test: Validator Nodes ===~n~n"),

    %% Étape 1 : Créer une liste de tous les validateurs
    io:format("Step 1: Creating list of all validators...~n"),
    AllValidators = ["Validator_1", "Validator_2", "Validator_3", "Validator_4", "Validator_5"],
    io:format("  All validators: ~p~n~n", [AllValidators]),

    %% Étape 2 : Démarrer les 5 validateurs
    io:format("Step 2: Starting validators...~n"),
    Validators = lists:map(fun(Index) ->
        {ok, Pid, Name} = node:start_validator(Index, AllValidators, []),
        io:format("  Started ~s (PID: ~p)~n", [Name, Pid]),
        Name
    end, lists:seq(1, 5)),

    timer:sleep(500),

    %% Étape 3 : Vérifier que le proposer group est vide au départ
    io:format("~nStep 3: Checking initial proposer group (should be empty)...~n"),
    lists:foreach(fun(ValidatorName) ->
        {ok, ProposerGroup} = node:get_proposer_group(ValidatorName),
        io:format("  ~s proposer group: ~p~n", [ValidatorName, ProposerGroup])
    end, Validators),

    %% Étape 4 : Définir un proposer group initial (10% = 1 validateur sur 5)
    io:format("~nStep 4: Setting initial proposer group...~n"),
    %% On prend le premier validateur comme proposer group
    InitialProposerGroup = ["Validator_1"],
    io:format("  Setting proposer group to: ~p~n", [InitialProposerGroup]),

    lists:foreach(fun(ValidatorName) ->
        node:set_proposer_group(ValidatorName, InitialProposerGroup)
    end, Validators),

    timer:sleep(200),

    %% Étape 5 : Vérifier que tous les validateurs ont le même proposer group
    io:format("~nStep 5: Verifying all validators have the same proposer group...~n"),
    lists:foreach(fun(ValidatorName) ->
        {ok, ProposerGroup} = node:get_proposer_group(ValidatorName),
        io:format("  ~s proposer group: ~p~n", [ValidatorName, ProposerGroup])
    end, Validators),

    %% Étape 6 : Tester la fonction shuffle
    io:format("~nStep 6: Testing shuffle function...~n"),
    io:format("  Original list: ~p~n", [AllValidators]),
    Shuffled1 = node:shuffle_list(AllValidators),
    io:format("  Shuffled 1:    ~p~n", [Shuffled1]),
    Shuffled2 = node:shuffle_list(AllValidators),
    io:format("  Shuffled 2:    ~p~n", [Shuffled2]),
    Shuffled3 = node:shuffle_list(AllValidators),
    io:format("  Shuffled 3:    ~p~n", [Shuffled3]),

    %% Étape 7 : Simuler une élection simple (prendre les 10% premiers après shuffle)
    io:format("~nStep 7: Simulating a simple election...~n"),
    ShuffledValidators = node:shuffle_list(AllValidators),
    io:format("  Shuffled validators: ~p~n", [ShuffledValidators]),

    %% Calculer 10% du nombre de validateurs (minimum 1)
    NumValidators = length(AllValidators),
    NumProposers = max(1, NumValidators div 10),
    io:format("  Total validators: ~p~n", [NumValidators]),
    io:format("  10%% = ~p proposers~n", [NumProposers]),

    %% Sélectionner les premiers
    NewProposerGroup = lists:sublist(ShuffledValidators, NumProposers),
    io:format("  New proposer group: ~p~n", [NewProposerGroup]),

    %% Envoyer le nouveau proposer group à tous les validateurs
    io:format("~nStep 8: Setting new proposer group to all validators...~n"),
    lists:foreach(fun(ValidatorName) ->
        node:set_proposer_group(ValidatorName, NewProposerGroup)
    end, Validators),

    timer:sleep(200),

    %% Vérifier
    io:format("~nStep 9: Verifying new proposer group...~n"),
    lists:foreach(fun(ValidatorName) ->
        {ok, ProposerGroup} = node:get_proposer_group(ValidatorName),
        IsInGroup = lists:member(ValidatorName, ProposerGroup),
        Status = case IsInGroup of
            true -> "[IN proposer group]";
            false -> "[not in proposer group]"
        end,
        io:format("  ~s: ~s~n", [ValidatorName, Status])
    end, Validators),

    io:format("~n=== Test Complete ===~n~n"),
    ok.
