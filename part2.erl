-module(part2).
-export([start/1, test/0, log_election_event/2, log_global_event/1]).

%% Partie 2 : Test du processus d'élection avec enregistrement des opérations
%% Objectif : Observer et mesurer le processus d'élection Whisk
%% Arguments:
%%   - NumValidators : nombre de validators participant à l'élection

start(NumValidators) ->
    io:format("~n=== Part 2: Election Process Testing ===~n"),
    io:format("Configuration:~n"),
    io:format("  Validators: ~p~n~n", [NumValidators]),

    %% Nettoyer les anciens fichiers de logs
    clean_election_logs(),

    %% Étape 1 : Créer les validators
    io:format("Step 1: Starting ~p validators...~n", [NumValidators]),
    ValidatorNames = lists:map(fun(Index) ->
        {ok, Pid, ValidatorName} = node:start_validator(Index, [], []),
        io:format("  Started ~s (PID: ~p)~n", [ValidatorName, Pid]),

        %% Créer un fichier de log pour chaque validator
        LogFileName = lists:flatten(io_lib:format("~s_election_log.txt", [ValidatorName])),
        file:write_file(LogFileName, io_lib:format("Election Log for ~s~n", [ValidatorName])),
        file:write_file(LogFileName, io_lib:format("================================~n~n", []), [append]),

        ValidatorName
    end, lists:seq(1, NumValidators)),
    timer:sleep(500),

    %% Tous les nœuds (ici seulement les validators)
    AllNodes = ValidatorNames,

    io:format("~nAll validators ready!~n"),
    io:format("Validators: ~p~n~n", [ValidatorNames]),

    %% Étape 2 : Lancer l'élection et mesurer le temps
    InitialBuilder = lists:nth(1, ValidatorNames),

    io:format("~n╔════════════════════════════════════════╗~n"),
    io:format("║   STARTING ELECTION PROCESS            ║~n"),
    io:format("╠════════════════════════════════════════╣~n"),
    io:format("║ Initiator: ~s                   ║~n", [InitialBuilder]),
    io:format("║ Total validators: ~p                    ║~n", [NumValidators]),
    io:format("║ ProposerGroup size: ~p                  ║~n", [max(1, NumValidators div 10)]),
    io:format("╚════════════════════════════════════════╝~n~n"),

    %% Log dans le fichier de l'initiateur
    log_election_operation(InitialBuilder, "Election initiated", []),

    %% Démarrer le chronomètre
    StartTime = erlang:monotonic_time(millisecond),

    %% Lancer l'élection
    io:format("Starting election at ~p ms...~n", [StartTime]),
    node:start_election(InitialBuilder, ValidatorNames, AllNodes),

    %% Attendre que l'élection se termine
    io:format("Waiting for election to complete...~n"),
    timer:sleep(8000),

    %% Arrêter le chronomètre
    EndTime = erlang:monotonic_time(millisecond),
    ElectionDuration = EndTime - StartTime,

    io:format("~nElection completed at ~p ms~n", [EndTime]),

    %% Étape 3 : Récupérer le ProposerGroup final
    io:format("~nRetrieving final ProposerGroup...~n"),
    case node:get_proposer_group(lists:nth(1, ValidatorNames)) of
        {ok, FinalProposerGroup} ->
            io:format("~n╔════════════════════════════════════════╗~n"),
            io:format("║   ELECTION RESULTS                     ║~n"),
            io:format("╠════════════════════════════════════════╣~n"),
            io:format("║ Election Duration: ~p ms             ║~n", [ElectionDuration]),
            io:format("╠════════════════════════════════════════╣~n"),
            io:format("║ Final ProposerGroup:                   ║~n"),
            lists:foreach(fun(V) ->
                io:format("║   - ~s                          ║~n", [V])
            end, FinalProposerGroup),
            io:format("╚════════════════════════════════════════╝~n~n"),

            %% Enregistrer les résultats dans un fichier de synthèse
            ResultFile = "election_results.txt",
            file:write_file(ResultFile,
                io_lib:format("ELECTION RESULTS~n", [])),
            file:write_file(ResultFile,
                io_lib:format("================~n~n", []), [append]),
            file:write_file(ResultFile,
                io_lib:format("Number of validators: ~p~n", [NumValidators]), [append]),
            file:write_file(ResultFile,
                io_lib:format("Initiator: ~s~n", [InitialBuilder]), [append]),
            file:write_file(ResultFile,
                io_lib:format("Election duration: ~p ms~n", [ElectionDuration]), [append]),
            file:write_file(ResultFile,
                io_lib:format("ProposerGroup size: ~p~n", [length(FinalProposerGroup)]), [append]),
            file:write_file(ResultFile,
                io_lib:format("~nFinal ProposerGroup:~n", []), [append]),
            lists:foreach(fun(V) ->
                file:write_file(ResultFile,
                    io_lib:format("  - ~s~n", [V]), [append])
            end, FinalProposerGroup),

            %% Logger le résultat pour chaque validator
            lists:foreach(fun(ValidatorName) ->
                log_election_operation(ValidatorName,
                    "Election completed",
                    [{duration_ms, ElectionDuration},
                     {proposer_group, FinalProposerGroup}])
            end, ValidatorNames),

            io:format("~nElection logs saved:~n"),
            lists:foreach(fun(ValidatorName) ->
                LogFile = lists:flatten(io_lib:format("~s_election_log.txt", [ValidatorName])),
                io:format("  - ~s~n", [LogFile])
            end, ValidatorNames),
            io:format("  - ~s~n~n", [ResultFile]),

            ok;
        {error, timeout} ->
            io:format("~n[ERROR] Timeout retrieving ProposerGroup!~n"),
            {error, timeout}
    end.

%% Nettoie les anciens fichiers de logs d'élection
clean_election_logs() ->
    io:format("Cleaning old election logs...~n"),
    file:delete("election_results.txt"),
    file:delete("election_global_log.txt"),
    lists:foreach(fun(Index) ->
        ValidatorName = lists:flatten(io_lib:format("Validator_~p", [Index])),
        LogFileName = lists:flatten(io_lib:format("~s_election_log.txt", [ValidatorName])),
        file:delete(LogFileName)
    end, lists:seq(1, 20)).  %% Nettoie jusqu'à 20 validators potentiels

%% Enregistre une opération d'élection dans le fichier log du validator
log_election_operation(ValidatorName, Operation, Details) ->
    LogFileName = lists:flatten(io_lib:format("~s_election_log.txt", [ValidatorName])),
    Timestamp = erlang:monotonic_time(millisecond),

    LogEntry = case Details of
        [] ->
            io_lib:format("[~p ms] ~s~n", [Timestamp, Operation]);
        _ ->
            io_lib:format("[~p ms] ~s~n  Details: ~p~n", [Timestamp, Operation, Details])
    end,

    file:write_file(LogFileName, LogEntry, [append]).

%% Fonction publique pour logger les événements d'élection depuis node.erl
log_election_event(ValidatorName, Event) ->
    LogFileName = lists:flatten(io_lib:format("~s_election_log.txt", [ValidatorName])),
    Timestamp = erlang:monotonic_time(millisecond),

    LogEntry = io_lib:format("[~p ms] ~s~n", [Timestamp, Event]),
    file:write_file(LogFileName, LogEntry, [append]).

%% Fonction publique pour logger dans le log global
log_global_event(Event) ->
    LogFileName = "election_global_log.txt",
    LogEntry = io_lib:format("~s~n", [Event]),
    file:write_file(LogFileName, LogEntry, [append]).

%% Fonction de test rapide
test() ->
    start(3).
