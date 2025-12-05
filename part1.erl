-module(part1).
-export([start/2, test/0]).

%% Partie 1 : Création automatique de blocs avec transactions CSV
%% Arguments:
%%   - NumNodes : nombre de nœuds non-validators qui recevront les blocs
%%   - TransactionFile : fichier CSV contenant les transactions

start(NumNodes, TransactionFile) ->
    io:format("~n=== Part 1: Automatic Block Creation with CSV Transactions ===~n"),
    io:format("Configuration:~n"),
    io:format("  Builder: 1~n"),
    io:format("  Non-validator nodes (receivers): ~p~n", [NumNodes]),
    io:format("  Transaction file: ~s~n~n", [TransactionFile]),

    %% Étape 1 : Créer les nœuds non-validators récepteurs
    io:format("Step 1: Starting ~p non-validator nodes (to receive blocks)...~n", [NumNodes]),
    NodeNames = lists:map(fun(Index) ->
        {ok, Pid, NodeName} = node:start(non_validator, Index, []),
        io:format("  Started ~s (PID: ~p)~n", [NodeName, Pid]),
        NodeName
    end, lists:seq(1, NumNodes)),
    timer:sleep(500),

    %% Étape 2 : Créer le builder unique avec la liste des nœuds connus
    io:format("~nStep 2: Starting the builder...~n"),
    {ok, BuilderPid, BuilderName} = node:start(builder, 1, NodeNames),
    io:format("  Started ~s (PID: ~p)~n", [BuilderName, BuilderPid]),
    timer:sleep(500),

    %% Étape 3 : Charger les transactions dans le builder
    io:format("~nStep 3: Loading transactions from ~s...~n", [TransactionFile]),
    node:load_transactions(BuilderName, TransactionFile),
    io:format("  Loaded transactions into ~s~n", [BuilderName]),
    timer:sleep(500),

    %% Étape 4 : Démarrer la création automatique de blocs
    io:format("~nStep 4: Starting automatic block creation...~n"),
    io:format("  Started block creation for ~s~n", [BuilderName]),

    io:format("~n=== Block creation running... ===~n"),
    io:format("Builder will create blocks every 0.5 seconds with 10 transactions each~n"),
    io:format("Blocks will be broadcast to all non-validators~n"),
    io:format("Press Ctrl+C to stop~n~n"),

    %% Démarrer la boucle de création de blocs
    block_creation_loop(BuilderName).

%% Boucle de création automatique de blocs
block_creation_loop(BuilderName) ->
    %% Envoyer le message pour créer un bloc
    BuilderAtom = list_to_atom(BuilderName),
    BuilderAtom ! {create_block_tick},

    %% Attendre 500ms avant le prochain bloc
    timer:sleep(500),

    %% Continuer la boucle
    block_creation_loop(BuilderName).

%% Fonction de test rapide
test() ->
    start(5, "trasactions2.csv").
