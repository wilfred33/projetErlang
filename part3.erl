-module(part3).
-export([start/3, test/0]).

%% Partie 3 : Proof of Stake complet avec élections automatiques
%% Arguments:
%%   - TotalNodes : nombre total de nœuds dans le système
%%   - NumValidators : nombre de validateurs
%%   - TransactionFile : fichier CSV contenant les transactions

start(TotalNodes, NumValidators, TransactionFile) ->
    io:format("~n=== Part 3: Proof of Stake Blockchain ===~n"),
    io:format("Configuration:~n"),
    io:format("  Total nodes: ~p~n", [TotalNodes]),
    io:format("  Validators: ~p~n", [NumValidators]),
    io:format("  Non-validators: ~p~n", [TotalNodes - NumValidators]),
    io:format("  Transaction file: ~s~n~n", [TransactionFile]),

    %% Calcul du nombre de non-validators
    NumNonValidators = TotalNodes - NumValidators,

    %% Étape 1 : Créer le pool de transactions centralisé
    io:format("Step 1: Creating centralized transaction pool...~n"),
    PoolPid = start_transaction_pool(TransactionFile),
    register(transaction_pool, PoolPid),
    timer:sleep(200),

    %% Étape 2 : Créer les listes de nœuds
    io:format("~nStep 2: Creating node lists...~n"),
    AllValidators = [lists:flatten(io_lib:format("Validator_~p", [I]))
                     || I <- lists:seq(1, NumValidators)],
    AllNonValidators = [lists:flatten(io_lib:format("NonValidator_~p", [I]))
                        || I <- lists:seq(1, NumNonValidators)],
    AllNodes = AllValidators ++ AllNonValidators,

    io:format("  All validators: ~p~n", [AllValidators]),
    io:format("  All non-validators: ~p~n", [AllNonValidators]),

    %% Étape 3 : Démarrer tous les validateurs
    io:format("~nStep 3: Starting ~p validators...~n", [NumValidators]),
    lists:foreach(fun(Index) ->
        {ok, Pid, Name} = node:start_validator(Index, AllValidators, AllNonValidators),
        io:format("  Started ~s (PID: ~p)~n", [Name, Pid])
    end, lists:seq(1, NumValidators)),
    timer:sleep(500),

    %% Étape 4 : Démarrer tous les non-validators
    io:format("~nStep 4: Starting ~p non-validators...~n", [NumNonValidators]),
    lists:foreach(fun(Index) ->
        {ok, Pid, Name} = node:start(non_validator, Index, AllNodes),
        io:format("  Started ~s (PID: ~p)~n", [Name, Pid])
    end, lists:seq(1, NumNonValidators)),
    timer:sleep(500),

    %% Étape 5 : Choisir le ProposerGroup initial (10% des validateurs au hasard)
    io:format("~nStep 5: Selecting initial ProposerGroup (10%% of validators)...~n"),
    NumProposers = max(1, NumValidators div 10),
    ShuffledValidators = node:shuffle_list(AllValidators),
    InitialProposerGroup = lists:sublist(ShuffledValidators, NumProposers),
    io:format("  Initial ProposerGroup (~p members): ~p~n", [NumProposers, InitialProposerGroup]),

    %% Étape 6 : Configurer le ProposerGroup pour tous les validateurs
    io:format("~nStep 6: Setting ProposerGroup for all validators...~n"),
    lists:foreach(fun(ValidatorName) ->
        node:set_proposer_group(ValidatorName, InitialProposerGroup)
    end, AllValidators),
    timer:sleep(200),

    %% Étape 7 : Le head du ProposerGroup devient le builder initial
    [InitialBuilder | _] = InitialProposerGroup,
    io:format("~nStep 7: ~s (head of ProposerGroup) is the initial builder~n", [InitialBuilder]),

    %% Étape 8 : Démarrer le contrôleur centralisé de blocs
    io:format("~nStep 8: Starting centralized block controller...~n"),
    ControllerPid = spawn(fun() ->
        block_controller(InitialBuilder, AllValidators, AllNodes, 0)
    end),
    register(block_controller, ControllerPid),

    io:format("~n=== System running... ===~n"),
    io:format("Press Ctrl+C to stop~n~n"),

    %% Boucle infinie pour garder le système vivant
    keep_alive().

%% Contrôleur centralisé qui gère la création de blocs et les élections
%% CurrentBuilder : le builder actuel
%% BlockCount : nombre de blocs créés depuis la dernière élection
block_controller(CurrentBuilder, AllValidators, AllNodes, BlockCount) ->
    %% Vérifie si on doit faire une élection (tous les 10 blocs)
    case BlockCount rem 10 of
        0 when BlockCount > 0 ->
            %% Temps pour une élection!
            io:format("~n~n╔════════════════════════════════════════╗~n"),
            io:format("║   EPOCH ~p COMPLETE!                   ║~n", [BlockCount div 10]),
            io:format("╠════════════════════════════════════════╣~n"),
            io:format("║ Blocks created: ~p                     ║~n", [BlockCount]),
            io:format("║ Current Builder: ~s              ║~n", [CurrentBuilder]),
            io:format("╠════════════════════════════════════════╣~n"),
            io:format("║ Starting ELECTION...                   ║~n"),
            io:format("╚════════════════════════════════════════╝~n~n"),

            %% Lance l'élection
            node:start_election(CurrentBuilder, AllValidators, AllNodes),
            timer:sleep(7000),

            %% Récupère le nouveau ProposerGroup
            case node:get_proposer_group(lists:nth(1, AllValidators)) of
                {ok, NewProposerGroup} ->
                    [NewBuilder | _] = NewProposerGroup,

                    io:format("~n╔════════════════════════════════════════╗~n"),
                    io:format("║   ELECTION COMPLETE!                   ║~n"),
                    io:format("╠════════════════════════════════════════╣~n"),
                    io:format("║ New ProposerGroup:                     ║~n"),
                    lists:foreach(fun(V) ->
                        io:format("║   - ~s                          ║~n", [V])
                    end, NewProposerGroup),
                    io:format("╠════════════════════════════════════════╣~n"),
                    io:format("║ New Builder: ~s                  ║~n", [NewBuilder]),
                    io:format("╚════════════════════════════════════════╝~n~n"),

                    %% Affiche si le builder a changé
                    case NewBuilder =/= CurrentBuilder of
                        true ->
                            io:format("[BlockController] Builder changed: ~s → ~s~n~n",
                                     [CurrentBuilder, NewBuilder]);
                        false ->
                            io:format("[BlockController] Builder remains: ~s~n~n", [NewBuilder])
                    end,

                    %% Continue avec le nouveau builder et reset le compteur
                    block_controller(NewBuilder, AllValidators, AllNodes, 0);
                _ ->
                    %% Erreur lors de la récupération du ProposerGroup
                    block_controller(CurrentBuilder, AllValidators, AllNodes, 0)
            end;
        _ ->
            %% Pas d'élection, créer le prochain bloc
            timer:sleep(1000),

            %% Demande 10 transactions au pool
            transaction_pool ! {take, 10, self()},
            receive
                {transactions, Transactions} ->
                    case Transactions of
                        [] ->
                            %% Plus de transactions
                            io:format("[BlockController] No more transactions, stopping system~n"),
                            ok;
                        _ ->
                            %% Envoie les transactions au builder pour créer le bloc
                            BuilderAtom = list_to_atom(CurrentBuilder),
                            BuilderAtom ! {create_block_with_transactions, Transactions},

                            %% Attend que le bloc soit propagé (2 secondes pour validation)
                            timer:sleep(2000),

                            %% Continue avec le prochain bloc
                            io:format("[BlockController] Block request sent to ~s~n", [CurrentBuilder]),
                            block_controller(CurrentBuilder, AllValidators, AllNodes, BlockCount + 1)
                    end
            after 5000 ->
                io:format("[BlockController] Timeout getting transactions~n"),
                block_controller(CurrentBuilder, AllValidators, AllNodes, BlockCount)
            end
    end.

%% Démarre le pool de transactions centralisé
start_transaction_pool(TransactionFile) ->
    Transactions = load_transactions_from_csv(TransactionFile),
    io:format("  Loaded ~p transactions from ~s~n", [length(Transactions), TransactionFile]),
    spawn(fun() -> transaction_pool_loop(Transactions) end).

%% Boucle du pool de transactions
transaction_pool_loop(Transactions) ->
    receive
        {take, N, From} ->
            case take_transactions(Transactions, N) of
                {Remaining, Taken} when length(Taken) > 0 ->
                    From ! {transactions, Taken},
                    io:format("[TransactionPool] Gave ~p transactions, ~p remaining~n",
                             [length(Taken), length(Remaining)]),
                    transaction_pool_loop(Remaining);
                {Remaining, []} ->
                    From ! {transactions, []},
                    io:format("[TransactionPool] No transactions available~n"),
                    transaction_pool_loop(Remaining)
            end;
        {get_size, From} ->
            From ! {pool_size, length(Transactions)},
            transaction_pool_loop(Transactions);
        stop ->
            io:format("[TransactionPool] Stopping~n"),
            ok
    end.

%% Prend N transactions du pool
take_transactions(TransactionPool, N) ->
    case length(TransactionPool) >= N of
        true ->
            {Used, Remaining} = lists:split(N, TransactionPool),
            {Remaining, Used};
        false ->
            %% Pas assez de transactions, prend tout ce qui reste
            {[], TransactionPool}
    end.

%% Charge les transactions depuis un fichier CSV
load_transactions_from_csv(TransactionFile) ->
    case file:read_file(TransactionFile) of
        {ok, Content} ->
            Lines = string:split(binary_to_list(Content), "\n", all),
            %% Ignore la première ligne (header) si elle existe
            DataLines = case Lines of
                [First | Rest] ->
                    case string:find(First, "emitter") =/= nomatch orelse
                         string:find(First, "Emitter") =/= nomatch of
                        true -> Rest;
                        false -> Lines
                    end;
                [] -> []
            end,
            %% Parse chaque ligne et filtre les transactions valides
            ParsedTransactions = [transaction:from_csv_line(Line) || Line <- DataLines,
                                  Line =/= "", Line =/= []],
            %% Garde seulement les transactions valides
            [Tx || Tx <- ParsedTransactions,
                   Tx =/= {error, invalid_format},
                   transaction:is_valid(Tx)];
        {error, Reason} ->
            io:format("[ERROR] Cannot read transaction file: ~p~n", [Reason]),
            []
    end.

%% Fonction de test rapide
test() ->
    start(10, 3, "trasactions2.csv").

%% Boucle infinie pour garder le système vivant
keep_alive() ->
    receive
        stop ->
            io:format("~n=== System stopped ===~n"),
            ok
    after 1000 ->
        keep_alive()
    end.
