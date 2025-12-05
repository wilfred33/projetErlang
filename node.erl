
-module(node).
-export([start/2, start/3, start_validator/3, stop/1, get_blockchain/1, get_type/1, set_proposer_group/2, get_proposer_group/1, shuffle_list/1, start_election/3]).
-export([init/4, init_validator/6]).
-export([start_block_creation/1, stop_block_creation/1, load_transactions/2, get_transaction_pool_size/1]).

%% Types de nœuds: builder, validator, non_validator
%% Structure: {Type_Index, Type, Blockchain, KnownNodes, StorageFile}

%% Démarre un nœud
start(Type, Index) ->
    start(Type, Index, []).

start(Type, Index, KnownNodes) ->
    NodeName = build_node_name(Type, Index),
    StorageFile = build_storage_filename(Type, Index),

    %% Crée le processus du nœud
    Pid = spawn(?MODULE, init, [NodeName, Type, KnownNodes, StorageFile]),

    %% Enregistre le processus avec son nom
    register(list_to_atom(NodeName), Pid),

    {ok, Pid, NodeName}.

%% Démarre un nœud validateur avec la liste de tous les validateurs
%% Index : numéro du validateur (1, 2, 3...)
%% AllValidators : liste complète de tous les validateurs ["Validator_1", "Validator_2", ...]
%% KnownNodes : liste des autres nœuds (builders, non_validators)
start_validator(Index, AllValidators, KnownNodes) ->
    NodeName = build_node_name(validator, Index),
    StorageFile = build_storage_filename(validator, Index),

    %% AllNodes = tous les nœuds (validateurs + autres)
    AllNodes = AllValidators ++ KnownNodes,

    %% Crée le processus du validateur avec la liste de tous les validateurs et tous les nœuds
    Pid = spawn(?MODULE, init_validator, [NodeName, AllValidators, AllNodes, KnownNodes, StorageFile, []]),

    %% Enregistre le processus avec son nom
    register(list_to_atom(NodeName), Pid),

    io:format("[Validator] Started ~s with PID ~p~n", [NodeName, Pid]),
    {ok, Pid, NodeName}.

%% Initialise un nœud
init(NodeName, Type, KnownNodes, StorageFile) ->
    %% Charge la blockchain depuis le fichier si il existe (sinon blockchain vide)
    Blockchain = load_blockchain(StorageFile, []),

    io:format("[~s] Node started with type: ~p (blockchain: ~p blocks)~n",
              [NodeName, Type, length(Blockchain)]),

    %% Entre dans la boucle du nœud
    node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile).

%% Initialise un validateur avec la liste de tous les validateurs
%% NodeName : "Validator_1", "Validator_2", etc.
%% AllValidators : liste complète ["Validator_1", "Validator_2", ...]
%% AllNodes : liste de TOUS les nœuds (validateurs + builders + non_validators)
%% KnownNodes : autres nœuds du réseau
%% StorageFile : fichier CSV pour la blockchain
%% ProposerGroup : liste des validateurs élus (vide au début)
init_validator(NodeName, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup) ->
    %% Charge la blockchain depuis le fichier
    Blockchain = load_blockchain(StorageFile, []),

    io:format("[~s] Validator started (blockchain: ~p blocks, all validators: ~p)~n",
              [NodeName, length(Blockchain), length(AllValidators)]),

    %% Entre dans la boucle spécifique aux validateurs
    %% ElectionInProgress = false au départ
    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, false).

%% Boucle principale du nœud (avec gestion de l'élection)
node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile) ->
    node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, false, []).

node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress) ->
    node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, []).

node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool) ->
    node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, [], #{}).

node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations) ->
    receive
        %% Reçoit un nouveau bloc
        {new_block, Block} ->
            io:format("[~s] Received new block #~p~n", [NodeName, block:get_number(Block)]),

            %% Vérifie que le bloc est valide
            case block:is_valid(Block) of
                true ->
                    %% Vérifie que le bloc suit bien le dernier bloc de la chaîne
                    ShouldAccept = case Blockchain of
                        [] ->
                            %% Blockchain vide : accepter seulement le bloc #0
                            block:get_number(Block) =:= 0;
                        _ ->
                            %% Blockchain existante : vérifier le chaînage
                            LastBlock = lists:last(Blockchain),
                            LastBlockHash = block:hash(LastBlock),
                            PrevHash = block:get_prev_hash(Block),
                            LastBlockHash =:= PrevHash
                    end,

                    case ShouldAccept of
                        true ->
                            %% Ajoute le bloc à la blockchain
                            NewBlockchain = Blockchain ++ [Block],

                            %% Sauvegarde dans le fichier
                            save_block(StorageFile, Block),

                            io:format("[~s] Block #~p added to blockchain~n",
                                      [NodeName, block:get_number(Block)]),

                            %% Broadcast le bloc aux autres nœuds
                            broadcast_block(Block, KnownNodes, NodeName),

                            node_loop(NodeName, Type, NewBlockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations);
                        false ->
                            io:format("[~s] Block #~p rejected: invalid previous hash~n",
                                      [NodeName, block:get_number(Block)]),
                            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations)
                    end;
                false ->
                    io:format("[~s] Block #~p rejected: invalid block~n",
                              [NodeName, block:get_number(Block)]),
                    node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations)
            end;

        %% Ajoute un nœud à la liste des nœuds connus
        {add_node, NewNode} ->
            case lists:member(NewNode, KnownNodes) of
                false ->
                    io:format("[~s] Added node ~s to known nodes~n", [NodeName, NewNode]),
                    node_loop(NodeName, Type, Blockchain, [NewNode | KnownNodes], StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations);
                true ->
                    node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations)
            end;

        %% Charge des transactions depuis un fichier CSV (pour builders)
        {load_transactions, TransactionFile} ->
            LoadedTransactions = load_transactions_from_csv(TransactionFile),
            io:format("[~s] Loaded ~p transactions from ~s~n", [NodeName, length(LoadedTransactions), TransactionFile]),
            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, LoadedTransactions, ProposerGroup, PendingValidations);

        %% Retourne la blockchain
        {get_blockchain, From} ->
            From ! {blockchain, Blockchain},
            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations);

        %% Retourne le type du nœud
        {get_type, From} ->
            From ! {node_type, Type},
            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations);

        %% Retourne la taille du pool de transactions
        {get_transaction_pool_size, From} ->
            From ! {transaction_pool_size, length(TransactionPool)},
            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations);

        %% Définit le ProposerGroup (pour builders)
        {set_proposer_group, NewProposerGroup} ->
            io:format("[~s] ProposerGroup set: ~p~n", [NodeName, NewProposerGroup]),
            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, NewProposerGroup, PendingValidations);

        %% Élection commence (pour builders/non_validators)
        {start_election} ->
            io:format("[~s] Election started - block creation PAUSED~n", [NodeName]),
            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, true, TransactionPool, ProposerGroup, PendingValidations);

        %% Nouvelle epoch commence (pour builders/non_validators)
        {start_new_epoch} ->
            io:format("[~s] New epoch - block creation RESUMED~n", [NodeName]),
            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, false, TransactionPool, ProposerGroup, PendingValidations);

        %% Message interne pour la création de bloc (builders seulement)
        {create_block_tick} when Type =:= builder ->
            case ElectionInProgress of
                true ->
                    %% Élection en cours : ne pas créer de bloc, ignorer le tick
                    io:format("[~s] Skipping block creation (election in progress)~n", [NodeName]),
                    node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations);
                false ->
                    %% Pas d'élection : créer un bloc avec les transactions du pool
                    {NewTransactionPool, UsedTransactions} = take_transactions(TransactionPool, 10),
                    case UsedTransactions of
                        [] ->
                            %% Pas de transactions disponibles, skip
                            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations);
                        _ ->
                            create_and_broadcast_block(NodeName, Blockchain, ProposerGroup, StorageFile, UsedTransactions),
                            %% NE PAS envoyer le prochain tick - attendre la validation
                            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, NewTransactionPool, ProposerGroup, PendingValidations)
                    end
            end;

        %% Bloc approuvé par un validateur du ProposerGroup
        {block_approved, BlockHash, ValidatorName} ->
            io:format("[~s] Block approved by ~s~n", [NodeName, ValidatorName]),

            %% Ajoute l'approbation
            CurrentApprovals = maps:get(BlockHash, PendingValidations, []),
            NewApprovals = [ValidatorName | CurrentApprovals],
            NewPendingValidations = maps:put(BlockHash, NewApprovals, PendingValidations),

            %% Vérifie si on a la majorité (> 50%)
            TotalValidators = length(ProposerGroup),
            MajorityThreshold = (TotalValidators div 2) + 1,

            case length(NewApprovals) >= MajorityThreshold of
                true ->
                    io:format("[~s] MAJORITY REACHED (~p/~p)! Broadcasting block to all nodes~n",
                             [NodeName, length(NewApprovals), TotalValidators]),

                    %% Récupère le bloc depuis la blockchain ou le crée à partir du hash
                    %% Pour simplifier, on broadcast le dernier bloc créé
                    LastBlock = lists:last(Blockchain),
                    broadcast_block(LastBlock, KnownNodes, NodeName),

                    %% Nettoie les validations en attente pour ce bloc
                    CleanedValidations = maps:remove(BlockHash, NewPendingValidations),

                    %% Envoie le prochain tick après un délai
                    timer:sleep(500),
                    NodeAtom = list_to_atom(NodeName),
                    NodeAtom ! {create_block_tick},

                    node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, CleanedValidations);
                false ->
                    io:format("[~s] Approvals: ~p/~p (waiting for majority)~n",
                             [NodeName, length(NewApprovals), MajorityThreshold]),
                    node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, NewPendingValidations)
            end;

        %% Bloc rejeté par un validateur
        {block_rejected_by, _BlockHash, ValidatorName} ->
            io:format("[~s] Block REJECTED by ~s~n", [NodeName, ValidatorName]),
            %% TODO: gérer les rejets (pour l'instant on ignore)
            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations);

        %% Arrête le nœud
        stop ->
            io:format("[~s] Node stopping~n", [NodeName]),
            ok;

        %% Message inconnu
        Unknown ->
            io:format("[~s] Unknown message: ~p~n", [NodeName, Unknown]),
            node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool, ProposerGroup, PendingValidations)
    end.

%% Broadcast un bloc à tous les nœuds connus (sauf l'émetteur)
broadcast_block(Block, KnownNodes, SenderName) ->
    lists:foreach(fun(NodeName) ->
        case NodeName of
            SenderName ->
                ok; % Ne pas s'envoyer à soi-même
            _ ->
                try
                    NodeAtom = list_to_atom(NodeName),
                    NodeAtom ! {new_block, Block}
                catch
                    _:_ -> ok
                end
        end
    end, KnownNodes).

%% Construit le nom d'un nœud
build_node_name(builder, Index) ->
    "Builder_" ++ integer_to_list(Index);
build_node_name(validator, Index) ->
    "Validator_" ++ integer_to_list(Index);
build_node_name(non_validator, Index) ->
    "NonValidator_" ++ integer_to_list(Index).

%% Construit le nom du fichier de stockage
build_storage_filename(Type, Index) ->
    NodeName = build_node_name(Type, Index),
    "blockchain_" ++ NodeName ++ ".csv".

%% Sauvegarde un bloc dans le fichier CSV
save_block(Filename, Block) ->
    %% Ouvre le fichier en mode append
    case file:open(Filename, [append]) of
        {ok, File} ->
            Line = block:to_csv_line(Block),
            io:fwrite(File, "~s", [Line]),
            file:close(File);
        {error, Reason} ->
            io:format("Error opening file ~s: ~p~n", [Filename, Reason])
    end.

%% Charge la blockchain depuis un fichier
load_blockchain(Filename, DefaultBlockchain) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            Lines = string:split(binary_to_list(Content), "\n", all),
            ValidLines = [L || L <- Lines, L =/= "", L =/= []],
            case ValidLines of
                [] ->
                    DefaultBlockchain;
                _ ->
                    %% Parse chaque ligne en bloc
                    ParsedBlocks = [block:from_csv_line(Line) || Line <- ValidLines],

                    %% Filtre les blocs valides (ignore les erreurs de parsing)
                    ValidBlocks = [B || B <- ParsedBlocks,
                                       B =/= {error, invalid_format},
                                       is_tuple(B),
                                       element(1, B) =:= block],

                    case ValidBlocks of
                        [] -> DefaultBlockchain;
                        Blocks -> Blocks
                    end
            end;
        {error, enoent} ->
            %% Le fichier n'existe pas encore, retourne blockchain vide
            DefaultBlockchain;
        {error, _Reason} ->
            DefaultBlockchain
    end.

%% API pour obtenir la blockchain d'un nœud
get_blockchain(NodeName) when is_list(NodeName) ->
    try
        NodeAtom = list_to_atom(NodeName),
        NodeAtom ! {get_blockchain, self()},
        receive
            {blockchain, Blockchain} -> {ok, Blockchain}
        after 5000 ->
            {error, timeout}
        end
    catch
        _:_ -> {error, node_not_found}
    end.

%% API pour obtenir le type d'un nœud
get_type(NodeName) when is_list(NodeName) ->
    try
        NodeAtom = list_to_atom(NodeName),
        NodeAtom ! {get_type, self()},
        receive
            {node_type, Type} -> {ok, Type}
        after 5000 ->
            {error, timeout}
        end
    catch
        _:_ -> {error, node_not_found}
    end.

%% API pour obtenir la taille du pool de transactions
get_transaction_pool_size(NodeName) when is_list(NodeName) ->
    try
        NodeAtom = list_to_atom(NodeName),
        NodeAtom ! {get_transaction_pool_size, self()},
        receive
            {transaction_pool_size, Size} -> {ok, Size}
        after 5000 ->
            {error, timeout}
        end
    catch
        _:_ -> {error, node_not_found}
    end.

%% Arrête un nœud
stop(NodeName) when is_list(NodeName) ->
    try
        NodeAtom = list_to_atom(NodeName),
        NodeAtom ! stop,
        ok
    catch
        _:_ -> {error, node_not_found}
    end.

%% Charge des transactions depuis un fichier CSV dans le pool d'un builder
load_transactions(NodeName, TransactionFile) when is_list(NodeName) ->
    try
        NodeAtom = list_to_atom(NodeName),
        NodeAtom ! {load_transactions, TransactionFile},
        ok
    catch
        _:_ -> {error, node_not_found}
    end.

%%% ========================================
%%% Création automatique de blocs (pour builders)
%%% ========================================

%% Charge des transactions depuis un fichier CSV
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
            ParsedTransactions = [transaction:from_csv_line(Line) || Line <- DataLines, Line =/= "", Line =/= []],
            %% Garde seulement les transactions valides
            [Tx || Tx <- ParsedTransactions,
                   Tx =/= {error, invalid_format},
                   transaction:is_valid(Tx)];
        {error, Reason} ->
            io:format("[Builder] Error reading CSV file: ~p~n", [Reason]),
            []
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

%% Démarre la création automatique de blocs pour un builder
%% Crée des blocs de manière synchrone sans timer
start_block_creation(NodeName) when is_list(NodeName) ->
    NodeAtom = list_to_atom(NodeName),
    NodeAtom ! {create_block_tick},
    ok.

%% Arrête la création automatique de blocs (fonction gardée pour compatibilité)
stop_block_creation(_Pid) ->
    ok.

%% Crée et broadcast un nouveau bloc AU PROPOSER GROUP SEULEMENT (pour validation)
create_and_broadcast_block(NodeName, Blockchain, ProposerGroup, _StorageFile, Transactions) ->
    %% Calcule le numéro du prochain bloc et l'index de départ
    {BlockNumber, StartTxIndex} = case Blockchain of
        [] ->
            %% Blockchain vide : premier bloc #0
            {0, 1};
        _ ->
            %% Blockchain existante
            LastBlock = lists:last(Blockchain),
            LastTxIndex = block:get_tx_start_index(LastBlock),
            LastTxCount = length(block:get_transactions(LastBlock)),
            {block:get_number(LastBlock) + 1, LastTxIndex + LastTxCount}
    end,

    %% Création du bloc
    {NewBlock, PrevHashInfo} = case BlockNumber of
        0 ->
            %% Bloc genesis : utilise un hash nul (tous zéros) comme PrevHash
            NullHash = <<0:256>>,
            Block = block:new(BlockNumber, NodeName, NullHash, Transactions, StartTxIndex),
            {Block, "null PrevHash"};
        _ ->
            %% Bloc normal : utilise le hash du bloc précédent
            PrevBlock = lists:last(Blockchain),
            PrevBlockHash = block:hash(PrevBlock),
            Block = block:new(BlockNumber, NodeName, PrevBlockHash, Transactions, StartTxIndex),
            {Block, "valid PrevHash"}
    end,

    io:format("[~s] Created block #~p with ~p transactions (~s)~n",
              [NodeName, BlockNumber, length(Transactions), PrevHashInfo]),

    %% ENVOIE LE BLOC AU PROPOSER GROUP SEULEMENT (pas à tout le monde!)
    case ProposerGroup of
        [] ->
            io:format("[~s] WARNING: No ProposerGroup set! Cannot validate block.~n", [NodeName]);
        _ ->
            io:format("[~s] Sending block #~p to ProposerGroup for validation: ~p~n",
                     [NodeName, BlockNumber, ProposerGroup]),
            lists:foreach(fun(ValidatorName) ->
                ValidatorAtom = list_to_atom(ValidatorName),
                ValidatorAtom ! {validate_block, NewBlock, NodeName}
            end, ProposerGroup)
    end,

    %% Ajoute le bloc à la blockchain locale du builder (avant validation)
    BuilderAtom = list_to_atom(NodeName),
    BuilderAtom ! {new_block, NewBlock},

    ok.

%%% ========================================
%%% Fonctions pour les validateurs (Partie 2)
%%% ========================================

%% Définit le proposer group pour un validateur
%% ValidatorName : nom du validateur ("Validator_1")
%% ProposerGroup : liste des validateurs élus ["Validator_3", "Validator_1", ...]
set_proposer_group(ValidatorName, ProposerGroup) ->
    ValidatorAtom = list_to_atom(ValidatorName),
    ValidatorAtom ! {set_proposer_group, ProposerGroup},
    ok.

%% Obtient le proposer group d'un validateur
get_proposer_group(ValidatorName) ->
    ValidatorAtom = list_to_atom(ValidatorName),
    ValidatorAtom ! {get_proposer_group, self()},
    receive
        {proposer_group, ProposerGroup} -> {ok, ProposerGroup}
    after 5000 ->
        {error, timeout}
    end.

%% Démarre l'élection d'un nouveau proposer group
%% HeadValidatorName : le nom du validateur head qui lance l'élection ("Validator_1")
%% AllValidators : liste complète de tous les validateurs
%% AllNodes : liste de TOUS les nœuds (builders, validators, non_validators)
start_election(HeadValidatorName, AllValidators, AllNodes) ->
    io:format("[Election] Starting election initiated by ~s~n", [HeadValidatorName]),

    %% Log pour le head initiateur
    Event1 = "Initiating election as head validator",
    try part2:log_election_event(HeadValidatorName, Event1) catch _:_ -> ok end,

    %% Broadcast à TOUS les nœuds (pas juste validateurs) que l'élection commence
    Event2 = "Broadcasting election start to all nodes",
    try part2:log_election_event(HeadValidatorName, Event2) catch _:_ -> ok end,
    lists:foreach(fun(NodeName) ->
        NodeAtom = list_to_atom(NodeName),
        NodeAtom ! {start_election}
    end, AllNodes),

    %% Le head shuffle la liste initiale
    InitialShuffled = shuffle_list(AllValidators),
    io:format("[Election] Head validator ~s initial shuffle: ~p~n", [HeadValidatorName, InitialShuffled]),
    Event3 = lists:flatten(io_lib:format("Performed initial shuffle of validator list: ~p", [InitialShuffled])),
    try part2:log_election_event(HeadValidatorName, Event3) catch _:_ -> ok end,

    %% Le head retire son propre nom pour créer la liste des validateurs restants
    %% Ces validateurs vont shuffle à tour de rôle
    RemainingValidators = [V || V <- AllValidators, V =/= HeadValidatorName],

    %% Envoie au premier validateur de la liste
    case RemainingValidators of
        [] ->
            %% Cas où il n'y a qu'un seul validateur (le head lui-même)
            io:format("[Election] Only one validator, election trivial~n"),
            Event4 = "Only one validator - election trivial, completing directly",
            try part2:log_election_event(HeadValidatorName, Event4) catch _:_ -> ok end,
            HeadAtom = list_to_atom(HeadValidatorName),
            HeadAtom ! {election_complete, InitialShuffled};
        [FirstValidator | Rest] ->
            io:format("[Election] Sending to first validator: ~s~n", [FirstValidator]),
            io:format("[Election] Shuffled list being sent: ~p~n", [InitialShuffled]),
            Event4 = lists:flatten(io_lib:format("Sending shuffle to first validator: ~s. List: ~p", [FirstValidator, InitialShuffled])),
            try part2:log_election_event(HeadValidatorName, Event4) catch _:_ -> ok end,
            FirstValidatorAtom = list_to_atom(FirstValidator),
            FirstValidatorAtom ! {shuffle_round, InitialShuffled, Rest, HeadValidatorName}
    end,

    ok.

%% Mélange une liste de manière aléatoire
%% Utilise l'algorithme de Fisher-Yates
shuffle_list(List) ->
    %% Convertit la liste en une liste de tuples {Nombre_Aléatoire, Élément}
    Tagged = [{rand:uniform(), Element} || Element <- List],
    %% Trie par le nombre aléatoire
    Sorted = lists:sort(Tagged),
    %% Extrait juste les éléments
    [Element || {_, Element} <- Sorted].

%% Boucle principale pour les validateurs
%% Cette boucle gère à la fois les blocs ET les messages d'élection
%% AllNodes : liste de TOUS les nœuds (validateurs + builders + non_validators)
%% ElectionInProgress : true si une élection est en cours (bloque la création de blocs)
validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress) ->
    receive
        %% ===== Messages identiques à node_loop =====

        %% Reçoit un nouveau bloc
        {new_block, Block} ->
            io:format("[~s] Received new block #~p~n", [NodeName, block:get_number(Block)]),

            %% VÉRIFIER SI ÉLECTION EN COURS
            case ElectionInProgress of
                true ->
                    %% Élection en cours : rejeter le bloc
                    io:format("[~s] Block #~p REJECTED: election in progress~n",
                              [NodeName, block:get_number(Block)]),
                    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress);
                false ->
                    %% Pas d'élection : traiter le bloc normalement
                    case block:is_valid(Block) of
                        true ->
                            ShouldAccept = case Blockchain of
                                [] ->
                                    block:get_number(Block) =:= 0;
                                _ ->
                                    LastBlock = lists:last(Blockchain),
                                    LastBlockHash = block:hash(LastBlock),
                                    PrevHash = block:get_prev_hash(Block),
                                    LastBlockHash =:= PrevHash
                            end,

                            case ShouldAccept of
                                true ->
                                    NewBlockchain = Blockchain ++ [Block],
                                    save_block(StorageFile, Block),
                                    io:format("[~s] Block #~p added to blockchain~n",
                                              [NodeName, block:get_number(Block)]),
                                    broadcast_block(Block, KnownNodes, NodeName),
                                    validator_loop(NodeName, NewBlockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress);
                                false ->
                                    io:format("[~s] Block #~p rejected: invalid previous hash~n",
                                              [NodeName, block:get_number(Block)]),
                                    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress)
                            end;
                        false ->
                            io:format("[~s] Block #~p rejected: invalid block~n",
                                      [NodeName, block:get_number(Block)]),
                            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress)
                    end
            end;

        %% Ajoute un nœud à la liste des nœuds connus
        {add_node, NewNode} ->
            case lists:member(NewNode, KnownNodes) of
                false ->
                    io:format("[~s] Added node ~s to known nodes~n", [NodeName, NewNode]),
                    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, [NewNode | KnownNodes], StorageFile, ProposerGroup, ElectionInProgress);
                true ->
                    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress)
            end;

        %% Retourne la blockchain
        {get_blockchain, From} ->
            From ! {blockchain, Blockchain},
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress);

        %% Retourne le type du nœud
        {get_type, From} ->
            From ! {node_type, validator},
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress);

        %% Crée un bloc avec des transactions spécifiques (pour Part 3)
        {create_block_with_transactions, Transactions} ->
            io:format("[~s] Creating block with ~p transactions from controller~n",
                     [NodeName, length(Transactions)]),

            case ElectionInProgress of
                true ->
                    io:format("[~s] Skipping block creation (election in progress)~n", [NodeName]),
                    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress);
                false ->
                    %% Crée et broadcast le bloc
                    create_and_broadcast_block(NodeName, Blockchain, ProposerGroup, StorageFile, Transactions),
                    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress)
            end;

        %% Bloc approuvé par un validator du ProposerGroup (pour Part 3)
        {block_approved, BlockHash, ValidatorName} ->
            io:format("[~s] Block approved by ~s (hash: ~p) - ignoring in Part 3~n",
                     [NodeName, ValidatorName, BlockHash]),
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress);

        %% ===== Nouveaux messages pour l'élection =====

        %% Définit le proposer group
        {set_proposer_group, NewProposerGroup} ->
            io:format("[~s] Proposer group set: ~p~n", [NodeName, NewProposerGroup]),
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, NewProposerGroup, ElectionInProgress);

        %% Retourne le proposer group actuel
        {get_proposer_group, From} ->
            From ! {proposer_group, ProposerGroup},
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress);

        %% ===== Messages pour l'algorithme d'élection =====

        %% Message 1 : Début d'élection (envoyé par le head)
        {start_election} ->
            io:format("[~s] Election started! Block creation PAUSED.~n", [NodeName]),
            io:format("[~s] Current ProposerGroup before election: ~p~n", [NodeName, ProposerGroup]),
            %% Log l'événement pour Part 2
            Event = lists:flatten(io_lib:format("Election started - block creation PAUSED. Current ProposerGroup: ~p", [ProposerGroup])),
            try part2:log_election_event(NodeName, Event) catch _:_ -> ok end,
            %% Passe ElectionInProgress à true
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, true);

        %% Message 2 : Shuffle de la liste (circulation entre validateurs)
        %% ShuffledList : la liste des validateurs mélangée
        %% RemainingValidators : les validateurs qui n'ont pas encore shufflé
        %% Initiator : le head qui a lancé l'élection
        {shuffle_round, ShuffledList, RemainingValidators, Initiator} ->
            io:format("[~s] Received shuffle round. Remaining: ~p~n", [NodeName, length(RemainingValidators)]),
            io:format("[~s] Received shuffled list: ~p~n", [NodeName, ShuffledList]),
            %% Log l'événement
            Event1 = lists:flatten(io_lib:format("Received shuffle round from previous validator. Remaining validators: ~p, List: ~p", [length(RemainingValidators), ShuffledList])),
            try part2:log_election_event(NodeName, Event1) catch _:_ -> ok end,

            %% Re-shuffle la liste
            NewShuffledList = shuffle_list(ShuffledList),
            io:format("[~s] After my shuffle: ~p~n", [NodeName, NewShuffledList]),
            Event2 = lists:flatten(io_lib:format("Shuffled the list. New order: ~p", [NewShuffledList])),
            try part2:log_election_event(NodeName, Event2) catch _:_ -> ok end,

            case RemainingValidators of
                %% Plus personne : la liste revient au head initiateur
                [] ->
                    io:format("[~s] Shuffle complete, sending back to initiator: ~s~n", [NodeName, Initiator]),
                    io:format("[~s] Final shuffled list being sent: ~p~n", [NodeName, NewShuffledList]),
                    Event3 = lists:flatten(io_lib:format("Last validator in shuffle chain - sending result back to initiator ~s. Final list: ~p", [Initiator, NewShuffledList])),
                    try part2:log_election_event(NodeName, Event3) catch _:_ -> ok end,
                    InitiatorAtom = list_to_atom(Initiator),
                    InitiatorAtom ! {election_complete, NewShuffledList},
                    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress);

                %% Il reste des validateurs : envoyer au suivant
                [NextValidator | Rest] ->
                    io:format("[~s] Shuffling and sending to: ~s~n", [NodeName, NextValidator]),
                    io:format("[~s] Sending shuffled list to next validator: ~p~n", [NodeName, NewShuffledList]),
                    Event3 = lists:flatten(io_lib:format("Forwarding shuffle to next validator: ~s. List sent: ~p", [NextValidator, NewShuffledList])),
                    try part2:log_election_event(NodeName, Event3) catch _:_ -> ok end,
                    NextValidatorAtom = list_to_atom(NextValidator),
                    NextValidatorAtom ! {shuffle_round, NewShuffledList, Rest, Initiator},
                    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress)
            end;

        %% Message 3 : Élection terminée (reçu par le head initiateur)
        {election_complete, FinalShuffledList} ->
            io:format("[~s] Election complete! Final shuffled list received~n", [NodeName]),
            Event1 = "Received final shuffled list from last validator",
            try part2:log_election_event(NodeName, Event1) catch _:_ -> ok end,

            %% Sélectionne les 10% premiers validateurs
            NumValidators = length(FinalShuffledList),
            NumProposers = max(1, NumValidators div 10),  % Minimum 1 validateur
            NewProposerGroup = lists:sublist(FinalShuffledList, NumProposers),

            io:format("[~s] New proposer group elected (~p members): ~p~n",
                      [NodeName, NumProposers, NewProposerGroup]),
            Event2 = lists:flatten(io_lib:format("Selected ProposerGroup (~p members): ~p", [NumProposers, NewProposerGroup])),
            try part2:log_election_event(NodeName, Event2) catch _:_ -> ok end,

            %% Broadcast le nouveau proposer group à TOUS les validateurs
            Event3 = "Broadcasting new ProposerGroup to all validators",
            try part2:log_election_event(NodeName, Event3) catch _:_ -> ok end,
            lists:foreach(fun(ValidatorName) ->
                ValidatorAtom = list_to_atom(ValidatorName),
                ValidatorAtom ! {new_proposer_group, NewProposerGroup}
            end, AllValidators),

            %% Met à jour son propre proposer group
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, NewProposerGroup, ElectionInProgress);

        %% Message 4 : Nouveau proposer group annoncé
        {new_proposer_group, NewProposerGroup} ->
            io:format("[~s] Received new proposer group: ~p~n", [NodeName, NewProposerGroup]),
            Event1 = lists:flatten(io_lib:format("Received new ProposerGroup: ~p", [NewProposerGroup])),
            try part2:log_election_event(NodeName, Event1) catch _:_ -> ok end,

            %% Vérifie si ce validateur est le nouveau head (premier de la liste)
            IsNewHead = case NewProposerGroup of
                [NodeName | _] ->
                    io:format("[~s] I am the new HEAD of proposer group!~n", [NodeName]),
                    true;
                _ ->
                    false
            end,

            %% Si c'est le nouveau head, broadcast "start new epoch" à TOUS les nœuds
            case IsNewHead of
                true ->
                    Event2 = "I am the HEAD of the new ProposerGroup - broadcasting start of new epoch",
                    try part2:log_election_event(NodeName, Event2) catch _:_ -> ok end,
                    io:format("[~s] Broadcasting start of new epoch to ALL nodes...~n", [NodeName]),
                    lists:foreach(fun(Node) ->
                        NodeAtom = list_to_atom(Node),
                        NodeAtom ! {start_new_epoch}
                    end, AllNodes);
                false ->
                    Event2 = "Updated local ProposerGroup",
                    try part2:log_election_event(NodeName, Event2) catch _:_ -> ok end,
                    ok
            end,

            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, NewProposerGroup, ElectionInProgress);

        %% Message 5 : Début de nouvelle epoch
        {start_new_epoch} ->
            io:format("[~s] New epoch started! Block creation RESUMED.~n", [NodeName]),
            Event = "New epoch started - election complete, block creation RESUMED",
            try part2:log_election_event(NodeName, Event) catch _:_ -> ok end,
            %% Remet ElectionInProgress à false
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, false);

        %% ===== ProposerGroup: Validation de blocs =====

        %% Reçoit un bloc à valider (uniquement si on fait partie du ProposerGroup)
        {validate_block, Block, BuilderName} ->
            io:format("[~s] Received block #~p from ~s for VALIDATION~n",
                     [NodeName, block:get_number(Block), BuilderName]),

            %% Vérifie si ce validateur fait partie du ProposerGroup
            IsMemberOfProposerGroup = lists:member(NodeName, ProposerGroup),

            case IsMemberOfProposerGroup of
                false ->
                    io:format("[~s] NOT in ProposerGroup - IGNORING validation request~n", [NodeName]),
                    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress);
                true ->
                    %% Valide le bloc
                    IsValid = block:is_valid(Block),
                    BlockHash = block:hash(Block),

                    case IsValid of
                        true ->
                            io:format("[~s] Block #~p is VALID - sending approval to ~s~n",
                                     [NodeName, block:get_number(Block), BuilderName]),

                            %% Envoie l'approbation au builder
                            BuilderAtom = list_to_atom(BuilderName),
                            BuilderAtom ! {block_approved, BlockHash, NodeName},

                            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress);
                        false ->
                            io:format("[~s] Block #~p is INVALID - sending rejection to ~s~n",
                                     [NodeName, block:get_number(Block), BuilderName]),

                            %% Envoie le rejet au builder
                            BuilderAtom = list_to_atom(BuilderName),
                            BuilderAtom ! {block_rejected_by, BlockHash, NodeName},

                            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress)
                    end
            end;

        %% Arrête le nœud
        stop ->
            io:format("[~s] Validator stopped~n", [NodeName]),
            ok;

        %% Message inconnu
        Other ->
            io:format("[~s] Received unknown message: ~p~n", [NodeName, Other]),
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes, StorageFile, ProposerGroup, ElectionInProgress)
    end.
