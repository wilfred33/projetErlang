
-module(builder).
-export([start/2, create_blocks_from_csv/3]).

%% Démarre un builder et crée des blocs depuis un fichier CSV
start(Index, TransactionCSV) ->
    %% Démarre le nœud builder
    {ok, Pid, NodeName} = node:start(builder, Index),

    io:format("[Builder] Builder ~s started~n", [NodeName]),

    {ok, Pid, NodeName}.

%% Crée et envoie des blocs depuis un fichier CSV de transactions
create_blocks_from_csv(BuilderName, TransactionCSVFile, KnownNodes) ->
    io:format("[Builder] ~s reading transactions from ~s~n", [BuilderName, TransactionCSVFile]),

    %% Lit le fichier CSV
    case file:read_file(TransactionCSVFile) of
        {ok, Content} ->
            Lines = string:split(binary_to_list(Content), "\n", all),

            %% Parse les transactions
            Transactions = parse_transactions(Lines),

            io:format("[Builder] Parsed ~p transactions (~p valid)~n",
                      [length(Lines) - 1, length(Transactions)]),

            %% Récupère la blockchain actuelle
            {ok, Blockchain} = node:get_blockchain(BuilderName),

            %% Crée les blocs (10 transactions par bloc)
            %% Démarre à l'index 1 pour les transactions
            create_blocks(BuilderName, Transactions, Blockchain, KnownNodes, 1);

        {error, Reason} ->
            io:format("[Builder] Error reading CSV file: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Parse les transactions depuis les lignes du CSV
parse_transactions(Lines) ->
    %% Ignore la première ligne (header) si elle existe
    DataLines = case Lines of
        [First | Rest] ->
            %% Vérifie si c'est un header
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
           transaction:is_valid(Tx)].

%% Crée des blocs avec les transactions (10 par bloc)
create_blocks(_BuilderName, [], _Blockchain, _KnownNodes, _TxIndex) ->
    io:format("[Builder] All blocks created~n"),
    ok;

create_blocks(BuilderName, Transactions, Blockchain, KnownNodes, TxStartIndex) ->
    %% Prend jusqu'à 10 transactions
    {BlockTransactions, RemainingTransactions} =
        case length(Transactions) >= 10 of
            true ->
                {lists:sublist(Transactions, 10), lists:nthtail(10, Transactions)};
            false ->
                {Transactions, []}
        end,

    %% Calcule le numéro du prochain bloc et le hash précédent
    {BlockNumber, PrevBlockHash} = case Blockchain of
        [] ->
            %% Blockchain vide : premier bloc #0
            {0, crypto:hash(sha256, "initial_block")};
        _ ->
            %% Blockchain existante
            LastBlock = lists:last(Blockchain),
            {block:get_number(LastBlock) + 1, block:hash(LastBlock)}
    end,

    %% Crée le nouveau bloc avec l'index de départ des transactions
    NewBlock = block:new(BlockNumber, BuilderName, PrevBlockHash, BlockTransactions, TxStartIndex),

    io:format("[Builder] Created block #~p with ~p transactions (indices ~p-~p)~n",
              [BlockNumber, length(BlockTransactions), TxStartIndex, TxStartIndex + length(BlockTransactions) - 1]),

    %% Envoie le bloc au builder node pour qu'il le traite
    BuilderAtom = list_to_atom(BuilderName),
    BuilderAtom ! {new_block, NewBlock},

    %% Récupère la blockchain mise à jour
    {ok, UpdatedBlockchain} = node:get_blockchain(BuilderName),

    %% Continue avec les transactions restantes
    case RemainingTransactions of
        [] -> ok;
        _ ->
            %% Calcule le prochain index de départ
            NextTxIndex = TxStartIndex + length(BlockTransactions),
            create_blocks(BuilderName, RemainingTransactions, UpdatedBlockchain, KnownNodes, NextTxIndex)
    end.
