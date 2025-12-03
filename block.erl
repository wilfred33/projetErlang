
-module(block).
-export([new/4, new/5, genesis/0, hash/1, is_valid/1, to_csv_line/1, from_csv_line/1,
         get_number/1, get_prev_hash/1, get_builder_address/1, get_merkle_root/1,
         get_transactions/1, get_tx_start_index/1]).

%% Structure d'un bloc
%% {block, BlockNumber, MerkleRoot, BuilderAddress, PrevBlockHash, Transactions, TxStartIndex}

%% Crée un nouveau bloc (version sans index de départ)
new(BlockNumber, BuilderAddress, PrevBlockHash, Transactions) ->
    new(BlockNumber, BuilderAddress, PrevBlockHash, Transactions, 1).

%% Crée un nouveau bloc avec index de départ des transactions
new(BlockNumber, BuilderAddress, PrevBlockHash, Transactions, TxStartIndex) ->
    %% Limite à 10 transactions maximum
    LimitedTxs = lists:sublist(Transactions, 10),

    %% Calcule l'arbre de Merkle des transactions
    TxHashes = [transaction:hash(Tx) || Tx <- LimitedTxs],
    MerkleRoot = merkle_tree:build(TxHashes),

    {block, BlockNumber, MerkleRoot, BuilderAddress, PrevBlockHash, LimitedTxs, TxStartIndex}.

%% Crée le bloc genesis (premier bloc de la chaîne)
genesis() ->
    %% Pas de transactions, donc Merkle Root = hash de liste vide
    EmptyMerkleRoot = merkle_tree:build([]),
    %% Pas de bloc précédent, on utilise un hash spécial
    GenesisPrevHash = crypto:hash(sha256, "genesis_block"),
    {block, 0, EmptyMerkleRoot, "Genesis", GenesisPrevHash, [], 0}.

%% Hash un bloc (compatible avec les deux structures)
hash({block, BlockNumber, MerkleRoot, BuilderAddress, PrevBlockHash, _Transactions}) ->
    Data = <<BlockNumber:64, MerkleRoot/binary,
             (list_to_binary(BuilderAddress))/binary,
             PrevBlockHash/binary>>,
    crypto:hash(sha256, Data);
hash({block, BlockNumber, MerkleRoot, BuilderAddress, PrevBlockHash, _Transactions, _TxStartIndex}) ->
    Data = <<BlockNumber:64, MerkleRoot/binary,
             (list_to_binary(BuilderAddress))/binary,
             PrevBlockHash/binary>>,
    crypto:hash(sha256, Data).

%% Vérifie si un bloc est valide (compatible avec les deux structures)
is_valid({block, BlockNumber, MerkleRoot, BuilderAddress, PrevBlockHash, Transactions})
  when is_integer(BlockNumber), BlockNumber >= 0,
       is_binary(MerkleRoot),
       is_list(BuilderAddress), BuilderAddress =/= "",
       is_binary(PrevBlockHash),
       is_list(Transactions), length(Transactions) =< 10 ->
    %% Vérifie que toutes les transactions sont valides
    lists:all(fun transaction:is_valid/1, Transactions);
is_valid({block, BlockNumber, MerkleRoot, BuilderAddress, PrevBlockHash, Transactions, TxStartIndex})
  when is_integer(BlockNumber), BlockNumber >= 0,
       is_binary(MerkleRoot),
       is_list(BuilderAddress), BuilderAddress =/= "",
       is_binary(PrevBlockHash),
       is_list(Transactions), length(Transactions) =< 10,
       is_integer(TxStartIndex), TxStartIndex >= 0 ->
    %% Vérifie que toutes les transactions sont valides
    lists:all(fun transaction:is_valid/1, Transactions);
is_valid(_) ->
    false.

%% Convertit un bloc en ligne CSV (version sans index - obsolète)
%% Format: BlockNumber,MerkleRoot,BuilderAddress,PrevBlockHash,TransactionIDs
to_csv_line({block, BlockNumber, MerkleRoot, BuilderAddress, PrevBlockHash, Transactions}) ->
    to_csv_line({block, BlockNumber, MerkleRoot, BuilderAddress, PrevBlockHash, Transactions, 1});

%% Convertit un bloc en ligne CSV avec index de départ des transactions
to_csv_line({block, BlockNumber, MerkleRoot, BuilderAddress, PrevBlockHash, Transactions, TxStartIndex}) ->
    MerkleRootHex = hash_to_hex(MerkleRoot),
    PrevHashHex = hash_to_hex(PrevBlockHash),

    %% Génère les IDs de transaction avec les vrais indices du CSV
    TxCount = length(Transactions),
    TxIDs = case TxCount of
        0 -> "";
        _ -> string:join([integer_to_list(I) || I <- lists:seq(TxStartIndex, TxStartIndex + TxCount - 1)], ";")
    end,

    io_lib:format("~p,~s,~s,~s,~s~n",
                  [BlockNumber, MerkleRootHex, BuilderAddress, PrevHashHex, TxIDs]).

%% Parse une ligne CSV en bloc (version simplifiée pour lecture)
from_csv_line(Line) ->
    CleanLine = string:trim(Line),
    Parts = string:split(CleanLine, ",", all),
    case Parts of
        [BlockNumStr, MerkleRootHex, BuilderAddress, PrevHashHex, TxIDsStr] ->
            {BlockNum, _} = string:to_integer(BlockNumStr),
            MerkleRoot = hex_to_hash(MerkleRootHex),
            PrevHash = hex_to_hash(PrevHashHex),
            TxIDs = string:split(TxIDsStr, ";", all),
            {block, BlockNum, MerkleRoot, BuilderAddress, PrevHash, TxIDs};
        _ ->
            {error, invalid_format}
    end.

%% Getters (compatibles avec les deux structures)
get_number({block, BlockNumber, _, _, _, _}) -> BlockNumber;
get_number({block, BlockNumber, _, _, _, _, _}) -> BlockNumber.

get_prev_hash({block, _, _, _, PrevBlockHash, _}) -> PrevBlockHash;
get_prev_hash({block, _, _, _, PrevBlockHash, _, _}) -> PrevBlockHash.

get_builder_address({block, _, _, BuilderAddress, _, _}) -> BuilderAddress;
get_builder_address({block, _, _, BuilderAddress, _, _, _}) -> BuilderAddress.

get_merkle_root({block, _, MerkleRoot, _, _, _}) -> MerkleRoot;
get_merkle_root({block, _, MerkleRoot, _, _, _, _}) -> MerkleRoot.

get_transactions({block, _, _, _, _, Transactions}) -> Transactions;
get_transactions({block, _, _, _, _, Transactions, _}) -> Transactions.

get_tx_start_index({block, _, _, _, _, _}) -> 1;
get_tx_start_index({block, _, _, _, _, _, TxStartIndex}) -> TxStartIndex.

%% Utilitaires de conversion hash <-> hex
hash_to_hex(Hash) when is_binary(Hash) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]);
hash_to_hex(Hash) when is_list(Hash) ->
    Hash.

hex_to_hash(HexStr) when is_list(HexStr) ->
    try
        list_to_binary([list_to_integer([C1, C2], 16) || {C1, C2} <- pair_chars(HexStr)])
    catch
        _:_ -> list_to_binary(HexStr)
    end;
hex_to_hash(Bin) when is_binary(Bin) ->
    Bin.

pair_chars([]) -> [];
pair_chars([C1, C2 | Rest]) -> [{C1, C2} | pair_chars(Rest)];
pair_chars([_]) -> [].
