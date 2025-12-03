
-module(merkle_tree).
-export([build/1, get_root/1, hash_data/1]).

%% Construit un arbre de Merkle à partir d'une liste de données
%% Retourne la racine de l'arbre (le hash final)
build([]) ->
    crypto:hash(sha256, "empty");
build(DataList) ->
    %% Hash chaque élément de données
    Hashes = [hash_data(Data) || Data <- DataList],
    %% Construit l'arbre récursivement
    build_tree(Hashes).

%% Construit récursivement l'arbre de Merkle
build_tree([SingleHash]) ->
    SingleHash;
build_tree(Hashes) ->
    %% Groupe les hash par paires et hash chaque paire
    PairedHashes = pair_and_hash(Hashes),
    build_tree(PairedHashes).

%% Groupe les hash par paires et calcule le hash de chaque paire
pair_and_hash([]) ->
    [];
pair_and_hash([H]) ->
    %% Si nombre impair, duplique le dernier hash
    [hash_pair(H, H)];
pair_and_hash([H1, H2 | Rest]) ->
    [hash_pair(H1, H2) | pair_and_hash(Rest)].

%% Hash une paire de hash
hash_pair(Hash1, Hash2) ->
    Combined = <<Hash1/binary, Hash2/binary>>,
    crypto:hash(sha256, Combined).

%% Hash une donnée
hash_data(Data) when is_binary(Data) ->
    crypto:hash(sha256, Data);
hash_data(Data) when is_list(Data) ->
    crypto:hash(sha256, Data);
hash_data(Data) ->
    crypto:hash(sha256, io_lib:format("~p", [Data])).

%% Retourne la racine de l'arbre (alias pour build)
get_root(DataList) ->
    build(DataList).

%% Convertit un hash binaire en string hexadécimal
hash_to_hex(Hash) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]).
