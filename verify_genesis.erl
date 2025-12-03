-module(verify_genesis).
-export([test/0]).

%% Vérifie que le bloc genesis a bien son propre hash comme PrevHash
test() ->
    io:format("~n=== Verification du bloc genesis ===~n~n"),

    %% Lire le premier bloc du fichier
    {ok, Content} = file:read_file("blockchain_Builder_1.csv"),
    [FirstLine | _] = string:split(binary_to_list(Content), "\n", all),

    %% Parser le bloc
    GenesisBlock = block:from_csv_line(FirstLine),

    %% Obtenir le hash du bloc
    BlockHash = block:hash(GenesisBlock),
    BlockHashHex = binary:encode_hex(BlockHash, lowercase),

    %% Obtenir le PrevHash du bloc
    PrevHash = block:get_prev_hash(GenesisBlock),
    PrevHashHex = binary:encode_hex(PrevHash, lowercase),

    io:format("Hash du bloc genesis: ~s~n", [BlockHashHex]),
    io:format("PrevHash du bloc genesis: ~s~n", [PrevHashHex]),

    case BlockHash =:= PrevHash of
        true ->
            io:format("~n✓ SUCCESS: Le bloc genesis utilise bien son propre hash comme PrevHash!~n");
        false ->
            io:format("~n✗ ERREUR: Le PrevHash ne correspond pas au hash du bloc~n")
    end,

    ok.
