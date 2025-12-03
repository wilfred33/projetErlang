
-module(test_simple).
-export([test_builder_create_block/0]).

%% Test simple: Un builder lit le CSV et crée un bloc
test_builder_create_block() ->
    io:format("~n=== Test: Builder Create Block ===~n~n"),

    %% Étape 1: Démarre un nœud builder
    io:format("Step 1: Starting builder node...~n"),
    {ok, Pid, BuilderName} = node:start(builder, 1),
    io:format("  Builder started: ~s (PID: ~p)~n~n", [BuilderName, Pid]),

    %% Étape 2: Lit les transactions du CSV
    io:format("Step 2: Reading transactions from CSV...~n"),
    TransactionFile = "transactions.csv",

    case file:read_file(TransactionFile) of
        {ok, Content} ->
            Lines = string:split(binary_to_list(Content), "\n", all),
            io:format("  Read ~p lines from ~s~n", [length(Lines), TransactionFile]),

            %% Parse les transactions (ignore le header)
            [_Header | DataLines] = Lines,
            ValidLines = [L || L <- DataLines, L =/= "", L =/= []],

            %% Parse les 10 premières transactions
            io:format("  Parsing first 10 transactions...~n"),
            Transactions = transaction:parse_valid_transactions(lists:sublist(ValidLines, 10)),

            io:format("  Parsed ~p valid transactions~n~n", [length(Transactions)]),

            %% Affiche les transactions
            lists:foreach(fun({I, Tx}) ->
                {transaction, Emitter, Receiver, Amount} = Tx,
                io:format("    Tx ~p: ~s -> ~s : ~p~n", [I, Emitter, Receiver, Amount])
            end, lists:zip(lists:seq(1, length(Transactions)), Transactions)),

            %% Étape 3: Crée un bloc avec ces transactions
            io:format("~nStep 3: Creating block...~n"),

            %% Récupère la blockchain actuelle (devrait contenir seulement genesis)
            {ok, Blockchain} = node:get_blockchain(BuilderName),
            io:format("  Current blockchain length: ~p blocks~n", [length(Blockchain)]),

            %% Dernier bloc
            LastBlock = lists:last(Blockchain),
            BlockNumber = block:get_number(LastBlock) + 1,
            PrevHash = block:hash(LastBlock),

            %% Crée le nouveau bloc
            NewBlock = block:new(BlockNumber, BuilderName, PrevHash, Transactions),

            io:format("  Created block #~p~n", [BlockNumber]),
            io:format("    Builder: ~s~n", [block:get_builder_address(NewBlock)]),
            io:format("    Transactions: ~p~n", [length(block:get_transactions(NewBlock))]),
            io:format("    Merkle Root: ~s~n", [hash_to_hex(block:get_merkle_root(NewBlock))]),
            io:format("    Block Hash: ~s~n", [hash_to_hex(block:hash(NewBlock))]),

            %% Étape 4: Vérifie que le bloc est valide
            io:format("~nStep 4: Validating block...~n"),
            case block:is_valid(NewBlock) of
                true ->
                    io:format("  ✓ Block is valid!~n~n");
                false ->
                    io:format("  ✗ Block is INVALID!~n~n")
            end,

            %% Étape 5: Envoie le bloc au nœud builder
            io:format("Step 5: Sending block to builder node...~n"),
            BuilderAtom = list_to_atom(BuilderName),
            BuilderAtom ! {new_block, NewBlock},

            %% Attend un peu
            timer:sleep(500),

            %% Vérifie la blockchain
            io:format("~nStep 6: Checking blockchain...~n"),
            {ok, UpdatedBlockchain} = node:get_blockchain(BuilderName),
            io:format("  Updated blockchain length: ~p blocks~n", [length(UpdatedBlockchain)]),

            %% Affiche tous les blocs
            io:format("~n  Blocks in chain:~n"),
            lists:foreach(fun(Block) ->
                Num = block:get_number(Block),
                Builder = block:get_builder_address(Block),
                NumTx = length(block:get_transactions(Block)),
                io:format("    Block #~p: Builder=~s, Transactions=~p~n", [Num, Builder, NumTx])
            end, UpdatedBlockchain),

            io:format("~n=== Test Complete ===~n~n"),
            {ok, BuilderName, NewBlock};

        {error, Reason} ->
            io:format("  Error reading file: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Convertit un hash en hexadécimal
hash_to_hex(Hash) when is_binary(Hash) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]).
