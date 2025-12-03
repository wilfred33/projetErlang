-module(test_broadcast).
-export([test/0]).

%% Test: Un builder crée un bloc et le broadcast à plusieurs nœuds
test() ->
    io:format("~n=== Test: Broadcast from Builder to Multiple Nodes ===~n~n"),

    %% Nettoie les anciens fichiers
    io:format("Cleaning old blockchain files...~n"),
    os:cmd("rm -f blockchain_*.csv"),

    %% Étape 1: Démarre 1 Builder + 3 NonValidators
    io:format("~nStep 1: Starting nodes...~n"),

    {ok, _Pid1, Builder1} = node:start(builder, 1),
    io:format("  Started ~s~n", [Builder1]),

    {ok, _Pid2, NonVal1} = node:start(non_validator, 1),
    io:format("  Started ~s~n", [NonVal1]),

    {ok, _Pid3, NonVal2} = node:start(non_validator, 2),
    io:format("  Started ~s~n", [NonVal2]),

    {ok, _Pid4, NonVal3} = node:start(non_validator, 3),
    io:format("  Started ~s~n", [NonVal3]),

    AllNodes = [Builder1, NonVal1, NonVal2, NonVal3],
    io:format("~nAll nodes: ~p~n", [AllNodes]),

    %% Étape 2: Configure le réseau (chaque nœud connaît tous les autres)
    io:format("~nStep 2: Configuring network...~n"),
    lists:foreach(fun(NodeName) ->
        OtherNodes = [N || N <- AllNodes, N =/= NodeName],
        NodeAtom = list_to_atom(NodeName),
        lists:foreach(fun(OtherNode) ->
            NodeAtom ! {add_node, OtherNode}
        end, OtherNodes)
    end, AllNodes),
    io:format("  All nodes know each other~n"),

    %% Attend que le réseau soit prêt
    timer:sleep(500),

    %% Étape 3: Le builder crée un bloc depuis le CSV
    io:format("~nStep 3: Builder creating block from CSV...~n"),
    builder:create_blocks_from_csv(Builder1, "transactions.csv", AllNodes),

    %% Attend que le bloc se propage
    timer:sleep(1000),

    %% Étape 4: Vérifie que tous les nœuds ont le même nombre de blocs
    io:format("~nStep 4: Checking blockchain consistency...~n"),

    BlockchainLengths = lists:map(fun(NodeName) ->
        case node:get_blockchain(NodeName) of
            {ok, Blockchain} ->
                Length = length(Blockchain),
                io:format("  ~s: ~p blocks~n", [NodeName, Length]),
                {NodeName, Length, Blockchain};
            {error, Reason} ->
                io:format("  ~s: Error - ~p~n", [NodeName, Reason]),
                {NodeName, 0, []}
        end
    end, AllNodes),

    %% Vérifie la cohérence
    Lengths = [L || {_, L, _} <- BlockchainLengths],
    AllSame = lists:all(fun(L) -> L =:= hd(Lengths) end, Lengths),

    io:format("~n"),
    case AllSame of
        true ->
            io:format("✓ SUCCESS: All nodes have the same blockchain (~p blocks)~n", [hd(Lengths)]),

            %% Affiche les détails de chaque blockchain
            io:format("~nBlockchain details:~n"),
            lists:foreach(fun({NodeName, _, Blockchain}) ->
                io:format("~n~s blockchain:~n", [NodeName]),
                lists:foreach(fun(Block) ->
                    Num = block:get_number(Block),
                    Builder = block:get_builder_address(Block),
                    NumTx = length(block:get_transactions(Block)),
                    io:format("  Block #~p: Builder=~s, Transactions=~p~n",
                              [Num, Builder, NumTx])
                end, Blockchain)
            end, BlockchainLengths);
        false ->
            io:format("✗ FAILURE: Blockchains are inconsistent!~n"),
            lists:foreach(fun({NodeName, Length, _}) ->
                io:format("  ~s: ~p blocks~n", [NodeName, Length])
            end, BlockchainLengths)
    end,

    io:format("~n=== Test Complete ===~n~n"),
    {ok, AllSame}.
