
-module(transaction).
-export([new/3, is_valid/1, hash/1, to_string/1, from_csv_line/1, parse_valid_transactions/1]).

%% Structure d'une transaction
%% {transaction, Emitter, Receiver, Amount}

%% Crée une nouvelle transaction
new(Emitter, Receiver, Amount) ->
    {transaction, Emitter, Receiver, Amount}.

%% Vérifie si une transaction est valide
%% Une transaction est valide si tous les champs sont non vides
is_valid({transaction, Emitter, Receiver, Amount}) ->
    is_non_empty(Emitter) andalso
    is_non_empty(Receiver) andalso
    is_non_empty(Amount);
is_valid(_) ->
    false.

%% Vérifie si une valeur est non vide
is_non_empty(Value) when is_list(Value) ->
    Value =/= "" andalso Value =/= [];
is_non_empty(Value) when is_binary(Value) ->
    Value =/= <<>>;
is_non_empty(Value) when is_number(Value) ->
    true;
is_non_empty(_) ->
    false.

%% Hash une transaction avec SHA-256
hash({transaction, Emitter, Receiver, Amount}) ->
    Data = io_lib:format("~p~p~p", [Emitter, Receiver, Amount]),
    crypto:hash(sha256, Data).

%% Convertit une transaction en string pour affichage
to_string({transaction, Emitter, Receiver, Amount}) ->
    io_lib:format("~s,~s,~p", [Emitter, Receiver, Amount]).

%% Parse une ligne CSV en transaction
%% Format: Emitter,Receiver,Amount
from_csv_line(Line) ->
    CleanLine = string:trim(Line),
    Parts = string:split(CleanLine, ",", all),
    case Parts of
        [Emitter, Receiver, AmountStr] ->
            Amount = parse_amount(AmountStr),
            new(string:trim(Emitter), string:trim(Receiver), Amount);
        _ ->
            {error, invalid_format}
    end.

%% Parse le montant (peut être un nombre ou une string)
parse_amount(AmountStr) ->
    CleanAmount = string:trim(AmountStr),
    case string:to_float(CleanAmount) of
        {Float, ""} -> Float;
        _ ->
            case string:to_integer(CleanAmount) of
                {Int, ""} -> Int;
                _ -> CleanAmount
            end
    end.

%% Parse plusieurs lignes CSV et retourne seulement les transactions valides
parse_valid_transactions(Lines) ->
    ParsedTransactions = [from_csv_line(Line) || Line <- Lines],
    [Tx || Tx <- ParsedTransactions,
           Tx =/= {error, invalid_format},
           is_valid(Tx)].
