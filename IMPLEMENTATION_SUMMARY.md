# Implémentation de la Création Automatique de Blocs avec Transactions CSV

## Vue d'ensemble

Cette implémentation permet aux builders de créer automatiquement des blocs avec des transactions chargées depuis un fichier CSV, tout en respectant le protocole d'élection (pause pendant l'élection, reprise après).

## Modifications apportées à node.erl

### 1. Ajout du Transaction Pool

Le `node_loop` a été étendu avec un 7ème paramètre `TransactionPool` qui stocke les transactions en attente :

```erlang
node_loop(NodeName, Type, Blockchain, KnownNodes, StorageFile, ElectionInProgress, TransactionPool)
```

### 2. Chargement des transactions depuis CSV

**Fonction API publique** (ligne 307-315) :
```erlang
load_transactions(NodeName, TransactionFile)
```

**Handler de message** (ligne 140-144) :
```erlang
{load_transactions, TransactionFile} ->
    LoadedTransactions = load_transactions_from_csv(TransactionFile),
    ...
```

**Fonction de parsing** (ligne 311-335) - réutilise la logique de builder.erl :
```erlang
load_transactions_from_csv(TransactionFile) ->
    %% Lit le fichier CSV
    %% Parse les transactions avec transaction:from_csv_line/1
    %% Filtre les transactions valides
```

### 3. Consommation des transactions

**Fonction take_transactions/2** (ligne 337-346) :
```erlang
take_transactions(TransactionPool, N) ->
    %% Prend N transactions du pool
    %% Retourne {PoolRestant, TransactionsUtilisées}
```

### 4. Création de blocs avec transactions réelles

**Handler {create_block_tick}** (ligne 167-184) :
```erlang
{create_block_tick} when Type =:= builder ->
    case ElectionInProgress of
        true ->
            %% Pause pendant l'élection
            node_loop(..., ElectionInProgress, TransactionPool);
        false ->
            %% Prend 10 transactions du pool
            {NewTransactionPool, UsedTransactions} = take_transactions(TransactionPool, 10),
            case UsedTransactions of
                [] ->
                    %% Pas de transactions : ne crée pas de bloc
                    node_loop(..., TransactionPool);
                _ ->
                    %% Crée le bloc avec les transactions
                    create_and_broadcast_block(..., UsedTransactions),
                    node_loop(..., NewTransactionPool)
            end
    end;
```

**Fonction create_and_broadcast_block/5** (ligne 370-403) :
```erlang
create_and_broadcast_block(NodeName, Blockchain, _KnownNodes, _StorageFile, Transactions) ->
    %% Calcule le numéro de bloc et hash précédent
    %% Calcule l'index de départ des transactions
    %% Crée le bloc avec block:new/5
    %% Envoie le bloc au builder lui-même via message {new_block, NewBlock}
```

## Flux de fonctionnement

### Initialisation
1. Démarrer un builder : `node:start(builder, Index, KnownNodes)`
2. Charger les transactions : `node:load_transactions("Builder_1", "transactions.csv")`
3. Démarrer la création automatique : `node:start_block_creation("Builder_1")`

### Création de blocs en continu
- Timer envoie `{create_block_tick}` toutes les 0.5 secondes
- Le builder prend 10 transactions du pool
- Si des transactions sont disponibles, crée un bloc et l'envoie
- Le pool de transactions diminue progressivement

### Pause pendant l'élection
1. Un validateur envoie `{start_election}` à tous les nœuds
2. Le builder marque `ElectionInProgress = true`
3. Le handler `{create_block_tick}` ignore la création de blocs
4. Les transactions restent dans le pool

### Reprise après l'élection
1. Le nouveau head envoie `{start_new_epoch}` à tous les nœuds
2. Le builder marque `ElectionInProgress = false`
3. La création de blocs reprend normalement
4. Les transactions du pool continuent d'être consommées

## Format du fichier CSV

Le fichier doit avoir le format suivant :

```csv
emitter,receiver,amount
0xA94F72C7,0xF31A7721,42.18
0xBC21D8FA,0x9EE8930D,5.77
...
```

- Première ligne : header (optionnel, automatiquement détecté et ignoré)
- Lignes suivantes : emitter, receiver, amount séparés par des virgules
- Les transactions invalides sont automatiquement filtrées

## Test

Un fichier de test complet a été créé : [test_csv_transactions.erl](test_csv_transactions.erl)

Pour l'exécuter :
```batch
test_csv.bat
```

Le test :
1. Démarre deux builders
2. Charge transactions.csv dans chaque builder
3. Démarre la création automatique de blocs
4. Attend 5 secondes et vérifie que des blocs ont été créés
5. Affiche les détails des transactions dans les premiers blocs
6. Arrête la création automatique
7. Vérifie que plus aucun bloc n'est créé

## Différences avec builder.erl

L'ancien module `builder.erl` créait tous les blocs d'un coup depuis un CSV via `builder:create_blocks_from_csv/3`.

La nouvelle implémentation dans `node.erl` :
- Charge toutes les transactions en mémoire (transaction pool)
- Crée les blocs progressivement, 10 transactions à la fois
- Respecte le timer de 0.5 secondes entre chaque bloc
- S'arrête pendant les élections
- Permet de recharger des transactions à tout moment

## Fonctions exportées ajoutées

```erlang
-export([start_block_creation/1, stop_block_creation/1, load_transactions/2]).
```

- `start_block_creation/1` : Lance le timer de création automatique
- `stop_block_creation/1` : Arrête le timer
- `load_transactions/2` : Charge les transactions depuis un fichier CSV
