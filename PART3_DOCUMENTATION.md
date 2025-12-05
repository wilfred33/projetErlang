# Part 3 : Proof of Stake avec Élections Automatiques

## Vue d'ensemble

Part 3 implémente un système blockchain Proof of Stake **entièrement automatisé** avec :
- Élections automatiques tous les 10 blocs (3 époques)
- Changement dynamique du rôle de builder
- Pool de transactions centralisé
- Contrôleur de blocs centralisé pour éviter les forks

## Architecture du Système

```
┌─────────────────────────────────────────────────────────────┐
│                    CENTRALIZED CONTROLLER                    │
│                                                              │
│  ┌────────────────┐         ┌─────────────────────┐        │
│  │ Transaction    │────────▶│ Block Controller    │        │
│  │ Pool           │         │ (block_controller)  │        │
│  │ (294 txs)      │         │                     │        │
│  └────────────────┘         └──────────┬──────────┘        │
│                                        │                    │
└────────────────────────────────────────┼────────────────────┘
                                         │
                    {create_block_with_transactions, Txs}
                                         │
                                         ▼
        ┌────────────────────────────────────────────────┐
        │           CURRENT BUILDER                      │
        │  (Validator_1, puis Validator_2, puis ...)     │
        └────────────────────────────────────────────────┘
                                         │
                    Crée bloc + Envoie au ProposerGroup
                                         │
                                         ▼
        ┌────────────────────────────────────────────────┐
        │              PROPOSER GROUP                     │
        │         (10% des validators)                    │
        │         Vote pour valider le bloc               │
        └────────────────────────────────────────────────┘
                                         │
                    Majorité atteinte → Broadcast
                                         │
                                         ▼
        ┌────────────────────────────────────────────────┐
        │        TOUS LES NŒUDS DU RÉSEAU                │
        │  (3 Validators + 7 NonValidators)              │
        │         Ajoutent le bloc validé                │
        └────────────────────────────────────────────────┘
```

## Composants Principaux

### 1. Transaction Pool Centralisé

**Localisation :** `part3.erl:173-199`

```erlang
start_transaction_pool(TransactionFile) ->
    Transactions = load_transactions_from_csv(TransactionFile),
    spawn(fun() -> transaction_pool_loop(Transactions) end).
```

**Fonctionnement :**
1. Charge 294 transactions depuis `trasactions2.csv`
2. Distribue 10 transactions à la fois au contrôleur
3. Processus enregistré globalement : `transaction_pool`

**Messages supportés :**
- `{take, N, From}` : Donne N transactions au demandeur
- `{get_size, From}` : Retourne le nombre de transactions restantes
- `stop` : Arrête le pool

**Exemple d'interaction :**
```erlang
transaction_pool ! {take, 10, self()},
receive
    {transactions, Transactions} ->
        %% Utilise les 10 transactions
end.
```

### 2. Block Controller Centralisé (CŒUR DU SYSTÈME)

**Localisation :** `part3.erl:88-166`

C'est le **cerveau** de Part 3. Il contrôle :
- Quel validator crée les blocs
- Quand déclencher les élections
- Le changement de builder après élection

#### 2.1 Signature de la fonction

```erlang
block_controller(CurrentBuilder, AllValidators, AllNodes, BlockCount)
```

**Paramètres :**
- `CurrentBuilder` : Nom du builder actuel (ex: `"Validator_2"`)
- `AllValidators` : Liste de tous les validators (ex: `["Validator_1", "Validator_2", "Validator_3"]`)
- `AllNodes` : Liste de TOUS les nœuds (validators + non-validators)
- `BlockCount` : Nombre de blocs créés depuis la dernière élection

#### 2.2 Logique de Décision : Élection ou Création de Bloc ?

```erlang
block_controller(CurrentBuilder, AllValidators, AllNodes, BlockCount) ->
    case BlockCount rem 10 of
        0 when BlockCount > 0 ->
            %% ÉLECTION!
            conduct_election(...);
        _ ->
            %% CRÉATION DE BLOC
            create_next_block(...)
    end.
```

**Décision :**
- Si `BlockCount rem 10 = 0` ET `BlockCount > 0` → **ÉLECTION**
- Sinon → **CRÉATION DE BLOC**

**Exemples :**
| BlockCount | Condition | Action |
|------------|-----------|--------|
| 0 | `0 rem 10 = 0` mais `BlockCount = 0` | Création de bloc #0 |
| 5 | `5 rem 10 = 5` | Création de bloc #5 |
| 10 | `10 rem 10 = 0` et `BlockCount > 0` | **ÉLECTION** |
| 15 | `15 rem 10 = 5` | Création de bloc #15 |
| 20 | `20 rem 10 = 0` et `BlockCount > 0` | **ÉLECTION** |

#### 2.3 Branche ÉLECTION (tous les 10 blocs)

**Localisation :** `part3.erl:90-136`

```erlang
case BlockCount rem 10 of
    0 when BlockCount > 0 ->
        %% Affiche un message d'élection
        io:format("~n~n╔════════════════════════════════════════╗~n"),
        io:format("║   EPOCH ~p COMPLETE!                   ║~n", [BlockCount div 10]),
        io:format("║ Blocks created: ~p                     ║~n", [BlockCount]),
        io:format("║ Current Builder: ~s              ║~n", [CurrentBuilder]),
        io:format("╠════════════════════════════════════════╣~n"),
        io:format("║ Starting ELECTION...                   ║~n"),
        io:format("╚════════════════════════════════════════╝~n~n"),

        %% 1. Lance l'élection via l'algorithme Whisk
        node:start_election(CurrentBuilder, AllValidators, AllNodes),

        %% 2. Attend 7 secondes pour que l'élection se termine
        timer:sleep(7000),

        %% 3. Récupère le nouveau ProposerGroup
        case node:get_proposer_group(lists:nth(1, AllValidators)) of
            {ok, NewProposerGroup} ->
                %% 4. Le head du ProposerGroup devient le nouveau builder
                [NewBuilder | _] = NewProposerGroup,

                %% 5. Affiche les résultats
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

                %% 6. Affiche si le builder a changé
                case NewBuilder =/= CurrentBuilder of
                    true ->
                        io:format("[BlockController] Builder changed: ~s → ~s~n~n",
                                 [CurrentBuilder, NewBuilder]);
                    false ->
                        io:format("[BlockController] Builder remains: ~s~n~n", [NewBuilder])
                end,

                %% 7. IMPORTANT : Reset BlockCount à 0 et continue avec le nouveau builder
                block_controller(NewBuilder, AllValidators, AllNodes, 0);
            _ ->
                %% Erreur : continue avec l'ancien builder
                block_controller(CurrentBuilder, AllValidators, AllNodes, 0)
        end;
```

**Étapes détaillées :**

1. **Affichage de l'epoch complété**
   - Affiche que l'epoch est terminé
   - Montre le nombre de blocs créés
   - Indique le builder actuel

2. **Lancement de l'élection**
   ```erlang
   node:start_election(CurrentBuilder, AllValidators, AllNodes)
   ```
   - Appelle l'algorithme Whisk dans `node.erl`
   - Le builder actuel initie l'élection
   - Tous les validators participent au shuffle

3. **Attente de la fin de l'élection**
   ```erlang
   timer:sleep(7000)
   ```
   - Attend 7 secondes pour que l'algorithme Whisk se termine
   - Pendant ce temps :
     - Messages `{shuffle_round, ...}` circulent entre validators
     - Chaque validator re-shuffle la liste
     - Le dernier validator sélectionne le nouveau ProposerGroup

4. **Récupération du nouveau ProposerGroup**
   ```erlang
   node:get_proposer_group(lists:nth(1, AllValidators))
   ```
   - Demande au premier validator le nouveau ProposerGroup
   - Reçoit la liste des validators élus (10% des validators)

5. **Sélection du nouveau builder**
   ```erlang
   [NewBuilder | _] = NewProposerGroup
   ```
   - Le **head** (premier élément) du ProposerGroup devient le builder
   - Exemple : Si `NewProposerGroup = ["Validator_3", "Validator_1"]`
     → `NewBuilder = "Validator_3"`

6. **Affichage des résultats**
   - Montre le nouveau ProposerGroup
   - Indique le nouveau builder
   - Affiche si le builder a changé ou non

7. **Redémarrage du cycle avec reset**
   ```erlang
   block_controller(NewBuilder, AllValidators, AllNodes, 0)
   ```
   - **CRITIQUE** : `BlockCount` est remis à **0**
   - Le nouveau builder prend le relais
   - L'ancien builder ne reçoit plus de messages → **s'arrête automatiquement**

**Pourquoi l'ancien builder s'arrête ?**

L'ancien builder s'arrête **passivement** :
- Il ne reçoit plus de messages `{create_block_with_transactions, ...}` du contrôleur
- Il reste en attente dans sa boucle `validator_loop`
- Il continue seulement à recevoir et valider les nouveaux blocs du réseau
- Il ne crée plus de blocs

C'est la **clé de l'absence de forks** : un seul builder actif à la fois, contrôlé centralement.

#### 2.4 Branche CRÉATION DE BLOC (entre les élections)

**Localisation :** `part3.erl:137-166`

```erlang
_ ->
    %% Pas d'élection, créer le prochain bloc

    %% 1. Attente pour ralentir la création
    timer:sleep(1000),

    %% 2. Demande 10 transactions au pool
    transaction_pool ! {take, 10, self()},

    receive
        {transactions, Transactions} ->
            case Transactions of
                [] ->
                    %% Plus de transactions : arrêt du système
                    io:format("[BlockController] No more transactions, stopping system~n"),
                    ok;
                _ ->
                    %% 3. Envoie les transactions au builder actuel
                    BuilderAtom = list_to_atom(CurrentBuilder),
                    BuilderAtom ! {create_block_with_transactions, Transactions},

                    %% 4. Attend que le bloc soit propagé (2 secondes pour validation)
                    timer:sleep(2000),

                    %% 5. Continue avec le prochain bloc
                    io:format("[BlockController] Block request sent to ~s~n", [CurrentBuilder]),

                    %% 6. IMPORTANT : Incrémente BlockCount de 1
                    block_controller(CurrentBuilder, AllValidators, AllNodes, BlockCount + 1)
            end
    after 5000 ->
        %% Timeout si le pool ne répond pas
        io:format("[BlockController] Timeout getting transactions~n"),
        block_controller(CurrentBuilder, AllValidators, AllNodes, BlockCount)
    end
```

**Étapes détaillées :**

1. **Délai de 1 seconde**
   ```erlang
   timer:sleep(1000)
   ```
   - Ralentit la création pour éviter de surcharger le système
   - Laisse le temps aux blocs de se propager

2. **Récupération de 10 transactions**
   ```erlang
   transaction_pool ! {take, 10, self()}
   ```
   - Message asynchrone au pool centralisé
   - Demande 10 transactions pour le prochain bloc

3. **Traitement de la réponse**

   **Cas A : Plus de transactions (liste vide)**
   ```erlang
   [] ->
       io:format("[BlockController] No more transactions, stopping system~n"),
       ok;
   ```
   - Arrête le système proprement
   - Retourne `ok` (fin de la fonction récursive)

   **Cas B : Transactions reçues**
   ```erlang
   _ ->
       BuilderAtom = list_to_atom(CurrentBuilder),
       BuilderAtom ! {create_block_with_transactions, Transactions},
   ```
   - Convertit le nom du builder en atom (ex: `"Validator_1"` → `'Validator_1'`)
   - Envoie le message au processus du builder

4. **Message envoyé au builder**
   ```erlang
   {create_block_with_transactions, Transactions}
   ```

   Ce message est **reçu par le validator** dans `node.erl:635` :
   ```erlang
   {create_block_with_transactions, Transactions} ->
       create_and_broadcast_block(NodeName, Blockchain, ProposerGroup, StorageFile, Transactions)
   ```

   Le validator :
   - Crée le bloc avec `block:new(...)`
   - Calcule le hash du bloc précédent pour `PrevHash`
   - Envoie le bloc au ProposerGroup pour validation
   - Attend l'approbation majoritaire
   - Broadcast le bloc à tous les nœuds

5. **Attente de la propagation**
   ```erlang
   timer:sleep(2000)
   ```
   - Attend 2 secondes pour que :
     - Le builder crée le bloc
     - Le ProposerGroup valide le bloc
     - Le bloc soit broadcasté au réseau
     - Tous les nœuds ajoutent le bloc à leur blockchain

6. **Incrémentation et récursion**
   ```erlang
   block_controller(CurrentBuilder, AllValidators, AllNodes, BlockCount + 1)
   ```
   - **`BlockCount + 1`** : Incrémente le compteur de blocs
   - Rappel récursif de la fonction
   - Le cycle recommence :
     - Si `BlockCount + 1 = 10` → Élection
     - Sinon → Création du bloc suivant

**Timeout après 5 secondes**
```erlang
after 5000 ->
    io:format("[BlockController] Timeout getting transactions~n"),
    block_controller(CurrentBuilder, AllValidators, AllNodes, BlockCount)
```
- Si le pool ne répond pas en 5 secondes
- Continue avec le même `BlockCount` (ne perd pas le compte)

### 3. Handlers dans node.erl

#### 3.1 Handler de création de bloc

**Localisation :** `node.erl:635-647`

```erlang
{create_block_with_transactions, Transactions} ->
    io:format("[~s] Creating block with ~p transactions from controller~n",
             [NodeName, length(Transactions)]),

    case ElectionInProgress of
        true ->
            io:format("[~s] Skipping block creation (election in progress)~n", [NodeName]),
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes,
                          StorageFile, ProposerGroup, ElectionInProgress);
        false ->
            %% Crée et broadcast le bloc
            create_and_broadcast_block(NodeName, Blockchain, ProposerGroup, StorageFile, Transactions),
            validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes,
                          StorageFile, ProposerGroup, ElectionInProgress)
    end;
```

**Fonctionnement :**

1. **Vérification de l'état d'élection**
   - Si `ElectionInProgress = true` → **Rejette** la création de bloc
   - Ceci garantit qu'aucun bloc n'est créé pendant une élection

2. **Appel de create_and_broadcast_block**

   Cette fonction (`node.erl:442-491`) :

   ```erlang
   create_and_broadcast_block(NodeName, Blockchain, ProposerGroup, _StorageFile, Transactions) ->
       %% Calcule le numéro du prochain bloc
       BlockNumber = case Blockchain of
           [] -> 0;
           _ -> block:get_number(lists:last(Blockchain)) + 1
       end,

       %% Calcule le PrevHash
       PrevHash = case BlockNumber of
           0 -> <<0:256>>;  %% Bloc genesis : hash nul
           _ -> block:hash(lists:last(Blockchain))  %% Hash du bloc précédent
       end,

       %% Crée le nouveau bloc
       NewBlock = block:new(BlockNumber, NodeName, PrevHash, Transactions, StartTxIndex),

       %% Envoie au ProposerGroup pour validation
       lists:foreach(fun(ValidatorName) ->
           ValidatorAtom = list_to_atom(ValidatorName),
           ValidatorAtom ! {validate_block, NewBlock, NodeName}
       end, ProposerGroup),

       %% Ajoute le bloc à sa propre blockchain
       BuilderAtom ! {new_block, NewBlock},
       ok.
   ```

   **Étapes :**
   1. Calcule `BlockNumber` (numéro séquentiel)
   2. Récupère `PrevHash` du dernier bloc (ou hash nul si genesis)
   3. Crée le nouveau bloc avec `block:new(...)`
   4. Envoie `{validate_block, NewBlock, NodeName}` à chaque membre du ProposerGroup
   5. S'envoie `{new_block, NewBlock}` pour l'ajouter à sa blockchain

3. **Validation par le ProposerGroup**

   Chaque validator du ProposerGroup reçoit :
   ```erlang
   {validate_block, Block, BuilderName} ->
       %% Vérifie si le bloc est valide
       case block:is_valid(Block) of
           true ->
               BuilderAtom ! {block_approved, BlockHash, NodeName};
           false ->
               BuilderAtom ! {block_rejected_by, BlockHash, NodeName}
       end
   ```

4. **Majorité atteinte (dans node_loop pour les builders)**

   Quand le builder reçoit assez d'approbations :
   ```erlang
   {block_approved, BlockHash, ValidatorName} ->
       %% Compte les approbations
       NewApprovals = [ValidatorName | CurrentApprovals],
       MajorityThreshold = (length(ProposerGroup) div 2) + 1,

       case length(NewApprovals) >= MajorityThreshold of
           true ->
               %% Broadcast à tous les nœuds
               broadcast_block(LastBlock, KnownNodes, NodeName);
           false ->
               %% Attend plus d'approbations
               ok
       end
   ```

#### 3.2 Handler d'approbation (Part 3)

**Localisation :** `node.erl:650-653`

```erlang
{block_approved, BlockHash, ValidatorName} ->
    io:format("[~s] Block approved by ~s (hash: ~p) - ignoring in Part 3~n",
             [NodeName, ValidatorName, BlockHash]),
    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes,
                  StorageFile, ProposerGroup, ElectionInProgress);
```

**Pourquoi ignorer ?**

Dans Part 3, les validators n'ont pas de `PendingValidations` map (contrairement aux builders de Part 2). L'approbation est gérée différemment :
- Le validator s'auto-approuve si le ProposerGroup n'a qu'un seul membre
- Les approbations sont simplement affichées et ignorées
- Le système fonctionne car le builder broadcast le bloc après validation

## Flux Complet de Création de 30 Blocs

### Initialisation (part3:start)

```
1. start_transaction_pool("trasactions2.csv")
   └─> Charge 294 transactions
   └─> Enregistre le processus : transaction_pool

2. Créer les listes de nœuds
   ├─> AllValidators = ["Validator_1", "Validator_2", "Validator_3"]
   └─> AllNonValidators = ["NonValidator_1", ..., "NonValidator_7"]

3. Démarrer 3 validators
   ├─> node:start_validator(1, AllValidators, AllNonValidators)
   ├─> node:start_validator(2, AllValidators, AllNonValidators)
   └─> node:start_validator(3, AllValidators, AllNonValidators)

4. Démarrer 7 non-validators
   ├─> node:start(non_validator, 1, AllNodes)
   ├─> ...
   └─> node:start(non_validator, 7, AllNodes)

5. Sélectionner ProposerGroup initial (10% = 1 validator)
   └─> InitialProposerGroup = node:shuffle_list(AllValidators) |> sublist(1)
   └─> Exemple : ["Validator_1"]

6. Configurer le ProposerGroup pour tous les validators
   ├─> node:set_proposer_group("Validator_1", ["Validator_1"])
   ├─> node:set_proposer_group("Validator_2", ["Validator_1"])
   └─> node:set_proposer_group("Validator_3", ["Validator_1"])

7. Déterminer le builder initial
   └─> InitialBuilder = "Validator_1" (head du ProposerGroup)

8. Démarrer le contrôleur
   └─> spawn(fun() -> block_controller("Validator_1", AllValidators, AllNodes, 0) end)
```

### Epoch 1 : Blocs 0-9 (Builder = Validator_1)

```
BlockCount = 0:
  ├─> BlockCount rem 10 = 0 mais BlockCount = 0 → PAS D'ÉLECTION
  ├─> transaction_pool ! {take, 10, self()}
  ├─> Reçoit transactions 1-10
  ├─> Validator_1 ! {create_block_with_transactions, [tx1..tx10]}
  ├─> Validator_1 crée bloc #0 avec PrevHash = "000...000"
  ├─> ProposerGroup valide → Broadcast
  └─> block_controller("Validator_1", ..., 1)

BlockCount = 1:
  ├─> 1 rem 10 = 1 → CRÉATION
  ├─> Reçoit transactions 11-20
  ├─> Validator_1 crée bloc #1
  └─> block_controller("Validator_1", ..., 2)

... (blocs 2-8 similaires)

BlockCount = 9:
  ├─> 9 rem 10 = 9 → CRÉATION
  ├─> Reçoit transactions 91-100
  ├─> Validator_1 crée bloc #9
  └─> block_controller("Validator_1", ..., 10)

BlockCount = 10:
  ╔═══════════════════════════════════════╗
  ║   EPOCH 1 COMPLETE! ÉLECTION!         ║
  ╚═══════════════════════════════════════╝
  ├─> node:start_election("Validator_1", AllValidators, AllNodes)
  │   └─> Algorithme Whisk (7 secondes)
  ├─> node:get_proposer_group("Validator_1")
  │   └─> Retourne ["Validator_2"]
  ├─> NewBuilder = "Validator_2"
  ├─> Affiche : "Builder changed: Validator_1 → Validator_2"
  └─> block_controller("Validator_2", ..., 0)  ← RESET à 0!
```

### Epoch 2 : Blocs 10-19 (Builder = Validator_2)

```
BlockCount = 0:
  ├─> 0 rem 10 = 0 mais BlockCount = 0 → CRÉATION
  ├─> Validator_2 ! {create_block_with_transactions, [tx101..tx110]}
  ├─> Validator_2 crée bloc #10
  └─> block_controller("Validator_2", ..., 1)

... (blocs 11-18 similaires)

BlockCount = 9:
  ├─> Validator_2 crée bloc #19
  └─> block_controller("Validator_2", ..., 10)

BlockCount = 10:
  ╔═══════════════════════════════════════╗
  ║   EPOCH 2 COMPLETE! ÉLECTION!         ║
  ╚═══════════════════════════════════════╝
  ├─> Nouvelle élection
  ├─> NewBuilder = "Validator_3"
  └─> block_controller("Validator_3", ..., 0)
```

### Epoch 3 : Blocs 20-29 (Builder = Validator_3)

```
BlockCount = 0-8:
  └─> Validator_3 crée blocs #20-28

BlockCount = 9:
  ├─> Validator_3 crée bloc #29 avec SEULEMENT 4 transactions
  │   (car il ne reste que 294 - 290 = 4 transactions)
  └─> block_controller("Validator_3", ..., 10)

BlockCount = 10:
  ╔═══════════════════════════════════════╗
  ║   EPOCH 3 COMPLETE! ÉLECTION!         ║
  ╚═══════════════════════════════════════╝
  ├─> Nouvelle élection
  ├─> NewBuilder = "Validator_3" (peut rester le même!)
  └─> block_controller("Validator_3", ..., 0)

BlockCount = 0:
  ├─> transaction_pool ! {take, 10, self()}
  ├─> Reçoit []  (plus de transactions)
  └─> ARRÊT DU SYSTÈME
```

## Garanties du Système

### 1. Pas de Fork Blockchain

**Comment ?**
- **Un seul builder actif** à la fois, contrôlé par le contrôleur centralisé
- L'ancien builder ne reçoit plus de messages après l'élection
- Le nouveau builder prend le relais immédiatement

**Code responsable :**
```erlang
%% Après élection
block_controller(NewBuilder, ..., 0)  %% Seulement NewBuilder reçoit des messages
```

### 2. Synchronisation de Tous les Nœuds

**Comment ?**
- Chaque bloc validé est **broadcasté** à tous les nœuds
- Tous les nœuds vérifient `PrevHash` avant d'ajouter le bloc
- Rejet automatique si le hash ne correspond pas

**Code responsable :**
```erlang
broadcast_block(Block, KnownNodes, SenderName) ->
    lists:foreach(fun(NodeName) ->
        NodeAtom = list_to_atom(NodeName),
        NodeAtom ! {new_block, Block}
    end, KnownNodes).
```

### 3. Élections Automatiques Tous les 10 Blocs

**Comment ?**
- Compteur `BlockCount` remis à 0 après chaque élection
- Condition `BlockCount rem 10 = 0 and BlockCount > 0`
- Incrémentation `BlockCount + 1` après chaque bloc

**Timeline :**
```
Bloc #0  → BlockCount = 1
Bloc #1  → BlockCount = 2
...
Bloc #9  → BlockCount = 10 → ÉLECTION → Reset à 0
Bloc #10 → BlockCount = 1
...
Bloc #19 → BlockCount = 10 → ÉLECTION → Reset à 0
```

### 4. Changement Automatique de Builder

**Comment ?**
- Le head du nouveau ProposerGroup devient automatiquement le builder
- L'ancien builder n'est plus sollicité par le contrôleur
- Pas besoin de "dire" à l'ancien builder de s'arrêter

**Code responsable :**
```erlang
case node:get_proposer_group(Validator) of
    {ok, NewProposerGroup} ->
        [NewBuilder | _] = NewProposerGroup,
        block_controller(NewBuilder, ..., 0)  %% Changement ici!
end
```

## Problèmes Connus et Limitations

### 1. Broadcast en Boucle

**Problème :**
- Chaque nœud qui reçoit un bloc le rebroadcast
- Crée une boucle infinie de messages
- Ralentit le système considérablement

**Exemple de logs :**
```
[Validator_1] Received new block #5
[NonValidator_1] Received new block #5
[NonValidator_2] Received new block #5
... (répété des milliers de fois)
```

**Solution future :**
- Ne pas rebroadcaster un bloc déjà reçu
- Garder un cache des blocs vus récemment

### 2. Délais Fixes

**Problème :**
- `timer:sleep(1000)` et `timer:sleep(2000)` sont fixes
- Si le réseau est lent, les blocs peuvent ne pas se propager à temps
- Si le réseau est rapide, on perd du temps inutilement

**Solution future :**
- Attendre des confirmations explicites
- Délais adaptatifs basés sur la latence du réseau

### 3. Pas de Gestion d'Erreur Robuste

**Problème :**
- Si le builder crashe, le système s'arrête
- Si un validator crashe pendant l'élection, comportement indéfini

**Solution future :**
- Détection de crash avec `monitor/2`
- Relance automatique en cas d'échec

## Commandes Utiles

### Lancer le système

```erlang
%% Compiler
erl -compile node.erl part3.erl

%% Lancer le test (10 nœuds, 3 validators)
erl -noshell -s part3 test

%% Lancer avec configuration personnalisée
erl -noshell -eval "part3:start(15, 5, \"trasactions2.csv\")" -s init stop
```

### Vérifier les résultats

```bash
# Nombre de blocs dans chaque fichier
wc -l blockchain_*.csv

# Voir les builders de chaque époque
head -10 blockchain_Validator_1.csv | cut -d',' -f3  # Epoch 1
sed -n '11,20p' blockchain_Validator_1.csv | cut -d',' -f3  # Epoch 2
tail -10 blockchain_Validator_1.csv | cut -d',' -f3  # Epoch 3

# Vérifier qu'il n'y a pas de fork (tous les fichiers identiques)
md5sum blockchain_*.csv
```

### Nettoyer

```bash
# Supprimer tous les fichiers blockchain
rm blockchain_*.csv

# Supprimer les fichiers compilés
rm *.beam
```

## Comparaison avec Part 2

| Aspect | Part 2 | Part 3 |
|--------|--------|--------|
| **Builder** | Statique (configuré manuellement) | Dynamique (change après élections) |
| **Élections** | Manuelles (via commande) | Automatiques (tous les 10 blocs) |
| **Transactions** | Pool local dans chaque builder | Pool centralisé |
| **Contrôle** | Distribué (chaque builder décide) | Centralisé (block_controller décide) |
| **Forks** | Possibles | Impossibles (un seul builder actif) |
| **Complexité** | Simple | Moyenne |

## Processus d'Élection Détaillé (Algorithme Whisk)

### Vue d'ensemble de l'Élection

L'élection utilise l'**algorithme Whisk** (Secret Leader Election) implémenté dans `node.erl`. Cet algorithme garantit :
- **Anonymat** : Personne ne sait qui sera élu avant la fin
- **Décentralisation** : Tous les validators participent
- **Équité** : Shuffle cryptographique garantit un choix aléatoire

### Déclenchement de l'Élection

**Code dans part3.erl:103**
```erlang
node:start_election(CurrentBuilder, AllValidators, AllNodes)
```

**Paramètres :**
- `CurrentBuilder` : Le builder actuel qui initie l'élection (ex: `"Validator_1"`)
- `AllValidators` : Liste complète de tous les validators
- `AllNodes` : Liste de tous les nœuds (validators + non-validators)

### Étapes de l'Algorithme Whisk

#### Étape 1 : Initialisation par le Builder

**Code dans node.erl:519-547**

```erlang
start_election(HeadValidatorName, AllValidators, AllNodes) ->
    io:format("[Election] Starting election initiated by ~s~n", [HeadValidatorName]),

    %% 1. Avertir TOUS les nœuds que l'élection commence
    lists:foreach(fun(NodeName) ->
        NodeAtom = list_to_atom(NodeName),
        NodeAtom ! {start_election}
    end, AllNodes),

    timer:sleep(500),

    %% 2. Calculer le nombre de validators à élire (10%)
    NumValidators = length(AllValidators),
    ProposerGroupSize = max(1, NumValidators div 10),

    %% 3. Créer la liste initiale (tous les validators)
    InitialList = AllValidators,

    %% 4. Premier shuffle par le head
    FirstShuffledList = shuffle_list(InitialList),

    %% 5. Préparer la liste des validators restants (tous sauf le head)
    RemainingValidators = AllValidators -- [HeadValidatorName],

    %% 6. Envoyer au prochain validator pour continuer le shuffle
    case RemainingValidators of
        [] ->
            %% Un seul validator : sélectionne directement
            SelectedProposerGroup = lists:sublist(FirstShuffledList, ProposerGroupSize),
            broadcast_new_proposer_group(SelectedProposerGroup, AllValidators);
        [NextValidator | _] ->
            %% Envoie au prochain validator
            NextAtom = list_to_atom(NextValidator),
            NextAtom ! {shuffle_round, FirstShuffledList, RemainingValidators, HeadValidatorName, ProposerGroupSize}
    end.
```

**Actions :**
1. **Envoie `{start_election}` à TOUS les nœuds**
   - Chaque nœud met `ElectionInProgress = true`
   - Les builders arrêtent de créer des blocs
   - Les validators attendent la fin de l'élection

2. **Calcule ProposerGroupSize**
   ```erlang
   ProposerGroupSize = max(1, NumValidators div 10)
   ```
   - Exemple : 3 validators → `max(1, 3 div 10) = max(1, 0) = 1`
   - Au moins 1 validator dans le ProposerGroup

3. **Premier shuffle**
   ```erlang
   FirstShuffledList = shuffle_list(AllValidators)
   ```
   - Utilise un générateur aléatoire pour mélanger la liste
   - Exemple : `["Validator_1", "Validator_2", "Validator_3"]`
     → `["Validator_3", "Validator_1", "Validator_2"]`

4. **Envoie au prochain validator**
   - Prépare `RemainingValidators` = Tous les validators sauf le head
   - Envoie `{shuffle_round, ...}` au premier de la liste

#### Étape 2 : Shuffle en Cascade

**Code dans node.erl:658-678**

```erlang
{shuffle_round, ShuffledList, RemainingValidators, Initiator, ProposerGroupSize} ->
    io:format("[~s] Received shuffle round. Remaining: ~p~n",
             [NodeName, length(RemainingValidators)]),

    %% 1. Re-shuffle la liste reçue
    NewShuffledList = shuffle_list(ShuffledList),

    %% 2. Retirer ce validator de la liste des validators restants
    NewRemainingValidators = RemainingValidators -- [NodeName],

    %% 3. Vérifier si on est le dernier
    case NewRemainingValidators of
        [] ->
            %% DERNIER VALIDATOR : Sélectionne le ProposerGroup final
            io:format("[~s] Last validator - selecting final ProposerGroup~n", [NodeName]),

            SelectedProposerGroup = lists:sublist(NewShuffledList, ProposerGroupSize),

            io:format("[~s] Final ProposerGroup selected: ~p~n",
                     [NodeName, SelectedProposerGroup]),

            %% Broadcast le nouveau ProposerGroup à tous les validators
            broadcast_new_proposer_group(SelectedProposerGroup, AllValidators, AllNodes);

        [NextValidator | _] ->
            %% PAS LE DERNIER : Passe au suivant
            io:format("[~s] Forwarding to ~s~n", [NodeName, NextValidator]),

            NextAtom = list_to_atom(NextValidator),
            NextAtom ! {shuffle_round, NewShuffledList, NewRemainingValidators,
                       Initiator, ProposerGroupSize}
    end,

    validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes,
                  StorageFile, ProposerGroup, ElectionInProgress);
```

**Circulation du message :**

```
Validator_1 (head):
  ├─> Shuffle initial : ["V1", "V2", "V3"] → ["V3", "V1", "V2"]
  ├─> RemainingValidators = ["Validator_2", "Validator_3"]
  └─> Envoie à Validator_2

Validator_2:
  ├─> Reçoit ["V3", "V1", "V2"]
  ├─> Re-shuffle : ["V3", "V1", "V2"] → ["V2", "V3", "V1"]
  ├─> RemainingValidators = ["Validator_3"]
  └─> Envoie à Validator_3

Validator_3 (dernier):
  ├─> Reçoit ["V2", "V3", "V1"]
  ├─> Re-shuffle : ["V2", "V3", "V1"] → ["V1", "V2", "V3"]
  ├─> RemainingValidators = []  ← LISTE VIDE!
  ├─> SÉLECTION FINALE : lists:sublist(["V1", "V2", "V3"], 1) = ["V1"]
  └─> Broadcast le ProposerGroup = ["V1"]
```

**Pourquoi plusieurs shuffles ?**
- Chaque validator ajoute son propre aléatoire
- Empêche un validator unique de manipuler le résultat
- Plus sécurisé : même si un validator est malveillant, les autres compensent

#### Étape 3 : Broadcast du Nouveau ProposerGroup

**Code dans node.erl:689-700**

```erlang
broadcast_new_proposer_group(ProposerGroup, AllValidators, AllNodes) ->
    io:format("[Election] Broadcasting new ProposerGroup: ~p~n", [ProposerGroup]),

    %% Envoie le nouveau ProposerGroup à tous les validators
    lists:foreach(fun(ValidatorName) ->
        ValidatorAtom = list_to_atom(ValidatorName),
        ValidatorAtom ! {new_proposer_group, ProposerGroup}
    end, AllValidators),

    timer:sleep(500),

    %% Envoie la fin d'élection à tous les nœuds
    lists:foreach(fun(NodeName) ->
        NodeAtom = list_to_atom(NodeName),
        NodeAtom ! {election_complete}
    end, AllNodes).
```

**Actions :**

1. **Envoie `{new_proposer_group, ProposerGroup}` à tous les validators**
   ```erlang
   Validator_1 ! {new_proposer_group, ["Validator_2"]}
   Validator_2 ! {new_proposer_group, ["Validator_2"]}
   Validator_3 ! {new_proposer_group, ["Validator_2"]}
   ```

2. **Chaque validator met à jour son ProposerGroup**

   **Code dans node.erl:702-712**
   ```erlang
   {new_proposer_group, NewProposerGroup} ->
       io:format("[~s] Received new ProposerGroup from election: ~p~n",
                [NodeName, NewProposerGroup]),

       NewProposerGroupSorted = lists:sort(NewProposerGroup),

       io:format("[~s] Updated ProposerGroup to: ~p~n",
                [NodeName, NewProposerGroupSorted]),

       validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes,
                     StorageFile, NewProposerGroupSorted, ElectionInProgress);
   ```

3. **Envoie `{election_complete}` à tous les nœuds**
   ```erlang
   AllNodes ! {election_complete}
   ```

4. **Chaque nœud remet ElectionInProgress = false**

   **Code dans node.erl:728-733**
   ```erlang
   {election_complete} ->
       io:format("[~s] Election complete - resuming normal operations~n", [NodeName]),

       validator_loop(NodeName, Blockchain, AllValidators, AllNodes, KnownNodes,
                     StorageFile, ProposerGroup, false);  %% ElectionInProgress = false
   ```

#### Étape 4 : Récupération du ProposerGroup par le Contrôleur

**Code dans part3.erl:107-109**

```erlang
case node:get_proposer_group(lists:nth(1, AllValidators)) of
    {ok, NewProposerGroup} ->
        %% Traite le nouveau ProposerGroup
end
```

**Appel de get_proposer_group dans node.erl:505-513**
```erlang
get_proposer_group(ValidatorName) ->
    ValidatorAtom = list_to_atom(ValidatorName),
    ValidatorAtom ! {get_proposer_group, self()},
    receive
        {proposer_group, ProposerGroup} -> {ok, ProposerGroup}
    after 5000 ->
        {error, timeout}
    end.
```

**Handler dans le validator (node.erl:642-644)**
```erlang
{get_proposer_group, From} ->
    From ! {proposer_group, ProposerGroup},
    validator_loop(...);
```

**Résultat :**
- Le contrôleur récupère `["Validator_2"]` (par exemple)
- Continue avec le changement de builder

### Changement de Builder Détaillé

#### Étape 1 : Extraction du Nouveau Builder

**Code dans part3.erl:109**

```erlang
[NewBuilder | _] = NewProposerGroup
```

**Exemple :**
```erlang
NewProposerGroup = ["Validator_2"]
[NewBuilder | _] = ["Validator_2"]
NewBuilder = "Validator_2"
```

**Pourquoi le head ?**
- Convention : Le premier validator du ProposerGroup est le builder
- Simple et déterministe
- Tous les nœuds appliquent la même règle

#### Étape 2 : Affichage du Changement

**Code dans part3.erl:111-129**

```erlang
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

case NewBuilder =/= CurrentBuilder of
    true ->
        io:format("[BlockController] Builder changed: ~s → ~s~n~n",
                 [CurrentBuilder, NewBuilder]);
    false ->
        io:format("[BlockController] Builder remains: ~s~n~n", [NewBuilder])
end
```

**Output exemple :**
```
╔════════════════════════════════════════╗
║   ELECTION COMPLETE!                   ║
╠════════════════════════════════════════╣
║ New ProposerGroup:                     ║
║   - Validator_2                        ║
╠════════════════════════════════════════╣
║ New Builder: Validator_2               ║
╚════════════════════════════════════════╝

[BlockController] Builder changed: Validator_1 → Validator_2
```

#### Étape 3 : Appel Récursif avec le Nouveau Builder

**Code dans part3.erl:132**

```erlang
block_controller(NewBuilder, AllValidators, AllNodes, 0)
```

**CRITIQUEMENT IMPORTANT :**
- `NewBuilder` remplace `CurrentBuilder`
- `BlockCount` est **REMIS À 0**
- L'ancien builder n'est plus référencé

**Avant l'élection :**
```erlang
block_controller("Validator_1", AllValidators, AllNodes, 10)
```

**Après l'élection :**
```erlang
block_controller("Validator_2", AllValidators, AllNodes, 0)
```

#### Étape 4 : Que Devient l'Ancien Builder ?

**Réponse : Il reste en attente passivement**

```
Validator_1 (ancien builder):
  ├─> Processus toujours vivant
  ├─> Dans validator_loop, attend des messages
  ├─> NE REÇOIT PLUS {create_block_with_transactions, ...}
  ├─> Reçoit seulement :
  │   ├─> {new_block, ...} (nouveaux blocs du réseau)
  │   ├─> {validate_block, ...} (si dans le ProposerGroup)
  │   └─> {start_election} (prochaine élection)
  └─> Reste synchronisé avec la blockchain
```

**Pourquoi pas de message "stop building" ?**
- Pas nécessaire : le contrôleur contrôle qui reçoit les transactions
- Plus simple : pas de gestion d'état complexe
- Plus sûr : l'ancien builder ne peut pas continuer par erreur

### Timeline Complète d'une Élection

```
T=0 : Bloc #9 créé par Validator_1
      └─> BlockCount = 10

T=1s : block_controller détecte BlockCount rem 10 = 0
       ├─> Affiche "EPOCH 1 COMPLETE!"
       └─> Appelle node:start_election("Validator_1", ...)

T=1.5s : Validator_1 (head) :
         ├─> Envoie {start_election} à TOUS les nœuds
         │   └─> Tous les nœuds : ElectionInProgress = true
         ├─> Shuffle : ["V1","V2","V3"] → ["V3","V1","V2"]
         └─> Envoie {shuffle_round, ...} à Validator_2

T=2s : Validator_2 :
       ├─> Re-shuffle : ["V3","V1","V2"] → ["V2","V3","V1"]
       └─> Envoie {shuffle_round, ...} à Validator_3

T=2.5s : Validator_3 (dernier) :
         ├─> Re-shuffle : ["V2","V3","V1"] → ["V1","V2","V3"]
         ├─> Sélectionne : lists:sublist(["V1","V2","V3"], 1) = ["V1"]
         └─> Broadcast {new_proposer_group, ["V1"]}

T=3s : Tous les validators :
       ├─> Reçoivent {new_proposer_group, ["V1"]}
       ├─> Mettent à jour leur ProposerGroup local
       └─> ProposerGroup = ["V1"]

T=3.5s : Tous les nœuds :
         ├─> Reçoivent {election_complete}
         └─> ElectionInProgress = false

T=8s : block_controller (après timer:sleep(7000)) :
       ├─> Appelle node:get_proposer_group("Validator_1")
       ├─> Reçoit {ok, ["V1"]}
       ├─> NewBuilder = "V1" (head du ProposerGroup)
       ├─> Affiche "ELECTION COMPLETE!"
       ├─> Affiche "New Builder: V1"
       └─> Appelle block_controller("V1", ..., 0)

T=9s : Validator_1 (nouveau builder) :
       ├─> Reçoit {create_block_with_transactions, [tx101..tx110]}
       └─> Crée bloc #10
```

## Cas Particuliers

### 1. Builder Reste le Même Après Élection

**Exemple :**
```erlang
CurrentBuilder = "Validator_3"
%% Élection
NewProposerGroup = ["Validator_3", "Validator_1"]
NewBuilder = "Validator_3"  %% Identique!
```

**Affichage :**
```
[BlockController] Builder remains: Validator_3
```

**Comportement :**
- Le même builder continue
- BlockCount est **quand même remis à 0**
- Garantit que la prochaine élection sera dans 10 blocs

### 2. ProposerGroup avec Un Seul Membre

**Avec 3 validators et 10% :**
```erlang
NumValidators = 3
ProposerGroupSize = max(1, 3 div 10) = max(1, 0) = 1
```

**Résultat :**
- ProposerGroup = `["Validator_X"]` (un seul membre)
- Ce validator valide ses propres blocs
- Auto-approbation systématique

### 3. Plus de Transactions Disponibles

**Cas : Bloc #29 créé avec seulement 4 transactions**

```erlang
transaction_pool ! {take, 10, self()},
receive
    {transactions, [tx291, tx292, tx293, tx294]} ->  %% Seulement 4!
        %% Crée quand même le bloc avec 4 transactions
        Validator_3 ! {create_block_with_transactions, [tx291..tx294]}
end
```

**Bloc suivant :**
```erlang
transaction_pool ! {take, 10, self()},
receive
    {transactions, []} ->  %% Liste vide!
        io:format("[BlockController] No more transactions, stopping system~n"),
        ok  %% Arrêt du système
end
```

## Conclusion

Le **Block Controller** est le cœur de Part 3. Il :
- Orchestre la création de blocs de manière centralisée
- Déclenche les élections automatiquement tous les 10 blocs
- Change le builder dynamiquement sans intervention manuelle
- Garantit l'absence de forks en contrôlant strictement qui crée les blocs

L'**Algorithme Whisk** garantit :
- Des élections décentralisées et équitables
- L'anonymat du futur builder jusqu'à la fin de l'élection
- La participation de tous les validators au processus

Cette architecture centralisée simplifie le système et garantit la cohérence de la blockchain, au prix d'un point de défaillance unique (le contrôleur lui-même).
