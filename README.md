# Projet Blockchain Erlang avec Proof of Stake et Secret Leader Election

Implémentation d'une blockchain en Erlang utilisant l'algorithme Whisk pour l'élection secrète des leaders.

## Structure du Projet

- **transaction.erl** : Gestion des transactions
- **merkle_tree.erl** : Implémentation de l'arbre de Merkle
- **block.erl** : Structure et validation des blocs
- **node.erl** : Logique des nœuds (validators, builders, non-validators)
- **part1.erl** : Création automatique de blocs avec transactions CSV
- **part2.erl** : Test du processus d'élection avec logging
- **part3.erl** : Block controller avec élections automatiques

## Instructions de Test

### Prérequis

Assurez-vous d'avoir Erlang installé sur votre système.

### Partie 1 : Création Automatique de Blocs

**Objectif** : Tester la création automatique de blocs à partir d'un fichier CSV de transactions.

**Étapes** :

1. **Compiler les modules nécessaires** :
```bash
erl -compile transaction.erl
erl -compile merkle_tree.erl
erl -compile block.erl
erl -compile node.erl
erl -compile part1.erl
```

2. **Lancer le test** :
```bash
erl -noshell -s part1 test
```

**OU utiliser le script batch** :
```bash
test_part1.bat
```

**Ce qui se passe** :
- Démarre 5 nœuds non-validators
- Démarre 1 builder
- Charge les transactions depuis `trasactions2.csv`
- Le builder crée automatiquement un bloc toutes les 0.5 secondes avec 10 transactions
- Les blocs sont broadcast à tous les nœuds

**Pour arrêter** : Appuyez sur `Ctrl+C` puis `a` (abort)

---

### Partie 2 : Test du Processus d'Élection

**Objectif** : Observer et mesurer le processus d'élection Whisk avec enregistrement détaillé des opérations.

**Étapes** :

1. **Compiler les modules nécessaires** :
```bash
erl -compile transaction.erl
erl -compile merkle_tree.erl
erl -compile block.erl
erl -compile node.erl
erl -compile part2.erl
```

2. **Lancer le test** :
```bash
erl -noshell -s part2 test
```

**OU utiliser le script batch** :
```bash
test_part2.bat
```

**Ce qui se passe** :
- Démarre 3 validators
- Lance une élection avec l'algorithme Whisk
- Affiche le ProposerGroup actuel avant l'élection
- Affiche les listes shufflées à chaque étape du processus
- Mesure la durée de l'élection
- Génère le ProposerGroup final

**Fichiers générés** :
- `Validator_1_election_log.txt` : Log détaillé du Validator 1
- `Validator_2_election_log.txt` : Log détaillé du Validator 2
- `Validator_3_election_log.txt` : Log détaillé du Validator 3
- `election_results.txt` : Résumé de l'élection

**Informations affichées** :
- ProposerGroup actuel de chaque validator
- Liste shufflée initiale du head validator
- Listes reçues et envoyées par chaque validator
- ProposerGroup final élu
- Durée totale de l'élection

---

### Partie 3 : Block Controller avec Élections Automatiques

**Objectif** : Tester le système complet avec un contrôleur centralisé qui gère les blocs et déclenche les élections automatiquement tous les 10 blocs.

**Étapes** :

1. **Compiler tous les modules** :
```bash
erl -compile transaction.erl
erl -compile merkle_tree.erl
erl -compile block.erl
erl -compile node.erl
erl -compile part3.erl
```

2. **Lancer le test** :
```bash
erl -noshell -s part3 test
```

**Ce qui se passe** :
- Démarre 3 validators
- Démarre 2 nœuds non-validators
- Crée un transaction pool centralisé avec 300 transactions
- Le block controller :
  - Envoie 10 transactions au builder actuel
  - Attend la création du bloc
  - Répète jusqu'à 10 blocs
  - Déclenche automatiquement une élection
  - Change le builder selon le résultat de l'élection
  - Remet le compteur à 0 et continue

**Cycles observables** :
- **Epoch 0** : Blocs 0-9 créés par le builder initial
- **Élection 1** : Nouveau ProposerGroup élu
- **Epoch 1** : Blocs 10-19 créés par le nouveau builder
- **Élection 2** : Nouveau ProposerGroup élu
- Et ainsi de suite jusqu'à épuisement des transactions

**Arrêt automatique** : Le système s'arrête quand il n'y a plus de transactions disponibles.

---

## Scripts de Compilation et Déploiement

### Compilation Simple

Pour compiler tous les modules à la fois :
```bash
erl -compile transaction.erl merkle_tree.erl block.erl node.erl part1.erl part2.erl part3.erl
```

### Script de Compilation et Push

Le fichier `compile_and_push.bat` compile tous les modules et push le code sur GitHub :
```bash
compile_and_push.bat
```

---

## Détails de l'Algorithme Whisk

L'algorithme Whisk (Secret Leader Election) fonctionne en plusieurs étapes :

1. **Initialisation** : Le validator head initie l'élection et broadcast à tous les nœuds
2. **Shuffle en cascade** : Chaque validator shuffle la liste et la passe au suivant
3. **Sélection finale** : Le dernier validator envoie la liste au head initiateur
4. **Broadcast du résultat** : Le head sélectionne le ProposerGroup (10% des validators) et le broadcast

### Exemple avec 3 Validators

```
Validator_1 (head):
  └─> Shuffle initial : ["V1", "V2", "V3"] → ["V3", "V1", "V2"]
  └─> Envoie à Validator_2

Validator_2:
  └─> Reçoit : ["V3", "V1", "V2"]
  └─> Re-shuffle : ["V3", "V1", "V2"] → ["V2", "V3", "V1"]
  └─> Envoie à Validator_3

Validator_3 (dernier):
  └─> Reçoit : ["V2", "V3", "V1"]
  └─> Re-shuffle : ["V2", "V3", "V1"] → ["V1", "V2", "V3"]
  └─> Envoie au head Validator_1

Validator_1:
  └─> Reçoit : ["V1", "V2", "V3"]
  └─> Sélectionne ProposerGroup : ["V1"] (10% de 3 = 1)
  └─> Broadcast à tous les validators
```

---

## Fichiers de Données

- **transactions.csv** : Fichier de transactions de test
- **trasactions2.csv** : Fichier alternatif de transactions (utilisé par défaut dans part1)

---

## Architecture Part 3

### Block Controller
Le Block Controller est un processus centralisé qui :
- Gère le transaction pool
- Contrôle quel validator crée les blocs
- Déclenche les élections tous les 10 blocs
- Change automatiquement le builder selon le résultat des élections

### Transaction Pool
Pool centralisé de transactions géré comme un processus Erlang.

### Différences Part2 vs Part3

| Aspect | Part 2 | Part 3 |
|--------|--------|--------|
| **Builder** | Statique | Dynamique (change après élections) |
| **Élections** | Manuelle (une seule fois) | Automatiques (tous les 10 blocs) |
| **Transactions** | Aucune | Pool centralisé de 300 transactions |
| **Contrôle** | Test simple | Block controller centralisé |
| **Objectif** | Observer l'élection | Système blockchain complet |

---

## Nettoyage des Fichiers Générés

Les fichiers `.beam` (compilés), `.csv` (blockchain), et logs d'élection peuvent être nettoyés avec :

```bash
rm *.beam blockchain_*.csv *_election_log.txt election_results.txt
```

Sous Windows :
```cmd
del /Q *.beam blockchain_*.csv *_election_log.txt election_results.txt
```

---

## Auteurs

Projet de blockchain Erlang avec système d'élection secrète (Whisk algorithm).
