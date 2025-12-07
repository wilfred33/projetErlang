# Erlang Blockchain with Proof of Stake and Secret Leader Election

Implementation of a blockchain in Erlang using the Whisk algorithm for secret leader election.

## Project Structure

- **transaction.erl**: Transaction management
- **merkle_tree.erl**: Merkle tree implementation
- **block.erl**: Block structure and validation
- **node.erl**: Node logic (validators, builders, non-validators)
- **part1.erl**: Automatic block creation with CSV transactions
- **part2.erl**: Election process testing with logging
- **part3.erl**: Block controller with automatic elections

## Test Instructions

### Prerequisites

Make sure you have Erlang installed on your system.

### Part 1: Automatic Block Creation

**Objective**: Test automatic block creation from a CSV transaction file.

**Steps**:

1. **Compile required modules**:
```bash
erl -compile transaction.erl
erl -compile merkle_tree.erl
erl -compile block.erl
erl -compile node.erl
erl -compile part1.erl
```

2. **Run the test**:
```bash
erl -noshell -s part1 test
```

**OR use the batch script**:
```bash
test_part1.bat
```

**What happens**:
- Starts 5 non-validator nodes
- Starts 1 builder
- Loads transactions from `trasactions2.csv`
- Builder automatically creates a block every 0.5 seconds with 10 transactions
- Blocks are broadcast to all nodes

**To stop**: Press `Ctrl+C` then `a` (abort)

#### **Testing with custom parameters**

To test Part1 with a different number of nodes or CSV file:

```bash
erl -noshell -eval "part1:start(10, \"transactions.csv\"), init:stop()."
```

Examples:
- `part1:start(3, "trasactions2.csv")`: 3 nodes, trasactions2.csv file
- `part1:start(10, "transactions.csv")`: 10 nodes, transactions.csv file
- `part1:start(7, "my_transactions.csv")`: 7 nodes, custom file

---

### Part 2: Election Process Testing

**Objective**: Observe and measure the Whisk election process with detailed operation logging.

**Steps**:

1. **Compile required modules**:
```bash
erl -compile transaction.erl
erl -compile merkle_tree.erl
erl -compile block.erl
erl -compile node.erl
erl -compile part2.erl
```

2. **Run the test**:
```bash
erl -noshell -s part2 test
```

**OR use the batch script**:
```bash
test_part2.bat
```

**What happens**:
- Starts 3 validators
- Launches an election with the Whisk algorithm
- Displays current ProposerGroup before election
- Displays shuffled lists at each step of the process
- Measures election duration
- Generates final ProposerGroup

**Generated files**:
- `Validator_1_election_log.txt`: Detailed log for Validator 1
- `Validator_2_election_log.txt`: Detailed log for Validator 2
- `Validator_3_election_log.txt`: Detailed log for Validator 3
- `election_results.txt`: Election summary

**Information displayed**:
- Current ProposerGroup of each validator
- Initial shuffled list from head validator
- Lists received and sent by each validator
- Final elected ProposerGroup
- Total election duration

#### **Testing with custom parameters**

To test Part2 with a different number of validators:

```bash
erl -noshell -eval "part2:start(5), init:stop()."
```

Examples:
- `part2:start(3)`: 3 validators (default test)
- `part2:start(5)`: 5 validators
- `part2:start(10)`: 10 validators
- `part2:start(20)`: 20 validators

**Note**: More validators means longer election time as each validator must shuffle the list in turn.

---

### Part 3: Block Controller with Automatic Elections

**Objective**: Test the complete system with a centralized controller that manages blocks and automatically triggers elections every 10 blocks.

**Steps**:

1. **Compile all modules**:
```bash
erl -compile transaction.erl
erl -compile merkle_tree.erl
erl -compile block.erl
erl -compile node.erl
erl -compile part3.erl
```

2. **Run the test**:
```bash
erl -noshell -s part3 test
```

**What happens**:
- Starts 3 validators
- Starts 2 non-validator nodes
- Creates a centralized transaction pool with 300 transactions
- The block controller:
  - Sends 10 transactions to the current builder
  - Waits for block creation
  - Repeats for 10 blocks
  - Automatically triggers an election
  - Changes builder according to election result
  - Resets counter to 0 and continues

**Automatic stop**: The system stops when there are no more transactions available.

#### **Testing with custom parameters**

To test Part3 with different parameters:

```bash
erl -noshell -eval "part3:start(TotalNodes, NumValidators, \"file.csv\"), init:stop()."
```

**Parameters**:
- `TotalNodes`: Total number of nodes (validators + non-validators)
- `NumValidators`: Number of validators
- `"file.csv"`: CSV file containing transactions

**Examples**:

```bash
# Default configuration (10 total nodes, 3 validators, 7 non-validators)
erl -noshell -eval "part3:start(10, 3, \"trasactions2.csv\"), init:stop()."

# More validators (20 total nodes, 5 validators, 15 non-validators)
erl -noshell -eval "part3:start(20, 5, \"transactions.csv\"), init:stop()."

# Minimal configuration (3 total nodes, 3 validators, 0 non-validators)
erl -noshell -eval "part3:start(3, 3, \"trasactions2.csv\"), init:stop()."

# Large configuration (50 total nodes, 10 validators, 40 non-validators)
erl -noshell -eval "part3:start(50, 10, \"transactions.csv\"), init:stop()."
```

**Important notes**:
- `NumValidators` must be ≤ `TotalNodes`
- ProposerGroup size will be `max(1, NumValidators div 10)` (10% of validators)
- More validators means longer elections
- Non-validators receive blocks but don't participate in elections

---

## Compilation Scripts

### Simple Compilation

To compile all modules at once:
```bash
erl -compile transaction.erl merkle_tree.erl block.erl node.erl part1.erl part2.erl part3.erl
```

### Compilation Script

The `compile.bat` file compiles all modules:
```bash
compile.bat
```

---

## Data Files

- **transactions.csv**: Test transaction file
- **trasactions2.csv**: Alternative transaction file (used by default in part1)

---

## ALWAYS CLEAN GENERATED CSV FILES AFTER EACH RUN

Generated files `.beam` (compiled), `.csv` (blockchain), and election logs can be cleaned with:

```bash
rm *.beam blockchain_*.csv *_election_log.txt election_results.txt
```

On Windows:
```cmd
del /Q *.beam blockchain_*.csv *_election_log.txt election_results.txt
```

---

## Whisk Algorithm Details

The Whisk (Secret Leader Election) algorithm works in several steps:

1. **Initialization**: The head validator initiates the election and broadcasts to all nodes
2. **Cascading shuffle**: Each validator shuffles the list and passes it to the next
3. **Final selection**: The last validator sends the list to the head initiator
4. **Result broadcast**: The head selects the ProposerGroup (10% of validators) and broadcasts it

### Example with 3 Validators

```bash
Validator_1 (head):
  └─> Initial shuffle: ["V1", "V2", "V3"] → ["V3", "V1", "V2"]
  └─> Send to Validator_2

Validator_2:
  └─> Receives: ["V3", "V1", "V2"]
  └─> Re-shuffle: ["V3", "V1", "V2"] → ["V2", "V3", "V1"]
  └─> Send to Validator_3

Validator_3 (last):
  └─> Receives: ["V2", "V3", "V1"]
  └─> Re-shuffle: ["V2", "V3", "V1"] → ["V1", "V2", "V3"]
  └─> Send to head Validator_1

Validator_1:
  └─> Receives: ["V1", "V2", "V3"]
  └─> Selects ProposerGroup: ["V1"] (10% of 3 = 1)
  └─> Broadcast to all validators
```

---

## Part 3 Architecture

### Block Controller
The Block Controller is a centralized process that:
- Manages the transaction pool
- Controls which validator creates blocks
- Triggers elections every 10 blocks
- Automatically changes the builder according to election results

### Transaction Pool
Centralized transaction pool managed as an Erlang process.

### Differences Part2 vs Part3

| Aspect | Part 2 | Part 3 |
|--------|--------|--------|
| **Builder** | Static | Dynamic (changes after elections) |
| **Elections** | Manual (once) | Automatic (every 10 blocks) |
| **Transactions** | None | Centralized pool of 300 transactions |
| **Control** | Simple test | Centralized block controller |
| **Objective** | Observe election | Complete blockchain system |

---

## Authors

Erlang blockchain project with secret election system (Whisk algorithm).
