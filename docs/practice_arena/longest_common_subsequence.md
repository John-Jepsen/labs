# Longest Common Subsequence (LCS): Finding Shared Patterns

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

The Longest Common Subsequence (LCS) finds the longest sequence of characters that appear in the same order (but not necessarily consecutively) in two strings:

```
Example:
String X: "ABCBDAB"
String Y: "BDCABA"

Step 1: Create a table to track LCS lengths
   Initialize with zeros (empty strings have 0 length LCS)
        |   | B | D | C | A | B | A |
    ----|---|---|---|---|---|---|---|
        | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
        |---|---|---|---|---|---|---|
    A   | 0 | ? | ? | ? | ? | ? | ? |
    B   | 0 | ? | ? | ? | ? | ? | ? |
    C   | 0 | ? | ? | ? | ? | ? | ? |
    B   | 0 | ? | ? | ? | ? | ? | ? |
    D   | 0 | ? | ? | ? | ? | ? | ? |
    A   | 0 | ? | ? | ? | ? | ? | ? |
    B   | 0 | ? | ? | ? | ? | ? | ? |

Step 2: Fill the table using dynamic programming:
   If X[i] = Y[j]: dp[i][j] = dp[i-1][j-1] + 1
   Else: dp[i][j] = max(dp[i-1][j], dp[i][j-1])

        |   | B | D | C | A | B | A |
    ----|---|---|---|---|---|---|---|
        | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
        |---|---|---|---|---|---|---|
    A   | 0 | 0 | 0 | 0 | 1 | 1 | 1 |
    B   | 0 | 1 | 1 | 1 | 1 | 2 | 2 |
    C   | 0 | 1 | 1 | 2 | 2 | 2 | 2 |
    B   | 0 | 1 | 1 | 2 | 2 | 3 | 3 |
    D   | 0 | 1 | 2 | 2 | 2 | 3 | 3 |
    A   | 0 | 1 | 2 | 2 | 3 | 3 | 4 |
    B   | 0 | 1 | 2 | 2 | 3 | 4 | 4 |

The length of the LCS is 4 (found at dp[7][6])

Step 3: Reconstruct the LCS by backtracking through the table:
   Start at dp[7][6] and move up/left when values match
   When X[i] = Y[j], include the character and move diagonally

   The LCS is "BCBA"
```

## Pseudocode

```
function LCS(X, Y):
    m = length of X
    n = length of Y
    create a table dp of size (m+1) × (n+1)
    initialize all cells in dp to 0

    for i from 1 to m:
        for j from 1 to n:
            if X[i-1] equals Y[j-1]:
                dp[i][j] = dp[i-1][j-1] + 1
            else:
                dp[i][j] = max(dp[i-1][j], dp[i][j-1])

    return dp[m][n]

function backtrack_LCS(X, Y, dp):
    i = length of X
    j = length of Y
    lcs = empty string

    while i > 0 and j > 0:
        if X[i-1] equals Y[j-1]:
            add X[i-1] to the beginning of lcs
            i = i - 1
            j = j - 1
        else if dp[i-1][j] >= dp[i][j-1]:
            i = i - 1
        else:
            j = j - 1

    return lcs
```

## Annotated Code Template

```python
def longest_common_subsequence(X, Y):
    """
    Find the length of the longest common subsequence between two strings.

    Args:
        X: First string
        Y: Second string

    Returns:
        Length of the longest common subsequence
    """
    # TODO: Get the lengths of the input strings

    # TODO: Initialize the DP table with zeros

    # TODO: Fill the DP table using bottom-up approach
        # For each character in X
            # For each character in Y
                # If characters match, extend the subsequence
                # Otherwise, take the maximum of two possible subproblems

    # TODO: Return the length of LCS


def backtrack_lcs(X, Y, dp):
    """
    Reconstruct the longest common subsequence from the DP table.

    Args:
        X: First string
        Y: Second string
        dp: The dynamic programming table

    Returns:
        The longest common subsequence as a string
    """
    # TODO: Start from the bottom-right corner of the DP table

    # TODO: Backtrack through the table to reconstruct the LCS
        # If characters match, add to the LCS and move diagonally
        # Otherwise, move in the direction of the larger value

    # TODO: Return the LCS
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the dynamic programming approach and the meaning of each cell

### Task

Working together, complete the LCS implementation. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each step of the algorithm.

1. Start with implementing the function to find the length of the LCS
2. Fill the DP table step by step
3. Implement the backtracking function to reconstruct the actual subsequence
4. Test with various examples and trace through the execution

### Complete Implementation

```python
def longest_common_subsequence(X, Y):
    """
    Find the length of the longest common subsequence between two strings.

    Args:
        X: First string
        Y: Second string

    Returns:
        Length of the longest common subsequence
    """
    # Get the lengths of the input strings
    m, n = len(X), len(Y)

    # Initialize the DP table with zeros
    # dp[i][j] represents the length of LCS of X[0...i-1] and Y[0...j-1]
    dp = [[0] * (n + 1) for _ in range(m + 1)]

    # Fill the DP table using bottom-up approach
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            # If characters match, extend the subsequence
            if X[i - 1] == Y[j - 1]:
                dp[i][j] = dp[i - 1][j - 1] + 1
            # Otherwise, take the maximum of two possible subproblems
            else:
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])

    return dp, dp[m][n]


def backtrack_lcs(X, Y, dp):
    """
    Reconstruct the longest common subsequence from the DP table.

    Args:
        X: First string
        Y: Second string
        dp: The dynamic programming table

    Returns:
        The longest common subsequence as a string
    """
    # Start from the bottom-right corner of the DP table
    i, j = len(X), len(Y)
    lcs = []

    # Backtrack through the table to reconstruct the LCS
    while i > 0 and j > 0:
        # If characters match, add to the LCS and move diagonally
        if X[i - 1] == Y[j - 1]:
            lcs.append(X[i - 1])
            i -= 1
            j -= 1
        # Otherwise, move in the direction of the larger value
        elif dp[i - 1][j] >= dp[i][j - 1]:
            i -= 1
        else:
            j -= 1

    # Return the LCS (reversed since we added characters in reverse order)
    return ''.join(reversed(lcs))


def print_dp_table(X, Y, dp):
    """
    Print the DP table in a readable format for visualization.

    Args:
        X: First string
        Y: Second string
        dp: The dynamic programming table
    """
    # Print header row with Y characters
    print("    |   ", end="")
    for char in Y:
        print(f"| {char} ", end="")
    print("|")

    # Print separator
    print("----|-", end="")
    for _ in range(len(Y) + 1):
        print("---|-", end="")
    print())

    # Print first row (empty string case)
    print("    | 0 ", end="")
    for j in range(1, len(Y) + 1):
        print(f"| {dp[0][j]} ", end="")
    print("|")

    # Print separator
    print("----|-", end="")
    for _ in range(len(Y) + 1):
        print("---|-", end="")
    print())

    # Print remaining rows
    for i in range(1, len(X) + 1):
        print(f" {X[i-1]}  | {dp[i][0]} ", end="")
        for j in range(1, len(Y) + 1):
            print(f"| {dp[i][j]} ", end="")
        print("|")


# Example usage:
X = "ABCBDAB"
Y = "BDCABA"

print(f"Finding LCS of '{X}' and '{Y}':")
dp, lcs_length = longest_common_subsequence(X, Y)
print(f"Length of LCS: {lcs_length}")

print("\nDP Table:")
print_dp_table(X, Y, dp)

lcs = backtrack_lcs(X, Y, dp)
print(f"\nLongest Common Subsequence: '{lcs}'")

# Trace through a smaller example step by step
X2 = "ABCD"
Y2 = "ACBD"

print(f"\n\nTracing LCS algorithm for '{X2}' and '{Y2}':")
m, n = len(X2), len(Y2)
dp = [[0] * (n + 1) for _ in range(m + 1)]

print("Initial DP table (all zeros):")
print_dp_table(X2, Y2, dp)

print("\nFilling DP table step by step:")
for i in range(1, m + 1):
    for j in range(1, n + 1):
        if X2[i - 1] == Y2[j - 1]:
            dp[i][j] = dp[i - 1][j - 1] + 1
            print(f"\nMatch: {X2[i-1]} at positions X[{i-1}] and Y[{j-1}]")
            print(f"Setting dp[{i}][{j}] = dp[{i-1}][{j-1}] + 1 = {dp[i-1][j-1]} + 1 = {dp[i][j]}")
        else:
            dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
            print(f"\nMismatch: {X2[i-1]} != {Y2[j-1]} at positions X[{i-1}] and Y[{j-1}]")
            print(f"Setting dp[{i}][{j}] = max(dp[{i-1}][{j}], dp[{i}][{j-1}]) = max({dp[i-1][j]}, {dp[i][j-1]}) = {dp[i][j]}")

        print("Current DP table:")
        print_dp_table(X2, Y2, dp)

lcs2 = backtrack_lcs(X2, Y2, dp)
print(f"\nResult: LCS of '{X2}' and '{Y2}' is '{lcs2}' with length {dp[m][n]}")

print("\nBacktracking steps to reconstruct the LCS:")
i, j = m, n
steps = []
while i > 0 and j > 0:
    if X2[i - 1] == Y2[j - 1]:
        steps.append(f"Characters match: {X2[i-1]} at X[{i-1}] and Y[{j-1}], add to LCS")
        steps.append(f"Move diagonally: ({i},{j}) → ({i-1},{j-1})")
        i -= 1
        j -= 1
    elif dp[i - 1][j] >= dp[i][j - 1]:
        steps.append(f"Move up: ({i},{j}) → ({i-1},{j})")
        i -= 1
    else:
        steps.append(f"Move left: ({i},{j}) → ({i},{j-1})")
        j -= 1

for step in reversed(steps):
    print(step)
```

## Peer Discussion Prompts

1. How is the LCS problem different from the Longest Common Substring problem?
2. What other problems can be solved using a similar dynamic programming approach?
3. How would you modify the algorithm to find the Longest Common Increasing Subsequence?
4. Can you think of any real-world applications of the LCS algorithm?

## Checkpoint Questions

1. **Checkpoint 1**: What does each cell dp[i][j] in the DP table represent?
2. **Checkpoint 2**: Why do we initialize the first row and column of the DP table with zeros?
3. **Checkpoint 3**: What is the recurrence relation used to fill the DP table?
4. **Checkpoint 4**: How many different longest common subsequences might exist for two strings?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(m × n)** where m and n are the lengths of the two input strings
- We need to fill a table of size (m+1) × (n+1), and each cell takes constant time to compute

### Space Complexity

- **O(m × n)** for the DP table
- If we only need the length of the LCS, we can optimize to O(min(m, n)) by using only two rows

## Common Implementation Mistakes

1. **Off-by-one errors**: Forgetting to handle 0-indexed strings vs. 1-indexed DP table
2. **Backtracking errors**: Not properly reconstructing the actual LCS from the DP table
3. **Confusing LCS with Longest Common Substring**: LCS allows for non-contiguous characters
4. **Inefficient space usage**: Not optimizing the space complexity when only the length is needed

## Mini-Challenge

1. Implement a space-optimized version of the LCS algorithm that uses only O(min(m, n)) space
2. Modify the algorithm to find all possible longest common subsequences
3. Extend the implementation to work with three or more strings
4. Implement a solution for the "Shortest Common Supersequence" problem using LCS

For the team:

- Compare the performance of the dynamic programming solution with a recursive solution with memoization
- Create a visualization that shows how the DP table is filled and how the backtracking works
- Apply the LCS algorithm to solve a specific problem like DNA sequence alignment
- Discuss how the LCS algorithm relates to the edit distance (Levenshtein distance) algorithm
