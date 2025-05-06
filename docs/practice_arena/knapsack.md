# Knapsack Problem: Optimizing Value with Limited Capacity

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

The knapsack problem involves selecting items with different weights and values to maximize total value while staying within a weight limit:

```
Items:
| Item | Weight | Value |
|------|--------|-------|
|  1   |   1    |   1   |
|  2   |   3    |   4   |
|  3   |   4    |   5   |
|  4   |   5    |   7   |

Knapsack capacity: 7 units

Dynamic Programming Table:
dp[i][w] = maximum value with first i items and weight capacity w

Step 1: Initialize dp table with 0s
   w→  0  1  2  3  4  5  6  7
i↓
0     0  0  0  0  0  0  0  0
1     0  ?  ?  ?  ?  ?  ?  ?
2     0  ?  ?  ?  ?  ?  ?  ?
3     0  ?  ?  ?  ?  ?  ?  ?
4     0  ?  ?  ?  ?  ?  ?  ?

Step 2: Fill dp table row by row
- For row 1 (item 1, weight=1, value=1):
   w→  0  1  2  3  4  5  6  7
i↓
0     0  0  0  0  0  0  0  0
1     0  1  1  1  1  1  1  1

- For row 2 (item 2, weight=3, value=4):
   w→  0  1  2  3  4  5  6  7
i↓
0     0  0  0  0  0  0  0  0
1     0  1  1  1  1  1  1  1
2     0  1  1  4  5  5  5  5

- For row 3 (item 3, weight=4, value=5):
   w→  0  1  2  3  4  5  6  7
i↓
0     0  0  0  0  0  0  0  0
1     0  1  1  1  1  1  1  1
2     0  1  1  4  5  5  5  5
3     0  1  1  4  5  6  6  9

- For row 4 (item 4, weight=5, value=7):
   w→  0  1  2  3  4  5  6  7
i↓
0     0  0  0  0  0  0  0  0
1     0  1  1  1  1  1  1  1
2     0  1  1  4  5  5  5  5
3     0  1  1  4  5  6  6  9
4     0  1  1  4  5  7  8  9

The maximum value possible is 9 (found at dp[4][7])
```

## Pseudocode

```
function knapsack(weights, values, capacity):
    n = length of weights array
    create a 2D array dp of size (n+1) x (capacity+1)
    initialize all elements of dp to 0

    for i from 1 to n:
        for w from 1 to capacity:
            if weights[i-1] <= w:
                // We have two choices: include item i or exclude it
                dp[i][w] = max(
                    dp[i-1][w],  // exclude item i
                    dp[i-1][w-weights[i-1]] + values[i-1]  // include item i
                )
            else:
                // Item i is too heavy, can't include it
                dp[i][w] = dp[i-1][w]

    return dp[n][capacity]
```

## Annotated Code Template

```python
def knapsack(weights, values, capacity):
    """
    Solve the 0/1 knapsack problem using dynamic programming.

    Args:
        weights: List of weights for each item
        values: List of values for each item
        capacity: Maximum weight capacity of the knapsack

    Returns:
        Maximum value that can be obtained
    """
    n = len(weights)

    # TODO: Create a 2D array to store solutions to subproblems

    # TODO: Fill the dp table using bottom-up approach

    # TODO: Return the maximum value that can be obtained
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the dynamic programming approach and state transitions

### Task

Working together, complete the knapsack implementation. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each step of the algorithm.

1. Initialize the dp table with appropriate dimensions
2. Implement the nested loops to fill the dp table
3. Implement the decision logic for each cell (include or exclude the current item)
4. Test with the example items and trace through the execution

### Complete Implementation

```python
def knapsack(weights, values, capacity):
    n = len(weights)

    # Create a 2D array to store solutions to subproblems
    # dp[i][w] = max value using first i items with capacity w
    dp = [[0 for _ in range(capacity + 1)] for _ in range(n + 1)]

    # Fill the dp table using bottom-up approach
    for i in range(1, n + 1):
        for w in range(1, capacity + 1):
            if weights[i - 1] <= w:
                # We have two choices: include item i or exclude it
                # If we include item i, we add its value and look up the optimal
                # solution for remaining capacity
                include_item = dp[i - 1][w - weights[i - 1]] + values[i - 1]

                # If we exclude item i, the value is the same as optimal solution
                # with i-1 items and same capacity
                exclude_item = dp[i - 1][w]

                # Take the maximum of both choices
                dp[i][w] = max(include_item, exclude_item)
            else:
                # Item i is too heavy, can't include it
                dp[i][w] = dp[i - 1][w]

    # Return the maximum value that can be obtained
    return dp[n][capacity]

# Function to visualize the dp table
def print_dp_table(dp, n, capacity):
    print("DP Table:")
    print("   ", end="")
    for w in range(capacity + 1):
        print(f"{w:2d} ", end="")
    print()

    for i in range(n + 1):
        print(f"{i:2d} ", end="")
        for w in range(capacity + 1):
            print(f"{dp[i][w]:2d} ", end="")
        print()

# Example usage:
weights = [1, 3, 4, 5]
values = [1, 4, 5, 7]
capacity = 7

print(f"Items:")
print(f"| Item | Weight | Value |")
print(f"|------|--------|-------|")
for i in range(len(weights)):
    print(f"|  {i+1}   |   {weights[i]}    |   {values[i]}   |")
print(f"\nKnapsack capacity: {capacity} units\n")

n = len(weights)
dp = [[0 for _ in range(capacity + 1)] for _ in range(n + 1)]

print("Step 1: Initialize dp table with 0s")
print_dp_table(dp, n, capacity)

for i in range(1, n + 1):
    print(f"\nStep 2.{i}: Process item {i} (weight={weights[i-1]}, value={values[i-1]})")
    for w in range(1, capacity + 1):
        if weights[i - 1] <= w:
            dp[i][w] = max(dp[i - 1][w], dp[i - 1][w - weights[i - 1]] + values[i - 1])
        else:
            dp[i][w] = dp[i - 1][w]
    print_dp_table(dp, n, capacity)

print(f"\nThe maximum value possible is {dp[n][capacity]}")

# To find which items were included
def find_selected_items(dp, weights, values, capacity):
    n = len(weights)
    selected = []
    w = capacity

    for i in range(n, 0, -1):
        # If value comes from including this item
        if dp[i][w] != dp[i-1][w]:
            selected.append(i-1)  # Item index is i-1
            w -= weights[i-1]

    return selected[::-1]  # Reverse to get items in original order

selected_items = find_selected_items(dp, weights, values, capacity)
print(f"\nSelected items (indexed from 0): {selected_items}")
total_weight = sum(weights[i] for i in selected_items)
total_value = sum(values[i] for i in selected_items)
print(f"Total weight: {total_weight}/{capacity}, Total value: {total_value}")
```

## Peer Discussion Prompts

1. How does the knapsack problem demonstrate the dynamic programming principle of overlapping subproblems?
2. What is the difference between the 0/1 knapsack problem and the fractional knapsack problem?
3. Can you think of real-world applications of the knapsack problem?
4. How would you modify this algorithm to also return which items were selected?

## Checkpoint Questions

1. **Checkpoint 1**: What is the meaning of each cell dp[i][w] in our table?
2. **Checkpoint 2**: Why do we initialize the first row and column of the dp table to 0?
3. **Checkpoint 3**: When processing item 3, why does dp[3][7] = 9?
4. **Checkpoint 4**: What would change in our approach if we could take fractional amounts of items?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(n × W)** where n is the number of items and W is the capacity
- We fill a table of size (n+1) × (W+1)
- Each cell requires a constant time operation

### Space Complexity

- **O(n × W)** for the dp table
- This can be reduced to O(W) by using a 1D array and updating it in-place

## Common Implementation Mistakes

1. **Off-by-one errors**: Confusing 0-indexed and 1-indexed arrays
2. **Not handling edge cases**: Forgetting to check if the item weight exceeds capacity
3. **Using greedy approach**: Assuming sorting items by value/weight ratio would work (which doesn't for 0/1 knapsack)
4. **Incorrect state transition**: Mixing up the decision to include or exclude an item

## Mini-Challenge

1. Implement a space-optimized version of the knapsack algorithm using a 1D array
2. Modify the algorithm to solve the unbounded knapsack problem (can take multiple copies of each item)
3. Implement a solution that returns both the maximum value and the list of selected items
4. Create a visualization that shows how the dp table is filled step by step

For the team:

- Compare the performance of the dynamic programming solution with a recursive solution with memoization
- Explore how the algorithm's performance scales with increasing number of items and capacity
- Discuss how to apply this algorithm to related problems like subset sum or coin change
