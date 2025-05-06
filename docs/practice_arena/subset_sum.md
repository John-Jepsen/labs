# Subset Sum Problem: Finding a Subset with a Target Sum

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

The Subset Sum problem determines whether there exists a subset of a given set of integers that sums to a target value:

```
Example: Set = [3, 34, 4, 12, 5, 2], Target = 9

We need to find if any combination of these numbers sums to 9.

Step 1: Create a DP table where dp[i][j] represents:
       "Can a subset of the first i elements sum to j?"

DP Table initialization (rows: elements considered, columns: possible sums):
       0  1  2  3  4  5  6  7  8  9  <- Target sums
   0  [T, F, F, F, F, F, F, F, F, F]  <- Empty set (can only sum to 0)
   3  [?, ?, ?, ?, ?, ?, ?, ?, ?, ?]  <- After considering [3]
  34  [?, ?, ?, ?, ?, ?, ?, ?, ?, ?]  <- After considering [3, 34]
   4  [?, ?, ?, ?, ?, ?, ?, ?, ?, ?]  <- After considering [3, 34, 4]
  12  [?, ?, ?, ?, ?, ?, ?, ?, ?, ?]  <- After considering [3, 34, 4, 12]
   5  [?, ?, ?, ?, ?, ?, ?, ?, ?, ?]  <- After considering [3, 34, 4, 12, 5]
   2  [?, ?, ?, ?, ?, ?, ?, ?, ?, ?]  <- After considering [3, 34, 4, 12, 5, 2]

Step 2: Fill the DP table row by row:

For element 3:
- If we don't use 3: copy previous row (empty row)
- If we use 3: check if cell [0][j-3] is True for each j ≥ 3
       0  1  2  3  4  5  6  7  8  9
   0  [T, F, F, F, F, F, F, F, F, F]
   3  [T, F, F, T, F, F, F, F, F, F]  <- Cell [1][3] is True (we can sum to 3)

For element 34 (too large for our target, so no change):
       0  1  2  3  4  5  6  7  8  9
   3  [T, F, F, T, F, F, F, F, F, F]
  34  [T, F, F, T, F, F, F, F, F, F]

For element 4:
       0  1  2  3  4  5  6  7  8  9
  34  [T, F, F, T, F, F, F, F, F, F]
   4  [T, F, F, T, T, F, F, T, F, F]  <- We can now sum to 4 and 7 (3+4)

For element 12 (too large for remaining cells):
       0  1  2  3  4  5  6  7  8  9
   4  [T, F, F, T, T, F, F, T, F, F]
  12  [T, F, F, T, T, F, F, T, F, F]

For element 5:
       0  1  2  3  4  5  6  7  8  9
  12  [T, F, F, T, T, F, F, T, F, F]
   5  [T, F, F, T, T, T, F, T, T, T]  <- We can now sum to 5, 8, and 9 (4+5)

For element 2:
       0  1  2  3  4  5  6  7  8  9
   5  [T, F, F, T, T, T, F, T, T, T]
   2  [T, F, T, T, T, T, T, T, T, T]  <- Final row

Step 3: Check dp[n][target] (bottom-right cell)
dp[6][9] = True, so a subset sum of 9 is possible.

One solution is [2, 3, 4] = 9
```

## Pseudocode

```
function subset_sum(set, target):
    n = length of set

    // Create a 2D DP table of size (n+1) x (target+1)
    dp = new array of size (n+1) x (target+1), initialized with False

    // Base case: empty subset can sum to 0
    for i from 0 to n:
        dp[i][0] = True

    // Fill the DP table
    for i from 1 to n:
        for j from 1 to target:
            // If we don't include the current element
            dp[i][j] = dp[i-1][j]

            // If we include the current element (if possible)
            if set[i-1] <= j:
                dp[i][j] = dp[i][j] OR dp[i-1][j - set[i-1]]

    return dp[n][target]

// Optional: Function to find one valid subset
function find_subset(set, dp, target):
    n = length of set
    if dp[n][target] == False:
        return "No solution exists"

    result = empty list
    i = n
    j = target

    while i > 0 and j > 0:
        // Check if the result was obtained by excluding current element
        if dp[i-1][j] == True:
            i = i - 1
        else:
            // Include current element in the result
            result.add(set[i-1])
            j = j - set[i-1]
            i = i - 1

    return result
```

## Annotated Code Template

```python
def subset_sum(nums, target):
    """
    Determine if there exists a subset of the given numbers that sums to target.

    Args:
        nums: A list of integers
        target: The target sum

    Returns:
        A boolean indicating whether such a subset exists
    """
    # Get the length of the input array
    # TODO: Get array length

    # Create a 2D DP table of size (n+1) x (target+1)
    # TODO: Initialize DP table

    # Base case: empty subset can sum to 0
    # TODO: Set base case values

    # Fill the DP table
    # TODO: Implement the dynamic programming approach

    # Return the final result
    # TODO: Return whether a subset with the target sum exists


def find_subset(nums, target):
    """
    Find one subset of the given numbers that sums to target (if it exists).

    Args:
        nums: A list of integers
        target: The target sum

    Returns:
        A list containing a valid subset, or an empty list if no solution exists
    """
    # First, determine if a solution exists using the DP approach
    # TODO: Call subset_sum function and create DP table

    # If no solution exists, return an empty list
    # TODO: Check if a solution exists

    # Backtrack to find a valid subset
    # TODO: Implement backtracking to find one solution
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the dynamic programming approach at each step

### Task

Working together, implement the Subset Sum algorithm:

1. First, implement the function to determine if a subset sum exists
2. Then, extend it to find one valid subset that sums to the target
3. Test the implementation with different arrays and target values
4. Discuss the time and space complexity optimizations

### Complete Implementation

```python
def subset_sum(nums, target):
    """
    Determine if there exists a subset of the given numbers that sums to target.

    Args:
        nums: A list of integers
        target: The target sum

    Returns:
        A boolean indicating whether such a subset exists
    """
    # Get the length of the input array
    n = len(nums)

    # Create a 2D DP table of size (n+1) x (target+1)
    dp = [[False for _ in range(target + 1)] for _ in range(n + 1)]

    # Base case: empty subset can sum to 0
    for i in range(n + 1):
        dp[i][0] = True

    # Fill the DP table
    for i in range(1, n + 1):
        for j in range(1, target + 1):
            # If we don't include the current element
            dp[i][j] = dp[i - 1][j]

            # If we include the current element (if possible)
            if nums[i - 1] <= j:
                dp[i][j] = dp[i][j] or dp[i - 1][j - nums[i - 1]]

    return dp[n][target], dp


def find_subset(nums, target):
    """
    Find one subset of the given numbers that sums to target (if it exists).

    Args:
        nums: A list of integers
        target: The target sum

    Returns:
        A list containing a valid subset, or an empty list if no solution exists
    """
    # First, determine if a solution exists using the DP approach
    exists, dp = subset_sum(nums, target)

    # If no solution exists, return an empty list
    if not exists:
        return []

    # Backtrack to find a valid subset
    result = []
    i, j = len(nums), target

    while i > 0 and j > 0:
        # Check if the result was obtained by excluding current element
        if dp[i - 1][j]:
            i -= 1
        else:
            # Include current element in the result
            result.append(nums[i - 1])
            j -= nums[i - 1]
            i -= 1

    return result


def space_optimized_subset_sum(nums, target):
    """
    Space-optimized version that uses only 1D array.

    Args:
        nums: A list of integers
        target: The target sum

    Returns:
        A boolean indicating whether such a subset exists
    """
    # Create a 1D DP array of size (target+1)
    dp = [False] * (target + 1)

    # Base case: empty subset can sum to 0
    dp[0] = True

    # Fill the DP array
    for num in nums:
        # Iterate from right to left to avoid using updated values
        for j in range(target, num - 1, -1):
            dp[j] = dp[j] or dp[j - num]

    return dp[target]


def trace_subset_sum(nums, target):
    """
    Trace the execution of subset_sum with detailed explanation.
    """
    n = len(nums)

    print(f"Tracing subset_sum for nums={nums}, target={target}")

    # Create a 2D DP table
    dp = [[False for _ in range(target + 1)] for _ in range(n + 1)]

    # Base case: empty subset can sum to 0
    for i in range(n + 1):
        dp[i][0] = True

    print("\nInitialized DP table with base case (empty subset can sum to 0):")
    print_dp_table(dp, nums, target)

    # Fill the DP table
    for i in range(1, n + 1):
        current_num = nums[i - 1]
        print(f"\nProcessing element {current_num} (index {i-1}):")

        for j in range(1, target + 1):
            # First, copy the value from the previous row (not taking current element)
            dp[i][j] = dp[i - 1][j]

            # Check if we can include the current element
            if current_num <= j:
                # Can we achieve sum j-current_num with previous elements?
                if dp[i - 1][j - current_num]:
                    dp[i][j] = True
                    print(f"  dp[{i}][{j}] = True (can form sum {j} by adding {current_num} to previous sum {j-current_num})")
                elif dp[i][j]:
                    print(f"  dp[{i}][{j}] = True (can form sum {j} without using {current_num})")
            elif dp[i][j]:
                print(f"  dp[{i}][{j}] = True (can form sum {j} without using {current_num})")

        print("\nDP table after processing element", current_num, ":")
        print_dp_table(dp, nums, target)

    result = dp[n][target]
    print(f"\nFinal result: dp[{n}][{target}] = {result}")

    if result:
        subset = find_subset(nums, target)
        print(f"One valid subset is: {subset}, which sums to {sum(subset)}")
    else:
        print("No subset exists that sums to the target.")

    return result


def print_dp_table(dp, nums, target):
    """Helper function to print the DP table nicely."""
    # Print column headers (target values)
    print("      ", end="")
    for j in range(target + 1):
        print(f"{j:2d} ", end="")
    print()

    # Print the first row (empty set)
    print("  [] ", end="")
    for j in range(target + 1):
        print(" T " if dp[0][j] else " F ", end="")
    print()

    # Print remaining rows
    for i in range(1, len(dp)):
        print(f"{nums[i-1]:3d} ", end="")
        for j in range(target + 1):
            print(" T " if dp[i][j] else " F ", end="")
        print()


# Example usage
nums1 = [3, 34, 4, 12, 5, 2]
target1 = 9

print("Example 1: Basic usage")
exists1, _ = subset_sum(nums1, target1)
print(f"Does a subset of {nums1} sum to {target1}? {exists1}")

subset1 = find_subset(nums1, target1)
if subset1:
    print(f"One valid subset is: {subset1}, which sums to {sum(subset1)}")
else:
    print("No valid subset found.")

print("\nExample 1 with tracing:")
trace_subset_sum(nums1, target1)

# Additional examples
print("\nExample 2: No solution exists")
nums2 = [2, 4, 6, 8]
target2 = 11
exists2, _ = subset_sum(nums2, target2)
print(f"Does a subset of {nums2} sum to {target2}? {exists2}")

print("\nExample 3: Space-optimized version")
nums3 = [1, 5, 11, 5]
target3 = 11
exists3 = space_optimized_subset_sum(nums3, target3)
print(f"Does a subset of {nums3} sum to {target3}? {exists3}")

print("\nExample 4: Large numbers")
nums4 = [100, 200, 300, 400]
target4 = 700
exists4, _ = subset_sum(nums4, target4)
print(f"Does a subset of {nums4} sum to {target4}? {exists4}")
subset4 = find_subset(nums4, target4)
if subset4:
    print(f"One valid subset is: {subset4}, which sums to {sum(subset4)}")
```

## Peer Discussion Prompts

1. How does the Subset Sum problem relate to the Knapsack problem? What are the similarities and differences?
2. Can you think of real-world applications where solving the Subset Sum problem would be useful?
3. How would you approach this problem if the input array could contain negative numbers?
4. What optimizations could be made to the dynamic programming approach for specific input patterns?

## Checkpoint Questions

1. **Checkpoint 1**: Why is the Subset Sum problem considered NP-complete, and what does that mean for large inputs?
2. **Checkpoint 2**: Why do we set `dp[i][0] = True` for all i in the initialization step?
3. **Checkpoint 3**: How does the space-optimized version work with just a 1D array instead of a 2D array?
4. **Checkpoint 4**: What is the recurrence relation used in the dynamic programming approach?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(n × target)** where n is the number of items and target is the target sum
  - We fill a table of size (n+1) × (target+1)
  - Each cell calculation takes O(1) time

### Space Complexity

- **Standard approach**: O(n × target) for the 2D DP table
- **Space-optimized approach**: O(target) by using a 1D array and updating it in place

## Common Implementation Mistakes

1. **Incorrect base case**: Forgetting to set `dp[i][0] = True` for all rows
2. **Not checking array bounds**: Trying to access negative indices when the current number is larger than the current sum
3. **Logical error in DP state**: Mixing up when to use OR versus AND in the state transition
4. **Incorrect backtracking**: Making errors when reconstructing the subset
5. **Handling zero**: Not properly handling the case with zero values in the input array

## Mini-Challenge

1. Modify the algorithm to count the total number of distinct subsets that sum to the target.
2. Implement a solution to find all possible subsets that sum to the target value.
3. Adapt the algorithm to handle negative numbers in the input array.
4. Create a function that finds the subset with the minimum number of elements that sums to the target.

For the team: Compare the runtime of the dynamic programming approach versus a backtracking/recursive approach for different input sizes, and analyze the space-time tradeoffs.
