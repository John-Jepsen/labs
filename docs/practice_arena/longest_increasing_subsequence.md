# Longest Increasing Subsequence (LIS): Dynamic Programming Approach

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

The Longest Increasing Subsequence problem finds the longest subsequence of a given sequence such that all elements of the subsequence are sorted in increasing order:

```
Array: [10, 22, 9, 33, 21, 50, 41, 60, 80]

Initialize DP array (each element starts with LIS of 1):
DP: [1, 1, 1, 1, 1, 1, 1, 1, 1]

Process each element:
For i = 1 (value 22):
  - Check all previous j (just 10)
  - 10 < 22, so update DP[1] = max(DP[1], DP[0] + 1) = 2
DP: [1, 2, 1, 1, 1, 1, 1, 1, 1]

For i = 2 (value 9):
  - Check previous elements (10, 22)
  - 9 < 10? No, don't update
  - 9 < 22? No, don't update
DP: [1, 2, 1, 1, 1, 1, 1, 1, 1]

For i = 3 (value 33):
  - Check previous elements (10, 22, 9)
  - 10 < 33? Yes, DP[3] = max(1, 1+1) = 2
  - 22 < 33? Yes, DP[3] = max(2, 2+1) = 3
  - 9 < 33? Yes, DP[3] = max(3, 1+1) = 3
DP: [1, 2, 1, 3, 1, 1, 1, 1, 1]

...and so on for all elements

Final DP array: [1, 2, 1, 3, 2, 4, 4, 5, 6]

The longest increasing subsequence has length 6,
one such subsequence is: [10, 22, 33, 50, 60, 80]
```

## Pseudocode

```
function longest_increasing_subsequence(array):
    n = length of array
    dp = new array of size n with all values set to 1

    // Fill dp array
    for i = 1 to n-1:
        for j = 0 to i-1:
            if array[j] < array[i]:
                dp[i] = max(dp[i], dp[j] + 1)

    // Find the maximum value in dp array
    return maximum value in dp

function longest_increasing_subsequence_optimized(array):
    n = length of array
    tails = new array of size n
    len = 0

    for num in array:
        // Binary search for the largest positive j ≤ len
        // such that tails[j-1] < num
        lo = 0
        hi = len

        while lo < hi:
            mid = (lo + hi) / 2
            if tails[mid] < num:
                lo = mid + 1
            else:
                hi = mid

        // Update tails
        tails[lo] = num
        if lo == len:
            len += 1

    return len
```

## Annotated Code Template

```python
def longest_increasing_subsequence(nums):
    """
    Find the length of the longest increasing subsequence in an array.

    Args:
        nums: A list of integers

    Returns:
        The length of the longest increasing subsequence
    """
    if not nums:
        return 0

    n = len(nums)

    # Initialize DP array - DP[i] represents the length of the LIS ending at index i
    # TODO: Initialize DP array

    # Fill DP array
    # TODO: Implement the double loop to fill the DP array

    # Return the maximum value in DP array
    # TODO: Find and return the maximum length
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm at each step

### Task

Working together, implement two versions of the LIS algorithm:

1. First, implement the basic dynamic programming O(n²) solution
2. Then, implement the optimized O(n log n) solution with binary search
3. Test both algorithms on the same examples
4. Compare the time complexity and understand the tradeoffs

### Basic Dynamic Programming Implementation (O(n²))

```python
def longest_increasing_subsequence_basic(nums):
    """
    Find the length of the longest increasing subsequence using dynamic programming.
    Time Complexity: O(n²)
    """
    if not nums:
        return 0

    n = len(nums)

    # Initialize DP array - DP[i] represents the length of the LIS ending at index i
    dp = [1] * n

    # Fill DP array
    for i in range(1, n):
        for j in range(0, i):
            if nums[j] < nums[i]:
                dp[i] = max(dp[i], dp[j] + 1)

    # Return the maximum value in DP array
    return max(dp)

def print_longest_increasing_subsequence(nums):
    """
    Print one of the longest increasing subsequences.
    """
    if not nums:
        return []

    n = len(nums)
    dp = [1] * n
    prev = [-1] * n  # To track the previous element in LIS

    # Fill DP array and track previous elements
    for i in range(1, n):
        for j in range(0, i):
            if nums[j] < nums[i] and dp[i] < dp[j] + 1:
                dp[i] = dp[j] + 1
                prev[i] = j

    # Find the position of the maximum length
    max_length = max(dp)
    max_pos = dp.index(max_length)

    # Reconstruct the LIS
    lis = []
    while max_pos >= 0:
        lis.append(nums[max_pos])
        max_pos = prev[max_pos]

    return list(reversed(lis))

# Example usage
nums = [10, 22, 9, 33, 21, 50, 41, 60, 80]
lis_length = longest_increasing_subsequence_basic(nums)
lis = print_longest_increasing_subsequence(nums)

print(f"Length of LIS: {lis_length}")
print(f"One possible LIS: {lis}")
```

### Optimized Implementation with Binary Search (O(n log n))

```python
def longest_increasing_subsequence_optimized(nums):
    """
    Find the length of the longest increasing subsequence using patient sort.
    Time Complexity: O(n log n)
    """
    if not nums:
        return 0

    tails = []  # tails[i] stores the smallest tail of all increasing subsequences of length i+1

    for num in nums:
        # Binary search for the largest positive j ≤ len(tails)
        # such that tails[j-1] < num
        lo, hi = 0, len(tails)

        while lo < hi:
            mid = (lo + hi) // 2
            if tails[mid] < num:
                lo = mid + 1
            else:
                hi = mid

        # If we found no tail < num, append to tails
        if lo == len(tails):
            tails.append(num)
        # Otherwise, update the existing tail
        else:
            tails[lo] = num

    return len(tails)

# Example usage
nums = [10, 22, 9, 33, 21, 50, 41, 60, 80]
lis_length_optimized = longest_increasing_subsequence_optimized(nums)

print(f"Length of LIS (optimized): {lis_length_optimized}")
```

## Peer Discussion Prompts

1. How does the optimized algorithm achieve O(n log n) time complexity?
2. What is the trade-off between the two implementations (basic vs. optimized)?
3. Can you think of real-world applications for the LIS algorithm?
4. How would you modify the algorithm to find the longest decreasing subsequence?

## Checkpoint Questions

1. **Checkpoint 1**: What is the initial value of each element in the DP array and why?
2. **Checkpoint 2**: In the optimized algorithm, what does the `tails` array represent?
3. **Checkpoint 3**: Why do we need a binary search in the optimized version?
4. **Checkpoint 4**: How would the algorithm change if we wanted the longest non-decreasing subsequence (allowing equal elements)?

## Time and Space Complexity Walkthrough

### Time Complexity

- **Basic DP approach**: O(n²)
  - Nested loops: for each position i, we check all previous positions j
- **Optimized approach**: O(n log n)
  - For each element, we perform a binary search operation which is O(log n)

### Space Complexity

- **Basic DP approach**: O(n)
  - For storing the DP array
- **Optimized approach**: O(n)
  - For storing the tails array (in worst case, all elements form an increasing sequence)

## Common Implementation Mistakes

1. **Incorrect initialization**: Not initializing DP array with 1s (each element by itself forms an LIS of length 1)
2. **Misunderstanding the problem**: Confusing LIS with the longest contiguous increasing subsequence
3. **Binary search errors**: In the optimized version, not correctly implementing the binary search
4. **Off-by-one errors**: Especially when constructing the actual sequence
5. **Reconstruction issues**: Incorrectly tracking the prev array when reconstructing the sequence

## Mini-Challenge

1. Modify the algorithm to return all different longest increasing subsequences.
2. Implement a version that finds the longest bitonic subsequence (increasing then decreasing).
3. Solve the problem for a 2D grid, finding the longest path where each cell's value is greater than the previous cell.
4. Create a visualization of how the DP array or tails array evolves during the algorithm execution.

For the team: Compare the performance of both implementations on arrays of different sizes and patterns (sorted, reverse sorted, random).
