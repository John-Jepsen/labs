# Binary Search: Divide and Conquer Search Method

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Binary search is an efficient algorithm for finding an item in a sorted list:

```
Array: [1, 3, 5, 7, 9, 11, 13, 15, 17]
Target: 7

Step 1: Check middle element (9)
[1, 3, 5, 7, 9, 11, 13, 15, 17]
            ^
7 < 9, so search left half

Step 2: Check middle element (3)
[1, 3, 5, 7]
    ^
7 > 3, so search right half

Step 3: Check middle element (5)
[5, 7]
 ^
7 > 5, so search right half

Step 4: Only one element left (7)
[7]
 ^
7 = 7, element found at index 3!
```

## Pseudocode

```
function binary_search(array, target):
    left = 0
    right = length of array - 1

    while left <= right:
        mid = (left + right) / 2

        if array[mid] == target:
            return mid
        else if array[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1  # Target not found
```

## Annotated Code Template

```python
def binary_search(nums, target):
    """
    Perform binary search to find target in a sorted array.

    Args:
        nums: A sorted list of integers
        target: The integer value to find

    Returns:
        The index of the target if found, otherwise -1
    """
    left = 0
    right = len(nums) - 1

    while left <= right:
        # Calculate the middle index
        # TODO: Implement this calculation

        # Check if we found the target
        # TODO: Implement the condition to check if target is found

        # Decide which half to search next
        # TODO: Implement the conditions to search left or right

    # Target not found
    return -1
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm at each step

### Task

Working together, complete the binary search implementation. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each iteration.

1. Start with implementing the mid-point calculation
2. Add the condition to check if the target is found
3. Implement the logic to search either the left or right half

### Complete Implementation

```python
def binary_search(nums, target):
    left = 0
    right = len(nums) - 1

    while left <= right:
        # Calculate the middle index
        mid = (left + right) // 2

        # Check if we found the target
        if nums[mid] == target:
            return mid

        # Decide which half to search next
        elif nums[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    # Target not found
    return -1

# Example usage:
nums = [1, 3, 5, 7, 9, 11, 13, 15, 17]
target = 7
result = binary_search(nums, target)
print(f"Target {target} found at index: {result}")
```

## Peer Discussion Prompts

1. Why must the array be sorted for binary search to work?
2. What happens if you search for a value not present in the array?
3. How would you modify this algorithm to find the first occurrence of a duplicated element?
4. What could go wrong with the calculation `mid = (left + right) / 2` for very large arrays?

## Checkpoint Questions

1. **Checkpoint 1**: What is the value of `left`, `right`, and `mid` during the first iteration?
2. **Checkpoint 2**: After examining the first middle element, which half will we search next?
3. **Checkpoint 3**: What's the value of `mid` on the second iteration?
4. **Checkpoint 4**: How many iterations does it take to find the target in our example?

## Time and Space Complexity Walkthrough

### Time Complexity

- **Best case**: O(1) - Target is the middle element
- **Average case**: O(log n) - Each step eliminates half the remaining elements
- **Worst case**: O(log n) - Target is not in the array or at the first/last position

### Space Complexity

- **O(1)** - Only uses a constant amount of extra space regardless of input size

## Common Implementation Mistakes

1. **Off-by-one errors**: Using `<` instead of `<=` in the while loop condition
2. **Integer overflow**: Using `(left + right) / 2` instead of `left + (right - left) / 2` for very large arrays
3. **Infinite loops**: Not properly adjusting boundaries with `mid + 1` and `mid - 1`
4. **Wrong data type usage**: Using integer division `//` in Python vs floating-point `/` in other languages

## Mini-Challenge

1. Implement a binary search function that returns the index of the first occurrence of the target value.
2. Modify binary search to return the insertion point if the target is not found (where it would be inserted to maintain sort order).
3. Create a binary search that works on a circularly sorted array (e.g., `[4, 5, 6, 7, 0, 1, 2]`).

For the team: compare performance between binary search and linear search on arrays of different sizes.
