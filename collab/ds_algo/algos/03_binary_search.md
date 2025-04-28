# Binary Search Algorithm

## Objective

Master the Binary Search algorithm through collaborative implementation and analysis. By the end of this session, your team will understand both iterative and recursive approaches, when to use Binary Search, and its performance characteristics.

---

## Environment Setup

| Requirements          |
| :-------------------- |
| Python 3.8+           |
| Standard library only |

Verify your environment:

```bash
python --version  # Should be 3.8+
```

---

## Visual Explanation

Binary Search efficiently finds a target value in a **sorted** array by repeatedly dividing the search range in half.

```
Array: [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
Search for: 13

Step 1: mid = (0+9)/2 = 4, value = 9 < 13, so search right half
        [1, 3, 5, 7, 9, | 11, 13, 15, 17, 19]

Step 2: mid = (5+9)/2 = 7, value = 15 > 13, so search left half
        [11, 13, | 15, 17, 19]

Step 3: mid = (5+6)/2 = 5, value = 11 < 13, so search right half
        [11, | 13]

Step 4: mid = (6+6)/2 = 6, value = 13 == 13, found at index 6!
        [13]
```

Binary Search works by eliminating half of the remaining elements at each step.

---

## Algorithm Walkthrough

### Pseudocode (Iterative)

```
function BinarySearch(array, target):
    left = 0
    right = length(array) - 1

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

### Pseudocode (Recursive)

```
function BinarySearchRecursive(array, target, left, right):
    if left > right:
        return -1  # Base case: target not found

    mid = (left + right) / 2

    if array[mid] == target:
        return mid
    else if array[mid] < target:
        return BinarySearchRecursive(array, target, mid + 1, right)
    else:
        return BinarySearchRecursive(array, target, left, mid - 1)
```

### Time and Space Complexity

- **Time Complexity**: O(log n) - each step eliminates half of the remaining elements
- **Space Complexity**: O(1) for iterative, O(log n) for recursive due to call stack

---

## Annotated Code Template

### Iterative Binary Search

```python
def binary_search_iterative(arr, target):
    """
    Performs iterative binary search for target in a sorted array.

    Args:
        arr: A sorted list of elements
        target: The element to search for

    Returns:
        Index of target if found, -1 otherwise
    """
    # TODO: Implement iterative binary search
    # 1. Initialize left and right pointers
    # 2. While left <= right:
    #    a. Calculate middle index
    #    b. If element found, return index
    #    c. If element too small, search right half
    #    d. If element too large, search left half
    # 3. Return -1 if not found

    return -1
```

### Recursive Binary Search

```python
def binary_search_recursive(arr, target, left=None, right=None):
    """
    Performs recursive binary search for target in a sorted array.

    Args:
        arr: A sorted list of elements
        target: The element to search for
        left: The left boundary index (default to 0)
        right: The right boundary index (default to len(arr)-1)

    Returns:
        Index of target if found, -1 otherwise
    """
    # Initialize left and right on first call
    if left is None:
        left = 0
    if right is None:
        right = len(arr) - 1

    # TODO: Implement recursive binary search
    # 1. Check base case (left > right)
    # 2. Calculate middle index
    # 3. If element found, return index
    # 4. If element too small, search right half
    # 5. If element too large, search left half

    return -1
```

---

## Live Coding Group Activity

### Team Roles

- **Driver**: Types the code
- **Navigator**: Guides the implementation
- **Explainer**: Verbalizes what's happening at each step

### Task

Implement both the iterative and recursive versions of Binary Search by filling in the missing code. Test with these examples:

```python
# Example sorted arrays
simple_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
large_array = list(range(0, 1000000, 2))  # Even numbers from 0 to 999998

# Test cases
test_cases = [
    (simple_array, 13),    # Should return 6
    (simple_array, 1),     # Should return 0
    (simple_array, 20),    # Should return -1
    (large_array, 99998),  # Should return 49999
]

# Run tests
for arr, target in test_cases:
    print(f"Searching for {target} in array of length {len(arr)}")
    print(f"Iterative result: {binary_search_iterative(arr, target)}")
    print(f"Recursive result: {binary_search_recursive(arr, target)}")
    print()
```

### Checkpoint Questions

1. What happens if the array is not sorted?
2. How many comparisons are needed to find an element in an array of size n?
3. When would binary search be inefficient compared to linear search?

---

## Complete Implementation

After your group discussion, compare your solution with these implementations:

### Iterative Binary Search

```python
def binary_search_iterative(arr, target):
    """
    Performs iterative binary search for target in a sorted array.

    Args:
        arr: A sorted list of elements
        target: The element to search for

    Returns:
        Index of target if found, -1 otherwise
    """
    left = 0
    right = len(arr) - 1

    while left <= right:
        # Calculate middle index
        # Note: (left + right) // 2 can cause integer overflow in some languages
        # Using left + (right - left) // 2 is safer in those cases
        mid = (left + right) // 2

        # Check if target is present at mid
        if arr[mid] == target:
            return mid

        # If target greater, ignore left half
        elif arr[mid] < target:
            left = mid + 1

        # If target smaller, ignore right half
        else:
            right = mid - 1

    # Target not found
    return -1
```

### Recursive Binary Search

```python
def binary_search_recursive(arr, target, left=None, right=None):
    """
    Performs recursive binary search for target in a sorted array.

    Args:
        arr: A sorted list of elements
        target: The element to search for
        left: The left boundary index (default to 0)
        right: The right boundary index (default to len(arr)-1)

    Returns:
        Index of target if found, -1 otherwise
    """
    # Initialize left and right on first call
    if left is None:
        left = 0
    if right is None:
        right = len(arr) - 1

    # Base case: element not found
    if left > right:
        return -1

    # Calculate middle index
    mid = (left + right) // 2

    # Check if target is present at mid
    if arr[mid] == target:
        return mid

    # If target greater, search right half
    elif arr[mid] < target:
        return binary_search_recursive(arr, target, mid + 1, right)

    # If target smaller, search left half
    else:
        return binary_search_recursive(arr, target, left, mid - 1)

# Example usage
if __name__ == "__main__":
    simple_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    print(f"Array: {simple_array}")

    target = 13
    iterative_result = binary_search_iterative(simple_array, target)
    recursive_result = binary_search_recursive(simple_array, target)

    print(f"Search for {target}:")
    print(f"Iterative result: index {iterative_result}")
    print(f"Recursive result: index {recursive_result}")
```

---

## Common Implementation Mistakes

1. **Off-by-one errors**: Incorrect boundary conditions (`left <= right` vs `left < right`).
2. **Integer overflow**: When calculating the middle index in languages with fixed-size integers.
3. **Not handling duplicates**: Binary search finds _an_ occurrence, not necessarily the first/last.
4. **Using on unsorted arrays**: Binary search requires a sorted array to work correctly.
5. **Infinite loop**: Incorrectly updating left/right pointers can lead to infinite loops.

---

## Peer Discussion Prompts

1. Why is binary search so much faster than linear search for large arrays?
2. Can binary search be applied to linked lists? Why or why not?
3. How would you adapt binary search to find the first or last occurrence of a value in an array with duplicates?
4. Discuss real-world applications where binary search is used.

---

## Mini-Challenge: Rotated Binary Search

Implement a variation of binary search to find an element in a rotated sorted array (an array that is rotated at some pivot unknown to you).

```python
def search_rotated_array(arr, target):
    """
    Searches for target in a rotated sorted array.

    Example:
        arr = [4, 5, 6, 7, 0, 1, 2] was originally [0, 1, 2, 4, 5, 6, 7]
        and was rotated at index 3

    Returns:
        Index of target if found, -1 otherwise
    """
    # TODO: Implement rotated array search
    pass
```

### Extension for Fast Learners

Implement a binary search to find the pivot point (smallest element) in a rotated sorted array.

---

## Binary Search Variations

### Finding Insertion Point

When the target is not found, binary search can be modified to return the index where the element should be inserted:

```python
def binary_search_insertion_point(arr, target):
    """Returns the index where target should be inserted to maintain sorted order."""
    left = 0
    right = len(arr)

    while left < right:
        mid = (left + right) // 2

        if arr[mid] < target:
            left = mid + 1
        else:
            right = mid

    return left
```

### Finding Bounds

To find the first and last occurrence of a value in a sorted array with duplicates:

```python
def binary_search_first_occurrence(arr, target):
    """Returns the index of the first occurrence of target in a sorted array."""
    left = 0
    right = len(arr) - 1
    result = -1

    while left <= right:
        mid = (left + right) // 2

        if arr[mid] == target:
            result = mid  # Save the result
            right = mid - 1  # Continue searching left
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return result

def binary_search_last_occurrence(arr, target):
    """Returns the index of the last occurrence of target in a sorted array."""
    left = 0
    right = len(arr) - 1
    result = -1

    while left <= right:
        mid = (left + right) // 2

        if arr[mid] == target:
            result = mid  # Save the result
            left = mid + 1  # Continue searching right
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return result
```

---

## Team Reflection

As a group, discuss:

1. Why is the time complexity of binary search O(log n) and not O(n)?
2. When would you use the iterative version versus the recursive version?
3. What are the practical limitations of binary search?
4. Can you think of problems that binary search can help solve, but may not be immediately obvious?
