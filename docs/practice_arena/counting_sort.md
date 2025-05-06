# Counting Sort: Efficient Integer Sorting Algorithm

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Counting sort is a non-comparison based sorting algorithm that works well for integers within a specific range:

```
Array to sort: [4, 2, 2, 8, 3, 3, 1]

Step 1: Find the maximum value to determine count array size
max = 8, so we need count[0...8]

Step 2: Create count array and count occurrences
count[0] = 0 (0 appears 0 times)
count[1] = 1 (1 appears 1 time)
count[2] = 2 (2 appears 2 times)
count[3] = 2 (3 appears 2 times)
count[4] = 1 (4 appears 1 time)
count[5] = 0 (5 appears 0 times)
count[6] = 0 (6 appears 0 times)
count[7] = 0 (7 appears 0 times)
count[8] = 1 (8 appears 1 time)

Step 3: Modify count array to hold cumulative positions (optional in basic version)
count[0] = 0
count[1] = 0 + 1 = 1
count[2] = 1 + 2 = 3
count[3] = 3 + 2 = 5
count[4] = 5 + 1 = 6
count[5] = 6 + 0 = 6
count[6] = 6 + 0 = 6
count[7] = 6 + 0 = 6
count[8] = 6 + 1 = 7

Step 4: Build the sorted array
Basic approach: For each index i in count array, add i to output array count[i] times
Stable approach: Use cumulative count to place elements in correct positions

Final sorted array: [1, 2, 2, 3, 3, 4, 8]
```

## Pseudocode

### Basic Counting Sort

```
function counting_sort(array):
    // Find the maximum value in the array
    max_val = maximum value in array

    // Create a count array of size max_val + 1, initialize with zeros
    count = new array of size (max_val + 1) with all values set to 0

    // Count each element
    for each element in array:
        count[element] += 1

    // Create sorted array using counts
    sorted_array = empty array
    for i from 0 to max_val:
        add i to sorted_array count[i] times

    return sorted_array
```

### Stable Counting Sort

```
function stable_counting_sort(array):
    // Find the range of values
    max_val = maximum value in array

    // Create a count array of size max_val + 1, initialize with zeros
    count = new array of size (max_val + 1) with all values set to 0

    // Count each element
    for each element in array:
        count[element] += 1

    // Modify count array to store cumulative positions
    for i from 1 to max_val:
        count[i] += count[i-1]

    // Create output array of same size as input
    output = new array of same size as array

    // Place elements in their sorted positions (in reverse for stability)
    for i from length of array - 1 down to 0:
        output[count[array[i]] - 1] = array[i]
        count[array[i]] -= 1

    return output
```

## Annotated Code Template

```python
def counting_sort(arr):
    """
    Implement counting sort for an array of non-negative integers.

    Args:
        arr: A list of non-negative integers to be sorted

    Returns:
        A new sorted list
    """
    # Find the maximum value to determine count array size
    # TODO: Find the maximum value

    # Create count array and count occurrences
    # TODO: Initialize count array and count element occurrences

    # Build the sorted array
    # TODO: Create the sorted array using the count array

    return sorted_arr


def stable_counting_sort(arr):
    """
    Implement stable counting sort that preserves the relative order of equal elements.

    Args:
        arr: A list of non-negative integers to be sorted

    Returns:
        A new sorted list that maintains stability
    """
    # Find the maximum value
    # TODO: Find the maximum value

    # Create count array and count occurrences
    # TODO: Initialize count array and count element occurrences

    # Modify count array to store cumulative sum
    # TODO: Update count array with cumulative sum

    # Build the output array using the cumulative count
    # TODO: Create output array preserving stability

    return output
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm at each step

### Task

Working together, implement both the basic and stable versions of the counting sort algorithm:

1. First, implement the basic counting sort
2. Then, implement the stable counting sort
3. Test both algorithms with the same input array
4. Discuss the differences and when to use each version

### Complete Implementation

```python
def counting_sort_basic(arr):
    """Basic counting sort implementation."""
    # Check if array is empty
    if not arr:
        return []

    # Find the maximum value to determine count array size
    max_val = max(arr)

    # Create count array and count occurrences (initialize with zeros)
    count = [0] * (max_val + 1)
    for num in arr:
        count[num] += 1

    # Build the sorted array
    sorted_arr = []
    for i in range(len(count)):
        sorted_arr.extend([i] * count[i])

    return sorted_arr


def stable_counting_sort(arr):
    """Stable counting sort implementation that preserves order of equal elements."""
    # Check if array is empty
    if not arr:
        return []

    # Find the maximum value
    max_val = max(arr)

    # Create count array and count occurrences
    count = [0] * (max_val + 1)
    for num in arr:
        count[num] += 1

    # Modify count array to store cumulative sum
    for i in range(1, len(count)):
        count[i] += count[i-1]

    # Build the output array using the cumulative count (in reverse for stability)
    output = [0] * len(arr)
    for i in range(len(arr) - 1, -1, -1):
        output[count[arr[i]] - 1] = arr[i]
        count[arr[i]] -= 1

    return output


def counting_sort_with_objects(arr, key=lambda x: x):
    """
    Counting sort for arrays with objects, using a key function to extract the sort value.
    This demonstrates why stability matters.
    """
    if not arr:
        return []

    # Find the maximum key value
    max_val = max(key(item) for item in arr)

    # Create count array and count occurrences
    count = [0] * (max_val + 1)
    for item in arr:
        count[key(item)] += 1

    # Modify count array to store cumulative sum
    for i in range(1, len(count)):
        count[i] += count[i-1]

    # Build the output array using the cumulative count (in reverse for stability)
    output = [0] * len(arr)
    for i in range(len(arr) - 1, -1, -1):
        output[count[key(arr[i])] - 1] = arr[i]
        count[key(arr[i])] -= 1

    return output


# Example usage with integers
arr = [4, 2, 2, 8, 3, 3, 1]
print(f"Original array: {arr}")
print(f"Basic counting sort: {counting_sort_basic(arr)}")
print(f"Stable counting sort: {stable_counting_sort(arr)}")

# Example demonstrating stability with objects
objects = [("apple", 3), ("banana", 2), ("cherry", 2), ("date", 1), ("elderberry", 3)]
print("\nSorting objects by their numeric value:")
print(f"Original objects: {objects}")
sorted_objects = counting_sort_with_objects(objects, key=lambda x: x[1])
print(f"Sorted objects (preserving order of equal values): {sorted_objects}")
```

## Peer Discussion Prompts

1. How does the time complexity of counting sort compare to comparison-based sorting algorithms like quicksort or mergesort?
2. What are the limitations of counting sort and when should we avoid using it?
3. Why is stability important in certain applications? Give a real-world example.
4. How could counting sort be modified to work with negative integers?

## Checkpoint Questions

1. **Checkpoint 1**: What determines the size of the count array in counting sort?
2. **Checkpoint 2**: Why is the basic counting sort efficient for small ranges of integers?
3. **Checkpoint 3**: In the stable version, why do we process the input array in reverse order?
4. **Checkpoint 4**: What is the relationship between the count array and the final positions of elements?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(n + k)** where n is the number of elements and k is the range of input values
  - Finding maximum: O(n)
  - Counting occurrences: O(n)
  - Building sorted array: O(n + k)

### Space Complexity

- **O(n + k)**
  - Count array: O(k)
  - Output array: O(n)

## Common Implementation Mistakes

1. **Not handling empty arrays**: Always check if the input array is empty
2. **Using with negative numbers**: Basic implementation only works with non-negative integers
3. **Using with large ranges**: Can be inefficient if the range (k) is much larger than the array size (n)
4. **Confusing basic and stable implementations**: Using the wrong version when stability is required
5. **Forgetting to update cumulative counts**: Missing cumulative sum calculation in stable version

## Mini-Challenge

1. Modify the algorithm to work with negative integers.
2. Implement a version of counting sort that sorts by the least significant digit (for use in radix sort).
3. Create a version that works with a range [min, max] to optimize space usage.
4. Extend the implementation to sort strings lexicographically.

For the team: Compare the performance of counting sort with other sorting algorithms (e.g., quicksort, mergesort) for different input distributions and ranges.
