# Radix Sort: Digit-by-Digit Sorting Algorithm

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Radix sort is a non-comparative sorting algorithm that sorts integers by processing individual digits from least significant to most significant:

```
Array to sort: [170, 45, 75, 90, 802, 24, 2, 66]

Step 1: Sort by the least significant digit (1's place)
Group by last digit:
Bucket 0: [170, 90]
Bucket 2: [802, 2]
Bucket 4: [24]
Bucket 5: [45, 75]
Bucket 6: [66]
Bucket 7, 8, 9: []

After first pass: [170, 90, 802, 2, 24, 45, 75, 66]

Step 2: Sort by the second digit (10's place)
Bucket 0: [802, 2]
Bucket 1: []
Bucket 2: [24]
Bucket 3: []
Bucket 4: [45]
Bucket 5, 6: []
Bucket 7: [75, 170]
Bucket 8: []
Bucket 9: [90]

After second pass: [802, 2, 24, 45, 75, 170, 90, 66]
Note: 66 goes to bucket 6

Step 3: Sort by the third digit (100's place)
Bucket 0: [2, 24, 45, 75, 66, 90]
Bucket 1: [170]
Bucket 2, 3, 4, 5, 6, 7: []
Bucket 8: [802]
Bucket 9: []

After third pass: [2, 24, 45, 66, 75, 90, 170, 802]

Final sorted array: [2, 24, 45, 66, 75, 90, 170, 802]
```

## Pseudocode

```
function counting_sort_for_radix(array, exp):
    n = length of array
    output = new array of size n
    count = new array of size 10, all initialized to 0

    // Count the occurrences of each digit at position exp
    for i from 0 to n-1:
        digit = (array[i] / exp) % 10
        count[digit] += 1

    // Update count to contain actual position in output
    for i from 1 to 9:
        count[i] += count[i-1]

    // Build the output array (in reverse for stability)
    for i from n-1 down to 0:
        digit = (array[i] / exp) % 10
        output[count[digit] - 1] = array[i]
        count[digit] -= 1

    // Copy back to original array
    copy output to array

function radix_sort(array):
    max_val = maximum value in array
    exp = 1

    // Do counting sort for every digit
    while max_val / exp > 0:
        counting_sort_for_radix(array, exp)
        exp *= 10

    return array
```

## Annotated Code Template

```python
def counting_sort_for_radix(arr, exp):
    """
    Stable counting sort used as a subroutine in radix sort.
    Sorts the array based on the digit at the given place value.

    Args:
        arr: The array to be sorted
        exp: The current place value (1's, 10's, 100's, etc.)

    Returns:
        None (sorts the array in-place)
    """
    n = len(arr)

    # Initialize output array and count array for digits 0-9
    # TODO: Initialize output and count arrays

    # Count occurrences of each digit at position 'exp'
    # TODO: Count occurrences based on the digit at position 'exp'

    # Update count to store the position of each digit in output array
    # TODO: Make the count array cumulative

    # Build the output array in reverse for stability
    # TODO: Place each element in its correct position in the output array

    # Copy the output array back to the original array
    # TODO: Copy output array to original array


def radix_sort(arr):
    """
    Implement radix sort for positive integers.

    Args:
        arr: A list of positive integers to be sorted

    Returns:
        The sorted list
    """
    # Find the maximum number to know the number of digits
    # TODO: Find the maximum value

    # Do counting sort for every digit, starting from least significant
    # TODO: Perform counting sort for each decimal place

    return arr
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm at each step

### Task

Working together, implement the radix sort algorithm:

1. First, implement the counting sort subroutine for a specific digit
2. Then, implement the main radix sort function to sort by each digit
3. Test the algorithm with different input arrays
4. Trace through the execution with a small example to verify correctness

### Complete Implementation

```python
def counting_sort_for_radix(arr, exp):
    """
    Stable counting sort used as a subroutine in radix sort.
    Sorts the array based on the digit at the given place value.
    """
    n = len(arr)

    # Initialize output array and count array for digits 0-9
    output = [0] * n
    count = [0] * 10

    # Count occurrences of each digit at position 'exp'
    for i in range(n):
        digit = (arr[i] // exp) % 10
        count[digit] += 1

    # Update count to store the position of each digit in output array
    for i in range(1, 10):
        count[i] += count[i - 1]

    # Build the output array in reverse for stability
    for i in range(n - 1, -1, -1):
        digit = (arr[i] // exp) % 10
        output[count[digit] - 1] = arr[i]
        count[digit] -= 1

    # Copy the output array back to the original array
    for i in range(n):
        arr[i] = output[i]


def radix_sort(arr):
    """
    Implement radix sort for positive integers.
    """
    # Check if array is empty
    if not arr:
        return arr

    # Find the maximum number to know the number of digits
    max_val = max(arr)

    # Do counting sort for every digit, starting from least significant
    exp = 1
    while max_val // exp > 0:
        counting_sort_for_radix(arr, exp)
        exp *= 10

    return arr


def trace_radix_sort(arr):
    """
    Trace through radix sort execution step by step for demonstration.
    """
    print(f"Original array: {arr}")

    # Find max value
    max_val = max(arr)

    # Process each digit
    exp = 1
    pass_number = 1

    while max_val // exp > 0:
        print(f"\nPass {pass_number} - Sort by {exp}'s place:")

        # Group elements by current digit
        buckets = [[] for _ in range(10)]
        for num in arr:
            digit = (num // exp) % 10
            buckets[digit].append(num)

        # Print buckets
        for i, bucket in enumerate(buckets):
            if bucket:
                print(f"  Bucket {i}: {bucket}")

        # Perform the actual sorting for this pass
        counting_sort_for_radix(arr, exp)
        print(f"  After pass {pass_number}: {arr}")

        # Move to next digit
        exp *= 10
        pass_number += 1

    print(f"\nFinal sorted array: {arr}")


# Example usage
arr1 = [170, 45, 75, 90, 802, 24, 2, 66]
arr2 = arr1.copy()  # Make a copy for the trace function

print("Regular execution:")
radix_sort(arr1)
print(f"Sorted array: {arr1}")

print("\nStep-by-step execution:")
trace_radix_sort(arr2)

# Test with larger numbers and more digits
arr3 = [18903, 4, 7892, 56, 429, 1001, 5532, 10]
print("\nSorting array with larger numbers:")
radix_sort(arr3)
print(f"Sorted array: {arr3}")
```

## Peer Discussion Prompts

1. How does radix sort achieve linear time complexity even though it sorts the entire array multiple times?
2. Compare radix sort with counting sort. When would you prefer one over the other?
3. Discuss how radix sort can be adapted to sort strings or other non-integer data.
4. Could radix sort be parallelized? How might this improve performance?

## Checkpoint Questions

1. **Checkpoint 1**: Why does radix sort start with the least significant digit instead of the most significant?
2. **Checkpoint 2**: Why is it important that the counting sort used within radix sort is stable?
3. **Checkpoint 3**: How many passes does radix sort make for an array where the maximum value has d digits?
4. **Checkpoint 4**: What is the time complexity of radix sort, and how does the number of digits affect it?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(d × (n + k))** where:
  - n is the number of elements
  - k is the range of the digit (10 for decimal)
  - d is the number of digits in the maximum value
- For most practical cases, k is a small constant (usually 10), and d can be considered O(log n), so the time complexity is often described as O(n log n)

### Space Complexity

- **O(n + k)**
  - Output array: O(n)
  - Count array: O(k) (typically O(10) for decimal)

## Common Implementation Mistakes

1. **Not using a stable sort**: The digit-by-digit sorting must maintain relative order of elements with the same digit
2. **Incorrect digit extraction**: Confusing the formula for extracting a specific digit
3. **Not handling edge cases**: Empty arrays or arrays with only one element
4. **Incorrect base assumption**: Assuming base 10 when the algorithm can work with any base
5. **Memory management**: Creating too many temporary arrays and causing unnecessary memory overhead

## Mini-Challenge

1. Modify the algorithm to work with negative integers.
2. Implement a version of radix sort that uses base 16 (hexadecimal) instead of base 10.
3. Adapt the algorithm to sort strings of varying lengths.
4. Create a most-significant-digit (MSD) radix sort and compare its performance with the least-significant-digit (LSD) version implemented above.

For the team: Compare the performance of radix sort with quicksort and mergesort for arrays of varying sizes and distributions.
