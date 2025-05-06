# Bucket Sort: Distribution-Based Sorting Algorithm

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Bucket sort distributes elements into a fixed number of buckets, sorts each bucket individually, and then combines them:

```
Array to sort: [0.78, 0.17, 0.39, 0.26, 0.72, 0.94, 0.21, 0.12, 0.23, 0.68]

Step 1: Create n empty buckets (n = 10 in this example)
Bucket 0: []
Bucket 1: []
...
Bucket 9: []

Step 2: Distribute elements into buckets based on their value
For each value, calculate bucket index: index = int(value * n)

0.78 → Bucket 7: [0.78]
0.17 → Bucket 1: [0.17]
0.39 → Bucket 3: [0.39]
0.26 → Bucket 2: [0.26]
0.72 → Bucket 7: [0.78, 0.72]
0.94 → Bucket 9: [0.94]
0.21 → Bucket 2: [0.26, 0.21]
0.12 → Bucket 1: [0.17, 0.12]
0.23 → Bucket 2: [0.26, 0.21, 0.23]
0.68 → Bucket 6: [0.68]

Final buckets:
Bucket 0: []
Bucket 1: [0.17, 0.12]
Bucket 2: [0.26, 0.21, 0.23]
Bucket 3: [0.39]
Bucket 4: []
Bucket 5: []
Bucket 6: [0.68]
Bucket 7: [0.78, 0.72]
Bucket 8: []
Bucket 9: [0.94]

Step 3: Sort each bucket individually
Bucket 1: [0.12, 0.17]
Bucket 2: [0.21, 0.23, 0.26]
Bucket 3: [0.39]
Bucket 6: [0.68]
Bucket 7: [0.72, 0.78]
Bucket 9: [0.94]

Step 4: Concatenate all buckets
Sorted array: [0.12, 0.17, 0.21, 0.23, 0.26, 0.39, 0.68, 0.72, 0.78, 0.94]
```

## Pseudocode

```
function bucket_sort(array):
    n = length of array
    if n <= 1:
        return array

    // Create n empty buckets
    buckets = array of n empty lists

    // Distribute elements into buckets
    max_value = maximum value in array
    for each value in array:
        index = floor(value / max_value * (n - 1))
        add value to buckets[index]

    // Sort each bucket
    for each bucket in buckets:
        sort bucket using another algorithm (e.g., insertion sort)

    // Concatenate all buckets
    result = empty list
    for each bucket in buckets:
        append all elements in bucket to result

    return result
```

## Annotated Code Template

```python
def bucket_sort(arr):
    """
    Implement bucket sort for an array of floating-point numbers in the range [0.0, 1.0).

    Args:
        arr: A list of floating-point numbers to be sorted

    Returns:
        A new sorted list
    """
    # Check for edge cases
    # TODO: Handle empty arrays or single-element arrays

    # Create buckets
    # TODO: Create n empty buckets (usually n equals the length of the array)

    # Distribute elements into buckets
    # TODO: Distribute each element to the appropriate bucket based on its value

    # Sort each bucket
    # TODO: Sort the elements in each non-empty bucket

    # Concatenate all buckets to get the sorted array
    # TODO: Combine all buckets in order

    return sorted_arr


def bucket_sort_generic(arr, min_val=None, max_val=None, num_buckets=None):
    """
    Implement a more generic bucket sort that can handle any range of values.

    Args:
        arr: A list of numbers to be sorted
        min_val: The minimum value in the array (calculated if None)
        max_val: The maximum value in the array (calculated if None)
        num_buckets: Number of buckets to use (defaults to length of array if None)

    Returns:
        A new sorted list
    """
    # TODO: Implement a more general bucket sort
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm at each step

### Task

Working together, implement two versions of the bucket sort algorithm:

1. First, implement the basic bucket sort for floating-point numbers in the range [0,1)
2. Then, implement a more generic version that handles any range of values
3. Test both algorithms with different input arrays
4. Discuss how the choice of bucket count affects performance

### Complete Implementation

```python
def bucket_sort(arr):
    """
    Basic bucket sort implementation for floating-point numbers in the range [0.0, 1.0).
    """
    # Check for edge cases
    if len(arr) <= 1:
        return arr.copy()

    # Create buckets (one for each element)
    n = len(arr)
    buckets = [[] for _ in range(n)]

    # Distribute elements into buckets
    for value in arr:
        index = min(int(n * value), n - 1)  # Ensure index is within bounds
        buckets[index].append(value)

    # Sort each bucket
    for i in range(n):
        buckets[i].sort()  # Using Python's built-in sort

    # Concatenate all buckets
    sorted_arr = []
    for bucket in buckets:
        sorted_arr.extend(bucket)

    return sorted_arr


def bucket_sort_generic(arr, min_val=None, max_val=None, num_buckets=None):
    """
    Generic bucket sort implementation that can handle any range of values.
    """
    # Check for edge cases
    if len(arr) <= 1:
        return arr.copy()

    # Determine min and max values if not provided
    if min_val is None:
        min_val = min(arr)
    if max_val is None:
        max_val = max(arr)

    # Determine number of buckets if not provided
    if num_buckets is None:
        num_buckets = len(arr)

    # Handle case where all elements are the same
    if max_val == min_val:
        return arr.copy()

    # Create buckets
    buckets = [[] for _ in range(num_buckets)]

    # Distribute elements into buckets
    for value in arr:
        # Calculate bucket index
        index = min(int((value - min_val) / (max_val - min_val) * (num_buckets - 1)),
                   num_buckets - 1)
        buckets[index].append(value)

    # Sort each bucket
    for i in range(num_buckets):
        buckets[i].sort()  # Can replace with insertion sort or other algorithm

    # Concatenate all buckets
    sorted_arr = []
    for bucket in buckets:
        sorted_arr.extend(bucket)

    return sorted_arr


def trace_bucket_sort(arr):
    """
    Trace through bucket sort execution step by step for demonstration.
    """
    print(f"Original array: {arr}")

    # Create buckets
    n = len(arr)
    buckets = [[] for _ in range(n)]
    print(f"\nStep 1: Created {n} empty buckets")

    # Distribute elements into buckets
    print("\nStep 2: Distributing elements into buckets")
    for value in arr:
        index = min(int(n * value), n - 1)
        buckets[index].append(value)
        print(f"  Placed {value} into Bucket {index}")

    # Show final bucket distribution
    print("\nFinal bucket distribution:")
    for i, bucket in enumerate(buckets):
        if bucket:
            print(f"  Bucket {i}: {bucket}")

    # Sort each bucket
    print("\nStep 3: Sorting each bucket")
    for i in range(n):
        if buckets[i]:
            print(f"  Bucket {i} before sorting: {buckets[i]}")
            buckets[i].sort()
            print(f"  Bucket {i} after sorting: {buckets[i]}")

    # Concatenate buckets
    print("\nStep 4: Concatenating all buckets")
    sorted_arr = []
    for bucket in buckets:
        sorted_arr.extend(bucket)

    print(f"\nFinal sorted array: {sorted_arr}")
    return sorted_arr


# Example usage with uniformly distributed values
arr1 = [0.78, 0.17, 0.39, 0.26, 0.72, 0.94, 0.21, 0.12, 0.23, 0.68]
arr1_copy = arr1.copy()  # Make a copy for the trace function

print("Regular execution:")
sorted_arr1 = bucket_sort(arr1)
print(f"Sorted array: {sorted_arr1}")

print("\nStep-by-step execution:")
trace_bucket_sort(arr1_copy)

# Example with generic values (not in the range [0,1])
arr2 = [29, 4, 72, 18, 53, 91, 42, 6, 81, 33]
print("\nSorting array with generic values:")
sorted_arr2 = bucket_sort_generic(arr2)
print(f"Sorted array: {sorted_arr2}")

# Example with different bucket counts
arr3 = [0.78, 0.17, 0.39, 0.26, 0.72, 0.94, 0.21, 0.12, 0.23, 0.68]
print("\nUsing different bucket counts:")
sorted_arr3_5 = bucket_sort_generic(arr3, num_buckets=5)
sorted_arr3_20 = bucket_sort_generic(arr3, num_buckets=20)
print(f"With 5 buckets:  {sorted_arr3_5}")
print(f"With 20 buckets: {sorted_arr3_20}")
```

## Peer Discussion Prompts

1. How does the distribution of input data affect the performance of bucket sort?
2. Why might bucket sort be faster than comparison-based sorts like quicksort in some cases?
3. What factors should you consider when choosing the number of buckets?
4. How does bucket sort relate to other distribution sorts like counting sort and radix sort?

## Checkpoint Questions

1. **Checkpoint 1**: Why is bucket sort particularly effective for uniformly distributed data?
2. **Checkpoint 2**: How does the choice of the sorting algorithm for each bucket affect overall performance?
3. **Checkpoint 3**: What happens if all elements end up in the same bucket?
4. **Checkpoint 4**: In what scenarios might bucket sort perform worse than comparison-based sorts?

## Time and Space Complexity Walkthrough

### Time Complexity

- **Average case**: O(n + k) where n is the number of elements and k is the number of buckets

  - When k ≈ n and data is uniformly distributed
  - Each bucket contains approximately n/k elements, which is O(1) per bucket
  - Sorting each bucket takes O(1) \* O(log(1)) = O(1) time
  - Distributing and combining takes O(n) time

- **Worst case**: O(n²)
  - Occurs when all elements are placed in a single bucket
  - Sorting a single bucket with n elements takes O(n log n) or O(n²) depending on the sorting algorithm used

### Space Complexity

- **O(n + k)**
  - Storage for n elements across k buckets

## Common Implementation Mistakes

1. **Incorrect bucket index calculation**: Not properly mapping values to bucket indices
2. **Not handling edge cases**: Empty arrays, single-element arrays, or arrays with identical elements
3. **Out-of-range indices**: Not ensuring bucket indices are within valid range
4. **Inefficient bucket sorting**: Using an inefficient algorithm to sort individual buckets
5. **Too few or too many buckets**: Not choosing an appropriate number of buckets for the data distribution

## Mini-Challenge

1. Modify the algorithm to dynamically determine the optimal number of buckets based on the input data.
2. Implement a version that uses insertion sort for each bucket instead of the built-in sort.
3. Create a version that works efficiently with integer data of a large range.
4. Adapt the algorithm to handle string data by distributing based on the first character.

For the team: Conduct experiments to determine how the number of buckets and the distribution of data affect the performance of bucket sort compared to other sorting algorithms.
