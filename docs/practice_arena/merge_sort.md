# Merge Sort: Divide and Conquer Sorting Algorithm

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Merge sort uses a divide-and-conquer approach to sort an array:

```
Original array: [38, 27, 43, 3, 9, 82, 10]

Step 1: Divide into subarrays
        [38, 27, 43, 3]      [9, 82, 10]
        /           \         /        \
   [38, 27]      [43, 3]   [9]      [82, 10]
    /    \        /    \     |       /     \
  [38]   [27]   [43]   [3]  [9]    [82]   [10]

Step 2: Merge sorted subarrays back together
  [38]   [27]   [43]   [3]  [9]    [82]   [10]
    \    /        \    /     |       \     /
   [27, 38]      [3, 43]   [9]      [10, 82]
        \           /         \        /
        [3, 27, 38, 43]      [9, 10, 82]
                  \               /
              [3, 9, 10, 27, 38, 43, 82]
```

## Pseudocode

```
function merge_sort(array):
    if length of array <= 1:
        return array

    mid = length of array / 2
    left_half = merge_sort(first half of array)
    right_half = merge_sort(second half of array)

    return merge(left_half, right_half)

function merge(left, right):
    result = empty array

    while left and right both have elements:
        if first element of left <= first element of right:
            append first element of left to result
            remove first element from left
        else:
            append first element of right to result
            remove first element from right

    append remaining elements of left to result
    append remaining elements of right to result

    return result
```

## Annotated Code Template

```python
def merge_sort(arr):
    """
    Sort an array using the merge sort algorithm.

    Args:
        arr: A list of comparable elements

    Returns:
        A new sorted list containing the same elements
    """
    # Base case: arrays with 0 or 1 element are already sorted
    if len(arr) <= 1:
        return arr

    # TODO: Calculate the middle index and split the array

    # TODO: Recursively sort the two halves

    # TODO: Merge the sorted halves and return the result


def merge(left, right):
    """
    Merge two sorted arrays into a single sorted array.

    Args:
        left: First sorted list
        right: Second sorted list

    Returns:
        A new sorted list containing all elements from left and right
    """
    result = []
    i = j = 0

    # TODO: Compare elements from both lists and add the smaller one to result

    # TODO: Add any remaining elements from both lists

    return result
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the recursive division and merging process

### Task

Working together, complete the merge sort implementation. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each recursive call and merge operation.

1. Start with implementing the division of the array in merge_sort
2. Complete the merge function to combine two sorted arrays
3. Test with various arrays including empty, single element, and already sorted arrays

### Complete Implementation

```python
def merge_sort(arr):
    # Base case: arrays with 0 or 1 element are already sorted
    if len(arr) <= 1:
        return arr

    # Calculate the middle index and split the array
    mid = len(arr) // 2
    left_half = merge_sort(arr[:mid])
    right_half = merge_sort(arr[mid:])

    # Merge the sorted halves and return the result
    return merge(left_half, right_half)


def merge(left, right):
    result = []
    i = j = 0

    # Compare elements from both lists and add the smaller one to result
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1

    # Add any remaining elements from both lists
    result.extend(left[i:])
    result.extend(right[j:])

    return result

# Example usage:
arr = [38, 27, 43, 3, 9, 82, 10]
sorted_arr = merge_sort(arr)
print(f"Original array: {arr}")
print(f"Sorted array: {sorted_arr}")
```

## Peer Discussion Prompts

1. How does the divide-and-conquer approach make merge sort efficient?
2. What are the advantages and disadvantages of merge sort compared to other sorting algorithms like quicksort?
3. Can you think of real-world examples where merge sort would be more appropriate than other sorting algorithms?
4. How would you modify merge sort to sort in descending order?

## Checkpoint Questions

1. **Checkpoint 1**: How many recursive calls to merge_sort are made for an array of length 7?
2. **Checkpoint 2**: At what point does the recursion stop dividing the array?
3. **Checkpoint 3**: What is the state of `left_half` and `right_half` when merging [38, 27] and [43, 3]?
4. **Checkpoint 4**: What happens in the merge function when one of the arrays is empty?

## Time and Space Complexity Walkthrough

### Time Complexity

- **Best case**: O(n log n) - Even with sorted input, merge sort still divides and merges
- **Average case**: O(n log n) - Each level of recursion does O(n) work and there are O(log n) levels
- **Worst case**: O(n log n) - Merge sort has consistent performance regardless of input

### Space Complexity

- **O(n)** - Requires additional space proportional to the input size for the temporary arrays during merging

## Common Implementation Mistakes

1. **Modifying the original array**: This implementation creates new arrays which is less efficient but easier to understand
2. **Forgetting to merge remaining elements**: Always make sure to add remaining elements from both left and right arrays
3. **Incorrect base case**: The base case should return when array length is 0 or 1
4. **Inefficient merging**: Using extend() for the remaining elements is more efficient than appending elements one by one

## Mini-Challenge

1. Implement an in-place version of merge sort that doesn't create new arrays
2. Modify merge sort to count the number of inversions in an array (pairs of elements out of order)
3. Implement merge sort for a linked list instead of an array

For the team: Compare the performance of merge sort with other sorting algorithms (quick sort, heap sort) on:

- Random arrays
- Nearly sorted arrays
- Arrays with many duplicates
