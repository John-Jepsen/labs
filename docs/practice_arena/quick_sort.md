# Quick Sort: Efficient Divide and Conquer Sorting

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Quick sort selects a pivot element and partitions the array around it:

```
Original array: [3, 6, 8, 10, 1, 2, 1]

Step 1: Choose a pivot (8)
        [3, 6, 8, 10, 1, 2, 1]
              ^pivot

Step 2: Partition around pivot
        Elements < 8: [3, 6, 1, 2, 1]
        Pivot: [8]
        Elements > 8: [10]

Step 3: Recursively sort subarrays
        Sort [3, 6, 1, 2, 1]:
          Choose pivot (1): [1, 1] < [3] < [6, 2]
            Sort [1, 1]: Already sorted
            Sort [6, 2]:
              Choose pivot (6): [2] < [6]
                Sort [2]: Already sorted

        Sort [10]: Already sorted (single element)

Step 4: Combine sorted arrays
        [1, 1, 2, 3, 6] + [8] + [10] = [1, 1, 2, 3, 6, 8, 10]
```

## Pseudocode

```
function quick_sort(array):
    if length of array <= 1:
        return array

    pivot = select pivot element from array

    less = [elements in array less than pivot]
    equal = [elements in array equal to pivot]
    greater = [elements in array greater than pivot]

    return quick_sort(less) + equal + quick_sort(greater)
```

## Annotated Code Template

```python
def quick_sort(arr):
    """
    Sort an array using the quick sort algorithm.

    Args:
        arr: A list of comparable elements

    Returns:
        A sorted list containing the same elements
    """
    # Base case: arrays with 0 or 1 element are already sorted
    if len(arr) <= 1:
        return arr

    # TODO: Select a pivot element

    # TODO: Create partitions of elements less than, equal to, and greater than pivot

    # TODO: Recursively sort partitions and combine them
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the partitioning process and recursive calls

### Task

Working together, complete the quick sort implementation. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each partitioning step and recursive call.

1. Start with implementing the pivot selection
2. Create the three partitions (less, equal, greater)
3. Implement the recursive sorting and combination of partitions
4. Test with various arrays including empty, already sorted, and arrays with duplicates

### Complete Implementation

```python
def quick_sort(arr):
    # Base case: arrays with 0 or 1 element are already sorted
    if len(arr) <= 1:
        return arr

    # Select a pivot element (middle element in this case)
    pivot = arr[len(arr) // 2]

    # Create partitions of elements less than, equal to, and greater than pivot
    less = [x for x in arr if x < pivot]
    equal = [x for x in arr if x == pivot]
    greater = [x for x in arr if x > pivot]

    # Recursively sort partitions and combine them
    return quick_sort(less) + equal + quick_sort(greater)

# Example usage:
arr = [3, 6, 8, 10, 1, 2, 1]
sorted_arr = quick_sort(arr)
print(f"Original array: {arr}")
print(f"Sorted array: {sorted_arr}")
```

## Peer Discussion Prompts

1. How does the choice of pivot affect the performance of quicksort?
2. Why is quicksort often faster in practice than merge sort despite having the same average time complexity?
3. What are the worst-case inputs for quicksort, and how can we avoid them?
4. How would you implement an in-place version of quicksort that doesn't create new arrays?

## Checkpoint Questions

1. **Checkpoint 1**: What happens if we always choose the first or last element as the pivot?
2. **Checkpoint 2**: In our example, how many recursive calls to quick_sort are made?
3. **Checkpoint 3**: What is the state of less, equal, and greater after the first partition?
4. **Checkpoint 4**: How does quick sort handle duplicate elements in the array?

## Time and Space Complexity Walkthrough

### Time Complexity

- **Best case**: O(n log n) - When the pivot divides the array into nearly equal halves
- **Average case**: O(n log n) - With random pivot selection, this is the expected performance
- **Worst case**: O(n²) - When the pivot is always the smallest or largest element (e.g., with sorted array)

### Space Complexity

- **O(n)** - For this implementation due to creating new arrays for partitions
- **O(log n)** - For call stack space in the average case
- **O(n)** - For call stack space in the worst case

## Common Implementation Mistakes

1. **Poor pivot selection**: Always choosing the first or last element can lead to worst-case performance
2. **Inefficient partitioning**: This implementation creates new arrays which is less efficient than in-place partitioning
3. **Ignoring small arrays**: For very small arrays, insertion sort may be more efficient
4. **Duplicate handling**: Make sure to handle duplicate elements correctly (using equal partition)

## Mini-Challenge

1. Implement an in-place version of quicksort using the Lomuto or Hoare partitioning scheme
2. Modify the pivot selection to use the "median-of-three" approach (first, middle, last elements)
3. Create a hybrid sorting algorithm that uses quicksort for large arrays and insertion sort for small subarrays

For the team: Implement both quicksort and merge sort, then:

- Compare their performance on different types of input
- Analyze when each algorithm performs better
- Create a visualization showing the number of comparisons each makes on the same input
