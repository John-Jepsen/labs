# Heap Sort: Efficient Sorting with Binary Heaps

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Heap sort uses a binary heap data structure to sort elements:

```
Original array: [12, 11, 13, 5, 6, 7]

Step 1: Build a max heap
   Initial array representation as a binary tree:
        12
       /  \
     11    13
    / \    /
   5   6  7

   After heapify:
        13
       /  \
     11    12
    / \    /
   5   6  7

   Final max heap:
        13
       /  \
     11    12
    / \    /
   5   6  7

Step 2: Extract elements one by one
   1. Swap root (13) with last element (7)
      [7, 11, 12, 5, 6, 13]

      Heapify remaining elements:
           12
          /  \
        11    7
       / \
      5   6

   2. Swap root (12) with last element (6)
      [6, 11, 7, 5, 12, 13]

      Heapify remaining elements:
           11
          /  \
         6    7
        /
       5

   3. Swap root (11) with last element (5)
      [5, 6, 7, 11, 12, 13]

      Heapify remaining elements:
           7
          / \
         6   5

   4. Swap root (7) with last element (6)
      [6, 5, 7, 11, 12, 13]

      Heapify remaining elements:
           6
          /
         5

   5. Swap root (6) with last element (5)
      [5, 6, 7, 11, 12, 13]

   Final sorted array: [5, 6, 7, 11, 12, 13]
```

## Pseudocode

```
function heapify(array, size, root_index):
    largest = root_index
    left_child = 2 * root_index + 1
    right_child = 2 * root_index + 2

    if left_child < size and array[left_child] > array[largest]:
        largest = left_child

    if right_child < size and array[right_child] > array[largest]:
        largest = right_child

    if largest != root_index:
        swap array[root_index] with array[largest]
        heapify(array, size, largest)

function heap_sort(array):
    n = length of array

    // Build max heap
    for i from n/2-1 down to 0:
        heapify(array, n, i)

    // Extract elements one by one
    for i from n-1 down to 1:
        swap array[0] with array[i]
        heapify(array, i, 0)

    return array
```

## Annotated Code Template

```python
def heapify(arr, n, i):
    """
    Heapify a subtree rooted at index i.

    Args:
        arr: Array representation of heap
        n: Size of the heap
        i: Index of the subtree root
    """
    # TODO: Find largest among root, left child and right child

    # TODO: If largest is not root, swap and continue heapifying

def heap_sort(arr):
    """
    Sort an array using heap sort.

    Args:
        arr: Array to be sorted

    Returns:
        Sorted array
    """
    n = len(arr)

    # TODO: Build a max heap

    # TODO: Extract elements one by one

    return arr
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the heap operations and sorting process

### Task

Working together, complete the heap sort implementation. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each step of the algorithm.

1. Start with implementing the heapify function
2. Build the max heap from the input array
3. Extract elements one by one to get the sorted array
4. Test with various arrays and trace through the execution

### Complete Implementation

```python
def heapify(arr, n, i):
    """
    Heapify a subtree rooted at index i.

    Args:
        arr: Array representation of heap
        n: Size of the heap
        i: Index of the subtree root
    """
    # Initialize largest as root
    largest = i
    left = 2 * i + 1
    right = 2 * i + 2

    # See if left child of root exists and is greater than root
    if left < n and arr[left] > arr[largest]:
        largest = left

    # See if right child of root exists and is greater than largest so far
    if right < n and arr[right] > arr[largest]:
        largest = right

    # Change root if needed
    if largest != i:
        arr[i], arr[largest] = arr[largest], arr[i]  # Swap

        # Heapify the affected sub-tree
        heapify(arr, n, largest)

def heap_sort(arr):
    """
    Sort an array using heap sort.

    Args:
        arr: Array to be sorted

    Returns:
        Sorted array
    """
    n = len(arr)

    # Build a max heap
    # Start from the last non-leaf node and heapify all nodes in reverse order
    for i in range(n // 2 - 1, -1, -1):
        heapify(arr, n, i)

    # Extract elements one by one
    for i in range(n - 1, 0, -1):
        # Swap the root (maximum element) with the last element
        arr[i], arr[0] = arr[0], arr[i]

        # Heapify the reduced heap
        heapify(arr, i, 0)

    return arr

# Example usage:
arr = [12, 11, 13, 5, 6, 7]
sorted_arr = heap_sort(arr.copy())  # Create a copy to preserve original
print(f"Original array: {arr}")
print(f"Sorted array: {sorted_arr}")

# Trace through the algorithm with a simple example
arr = [4, 10, 3, 5, 1]
print("\nTracing heap sort on array:", arr)

# Build max heap
print("\nBuilding max heap:")
n = len(arr)
for i in range(n // 2 - 1, -1, -1):
    print(f"Heapifying subtree rooted at index {i}")
    heapify(arr, n, i)
    print(f"Array after heapify: {arr}")

# Extract elements
print("\nExtracting elements:")
for i in range(n - 1, 0, -1):
    print(f"Swap {arr[0]} with {arr[i]}")
    arr[i], arr[0] = arr[0], arr[i]
    print(f"Heapify reduced heap of size {i}")
    heapify(arr, i, 0)
    print(f"Array after heapify: {arr}")

print(f"\nFinal sorted array: {arr}")
```

## Peer Discussion Prompts

1. How does heap sort compare to other sorting algorithms like quicksort and merge sort in terms of performance?
2. What are the advantages of using heap sort over other comparison-based sorting algorithms?
3. In what scenarios might heap sort be preferred over other sorting algorithms?
4. How would you modify this algorithm to sort in descending order?

## Checkpoint Questions

1. **Checkpoint 1**: Why do we start building the heap from the middle of the array?
2. **Checkpoint 2**: What is the purpose of the heapify function?
3. **Checkpoint 3**: How many swaps are performed during the extraction phase for an array of length n?
4. **Checkpoint 4**: Why is heap sort considered an in-place sorting algorithm?

## Time and Space Complexity Walkthrough

### Time Complexity

- **Building the heap**: O(n)
  - Common misconception: Many assume this is O(n log n), but it's actually O(n) when done bottom-up
- **Extraction phase**: O(n log n)
  - Each extraction takes O(log n) time
  - We perform n-1 extractions
- **Overall**: O(n log n) for all cases (best, average, worst)

### Space Complexity

- **O(1)** auxiliary space - heap sort is an in-place sorting algorithm
- The recursion in heapify can use O(log n) stack space, but this can be avoided with an iterative implementation

## Common Implementation Mistakes

1. **Incorrect heap property**: Confusing min-heap and max-heap implementations
2. **Off-by-one errors**: Incorrect calculation of child indices
3. **Not using zero-based indexing**: Heap implementation assumes array indices start at 0
4. **Forgetting to heapify after swaps**: Each extraction requires reheapifying the heap

## Mini-Challenge

1. Implement heap sort using a min-heap instead of a max-heap
2. Modify the algorithm to sort only the k largest elements, leaving the rest of the array unsorted
3. Extend the implementation to work with objects that have a custom comparison function
4. Create a visualization that shows the heap structure at each step of the algorithm

For the team:

- Implement a priority queue using a heap data structure
- Compare the performance of heap sort with quicksort and merge sort on various types of input
- Analyze how heap sort performs on partially sorted arrays
- Discuss real-world applications where heap data structures are used
