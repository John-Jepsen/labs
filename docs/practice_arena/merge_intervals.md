# Merge Intervals: Combining Overlapping Ranges

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

The merge intervals algorithm combines overlapping intervals into a single continuous interval:

```
Input intervals: [[1,3], [2,6], [8,10], [15,18]]

Step 1: Sort intervals by start time
[[1,3], [2,6], [8,10], [15,18]] (already sorted)

Step 2: Initialize result with the first interval
result = [[1,3]]

Step 3: Process remaining intervals one by one
Current interval: [2,6]
Last in result: [1,3]
Does [2,6] overlap with [1,3]? Yes (2 <= 3)
Merge them: result = [[1,6]]

Current interval: [8,10]
Last in result: [1,6]
Does [8,10] overlap with [1,6]? No (8 > 6)
Add to result: result = [[1,6], [8,10]]

Current interval: [15,18]
Last in result: [8,10]
Does [15,18] overlap with [8,10]? No (15 > 10)
Add to result: result = [[1,6], [8,10], [15,18]]

Final merged intervals: [[1,6], [8,10], [15,18]]
```

## Pseudocode

```
function merge_intervals(intervals):
    if intervals is empty:
        return empty list

    // Sort intervals by start time
    sort intervals by their start values

    // Initialize the result with the first interval
    result = [intervals[0]]

    // Process remaining intervals
    for i from 1 to length(intervals) - 1:
        current_interval = intervals[i]
        last_merged_interval = last interval in result

        // Check if current interval overlaps with last merged interval
        if current_interval.start <= last_merged_interval.end:
            // Merge by updating the end of the last interval in result
            last_merged_interval.end = max(last_merged_interval.end, current_interval.end)
        else:
            // No overlap, add current interval to result
            append current_interval to result

    return result
```

## Annotated Code Template

```python
def merge_intervals(intervals):
    """
    Merge all overlapping intervals and return the non-overlapping intervals.

    Args:
        intervals: A list of intervals where each interval is [start, end]

    Returns:
        A list of merged non-overlapping intervals
    """
    # Handle edge case
    # TODO: Check if intervals list is empty

    # Sort intervals by start time
    # TODO: Sort the intervals by their start values

    # Initialize the result with the first interval
    # TODO: Initialize result list with the first interval

    # Process remaining intervals
    # TODO: Iterate through the remaining intervals and merge overlapping ones

    return merged
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm at each step

### Task

Working together, implement the merge intervals algorithm:

1. First, sort the intervals by their start times
2. Then, process intervals one by one and merge those that overlap
3. Test the implementation with different examples
4. Discuss how to handle edge cases like empty input, single interval, or intervals with negative values

### Complete Implementation

```python
def merge_intervals(intervals):
    """
    Merge all overlapping intervals and return the non-overlapping intervals.
    """
    # Handle edge case
    if not intervals:
        return []

    # Sort intervals by start time
    intervals.sort(key=lambda x: x[0])

    # Initialize the result with the first interval
    merged = [intervals[0]]

    # Process remaining intervals
    for current in intervals[1:]:
        # Get the last interval in the merged list
        last = merged[-1]

        # Check if current interval overlaps with last merged interval
        if current[0] <= last[1]:
            # Merge by updating the end of the last interval
            last[1] = max(last[1], current[1])
        else:
            # No overlap, add current interval to result
            merged.append(current)

    return merged


def trace_merge_intervals(intervals):
    """
    Trace through merge intervals execution step by step for demonstration.
    """
    print(f"Original intervals: {intervals}")

    if not intervals:
        print("Empty input, returning empty list.")
        return []

    # Sort intervals by start time
    intervals.sort(key=lambda x: x[0])
    print(f"\nStep 1: Sort intervals by start time\n{intervals}")

    # Initialize the result with the first interval
    merged = [intervals[0]]
    print(f"\nStep 2: Initialize result with the first interval\nresult = {merged}")

    # Process remaining intervals
    print("\nStep 3: Process remaining intervals one by one")
    for current in intervals[1:]:
        last = merged[-1]

        print(f"  Current interval: {current}")
        print(f"  Last in result: {last}")

        if current[0] <= last[1]:
            print(f"  Does {current} overlap with {last}? Yes ({current[0]} <= {last[1]})")
            last[1] = max(last[1], current[1])
            print(f"  Merge them: result = {merged}")
        else:
            print(f"  Does {current} overlap with {last}? No ({current[0]} > {last[1]})")
            merged.append(current)
            print(f"  Add to result: result = {merged}")

        print("")

    print(f"Final merged intervals: {merged}")
    return merged


# Example usage
intervals1 = [[1, 3], [2, 6], [8, 10], [15, 18]]
print("Example 1:")
result1 = merge_intervals(intervals1)
print(f"Merged intervals: {result1}\n")

print("Example 1 with tracing:")
trace_merge_intervals(intervals1.copy())

# Additional examples
print("\nExample 2 (overlapping all):")
intervals2 = [[1, 4], [4, 5], [5, 10], [10, 20]]
result2 = merge_intervals(intervals2)
print(f"Merged intervals: {result2}")

print("\nExample 3 (no overlapping):")
intervals3 = [[1, 2], [3, 4], [5, 6], [7, 8]]
result3 = merge_intervals(intervals3)
print(f"Merged intervals: {result3}")

print("\nExample 4 (complete overlap):")
intervals4 = [[1, 10], [2, 5], [3, 7], [4, 6]]
result4 = merge_intervals(intervals4)
print(f"Merged intervals: {result4}")

print("\nExample 5 (edge case - empty input):")
intervals5 = []
result5 = merge_intervals(intervals5)
print(f"Merged intervals: {result5}")

print("\nExample 6 (edge case - single interval):")
intervals6 = [[1, 5]]
result6 = merge_intervals(intervals6)
print(f"Merged intervals: {result6}")
```

## Peer Discussion Prompts

1. How would you modify the algorithm to handle intervals with different formats (e.g., objects with start/end properties)?
2. Can you think of real-world applications where merging intervals is useful?
3. How would the algorithm change if we needed to find the total covered length instead of the merged intervals?
4. What if we needed to find the intersection of multiple sets of intervals instead of the union?

## Checkpoint Questions

1. **Checkpoint 1**: Why do we need to sort the intervals before merging them?
2. **Checkpoint 2**: What is the significance of checking if `current[0] <= last[1]`?
3. **Checkpoint 3**: Why do we use `max(last[1], current[1])` when merging intervals?
4. **Checkpoint 4**: How many intervals will remain after merging if all input intervals overlap?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(n log n)** where n is the number of intervals
  - Sorting the intervals takes O(n log n) time
  - Merging the intervals takes O(n) time as we iterate through each interval once

### Space Complexity

- **O(n)** for storing the merged intervals
  - In the worst case (no overlapping intervals), we store all n intervals
  - Sorting may require O(n) additional space depending on the implementation

## Common Implementation Mistakes

1. **Not sorting intervals**: The algorithm relies on processing intervals in order of start times
2. **Incorrect overlap check**: Forgetting the equal case in `current[0] <= last[1]`
3. **Not updating the end correctly**: Using the current interval's end instead of the maximum end
4. **Not handling edge cases**: Empty array, single interval, or intervals with negative values
5. **Mutating input data**: Modifying the input intervals array directly instead of creating a new result

## Mini-Challenge

1. Implement a function to find the total length covered by all intervals (accounting for overlaps).
2. Modify the algorithm to handle intervals with different formats (e.g., (start, end) tuples or objects).
3. Create a function to find the complement of the merged intervals within a given range.
4. Implement an algorithm to find the intersection of two sets of intervals.

For the team: Consider how to optimize the algorithm for specific use cases, such as when intervals are already sorted or when there are many overlapping intervals.
