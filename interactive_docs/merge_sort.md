### Algorithm: Merge Sort

Sir Cedric and Ember needed to sort the Sacred Gems of Algoria to unlock the Portal of Time and reach Princess Elara. The gems had to be arranged in ascending order using the Merge Sort algorithm.

#### Divide the Gems:

- Sir Cedric divided the gems into smaller groups.

#### Conquer:

- He sorted each group individually.

#### Combine:

- Sir Cedric merged the sorted groups to form the final sequence.

#### Implementation:

```py
def merge_sort(gems: List[int]) -> List[int]:
    if len(gems) <= 1:
        return gems

    mid = len(gems) // 2
    left_half = merge_sort(gems[:mid])
    right_half = merge_sort(gems[mid:])

    return merge(left_half, right_half)

def merge(left: List[int], right: List[int]) -> List[int]:
    sorted_gems = []
    i = j = 0

    while i < len(left) and j < len(right):
        if left[i] < right[j]:
            sorted_gems.append(left[i])
            i += 1
        else:
            sorted_gems.append(right[j])
            j += 1

    sorted_gems.extend(left[i:])
    sorted_gems.extend(right[j:])

    return sorted_gems

# Example usage:
gems = [38, 27, 43, 3, 9, 82, 10]
print(merge_sort(gems))  # Output: [3, 9, 10, 27, 38, 43, 82]
```

#### Explanation:

Divide the Gems:

- `merge_sort`: Sir Cedric divided the gems into smaller groups.

Conquer:

- He sorted each group individually.

Combine:

- Sir Cedric merged the sorted groups to form the final sequence.
