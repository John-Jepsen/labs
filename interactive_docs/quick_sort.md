### Algorithm: Quick Sort

To navigate the Maze of Flames and reach Princess Elara, Sir Cedric and Ember needed to sort the Flames of Destiny in ascending order using the Quick Sort algorithm.

#### Choose a Pivot:

- Sir Cedric selected a pivot flame and divided the others into groups less than and greater than the pivot.

#### Conquer:

- He sorted each group individually.

#### Combine:

- Sir Cedric combined the sorted groups with the pivot to form the final sequence.

#### Implementation:

```py
def quick_sort(flames: List[int]) -> List[int]:
    if len(flames) <= 1:
        return flames

    pivot = flames[len(flames) // 2

]
    less = [x for x in flames if x < pivot]
    equal = [x for x in flames if x == pivot]
    greater = [x for x in flames if x > pivot]

    return quick_sort(less) + equal + quick_sort(greater)

# Example usage:
flames = [3, 6, 8, 10, 1, 2, 1]
print(quick_sort(flames))  # Output: [1, 1, 2, 3, 6, 8, 10]
```

#### Explanation:

Choose a Pivot:

- `pivot`: Sir Cedric selected a pivot flame and divided the others into groups.

Conquer:

- He sorted each group individually.

Combine:

- Sir Cedric combined the sorted groups with the pivot to form the final sequence.
