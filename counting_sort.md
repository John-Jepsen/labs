### Algorithm: Counting Sort

To organize her fleet's resources, Princess Elara used the Counting Sort algorithm to sort the quantities of various supplies quickly.

#### Initialize Data Structures:

- Princess Elara used a series of nets (count array) to count the occurrences of each supply quantity.

#### Count and Sort:

- She counted each quantity and then placed them back into the list in sorted order.

#### Implementation:

```py
def counting_sort(supplies: List[int]) -> List[int]:
    max_supply = max(supplies)
    count = [0] * (max_supply + 1)

    for supply in supplies:
        count[supply] += 1

    sorted_supplies = []
    for i in range(len(count)):
        sorted_supplies.extend([i] * count[i])

    return sorted_supplies

# Example usage:
supplies = [4, 2, 2, 8, 3, 3, 1]
print(counting_sort(supplies))  # Output: [1, 2, 2, 3, 3, 4, 8]
```

#### Explanation:

Initialize:

- `count`: An array to count occurrences of each supply quantity.

Count and Sort:

- Princess Elara counted each quantity and placed them back into the list in sorted order.
