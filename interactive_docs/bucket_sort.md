### Algorithm: Bucket Sort

### Introduction to Bucket Sort

Princess Elara, a legendary figure known for her wisdom and fairness, faced the daunting task of distributing vast amounts of treasure evenly among her crew. To achieve this, she relied on an efficient algorithm known as Bucket Sort. This sorting algorithm, especially useful for sorting a list of numerical values that are uniformly distributed over a range, works by dividing the data into several buckets, sorting the elements within each bucket, and then combining them to produce the sorted list.

### What is Bucket Sort?

Bucket Sort is a comparison-based sorting algorithm that divides the unsorted data into a fixed number of buckets, each of which is then sorted individually using a different sorting algorithm or recursively applying the same bucket sorting process. The idea behind Bucket Sort is to spread the elements uniformly across a range of buckets, where each bucket handles a portion of the data range. By sorting smaller subsets of the data (buckets) individually, the algorithm achieves an efficient overall sort, particularly when the input values are evenly distributed. This approach minimizes the need for extensive comparisons, making Bucket Sort a highly efficient and effective sorting method for specific types of data distributions.

#### Implementation:

```py
def bucket_sort(treasures: List[float]) -> List[float]:
    n = len(treasures)
    if n == 0:
        return treasures

    max_value = max(treasures)
    buckets = [[] for _ in range(n)]

    for treasure in treasures:
        index = int(treasure / max_value * (n - 1))
        buckets[index].append(treasure)

    for i in range(n):
        buckets[i].sort()

    sorted_treasures = []
    for bucket in buckets:
        sorted_treasures.extend(bucket)

    return sorted_treasures

# Example usage:
treasures = [0.78, 0.17, 0.39, 0.26, 0.72, 0.94, 0.21, 0.12, 0.23, 0.68]
print(bucket_sort(treasures))  # Output: [0.12, 0.17, 0.21, 0.23, 0.26, 0.39, 0.68, 0.72, 0.78, 0.94]
```

#### Explanation:

Initialize:

- `buckets`: Multiple buckets to categorize the treasure items.

Distribute and Sort:

- Princess Elara distributed the treasure items into buckets and sorted each bucket.
