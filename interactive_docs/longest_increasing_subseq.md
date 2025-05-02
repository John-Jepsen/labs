### Algorithm: Longest Increasing Subsequence (LIS)

Sir Cedric and Ember needed to traverse the Valley of Echoes, where the longest and most harmonious path would lead them to Princess Elara. They sought the Longest Increasing Subsequence (LIS) of melodies.

#### Initialize Data Structures:

- Sir Cedric used a melody crystal (list) to keep track of the longest harmonious path.

#### Traverse the Valley:

- He evaluated each melody, extending the harmonious path if possible.

#### Retrieve the Result:

- The crystal revealed the length of the longest harmonious path.

#### Implementation:

```py
def length_of_lis(melodies: List[int]) -> int:
    if not melodies:
        return 0

    dp = [1] * len(melodies)

    for i in range(len(melodies)):
        for j in range(i):
            if melodies[i] > melodies[j]:
                dp[i] = max(dp[i], dp[j] + 1)

    return max(dp)

# Example usage:
melodies = [10, 9, 2, 5, 3, 7, 101, 18]
print(length_of_lis(melodies))  # Output: 4
```

#### Explanation:

Initialize:

- `dp`: A melody crystal to track the longest harmonious path.

Traverse the Valley:

- Sir Cedric evaluated each melody, extending the harmonious path if possible.

Retrieve the Result:

- The crystal revealed the length of the longest harmonious path.
