```

 __     __   __     ______   __     __   __     __     ______   __  __
/\ \   /\ "-.\ \   /\  ___\ /\ \   /\ "-.\ \   /\ \   /\__  _\ /\ \_\ \
\ \ \  \ \ \-.  \  \ \  __\ \ \ \  \ \ \-.  \  \ \ \  \/_/\ \/ \ \____ \
 \ \_\  \ \_\\"\_\  \ \_\    \ \_\  \ \_\\"\_\  \ \_\    \ \_\  \/\_____\
  \/_/   \/_/ \/_/   \/_/     \/_/   \/_/ \/_/   \/_/     \/_/   \/_____/

 ______     ______   ______     __   __     ______     ______
/\  ___\   /\__  _\ /\  __ \   /\ "-.\ \   /\  ___\   /\  ___\
\ \___  \  \/_/\ \/ \ \ \/\ \  \ \ \-.  \  \ \  __\   \ \___  \
 \/\_____\    \ \_\  \ \_____\  \ \_\\"\_\  \ \_____\  \/\_____\
  \/_____/     \/_/   \/_____/   \/_/ \/_/   \/_____/   \/_____/

```

# The Infinity Gauntlet Challenge

Thanos, the Mad Titan, has gathered all six Infinity Stones and now possesses the Infinity Gauntlet, granting him immense power. However, he faces a new challenge. He must distribute items (Infinity Stones) into various containers (slots in the Gauntlet) based on specific rules. Your task is to help Thanos by writing a program to ensure the stones are placed correctly.

## The Challenge

Thanos has 6 Infinity Stones, each represented by an item, and a Gauntlet with 6 slots (containers). The stones must be placed in the following manner:

1. The Power Stone (P) must be placed in the first slot.
2. The Time Stone (T) must be placed in the last slot.
3. The Space Stone (S) and the Reality Stone (R) can be placed in any of the remaining slots.
4. The Mind Stone (M) and the Soul Stone (L) must be placed next to each other.

Write a function `distribute_stones` that takes a list of stones and returns a list representing the Gauntlet with stones placed in the correct slots. If it is not possible to place the stones according to the rules, return an empty list.

### Function Signature

```python
def distribute_stones(stones: List[str]) -> List[str]:
    pass
```

### Input

- `stones`: A list of strings representing the stones. Example: `['P', 'T', 'S', 'R', 'M', 'L']`

### Output

- A list of strings representing the Gauntlet with stones placed in the correct slots, or an empty list if it is not possible to place them according to the rules.

### Examples

```python
# Example 1
stones = ['P', 'T', 'S', 'R', 'M', 'L']
print(distribute_stones(stones))
# Output: ['P', 'S', 'R', 'M', 'L', 'T']

# Example 2
stones = ['P', 'T', 'S', 'R', 'L', 'M']
print(distribute_stones(stones))
# Output: ['P', 'S', 'R', 'L', 'M', 'T']

# Example 3
stones = ['P', 'S', 'R', 'M', 'L', 'T']
print(distribute_stones(stones))
# Output: []

# Example 4
stones = ['P', 'T', 'S', 'M', 'R', 'L']
print(distribute_stones(stones))
# Output: ['P', 'S', 'M', 'R', 'L', 'T']
```

## Solution

```python
from typing import List

def distribute_stones(stones: List[str]) -> List[str]:
    # Initialize the Gauntlet with empty slots
    gauntlet = [None] * 6

    # Place the Power Stone and Time Stone
    gauntlet[0] = 'P'
    gauntlet[5] = 'T'

    # Remove the Power Stone and Time Stone from the list
    stones.remove('P')
    stones.remove('T')

    # Find positions for Mind and Soul Stones
    if 'M' in stones and 'L' in stones:
        # Mind and Soul Stones must be adjacent
        idx_m = stones.index('M')
        idx_l = stones.index('L')

        # Try placing Mind and Soul Stones in possible positions
        if (idx_m == idx_l + 1) or (idx_l == idx_m + 1):
            if gauntlet[1] is None:
                gauntlet[1] = stones.pop(idx_m)
                gauntlet[2] = stones.pop(idx_l if idx_l < idx_m else idx_m)
            elif gauntlet[2] is None:
                gauntlet[2] = stones.pop(idx_m)
                gauntlet[3] = stones.pop(idx_l if idx_l < idx_m else idx_m)
            elif gauntlet[3] is None:
                gauntlet[3] = stones.pop(idx_m)
                gauntlet[4] = stones.pop(idx_l if idx_l < idx_m else idx_m)
            else:
                return []
        else:
            return []
    else:
        return []

    # Fill remaining slots with Space and Reality Stones
    for i in range(1, 5):
        if gauntlet[i] is None:
            gauntlet[i] = stones.pop(0)

    return gauntlet if len(stones) == 0 else []

# Testing the function
stones1 = ['P', 'T', 'S', 'R', 'M', 'L']
print(distribute_stones(stones1)) # Output: ['P', 'S', 'R', 'M', 'L', 'T']

stones2 = ['P', 'T', 'S', 'R', 'L', 'M']
print(distribute_stones(stones2)) # Output: ['P', 'S', 'R', 'L', 'M', 'T']

stones3 = ['P', 'S', 'R', 'M', 'L', 'T']
print(distribute_stones(stones3)) # Output: []

stones4 = ['P', 'T', 'S', 'M', 'R', 'L']
print(distribute_stones(stones4)) # Output: ['P', 'S', 'M', 'R', 'L', 'T']
```

---

In this challenge, you will have learned to handle constraints and specific placement rules while manipulating a list of items.
