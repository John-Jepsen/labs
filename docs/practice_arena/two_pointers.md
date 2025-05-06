# Two Pointers Technique: Efficient Array Navigation

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

The two pointers technique uses two indices to solve array problems efficiently:

```
Example: Find a pair that sums to 9 in a sorted array [2, 7, 11, 15]

Initialize:
   left             right
    ↓                 ↓
  [ 2,  7,  11,  15 ]

Step 1: Calculate sum = nums[left] + nums[right] = 2 + 15 = 17
   17 > 9, so move right pointer left

   left          right
    ↓              ↓
  [ 2,  7,  11,  15 ]

Step 2: Calculate sum = nums[left] + nums[right] = 2 + 11 = 13
   13 > 9, so move right pointer left

   left       right
    ↓           ↓
  [ 2,  7,  11,  15 ]

Step 3: Calculate sum = nums[left] + nums[right] = 2 + 7 = 9
   9 == 9, we found our pair!

   Answer: [0, 1] (indices of 2 and 7)
```

## Pseudocode

### Two Sum (Sorted Array)

```
function two_sum_sorted(nums, target):
    left = 0
    right = length of nums - 1

    while left < right:
        current_sum = nums[left] + nums[right]

        if current_sum == target:
            return [left, right]
        else if current_sum < target:
            left++
        else:
            right--

    return []  // No pair found
```

### Two Pointers for Palindrome Verification

```
function is_palindrome(s):
    left = 0
    right = length of s - 1

    while left < right:
        if s[left] != s[right]:
            return false
        left++
        right--

    return true
```

## Annotated Code Template

```python
def two_sum_sorted(nums, target):
    """
    Find a pair of numbers in a sorted array that sum to target.

    Args:
        nums: A sorted list of integers
        target: The target sum

    Returns:
        List of indices of the two numbers that add up to target, or empty list if not found
    """
    # TODO: Initialize two pointers

    # TODO: Implement the two-pointer loop
        # Calculate current sum

        # Check if current sum matches target

        # Adjust pointers based on comparison with target

    # TODO: Return empty list if no pair is found
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm's behavior with each pointer movement

### Task

Working together, implement multiple applications of the two pointers technique. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each pointer movement.

1. Start with implementing two sum for a sorted array
2. Then implement a function to check if a string is a palindrome
3. Finally, implement a function to remove duplicates from a sorted array
4. Test each implementation with various inputs

### Complete Implementation

```python
def two_sum_sorted(nums, target):
    """
    Find a pair of numbers in a sorted array that sum to target.

    Args:
        nums: A sorted list of integers
        target: The target sum

    Returns:
        List of indices of the two numbers that add up to target, or empty list if not found
    """
    # Initialize two pointers
    left, right = 0, len(nums) - 1

    # Implement the two-pointer loop
    while left < right:
        # Calculate current sum
        current_sum = nums[left] + nums[right]

        # Check if current sum matches target
        if current_sum == target:
            return [left, right]

        # Adjust pointers based on comparison with target
        elif current_sum < target:
            left += 1
        else:
            right -= 1

    # Return empty list if no pair is found
    return []


def is_palindrome(s):
    """
    Check if a string is a palindrome.

    Args:
        s: Input string

    Returns:
        True if the string is a palindrome, False otherwise
    """
    # Initialize two pointers
    left, right = 0, len(s) - 1

    # Compare characters from both ends
    while left < right:
        # Skip non-alphanumeric characters
        if not s[left].isalnum():
            left += 1
            continue
        if not s[right].isalnum():
            right -= 1
            continue

        # Compare characters (case-insensitive)
        if s[left].lower() != s[right].lower():
            return False

        # Move pointers towards the center
        left += 1
        right -= 1

    return True


def remove_duplicates(nums):
    """
    Remove duplicates from a sorted array in-place.

    Args:
        nums: A sorted list of integers

    Returns:
        Length of the array after removing duplicates
    """
    if not nums:
        return 0

    # Initialize two pointers
    slow = 0  # Points to the last unique element position

    # Fast pointer traverses the array
    for fast in range(1, len(nums)):
        # If we find a new unique element
        if nums[fast] != nums[slow]:
            # Move slow pointer and update value
            slow += 1
            nums[slow] = nums[fast]

    # Return the length of the unique elements subarray
    return slow + 1


# Test two sum
nums = [2, 7, 11, 15]
target = 9
print(f"Two Sum Test: {two_sum_sorted(nums, target)}")

# Test palindrome
test_strings = ["A man, a plan, a canal: Panama", "race a car", ""]
for s in test_strings:
    print(f"Is '{s}' a palindrome? {is_palindrome(s)}")

# Test remove duplicates
nums = [0, 0, 1, 1, 1, 2, 2, 3, 3, 4]
length = remove_duplicates(nums)
print(f"After removing duplicates: {nums[:length]}, length: {length}")

# Demonstration of two pointers with animation
def demonstrate_two_sum(nums, target):
    left, right = 0, len(nums) - 1
    steps = []

    while left < right:
        current_sum = nums[left] + nums[right]
        steps.append({
            "left": left,
            "right": right,
            "sum": current_sum,
            "nums": nums.copy(),
            "action": f"Sum={current_sum}" + (
                " (Found!)" if current_sum == target else
                f" (Moving {'left' if current_sum < target else 'right'} pointer)"
            )
        })

        if current_sum == target:
            break
        elif current_sum < target:
            left += 1
        else:
            right -= 1

    # Print the steps
    print("\nTwo Sum Animation:")
    for i, step in enumerate(steps):
        print(f"Step {i+1}: ", end="")
        array_str = "["
        for j, num in enumerate(step["nums"]):
            if j == step["left"]:
                array_str += f" *{num}*,"
            elif j == step["right"]:
                array_str += f" *{num}*,"
            else:
                array_str += f" {num},"
        array_str = array_str[:-1] + " ]"
        print(f"{array_str} - {step['action']}")

demonstrate_two_sum([2, 7, 11, 15], 9)
```

## Peer Discussion Prompts

1. What types of problems are well-suited for the two pointers technique?
2. How does using two pointers improve time complexity compared to nested loops?
3. When would you use the two pointers approach versus a hash table approach?
4. Can the two pointers technique be applied to unsorted arrays? If yes, in what scenarios?

## Checkpoint Questions

1. **Checkpoint 1**: In the two sum problem, why do we move the left pointer when the sum is less than the target?
2. **Checkpoint 2**: In the palindrome check, why do we need to handle non-alphanumeric characters?
3. **Checkpoint 3**: In the remove duplicates function, what is the role of the slow pointer?
4. **Checkpoint 4**: How would you modify the two pointers approach to find all pairs that sum to a target?

## Time and Space Complexity Walkthrough

### Time Complexity

- **Two Sum (sorted)**: O(n) - we traverse the array at most once
- **Palindrome Check**: O(n) - we examine each character at most once
- **Remove Duplicates**: O(n) - we traverse the array exactly once

### Space Complexity

- **Two Sum (sorted)**: O(1) - we use only a constant amount of extra space
- **Palindrome Check**: O(1) - we use only a constant amount of extra space
- **Remove Duplicates**: O(1) - we modify the array in-place

## Common Implementation Mistakes

1. **Not checking boundary conditions**: Failing to ensure that pointers stay within array bounds
2. **Incorrect pointer movement**: Moving the wrong pointer based on the comparison
3. **Duplicates handling**: Not accounting for duplicate elements in the array
4. **Off-by-one errors**: Miscalculating the initial or final positions of pointers

## Mini-Challenge

1. Implement a function to find the container with the most water (maximum area)
2. Write a function to find all triplets in an array that sum to zero
3. Implement a function to merge two sorted arrays in-place
4. Create a solution to the "trapping rain water" problem using two pointers

For the team:

- Compare the time and space complexity of two pointers solutions versus other approaches
- Implement visualizations of how the pointers move for different problems
- Discuss how the two pointers technique can be combined with other algorithms
