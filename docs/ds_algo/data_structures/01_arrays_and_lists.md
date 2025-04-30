# Arrays and Lists: Fundamental Sequential Data Structures

## Learning Objectives

- Understand the differences between arrays (fixed-size) and lists (dynamic)
- Implement key operations: indexing, appending, slicing, and insertion
- Analyze time and space complexity of different operations
- Make informed decisions about when to use arrays vs lists

## Environment Setup

| Setup Steps                                                               |
| :------------------------------------------------------------------------ |
| 1. Ensure Python 3.8+ is installed (`python --version`)                   |
| 2. No external packages required (uses Python's built-in data structures) |
| 3. Create a working directory for your code                               |
| 4. Save code examples as `.py` files                                      |
| 5. Run with `python filename.py`                                          |

## Concept Overview

Arrays and lists are sequential data structures that store elements in contiguous memory locations.

### Arrays vs Lists in Python

```
┌────────────────────────────────────────────────────┐
│ Python Lists                │ Traditional Arrays    │
├───────────────────────────────────────────────────-│
│ Dynamic size                │ Fixed size           │
│ Mixed data types            │ Homogeneous elements │
│ Built-in methods            │ Basic operations     │
│ Memory overhead             │ Memory efficient     │
│ Implemented as array lists  │ Direct memory access │
└────────────────────────────────────────────────────┘
```

In Python, the built-in `list` type is actually an array list - a dynamic array that resizes automatically. For true fixed-size arrays, you can use the `array` module or NumPy arrays.

## Starter Code with Gaps

Save this as `array_list_operations.py`:

```python
"""
Array and List Operations - Collaborative Learning Exercise
"""
import array

def main():
    # PART 1: Python Lists (Dynamic Arrays)
    print("===== PYTHON LISTS =====")

    # TODO: Create an empty list
    my_list = []

    # TODO: Append elements to the list (implement with your team)
    # Add code here

    # TODO: Demonstrate indexing (positive and negative)
    # Add code here

    # TODO: Implement list slicing examples
    # Add code here

    # TODO: Insert elements at specific positions
    # Add code here

    # PART 2: Fixed-size Arrays using array module
    print("\n===== FIXED-SIZE ARRAYS =====")

    # TODO: Create a fixed-size integer array
    # Hint: Use array.array('i', [...])
    # Add code here

    # TODO: Try operations and observe differences from lists
    # Add code here

    # PART 3: Implement a simple ArrayList class (similar to Java's ArrayList)
    print("\n===== CUSTOM ARRAYLIST IMPLEMENTATION =====")

class ArrayList:
    """A simplified implementation of a dynamic array (similar to ArrayList in Java)"""

    def __init__(self, capacity=10):
        """Initialize with a default capacity of 10 elements"""
        # TODO: Create a fixed-size array with initial capacity
        # Hint: Use [None] * capacity
        self.size = 0

    def append(self, element):
        """Add an element to the end of the array"""
        # TODO: Implement append with resizing when needed
        pass

    def get(self, index):
        """Get element at index"""
        # TODO: Implement with bounds checking
        pass

    def insert(self, index, element):
        """Insert element at specific index"""
        # TODO: Implement insert with shifting elements
        pass

    def remove(self, index):
        """Remove element at index"""
        # TODO: Implement remove with shifting elements
        pass

    def __str__(self):
        """String representation of the array"""
        # TODO: Implement string representation
        pass

if __name__ == "__main__":
    main()
```

## Live Coding Collaboration Tasks

### Task 1: Implement Missing List Operations

Working in pairs, fill in the TODOs for the list operations in the main function. Take turns writing code while the other reviews and suggests improvements.

### Task 2: Complete the ArrayList Class

In teams of 2-3:

1. First coder implements `__init__` and `append`
2. Second coder implements `get` and `__str__`
3. Third coder (or back to first) implements `insert` and `remove`
4. All review the implementation together

## Peer Discussion Questions

1. **Complexity Analysis**: What is the time complexity of:

   - Accessing an element by index in a list vs. ArrayList?
   - Appending an element to a list? What about when resizing occurs?
   - Inserting at the beginning of a list? How could you optimize this?

2. **Design Decisions**:

   - When would you use a fixed-size array vs. a dynamic list?
   - What factors affect the performance of arrays/lists?
   - How does memory layout affect performance for large datasets?

3. **Testing Edge Cases**:
   - What edge cases should we test for our ArrayList implementation?
   - How might our implementation fail with very large datasets?

## Time and Space Complexity

| Operation | Python List (Average) | Python List (Worst) | Fixed Array |
| --------- | --------------------- | ------------------- | ----------- |
| Access    | O(1)                  | O(1)                | O(1)        |
| Append    | O(1)                  | O(n) [resizing]     | N/A         |
| Insert    | O(n)                  | O(n)                | O(n)        |
| Delete    | O(n)                  | O(n)                | O(n)        |
| Search    | O(n)                  | O(n)                | O(n)        |

Space complexity: O(n) where n is the number of elements

## Common Bugs and Debugging Tips

1. **Index Out of Range**

   - Error: `IndexError: list index out of range`
   - Debug: Check bounds before accessing. Use try/except or explicit bounds checking.

2. **Modifying While Iterating**

   - Error: Unexpected behavior when modifying a list during iteration
   - Debug: Create a copy of the list or iterate backward when removing items.

3. **Aliasing vs Copying**

   ```python
   list1 = [1, 2, 3]
   list2 = list1          # Aliasing - both variables refer to the same list
   list3 = list1.copy()   # Creates a new copy of the list

   list1[0] = 99          # Modifies both list1 and list2, but not list3
   ```

4. **Inefficient Appending**
   - Error: Slow performance when building a list
   - Debug: Use `append()` instead of concatenation (`list += [item]`)

## Group Challenge: Custom List Implementation

### Challenge Task

Extend the ArrayList implementation to include:

1. A `capacity()` method that returns the current capacity
2. An efficient `extend()` method that adds all elements from another list
3. A method to shrink the internal array when it's significantly larger than needed
4. A performance test comparing your implementation to Python's built-in list

### Approach Instructions

1. **Plan together**: Draw the internal state of the ArrayList during operations
2. **Rotate roles**:
   - **Implementer**: Writes the code
   - **Tester**: Develops test cases
   - **Reviewer**: Evaluates performance and correctness
3. **Checkpoints**:
   - After implementing each method, run tests and discuss complexity
   - When all features are complete, compare performance with Python's built-in list

## Reflection Questions

After completing the exercises:

1. How does Python's list implementation differ from your ArrayList?
2. What were the most challenging aspects of the implementation?
3. In what scenarios would your custom implementation be better/worse than using a built-in list?
4. How would you improve your implementation for better performance?
