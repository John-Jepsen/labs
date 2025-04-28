# Stacks: Last-In-First-Out (LIFO) Data Structures

## Learning Objectives

- Understand the LIFO (Last-In-First-Out) principle of stacks
- Implement a stack using Python lists and a custom class
- Apply stack operations: push, pop, peek, and isEmpty
- Solve common problems using stacks
- Analyze time and space complexity of stack operations

## Environment Setup

| Setup Steps                                                               |
| :------------------------------------------------------------------------ |
| 1. Ensure Python 3.8+ is installed (`python --version`)                   |
| 2. No external packages required (uses Python's built-in data structures) |
| 3. Create a working directory for your code                               |
| 4. Save code examples as `.py` files                                      |
| 5. Run with `python filename.py`                                          |

## Concept Overview

A stack is a linear data structure that follows the LIFO principle - the last element added is the first one removed.

### Stack Visualization

```
    ┌────────────┐
    │   TOP      │ ← Most recently added (will be removed first)
    ├────────────┤
    │   Item 3   │
    ├────────────┤
    │   Item 2   │
    ├────────────┤
    │   Item 1   │ ← First added (will be removed last)
    └────────────┘
```

### Core Operations

- **push(item)**: Add an item to the top of the stack
- **pop()**: Remove and return the top item from the stack
- **peek()** or **top()**: Return the top item without removing it
- **isEmpty()**: Check if the stack is empty

## Starter Code with Gaps

Save this as `stack_implementations.py`:

```python
"""
Stack Implementations - Collaborative Learning Exercise
"""

def main():
    print("===== STACK USING PYTHON LIST =====")
    # TODO: Implement list-based stack operations
    # Hint: Use list methods like append() and pop()

    # PART 1: Create a stack using Python list
    stack = []

    # TODO: Push elements to the stack

    # TODO: Implement peek functionality

    # TODO: Pop elements from the stack

    # TODO: Check if stack is empty

    print("\n===== CUSTOM STACK IMPLEMENTATION =====")
    # Create a stack with our custom implementation
    my_stack = Stack()

    # TODO: Use the stack methods

    print("\n===== STACK APPLICATIONS =====")

    # TODO: Implement bracket matching function
    text = "((3 + 5) * [10 - 2]) / {7}"
    print(f"Brackets balanced in '{text}': {is_balanced(text)}")

    # TODO: Implement reverse string function using stack
    text = "Hello, World!"
    print(f"Original: {text}")
    print(f"Reversed: {reverse_string(text)}")


class Stack:
    """A custom Stack implementation"""

    def __init__(self):
        """Initialize an empty stack"""
        # TODO: Initialize the internal data structure
        # Hint: Use a Python list

    def push(self, item):
        """Add item to the top of the stack"""
        # TODO: Implement push operation
        pass

    def pop(self):
        """Remove and return the top item from the stack"""
        # TODO: Implement pop operation with error handling
        pass

    def peek(self):
        """Return the top item without removing it"""
        # TODO: Implement peek operation with error handling
        pass

    def is_empty(self):
        """Check if the stack is empty"""
        # TODO: Implement isEmpty check
        pass

    def size(self):
        """Return the number of items in the stack"""
        # TODO: Return stack size
        pass

    def __str__(self):
        """Return a string representation of the stack"""
        # TODO: Implement string representation
        pass


def is_balanced(text):
    """Check if brackets in the given text are balanced using a stack"""
    # TODO: Implement a bracket matching algorithm using stack
    # Hint: Push opening brackets, pop and check when closing brackets are found
    pass


def reverse_string(text):
    """Reverse a string using a stack"""
    # TODO: Implement string reversal using stack
    # Hint: Push each character, then pop them all to get reversed order
    pass


if __name__ == "__main__":
    main()
```

## Live Coding Collaboration Tasks

### Task 1: List-Based Stack Implementation

Working in pairs, implement the list-based stack operations in the main function:

1. First person implements push and peek operations
2. Second person implements pop and isEmpty operations
3. Test the operations together

### Task 2: Complete the Stack Class

In teams of 2-3:

1. First coder implements `__init__`, `push`, and `is_empty`
2. Second coder implements `pop` and `peek`
3. Third coder (or back to first) implements `size` and `__str__`
4. All review the implementation together

### Task 3: Stack Applications

Divide the `is_balanced` and `reverse_string` functions between team members and implement them using the Stack class.

## Peer Discussion Questions

1. **Implementation Choices**:

   - What are the advantages/disadvantages of using a Python list vs. a custom Stack class?
   - How would you implement a stack with a maximum capacity?
   - Could you implement a stack using a linked list instead? What changes?

2. **Application Analysis**:

   - What other problems could be solved effectively with a stack?
   - When is a stack more appropriate than a queue?
   - How would you implement an "undo" feature in a text editor using stacks?

3. **Performance Consideration**:
   - What is the time complexity of each stack operation?
   - How does memory usage compare between different implementations?
   - Are there any operations that could be optimized further?

## Time and Space Complexity

| Operation | Time Complexity | Space Complexity |
| --------- | --------------- | ---------------- |
| Push      | O(1)\*          | O(1)             |
| Pop       | O(1)            | O(1)             |
| Peek      | O(1)            | O(1)             |
| isEmpty   | O(1)            | O(1)             |
| Size      | O(1)            | O(1)             |

\*Note: Push can be O(n) in rare cases when the internal array needs to resize.

## Common Bugs and Debugging Tips

1. **Stack Underflow**

   - Error: Trying to pop from an empty stack
   - Debug: Always check if the stack is empty before popping

   ```python
   if not stack.is_empty():
       item = stack.pop()
   else:
       print("Cannot pop from empty stack")
   ```

2. **Reference vs. Value**

   - Issue: Pushing reference types (like lists) can lead to unexpected behavior if modified later
   - Debug: Use deep copying when pushing mutable objects if you need to preserve their state

   ```python
   import copy
   stack.push(copy.deepcopy(my_list))
   ```

3. **Missing Boundary Checks**

   - Error: Index out of range errors
   - Debug: Ensure all stack operations check boundaries

   ```python
   def peek(self):
       if self.is_empty():
           raise IndexError("Cannot peek at an empty stack")
       return self.items[-1]
   ```

4. **Stack Size Tracking**
   - Issue: Incorrect size tracking in custom implementations
   - Debug: Ensure size is updated correctly in all operations

## Group Challenge: Evaluate Expressions with Stacks

### Challenge Task

Implement a function to evaluate simple arithmetic expressions using two stacks:

1. One stack for operators
2. One stack for operands

Example expressions:

- "3 + 4 \* 2"
- "( 1 + 2 ) \* 3"
- "5 + ( 8 \* 3 - 2 )"

### Approach Instructions

1. **Planning Phase (10 minutes)**:

   - Draw the state of both stacks at each step
   - Define operator precedence
   - Decide how to handle parentheses

2. **Implementation Phase (15 minutes)**:

   - **Person 1**: Parse the expression into tokens
   - **Person 2**: Implement operator precedence logic
   - **Person 3**: Implement the evaluation algorithm

3. **Testing Phase (5 minutes)**:
   - Create test cases with different operators and parentheses
   - Verify the results against expected outputs
   - Test edge cases (empty expression, division by zero)

### Starter Code for Expression Evaluator

```python
def evaluate_expression(expression):
    """
    Evaluate arithmetic expression using two stacks
    """
    # TODO: Implement using operator and operand stacks
    operators = Stack()  # Stack for operators
    operands = Stack()   # Stack for numbers

    # TODO: Tokenize the expression

    # TODO: Process each token based on type (number, operator, parenthesis)

    # TODO: Final evaluation and return result

    return result
```

## Reflection Questions

After completing the exercises:

1. How do stacks help solve problems with a naturally recursive structure?
2. What were the most challenging aspects of implementing the expression evaluator?
3. How would your implementation change if you needed to support more complex operations (like exponents or functions)?
4. In what real-world applications have you encountered stacks (explicitly or implicitly)?
