# Trees: Hierarchical Data Structures

## Learning Objectives

- Understand the structure and terminology of tree data structures
- Implement binary trees and tree traversal algorithms
- Build trees from arrays and interact with tree structures
- Analyze time and space complexity of tree operations
- Explore common applications of tree data structures

## Environment Setup

| Setup Steps                                                               |
| :------------------------------------------------------------------------ |
| 1. Ensure Python 3.8+ is installed (`python --version`)                   |
| 2. No external packages required (uses Python's built-in data structures) |
| 3. Create a working directory for your code                               |
| 4. Save code examples as `.py` files                                      |
| 5. Run with `python filename.py`                                          |

## Concept Overview

A tree is a hierarchical data structure consisting of nodes connected by edges, with a single root node and no cycles.

### Tree Terminology

- **Node**: An element in the tree containing data and references to child nodes
- **Root**: The topmost node in the tree
- **Parent/Child**: Relationship between connected nodes
- **Leaf**: A node with no children
- **Depth**: Length of path from root to a node
- **Height**: Length of the longest path from a node to a leaf
- **Subtree**: Tree formed by a node and its descendants

### Binary Tree Visualization

```
         ┌─────┐
         │  A  │           Root (Level 0)
         └──┬──┘
      ┌─────┴─────┐
   ┌──┴──┐     ┌──┴──┐
   │  B  │     │  C  │    Level 1
   └──┬──┘     └──┬──┘
  ┌───┴───┐       └───┐
┌─┴─┐   ┌─┴─┐      ┌──┴─┐
│ D │   │ E │      │ F  │  Level 2 (D, E are leaf nodes)
└───┘   └───┘      └────┘
```

### Types of Trees

1. **Binary Tree**: Each node has at most two children
2. **Binary Search Tree (BST)**: Binary tree where left child < parent < right child
3. **AVL Tree**: Self-balancing BST
4. **B-Tree**: Self-balancing tree with multiple keys per node (not covered in this lab)

## Starter Code with Gaps

Save this as `tree_implementations.py`:

```python
"""
Tree Implementations - Collaborative Learning Exercise
"""
import queue

def main():
    print("===== BINARY TREE =====")
    # Create a binary tree manually
    root = TreeNode("A")
    root.left = TreeNode("B")
    root.right = TreeNode("C")
    root.left.left = TreeNode("D")
    root.left.right = TreeNode("E")
    root.right.right = TreeNode("F")

    # Build a tree from the above illustration
    print("Tree Structure:")
    print_tree(root)

    print("\n===== TREE TRAVERSALS =====")
    # TODO: Implement and demonstrate tree traversals

    print("\n===== BINARY SEARCH TREE =====")
    bst = BinarySearchTree()
    # TODO: Implement and demonstrate BST operations

    print("\n===== TREE APPLICATIONS =====")
    # TODO: Implement and demonstrate tree applications


class TreeNode:
    """Basic node for a binary tree"""

    def __init__(self, data):
        """Initialize a node with data and null children"""
        self.data = data
        self.left = None
        self.right = None


def print_tree(root, level=0, prefix="Root: "):
    """Print a visual representation of the tree"""
    if root is not None:
        print(" " * (level * 4) + prefix + str(root.data))
        if root.left is not None or root.right is not None:
            if root.left:
                print_tree(root.left, level + 1, "L── ")
            else:
                print(" " * ((level + 1) * 4) + "L── None")
            if root.right:
                print_tree(root.right, level + 1, "R── ")
            else:
                print(" " * ((level + 1) * 4) + "R── None")


def in_order_traversal(root):
    """Traverse tree in-order (left, root, right)"""
    # TODO: Implement in-order traversal
    # This can be done recursively or iteratively
    pass


def pre_order_traversal(root):
    """Traverse tree pre-order (root, left, right)"""
    # TODO: Implement pre-order traversal
    pass


def post_order_traversal(root):
    """Traverse tree post-order (left, right, root)"""
    # TODO: Implement post-order traversal
    pass


def level_order_traversal(root):
    """Traverse tree level by level (breadth-first)"""
    # TODO: Implement level-order traversal
    # Hint: Use a queue to keep track of nodes at each level
    pass


def build_tree_from_list(elements):
    """Build a binary tree from a list (array representation)"""
    # TODO: Implement tree building from a list
    # For a list [1,2,3,4,5], create a complete binary tree
    pass


class BinarySearchTree:
    """Binary Search Tree implementation"""

    def __init__(self):
        """Initialize an empty BST"""
        self.root = None

    def insert(self, data):
        """Insert a value into the BST"""
        # TODO: Implement insertion for BST
        # Remember BST property: left < root < right
        pass

    def search(self, data):
        """Search for a value in the BST"""
        # TODO: Implement search for BST
        pass

    def delete(self, data):
        """Delete a value from the BST"""
        # TODO: Implement deletion for BST
        # This is the most complex operation - handle all cases:
        # 1. Node with no children (leaf)
        # 2. Node with one child
        # 3. Node with two children
        pass

    def _find_min(self, node):
        """Find the minimum value node in a subtree"""
        # TODO: Implement min finding
        # Hint: Keep going left until you can't anymore
        pass

    def _find_max(self, node):
        """Find the maximum value node in a subtree"""
        # TODO: Implement max finding
        pass

    def is_valid_bst(self):
        """Check if the tree is a valid BST"""
        # TODO: Implement BST validation
        # Verify that for each node, all left subtree < node < all right subtree
        pass


def is_balanced(root):
    """Check if a binary tree is balanced (max difference in height between subtrees is 1)"""
    # TODO: Implement balance checking
    pass


def lowest_common_ancestor(root, p, q):
    """Find the lowest common ancestor of two nodes in a binary tree"""
    # TODO: Implement LCA finding
    pass


def serialize_tree(root):
    """Serialize a binary tree to a string"""
    # TODO: Implement tree serialization
    # Convert tree to a string format that can be sent over network or stored
    pass


def deserialize_tree(data):
    """Deserialize a string back to a binary tree"""
    # TODO: Implement tree deserialization
    # Convert string representation back to a tree
    pass


if __name__ == "__main__":
    main()
```

## Live Coding Collaboration Tasks

### Task 1: Tree Traversal Implementations

Working in pairs, implement the tree traversal algorithms:

1. First person implements `in_order_traversal` and `pre_order_traversal`
2. Second person implements `post_order_traversal` and `level_order_traversal`
3. Test all traversals on the sample tree and discuss the output differences

### Task 2: Binary Search Tree Implementation

In teams of 2-3:

1. First coder implements `insert` method
2. Second coder implements `search` method
3. Third coder (or back to first) implements `delete` with all cases
4. Test BST operations and verify correctness

### Task 3: Tree Algorithms and Applications

Divide the utility functions among team members:

1. One person implements `build_tree_from_list` and `is_balanced`
2. Another implements `lowest_common_ancestor`
3. A third implements `serialize_tree` and `deserialize_tree`
4. Test all algorithms together

## Peer Discussion Questions

1. **Tree Structure Design**:

   - How do trees compare to other data structures for different operations?
   - What factors affect the choice between different tree types?
   - What real-world hierarchical structures could be represented with trees?

2. **Algorithm Analysis**:

   - How do different traversal methods affect the processing order?
   - When would you use one traversal method over another?
   - What is the time and space complexity of each tree operation?

3. **Balancing Considerations**:
   - Why is tree balancing important for performance?
   - How do BST operations degrade with highly unbalanced trees?
   - What strategies exist for maintaining balanced trees?

## Time and Space Complexity

| Operation | Balanced BST | Unbalanced BST (worst case) |
| --------- | ------------ | --------------------------- |
| Search    | O(log n)     | O(n)                        |
| Insert    | O(log n)     | O(n)                        |
| Delete    | O(log n)     | O(n)                        |
| Traversal | O(n)         | O(n)                        |
| Space     | O(n)         | O(n)                        |
| Height    | O(log n)     | O(n)                        |

## Common Bugs and Debugging Tips

1. **Parent-Child Connection Problems**

   - Issue: Incorrectly setting or updating parent-child relationships
   - Debug: Carefully manage connections when modifying the tree

   ```python
   # Ensure both sides of the relationship are updated
   def set_left_child(self, node, left_child):
       node.left = left_child
       # For trees that track parent pointers:
       if left_child:
           left_child.parent = node
   ```

2. **BST Property Violations**

   - Issue: Insertions or modifications that break the BST property
   - Debug: Implement a validation function to check BST property

   ```python
   def is_valid_bst(root, min_val=float('-inf'), max_val=float('inf')):
       if not root:
           return True
       if root.data <= min_val or root.data >= max_val:
           return False
       return (is_valid_bst(root.left, min_val, root.data) and
               is_valid_bst(root.right, root.data, max_val))
   ```

3. **Infinite Recursion**

   - Issue: Missing base case in recursive tree operations
   - Debug: Always check for null/empty tree condition

   ```python
   def traverse(node):
       # Base case first to prevent infinite recursion
       if node is None:
           return
       # Recursive cases
       traverse(node.left)
       print(node.data)
       traverse(node.right)
   ```

4. **Deletion Edge Cases**
   - Issue: Not handling all cases in BST deletion
   - Debug: Carefully test all deletion scenarios
   ```python
   # Test cases for deletion:
   # 1. Delete a leaf node
   # 2. Delete a node with one child
   # 3. Delete a node with two children
   # 4. Delete the root node
   # 5. Delete a node that doesn't exist
   ```

## Group Challenge: Expression Tree Builder and Evaluator

### Challenge Task

Build a system that:

1. Converts an infix mathematical expression to a binary expression tree
2. Evaluates the expression tree to calculate the result
3. Prints the expression in prefix (Polish) and postfix (Reverse Polish) notation
4. Supports basic operators (+, -, \*, /) and parentheses for precedence

Example expression: "3 + 4 \* 2 - (6 / 3)"

### Approach Instructions

1. **Design Phase (10 minutes)**:

   - Define the node structure for operators and operands
   - Plan the algorithm for converting infix to expression tree
   - Outline the expression evaluation approach

2. **Implementation Phase (15 minutes)**:

   - **Person 1**: Implement tokenization and expression tree building
   - **Person 2**: Implement tree evaluation
   - **Person 3**: Implement notation conversions (infix to prefix/postfix)

3. **Testing Phase (5 minutes)**:
   - Create test expressions of varying complexity
   - Verify the tree structure through visualization
   - Compare evaluation results with expected answers

### Starter Code for Expression Tree

```python
class ExpressionNode:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None

    def is_operator(self):
        """Check if the node represents an operator"""
        return self.value in "+-*/"


def build_expression_tree(expression):
    """Build an expression tree from an infix expression string"""
    # TODO: Implement expression tree building
    # Hint: Use a stack-based algorithm
    pass


def evaluate_tree(root):
    """Evaluate an expression tree and return the result"""
    # TODO: Implement recursive evaluation
    # Base case: If leaf node (operand), return the value
    # Recursive case: Evaluate left and right subtrees, apply the operator
    pass


def infix_to_prefix(expression):
    """Convert infix expression to prefix notation using an expression tree"""
    # TODO: Implement prefix conversion
    pass


def infix_to_postfix(expression):
    """Convert infix expression to postfix notation using an expression tree"""
    # TODO: Implement postfix conversion
    pass


def test_expressions():
    """Test the expression tree with various expressions"""
    expressions = [
        "3 + 4",
        "3 + 4 * 2",
        "3 * (4 + 2)",
        "3 + 4 * 2 - (6 / 3)",
        "(7 - 2) * (3 + 4) / 2"
    ]

    for expr in expressions:
        # TODO: Process each expression and show results
        pass
```

## Reflection Questions

After completing the exercises:

1. How does the structure of a binary search tree optimize search operations compared to linear data structures?
2. What were the most challenging aspects of implementing tree traversal algorithms?
3. How would you adapt a basic binary tree to handle more complex hierarchical data?
4. In what real-world applications might you use different types of tree data structures?
