# Depth-First Search (DFS) Algorithm

## Objective

Master the Depth-First Search algorithm through collaborative coding and analysis. By the end of this session, your team will understand both recursive and iterative DFS implementations and their applications to graphs and trees.

---

## Environment Setup

| Requirements          |
| :-------------------- |
| Python 3.8+           |
| Standard library only |

Verify your environment:

```bash
python --version  # Should be 3.8+
```

---

## Visual Explanation

DFS explores a graph by going as deep as possible along each branch before backtracking.

```
    A
   / \
  B   C
 / \   \
D   E   F
```

Traversal order: A → B → D → E → C → F

DFS uses a **stack** (or recursion) to track nodes to visit next, following Last-In-First-Out (LIFO) order.

---

## Algorithm Walkthrough

### Pseudocode (Recursive)

```
function DFS_Recursive(graph, node, visited):
    if node not in visited:
        visited.add(node)
        process(node)

        for each neighbor of node:
            DFS_Recursive(graph, neighbor, visited)
```

### Pseudocode (Iterative)

```
function DFS_Iterative(graph, start_node):
    Stack s = new Stack()
    Set visited = new Set()

    s.push(start_node)

    while s is not empty:
        current = s.pop()

        if current not in visited:
            visited.add(current)
            process(current)

            for each neighbor of current:
                if neighbor not in visited:
                    s.push(neighbor)
```

### Time and Space Complexity

- **Time Complexity**: O(V + E) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V) for the stack/recursion and visited set

---

## Annotated Code Template

### Recursive DFS

```python
def dfs_recursive(graph, node, visited=None, traversal_order=None):
    """
    Performs recursive depth-first search on a graph starting from node.

    Args:
        graph: Dictionary representing an adjacency list graph
               {node: [neighbor1, neighbor2, ...]}
        node: Current node to explore
        visited: Set of visited nodes (initialized if None)
        traversal_order: List to track traversal order (initialized if None)

    Returns:
        List of nodes in DFS traversal order
    """
    # Initialize visited set and traversal list on first call
    if visited is None:
        visited = set()
    if traversal_order is None:
        traversal_order = []

    # TODO: Implement recursive DFS
    # 1. Mark current node as visited
    # 2. Add to traversal order
    # 3. Recursively visit all unvisited neighbors

    return traversal_order
```

### Iterative DFS

```python
def dfs_iterative(graph, start_node):
    """
    Performs iterative depth-first search on a graph starting from start_node.

    Args:
        graph: Dictionary representing an adjacency list graph
               {node: [neighbor1, neighbor2, ...]}
        start_node: Starting node for DFS

    Returns:
        List of nodes in DFS traversal order
    """
    # TODO: Implement iterative DFS using a stack
    # 1. Initialize stack with start node
    # 2. Initialize visited set and traversal list
    # 3. While stack is not empty:
    #    a. Pop a node
    #    b. If not visited, mark as visited and add to traversal
    #    c. Push unvisited neighbors to stack

    traversal_order = []
    return traversal_order
```

---

## Live Coding Group Activity

### Team Roles

- **Driver**: Types the code
- **Navigator**: Guides the implementation
- **Explainer**: Verbalizes what's happening at each step

### Task

Implement both the recursive and iterative DFS algorithms by filling in the missing code. Test with this graph:

```python
# Example graph as adjacency list
graph = {
    'A': ['B', 'C'],
    'B': ['A', 'D', 'E'],
    'C': ['A', 'F'],
    'D': ['B'],
    'E': ['B'],
    'F': ['C']
}

# Expected output from DFS starting at 'A' (one possible order):
# ['A', 'B', 'D', 'E', 'C', 'F']
```

### Tree DFS Variant

Also implement a DFS for a binary tree:

```python
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def tree_dfs(root):
    """Perform DFS on a binary tree"""
    # TODO: Implement tree DFS
    pass
```

### Checkpoint Questions

1. What happens if we push neighbors in reverse order to the stack?
2. How would we modify DFS to detect cycles in a graph?
3. What's the relationship between DFS and topological sorting?

---

## Complete Implementation

After your group discussion, compare your solution with these implementations:

### Recursive DFS

```python
def dfs_recursive(graph, node, visited=None, traversal_order=None):
    """
    Performs recursive depth-first search on a graph starting from node.

    Args:
        graph: Dictionary representing an adjacency list graph
               {node: [neighbor1, neighbor2, ...]}
        node: Current node to explore
        visited: Set of visited nodes (initialized if None)
        traversal_order: List to track traversal order (initialized if None)

    Returns:
        List of nodes in DFS traversal order
    """
    # Initialize visited set and traversal list on first call
    if visited is None:
        visited = set()
    if traversal_order is None:
        traversal_order = []

    # Mark current node as visited
    visited.add(node)

    # Add to traversal order
    traversal_order.append(node)

    # Recursively visit all unvisited neighbors
    for neighbor in graph[node]:
        if neighbor not in visited:
            dfs_recursive(graph, neighbor, visited, traversal_order)

    return traversal_order
```

### Iterative DFS

```python
def dfs_iterative(graph, start_node):
    """
    Performs iterative depth-first search on a graph starting from start_node.

    Args:
        graph: Dictionary representing an adjacency list graph
               {node: [neighbor1, neighbor2, ...]}
        start_node: Starting node for DFS

    Returns:
        List of nodes in DFS traversal order
    """
    # Initialize stack, visited set, and traversal list
    stack = [start_node]
    visited = set()
    traversal_order = []

    # DFS loop
    while stack:
        # Pop the next node from the stack
        current_node = stack.pop()

        # Process unvisited nodes
        if current_node not in visited:
            visited.add(current_node)
            traversal_order.append(current_node)

            # Add unvisited neighbors to stack (in reverse order for same traversal as recursive)
            for neighbor in reversed(graph[current_node]):
                if neighbor not in visited:
                    stack.append(neighbor)

    return traversal_order

# Example usage
if __name__ == "__main__":
    graph = {
        'A': ['B', 'C'],
        'B': ['A', 'D', 'E'],
        'C': ['A', 'F'],
        'D': ['B'],
        'E': ['B'],
        'F': ['C']
    }

    recursive_result = dfs_recursive(graph, 'A')
    iterative_result = dfs_iterative(graph, 'A')

    print(f"Recursive DFS traversal: {recursive_result}")
    print(f"Iterative DFS traversal: {iterative_result}")
```

### Tree DFS Implementation

```python
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def tree_dfs_recursive(root, traversal=None):
    """Perform DFS on a binary tree recursively (preorder traversal)"""
    if traversal is None:
        traversal = []

    if root is None:
        return traversal

    # Visit root (preorder traversal: root, left, right)
    traversal.append(root.val)

    # Visit left subtree
    tree_dfs_recursive(root.left, traversal)

    # Visit right subtree
    tree_dfs_recursive(root.right, traversal)

    return traversal

def tree_dfs_iterative(root):
    """Perform DFS on a binary tree iteratively (preorder traversal)"""
    if root is None:
        return []

    traversal = []
    stack = [root]

    while stack:
        node = stack.pop()
        traversal.append(node.val)

        # Push right first so left is processed first (LIFO)
        if node.right:
            stack.append(node.right)
        if node.left:
            stack.append(node.left)

    return traversal
```

---

## Common Implementation Mistakes

1. **Not tracking visited nodes**: Can cause infinite loops in graphs with cycles.
2. **Incorrect recursion base case**: Missing or improper termination conditions.
3. **Wrong order of pushing neighbors**: The order affects the traversal sequence.
4. **Stack overflow**: Very deep graphs can exceed recursion limits.
5. **Confusing traversal types**: For trees, DFS has three variants (preorder, inorder, postorder).

---

## Peer Discussion Prompts

1. Compare the recursive and iterative implementations. What are the advantages/disadvantages of each?
2. How would you use DFS to detect cycles in a directed graph?
3. Discuss real-world problems where DFS would be more appropriate than BFS.
4. How does tree traversal DFS differ from graph traversal DFS?

---

## Mini-Challenge: Path Finding with DFS

Modify your DFS implementation to find any path between two nodes (not necessarily the shortest).

```python
def dfs_find_path(graph, start_node, goal_node):
    """
    Finds a path from start_node to goal_node using DFS.

    Returns:
        List representing a path from start to goal, or None if no path exists
    """
    # TODO: Implement path-finding DFS
    pass
```

### Extension for Fast Learners

Implement a DFS-based algorithm to detect cycles in a directed graph.

---

## Applications of DFS

- Topological sorting
- Finding connected components
- Maze generation
- Cycle detection
- Solving puzzles (e.g., solving Sudoku)
- Backtracking algorithms
- Finding strongly connected components (Kosaraju's algorithm)

---

## Tree Traversal Modes

DFS on trees can be performed in three orders:

1. **Preorder**: Node, Left, Right
2. **Inorder**: Left, Node, Right
3. **Postorder**: Left, Right, Node

```python
def preorder(root, traversal=None):
    if traversal is None:
        traversal = []
    if root:
        traversal.append(root.val)  # Visit node
        preorder(root.left, traversal)
        preorder(root.right, traversal)
    return traversal

def inorder(root, traversal=None):
    if traversal is None:
        traversal = []
    if root:
        inorder(root.left, traversal)
        traversal.append(root.val)  # Visit node
        inorder(root.right, traversal)
    return traversal

def postorder(root, traversal=None):
    if traversal is None:
        traversal = []
    if root:
        postorder(root.left, traversal)
        postorder(root.right, traversal)
        traversal.append(root.val)  # Visit node
    return traversal
```

---

## Team Reflection

As a group, discuss:

1. How does the stack data structure (or recursion) enable the "deep" exploration pattern of DFS?
2. When would you choose DFS over BFS in a real problem?
3. What modifications would you need to make DFS work with weighted graphs?
4. How do the three tree traversal orders relate to different use cases?
