# Depth-First Search (DFS): Exploring Graphs Through Deep Traversal

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

DFS explores a graph by going as deep as possible along each branch before backtracking:

```
Graph:
    1
   / \
  2   3
 / \  / \
4   5 6  7

DFS Traversal from node 1 (recursive):

Step 1: Visit node 1
   Stack: [1]
   Visited: []

   Mark 1 as visited, explore first neighbor (2)
   Stack: [1]
   Visited: [1]

Step 2: Visit node 2
   Mark 2 as visited, explore first neighbor (4)
   Stack: [1, 2]
   Visited: [1, 2]

Step 3: Visit node 4
   Mark 4 as visited, no unvisited neighbors
   Stack: [1, 2, 4]
   Visited: [1, 2, 4]

   Backtrack to node 2, explore next neighbor (5)

Step 4: Visit node 5
   Mark 5 as visited, no unvisited neighbors
   Stack: [1, 2, 5]
   Visited: [1, 2, 4, 5]

   Backtrack to node 1, explore next neighbor (3)

Step 5: Visit node 3
   Mark 3 as visited, explore first neighbor (6)
   Stack: [1, 3]
   Visited: [1, 2, 4, 5, 3]

Step 6: Visit node 6
   Mark 6 as visited, no unvisited neighbors
   Stack: [1, 3, 6]
   Visited: [1, 2, 4, 5, 3, 6]

   Backtrack to node 3, explore next neighbor (7)

Step 7: Visit node 7
   Mark 7 as visited, no unvisited neighbors
   Stack: [1, 3, 7]
   Visited: [1, 2, 4, 5, 3, 6, 7]

Final traversal order: 1, 2, 4, 5, 3, 6, 7
```

## Pseudocode

### Recursive DFS

```
function DFS_recursive(graph, node, visited):
    if node is in visited:
        return

    mark node as visited

    for each neighbor of node:
        DFS_recursive(graph, neighbor, visited)

function DFS(graph, start):
    create empty set of visited nodes
    DFS_recursive(graph, start, visited)
    return visited nodes
```

### Iterative DFS

```
function DFS_iterative(graph, start):
    create empty stack
    create empty set of visited nodes

    push start node to stack

    while stack is not empty:
        node = pop from stack

        if node is not visited:
            mark node as visited

            for each neighbor of node:
                if neighbor is not visited:
                    push neighbor to stack

    return visited nodes
```

## Annotated Code Template

```python
def dfs_recursive(graph, start):
    """
    Perform a depth-first search traversal of a graph using recursion.

    Args:
        graph: A dictionary representing an adjacency list where keys are nodes
               and values are lists of neighboring nodes
        start: The starting node for the traversal

    Returns:
        A list of nodes in DFS traversal order
    """
    visited = set()
    result = []

    def dfs_util(node):
        # TODO: Mark node as visited

        # TODO: Add node to result

        # TODO: Recursively visit all unvisited neighbors

    # TODO: Start the recursive traversal

    return result


def dfs_iterative(graph, start):
    """
    Perform a depth-first search traversal of a graph using iteration.

    Args:
        graph: A dictionary representing an adjacency list where keys are nodes
               and values are lists of neighboring nodes
        start: The starting node for the traversal

    Returns:
        A list of nodes in DFS traversal order
    """
    # TODO: Initialize a stack for DFS traversal

    # TODO: Create a set to track visited nodes

    # TODO: Initialize result list to store traversal order

    # TODO: Implement DFS traversal loop
        # Pop a node from the stack

        # If node hasn't been visited:
            # Mark it as visited
            # Add it to the result
            # Add its unvisited neighbors to the stack

    # Return the traversal order
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes what's happening at each step of the traversal

### Task

Working together, complete both the recursive and iterative DFS implementations. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each step of the traversal.

1. Start with the recursive implementation
2. Then implement the iterative version
3. Test both implementations with the same graph structure
4. Compare the traversal orders to ensure they produce valid DFS orderings

### Complete Implementation

```python
def dfs_recursive(graph, start):
    visited = set()
    result = []

    def dfs_util(node):
        # Mark node as visited
        visited.add(node)

        # Add node to result
        result.append(node)

        # Recursively visit all unvisited neighbors
        for neighbor in graph.get(node, []):
            if neighbor not in visited:
                dfs_util(neighbor)

    # Start the recursive traversal
    dfs_util(start)

    return result


def dfs_iterative(graph, start):
    # Initialize a stack for DFS traversal
    stack = [start]

    # Create a set to track visited nodes
    visited = set()

    # Initialize result list to store traversal order
    result = []

    # DFS traversal loop
    while stack:
        # Pop a node from the stack
        node = stack.pop()

        # If node hasn't been visited
        if node not in visited:
            # Mark it as visited
            visited.add(node)

            # Add it to the result
            result.append(node)

            # Add its unvisited neighbors to the stack (in reverse order to match recursive DFS)
            for neighbor in reversed(graph.get(node, [])):
                if neighbor not in visited:
                    stack.append(neighbor)

    return result

# Example usage:
graph = {
    1: [2, 3],
    2: [4, 5],
    3: [6, 7],
    4: [],
    5: [],
    6: [],
    7: []
}
start_node = 1
recursive_traversal = dfs_recursive(graph, start_node)
iterative_traversal = dfs_iterative(graph, start_node)
print(f"DFS recursive traversal from {start_node}: {recursive_traversal}")
print(f"DFS iterative traversal from {start_node}: {iterative_traversal}")
```

## Peer Discussion Prompts

1. How does the traversal order of DFS differ from BFS, and what causes this difference?
2. What are the advantages and disadvantages of recursive vs. iterative DFS implementations?
3. In what problem scenarios would DFS be more appropriate than BFS?
4. How would you modify DFS to detect cycles in a graph?

## Checkpoint Questions

1. **Checkpoint 1**: How does the stack (or call stack in recursive version) help implement the "go deep first" behavior of DFS?
2. **Checkpoint 2**: What is the state of the visited set after visiting nodes 1, 2, and 4 in our example?
3. **Checkpoint 3**: Why do we need to reverse the neighbors in the iterative implementation but not in the recursive one?
4. **Checkpoint 4**: What would be different in the traversal order if we didn't check if a node was already visited?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(V + E)** where V is the number of vertices and E is the number of edges
- We visit each vertex once: O(V)
- We check each edge once: O(E)

### Space Complexity

- **O(V)** for the visited set
- **O(h)** for the call stack in recursive implementation, where h is the maximum depth of the recursion (worst case O(V))
- **O(V)** for the stack in iterative implementation (worst case all vertices might be in the stack)

## Common Implementation Mistakes

1. **Stack overflow**: The recursive implementation may cause stack overflow for very deep graphs
2. **Not checking for cycles**: Not tracking visited nodes can lead to infinite loops in graphs with cycles
3. **Different traversal orders**: The recursive and iterative implementations might produce different valid DFS traversals if not implemented carefully
4. **Forgetting to reverse neighbors**: In the iterative implementation, not reversing the order of neighbors can lead to a different traversal order than the recursive version

## Mini-Challenge

1. Implement DFS to detect cycles in a directed graph
2. Use DFS to find all connected components in an undirected graph
3. Implement topological sorting using DFS
4. Solve a maze problem using DFS by finding a path from start to end

For the team:

- Implement a visualization that shows the difference between DFS and BFS traversals on the same graph
- Compare how DFS and BFS would tackle finding paths in a maze
- Discuss how the choice of data structure (stack vs. queue) influences the traversal behavior
