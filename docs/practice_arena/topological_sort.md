# Topological Sort: Ordering Directed Acyclic Graphs

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Topological sort produces a linear ordering of vertices in a directed acyclic graph (DAG) such that for every directed edge (u,v), vertex u comes before vertex v:

```
Example Directed Acyclic Graph:
    0
   / \
  ↓   ↓
  1→→→2
  ↓   ↓
  ↓   ↓
  →→→→3

Step 1: Start DFS from node 0
   Visit 0 → Visit 1 → Visit 3 → Add 3 to stack
   Backtrack to 1 → No more unvisited neighbors → Add 1 to stack
   Backtrack to 0 → Visit 2 → Visit 3 (already visited)
   Backtrack to 2 → Add 2 to stack
   Backtrack to 0 → Add 0 to stack

   Stack (bottom to top): [3, 1, 2, 0]
   Topological Order: [0, 2, 1, 3]

Alternate valid topological ordering: [0, 1, 2, 3]
(There can be multiple valid topological orderings for a DAG)
```

## Pseudocode

### DFS-based Topological Sort

```
function topological_sort(graph):
    create empty set visited
    create empty stack result

    function dfs(node):
        mark node as visited

        for each neighbor of node:
            if neighbor is not visited:
                dfs(neighbor)

        push node onto result stack

    for each node in graph:
        if node is not visited:
            dfs(node)

    return result stack in reverse order
```

### Kahn's Algorithm (Alternative Approach)

```
function kahn_topological_sort(graph):
    calculate in-degree for each vertex
    create queue and add all vertices with in-degree 0
    create empty list result

    while queue is not empty:
        vertex = dequeue from queue
        add vertex to result

        for each neighbor of vertex:
            reduce in-degree of neighbor by 1
            if in-degree of neighbor becomes 0:
                enqueue neighbor to queue

    if length of result != number of vertices:
        return "Graph has a cycle"
    else:
        return result
```

## Annotated Code Template

```python
def topological_sort(graph):
    """
    Perform topological sorting of a directed acyclic graph (DAG).

    Args:
        graph: A dictionary representing an adjacency list where keys are nodes
               and values are lists of neighboring nodes

    Returns:
        A list of nodes in topological order
    """
    # TODO: Initialize data structures (visited set and result stack)

    # TODO: Define DFS function to perform topological sort
        # Mark node as visited

        # Explore all unvisited neighbors

        # Add current node to result after exploring all neighbors

    # TODO: Start DFS from each unvisited node

    # TODO: Return the topological order (reverse of result stack)


def kahn_topological_sort(graph):
    """
    Perform topological sorting of a directed acyclic graph (DAG) using Kahn's algorithm.

    Args:
        graph: A dictionary representing an adjacency list where keys are nodes
               and values are lists of neighboring nodes

    Returns:
        A list of nodes in topological order, or None if the graph has a cycle
    """
    # TODO: Calculate in-degree for each vertex

    # TODO: Initialize queue with nodes having in-degree 0

    # TODO: Process nodes in queue, reducing in-degree of neighbors

    # TODO: Return the topological order or detect cycles
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm's progress and the meaning of each step

### Task

Working together, implement both the DFS-based and Kahn's algorithm approaches to topological sorting. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each step of the algorithm.

1. Start with implementing the DFS-based approach
2. Then implement Kahn's algorithm
3. Test both implementations with the same directed acyclic graph
4. Discuss how these approaches would detect cycles in a graph

### Complete Implementation

```python
def topological_sort_dfs(graph):
    """DFS-based topological sort implementation."""
    # Initialize data structures
    visited = set()
    result_stack = []

    def dfs(node):
        # Mark node as visited
        visited.add(node)

        # Explore all unvisited neighbors
        for neighbor in graph.get(node, []):
            if neighbor not in visited:
                dfs(neighbor)

        # After exploring all neighbors, add current node to result
        result_stack.append(node)

    # Start DFS from each unvisited node
    for node in graph:
        if node not in visited:
            dfs(node)

    # Return the topological order (reverse of result stack)
    return result_stack[::-1]


def kahn_topological_sort(graph):
    """Kahn's algorithm implementation for topological sort."""
    # Create a copy of the graph and calculate in-degree for each vertex
    in_degree = {node: 0 for node in graph}
    for node in graph:
        for neighbor in graph[node]:
            in_degree[neighbor] = in_degree.get(neighbor, 0) + 1

    # Initialize queue with nodes having in-degree 0
    from collections import deque
    queue = deque([node for node in in_degree if in_degree[node] == 0])

    # Process nodes in queue
    result = []
    while queue:
        # Remove a node with in-degree 0
        node = queue.popleft()
        result.append(node)

        # Reduce in-degree of neighbors
        for neighbor in graph.get(node, []):
            in_degree[neighbor] -= 1
            # If in-degree becomes 0, add to queue
            if in_degree[neighbor] == 0:
                queue.append(neighbor)

    # Check if we have a valid topological sort
    if len(result) != len(graph):
        return None  # Graph has a cycle
    return result


# Example usage:
graph = {
    0: [1, 2],
    1: [3],
    2: [3],
    3: []
}

# Test the DFS-based approach
dfs_result = topological_sort_dfs(graph)
print(f"DFS-based Topological Sort: {dfs_result}")

# Test Kahn's algorithm
kahn_result = kahn_topological_sort(graph)
print(f"Kahn's Topological Sort: {kahn_result}")

# Test with a graph containing a cycle
cyclic_graph = {
    0: [1],
    1: [2],
    2: [0, 3],
    3: []
}
print("\nTesting with a cyclic graph:")
kahn_result = kahn_topological_sort(cyclic_graph)
print(f"Kahn's Topological Sort (should detect cycle): {'Cycle detected' if kahn_result is None else kahn_result}")

# Application example: Course scheduling
courses = {
    "Calculus I": ["Calculus II"],
    "Calculus II": ["Calculus III", "Differential Equations"],
    "Calculus III": ["Real Analysis"],
    "Algebra": ["Calculus I", "Discrete Math"],
    "Discrete Math": ["Data Structures"],
    "Data Structures": ["Algorithms"],
    "Differential Equations": []
}
course_order = topological_sort_dfs(courses)
print("\nCourse Schedule Order:")
for i, course in enumerate(course_order, 1):
    print(f"{i}. {course}")
```

## Peer Discussion Prompts

1. What real-world problems can be modeled as topological sorting problems?
2. What are the differences between the DFS-based approach and Kahn's algorithm for topological sorting?
3. How do these algorithms behave when the input graph contains a cycle?
4. Can there be multiple valid topological orderings for a DAG? If yes, how many?

## Checkpoint Questions

1. **Checkpoint 1**: Why do we add a node to the result stack only after exploring all its neighbors in the DFS approach?
2. **Checkpoint 2**: What is the significance of in-degree in Kahn's algorithm?
3. **Checkpoint 3**: How can Kahn's algorithm detect if a graph has a cycle?
4. **Checkpoint 4**: In the example graph, why can both [0, 2, 1, 3] and [0, 1, 2, 3] be valid topological orderings?

## Time and Space Complexity Walkthrough

### Time Complexity

- **DFS-based approach**: O(V + E) where V is the number of vertices and E is the number of edges
  - We visit each vertex once: O(V)
  - We explore each edge once: O(E)
- **Kahn's algorithm**: Also O(V + E)
  - Calculating in-degrees: O(E)
  - Processing all vertices and edges: O(V + E)

### Space Complexity

- **DFS-based approach**: O(V)
  - O(V) for the visited set
  - O(V) for the result stack
  - O(V) for the recursion call stack (in the worst case)
- **Kahn's algorithm**: O(V)
  - O(V) for the in-degree dictionary
  - O(V) for the queue
  - O(V) for the result list

## Common Implementation Mistakes

1. **Not checking for cycles**: A topological sort is only defined for DAGs; both algorithms need to handle cyclic graphs
2. **Incorrect graph representation**: Ensure the graph representation correctly represents the dependencies
3. **Forgetting to initialize in-degrees**: In Kahn's algorithm, all nodes need to have their in-degree initialized
4. **Not considering disconnected components**: The algorithm should process all nodes, even in disconnected components

## Mini-Challenge

1. Implement a function that detects if a directed graph has a cycle using topological sort
2. Modify the topological sort algorithm to output all possible valid topological orderings
3. Solve a real-world problem like course scheduling with prerequisites
4. Implement a function to find the longest path in a DAG using topological sort

For the team:

- Create a visualization that shows the step-by-step execution of topological sort on a DAG
- Compare the behavior of DFS-based and Kahn's algorithm on the same graph
- Discuss how topological sort can be applied in dependency resolution problems
