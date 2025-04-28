# Breadth-First Search (BFS) Algorithm

## Objective

Implement and understand the Breadth-First Search algorithm in a collaborative learning environment. By the end of this session, your team will have coded a working BFS implementation and gained insights into its applications and performance characteristics.

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

BFS explores a graph level by level, visiting all neighbors at the current depth before moving to the next level.

```
    A
   / \
  B   C
 / \   \
D   E   F
```

Traversal order: A → B → C → D → E → F

BFS uses a **queue** to track nodes to visit next, following First-In-First-Out (FIFO) order.

---

## Algorithm Walkthrough

### Pseudocode

```
function BFS(graph, start_node):
    Queue q = new Queue()
    Set visited = new Set()

    q.enqueue(start_node)
    visited.add(start_node)

    while q is not empty:
        current = q.dequeue()
        process(current)

        for each neighbor of current:
            if neighbor not in visited:
                visited.add(neighbor)
                q.enqueue(neighbor)
```

### Time and Space Complexity

- **Time Complexity**: O(V + E) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V) for the queue and visited set

---

## Annotated Code Template

```python
from collections import deque

def breadth_first_search(graph, start_node):
    """
    Performs breadth-first search on a graph starting from start_node.

    Args:
        graph: Dictionary representing an adjacency list graph
               {node: [neighbor1, neighbor2, ...]}
        start_node: Starting node for BFS

    Returns:
        List of nodes in BFS traversal order
    """
    # Initialize queue with start node
    queue = deque([start_node])

    # Track visited nodes to avoid cycles
    visited = set([start_node])

    # Store traversal order
    traversal_order = []

    # TODO: Implement the BFS algorithm
    # While the queue is not empty:
    #   1. Dequeue a node
    #   2. Add it to traversal_order
    #   3. Enqueue all unvisited neighbors
    #   4. Mark them as visited

    return traversal_order
```

---

## Live Coding Group Activity

### Team Roles

- **Driver**: Types the code
- **Navigator**: Guides the implementation
- **Explainer**: Verbalizes what's happening at each step

### Task

Complete the BFS implementation by filling in the missing code in the template. Test it with the following graph:

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

# Expected output from BFS starting at 'A': ['A', 'B', 'C', 'D', 'E', 'F']
```

### Checkpoint Questions

1. What happens if we use a stack instead of a queue?
2. How would we modify BFS to find the shortest path to a target node?
3. What would be different if we were traversing a binary tree instead of a graph?

---

## Complete Implementation

After your group discussion, compare your solution with this implementation:

```python
from collections import deque

def breadth_first_search(graph, start_node):
    """
    Performs breadth-first search on a graph starting from start_node.

    Args:
        graph: Dictionary representing an adjacency list graph
               {node: [neighbor1, neighbor2, ...]}
        start_node: Starting node for BFS

    Returns:
        List of nodes in BFS traversal order
    """
    # Initialize queue with start node
    queue = deque([start_node])

    # Track visited nodes to avoid cycles
    visited = set([start_node])

    # Store traversal order
    traversal_order = []

    # BFS loop
    while queue:
        # Dequeue the next node
        current_node = queue.popleft()

        # Add to traversal order
        traversal_order.append(current_node)

        # Check all neighbors
        for neighbor in graph[current_node]:
            # Only process unvisited nodes
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)

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

    result = breadth_first_search(graph, 'A')
    print(f"BFS traversal: {result}")
```

---

## Common Implementation Mistakes

1. **Forgetting to mark nodes as visited**: This can cause infinite loops in graphs with cycles.
2. **Using a stack instead of a queue**: This would result in DFS, not BFS.
3. **Not checking if a node is visited before enqueueing**: This can lead to duplicate processing.
4. **Missing edge cases**: Not handling disconnected graphs or invalid start nodes.

---

## Peer Discussion Prompts

1. How does BFS differ from other search algorithms?
2. What kind of problems is BFS particularly well-suited for?
3. Explain how BFS naturally finds the shortest path in an unweighted graph.
4. Discuss real-world applications where BFS would be useful.

---

## Mini-Challenge: BFS Path Finding

Modify your BFS implementation to find the shortest path between two nodes.

```python
def bfs_shortest_path(graph, start_node, goal_node):
    """
    Finds shortest path between start_node and goal_node using BFS.

    Returns:
        List representing the path from start to goal, or None if no path exists
    """
    # TODO: Implement shortest path BFS
    pass
```

### Extension for Fast Learners

Implement a BFS variant that calculates the distance (number of edges) from the start node to every other node in the graph.

---

## Applications of BFS

- Finding shortest paths in unweighted graphs
- Web crawling
- Social network connections (e.g., finding "friends of friends")
- Level order traversal of trees
- Finding connected components
- Solving puzzles like mazes or sliding puzzles

---

## Team Reflection

As a group, discuss:

1. What was the most challenging part of implementing BFS?
2. How does the queue data structure enable the "level-by-level" exploration?
3. What would you need to modify to make this work with different graph representations?
4. Can you think of a real-world problem where BFS would be the optimal solution?
