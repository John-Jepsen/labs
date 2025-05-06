# Breadth-First Search (BFS): Exploring Graphs Level by Level

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

BFS explores a graph level by level, starting from a source node:

```
Graph:
    1
   / \
  2   3
 / \  / \
4   5 6  7

BFS Traversal from node 1:

Step 1: Visit node 1
   Queue: [1]
   Visited: []

   Dequeue 1, mark as visited, enqueue neighbors (2, 3)
   Queue: [2, 3]
   Visited: [1]

Step 2: Visit node 2
   Dequeue 2, mark as visited, enqueue neighbors (4, 5)
   Queue: [3, 4, 5]
   Visited: [1, 2]

Step 3: Visit node 3
   Dequeue 3, mark as visited, enqueue neighbors (6, 7)
   Queue: [4, 5, 6, 7]
   Visited: [1, 2, 3]

Step 4-7: Visit nodes 4, 5, 6, 7
   Queue eventually becomes empty
   Visited: [1, 2, 3, 4, 5, 6, 7]

Final traversal order: 1, 2, 3, 4, 5, 6, 7
```

## Pseudocode

```
function BFS(graph, start):
    create empty queue
    create empty set of visited nodes

    enqueue start node

    while queue is not empty:
        node = dequeue

        if node is not visited:
            mark node as visited

            for each neighbor of node:
                if neighbor is not visited:
                    enqueue neighbor

    return visited nodes
```

## Annotated Code Template

```python
from collections import deque

def bfs(graph, start):
    """
    Perform a breadth-first search traversal of a graph.

    Args:
        graph: A dictionary representing an adjacency list where keys are nodes
               and values are lists of neighboring nodes
        start: The starting node for the traversal

    Returns:
        A list of nodes in BFS traversal order
    """
    # TODO: Initialize a queue for BFS traversal

    # TODO: Create a set to track visited nodes

    # TODO: Initialize result list to store traversal order

    # TODO: Add start node to the queue

    # TODO: Implement BFS traversal loop
        # Pop a node from the queue

        # If node hasn't been visited:
            # Mark it as visited
            # Add it to the result
            # Add its unvisited neighbors to the queue

    # Return the traversal order
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes what's happening at each step of the traversal

### Task

Working together, complete the BFS implementation. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each step of the traversal.

1. Initialize the necessary data structures
2. Implement the main BFS traversal loop
3. Test the implementation with different graph structures

### Complete Implementation

```python
from collections import deque

def bfs(graph, start):
    # Initialize a queue for BFS traversal
    queue = deque([start])

    # Create a set to track visited nodes
    visited = set()

    # Initialize result list to store traversal order
    result = []

    # BFS traversal loop
    while queue:
        # Pop a node from the queue
        node = queue.popleft()

        # If node hasn't been visited
        if node not in visited:
            # Mark it as visited
            visited.add(node)

            # Add it to the result
            result.append(node)

            # Add its unvisited neighbors to the queue
            for neighbor in graph.get(node, []):
                if neighbor not in visited:
                    queue.append(neighbor)

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
traversal = bfs(graph, start_node)
print(f"BFS traversal starting from {start_node}: {traversal}")
```

## Peer Discussion Prompts

1. How would you modify BFS to find the shortest path between two nodes?
2. What are the differences between BFS and DFS, and when would you choose one over the other?
3. Can BFS be used to determine if a graph is bipartite? How?
4. How would you implement BFS for a disconnected graph to visit all nodes?

## Checkpoint Questions

1. **Checkpoint 1**: Why do we use a queue for BFS instead of a stack?
2. **Checkpoint 2**: What is the state of the queue and visited set after visiting node 1 in our example?
3. **Checkpoint 3**: In what order will nodes 4, 5, 6, and 7 be visited?
4. **Checkpoint 4**: What would happen if we did not check if a node was already visited before processing it?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(V + E)** where V is the number of vertices and E is the number of edges
- We visit each vertex once: O(V)
- We check each edge once: O(E)

### Space Complexity

- **O(V)** for the queue in the worst case (all vertices might be in the queue)
- **O(V)** for the visited set
- Total: O(V)

## Common Implementation Mistakes

1. **Not checking for visited nodes**: This can lead to infinite loops in graphs with cycles
2. **Checking for visited nodes too late**: If we mark nodes as visited when we add them to the queue (instead of when we process them), we can add the same node multiple times
3. **Not handling disconnected components**: Standard BFS only visits nodes reachable from the start node
4. **Not handling empty or None graph cases**: Always validate inputs to avoid runtime errors

## Mini-Challenge

1. Implement BFS to find the shortest path between two nodes
2. Modify BFS to detect cycles in an undirected graph
3. Use BFS to determine if a binary tree is a complete binary tree
4. Implement a solution using BFS for finding the minimum number of moves to solve a puzzle

For the team:

- Implement both BFS and DFS on the same graph
- Compare the traversal orders
- Visualize the search process step by step
- Analyze scenarios where one outperforms the other
