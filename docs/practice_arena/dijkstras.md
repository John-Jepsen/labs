# Dijkstra's Algorithm: Finding Shortest Paths in Weighted Graphs

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Dijkstra's algorithm finds the shortest path from a source node to all other nodes in a weighted graph:

```
Graph (node: [(neighbor, weight)]):
1: [(2, 2), (3, 4)]
2: [(3, 1), (4, 7)]
3: [(4, 3)]
4: []

Visual representation:
  2
1---2
|   | \
4   1  7
|   |   \
3---4----4
    3

Dijkstra's Algorithm from node 1:

Step 1: Initialize
   Distance to 1: 0 (starting node)
   Distance to all other nodes: infinity
   Priority Queue: [(0, 1)]

Step 2: Process node 1
   Extract (0, 1) from queue
   Check neighbors of 1:
     Node 2: 0 + 2 = 2 (better than infinity)
     Node 3: 0 + 4 = 4 (better than infinity)
   Priority Queue: [(2, 2), (4, 3)]
   Distances: {1: 0, 2: 2, 3: 4, 4: infinity}

Step 3: Process node 2
   Extract (2, 2) from queue
   Check neighbors of 2:
     Node 3: 2 + 1 = 3 (better than current 4)
     Node 4: 2 + 7 = 9 (better than infinity)
   Priority Queue: [(3, 3), (4, 3), (9, 4)]
   Distances: {1: 0, 2: 2, 3: 3, 4: 9}

Step 4: Process node 3
   Extract (3, 3) from queue
   Check neighbors of 3:
     Node 4: 3 + 3 = 6 (better than current 9)
   Priority Queue: [(6, 4)]
   Distances: {1: 0, 2: 2, 3: 3, 4: 6}

Step 5: Process node 4
   Extract (6, 4) from queue
   No neighbors to process
   Priority Queue: []

Final distances from node 1:
   Node 1: 0
   Node 2: 2
   Node 3: 3
   Node 4: 6
```

## Pseudocode

```
function dijkstra(graph, start):
    create priority queue
    create distances map, initialize all to infinity
    set distance to start node as 0
    add start node to priority queue with priority 0

    while priority queue is not empty:
        current_node = extract minimum from priority queue

        if current node's distance in queue > stored distance:
            continue  // Skip outdated entries

        for each neighbor of current_node:
            calculate distance to neighbor through current_node

            if calculated distance < current distance to neighbor:
                update distance to neighbor
                add neighbor to priority queue with new distance as priority

    return distances
```

## Annotated Code Template

```python
import heapq

def dijkstra(graph, start):
    """
    Find shortest paths from start node to all other nodes in a weighted graph.

    Args:
        graph: A dictionary representing an adjacency list where keys are nodes
               and values are lists of (neighbor, weight) tuples
        start: The starting node

    Returns:
        A dictionary with shortest distances from start to all nodes
    """
    # TODO: Initialize priority queue with start node

    # TODO: Create distances map, initialize all to infinity, start to 0

    # TODO: Process nodes in priority order
        # Extract node with minimum distance

        # Skip outdated entries

        # Process all neighbors
            # Calculate distance through current node

            # If better path found, update distance and add to queue

    # Return shortest distances
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm's progress at each step

### Task

Working together, complete the Dijkstra's algorithm implementation. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each step of the algorithm.

1. Start with initializing the data structures
2. Implement the main loop that processes nodes in priority order
3. Add the logic to update distances and enqueue nodes
4. Test with the example graph and trace through the execution

### Complete Implementation

```python
import heapq

def dijkstra(graph, start):
    # Initialize priority queue with start node
    priority_queue = [(0, start)]

    # Create distances map, initialize all to infinity, start to 0
    distances = {node: float('infinity') for node in graph}
    distances[start] = 0

    # Track visited nodes (optional optimization)
    visited = set()

    # Process nodes in priority order
    while priority_queue:
        # Extract node with minimum distance
        current_distance, current_node = heapq.heappop(priority_queue)

        # Skip if already processed or outdated entry
        if current_node in visited or current_distance > distances[current_node]:
            continue

        # Mark as visited
        visited.add(current_node)

        # Process all neighbors
        for neighbor, weight in graph[current_node]:
            # Calculate distance through current node
            distance = current_distance + weight

            # If better path found, update distance and add to queue
            if distance < distances[neighbor]:
                distances[neighbor] = distance
                heapq.heappush(priority_queue, (distance, neighbor))

    return distances

# Example usage:
graph = {
    1: [(2, 2), (3, 4)],
    2: [(3, 1), (4, 7)],
    3: [(4, 3)],
    4: []
}
start_node = 1
shortest_distances = dijkstra(graph, start_node)
print(f"Shortest distances from node {start_node}: {shortest_distances}")

# To trace the path to a specific node
def get_shortest_path(graph, start, end):
    # First find all shortest distances
    distances = dijkstra(graph, start)

    # Then reconstruct the path
    path = []
    current = end

    # Implementation requires parent tracking - left as a challenge
    # This is placeholder logic
    return path
```

## Peer Discussion Prompts

1. How does Dijkstra's algorithm differ from BFS for finding shortest paths?
2. Why doesn't Dijkstra's algorithm work with negative weights, and what algorithm could we use instead?
3. What real-world applications can be modeled using Dijkstra's algorithm?
4. How would you optimize this implementation for performance in a large graph?

## Checkpoint Questions

1. **Checkpoint 1**: Why do we need a priority queue instead of a regular queue?
2. **Checkpoint 2**: What is the purpose of checking if `current_distance > distances[current_node]`?
3. **Checkpoint 3**: What would happen if we processed nodes without using a priority queue?
4. **Checkpoint 4**: How would we modify this algorithm to track the actual paths, not just distances?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O((V + E) log V)** with binary heap implementation of priority queue
  - Each vertex is inserted into priority queue once: O(V log V)
  - Each edge relaxation takes O(log V) time for the priority queue update
  - Total: O(V log V + E log V) = O((V + E) log V)
- **O(V²)** with array implementation of priority queue (no heap)
  - Finding minimum distance vertex takes O(V) time
  - We do this V times: O(V²)
  - Edge relaxation is O(1)
  - Total: O(V²)

### Space Complexity

- **O(V)** for the distances array
- **O(V)** for the priority queue
- Total: O(V)

## Common Implementation Mistakes

1. **Forgetting to skip outdated entries**: If we don't check if a node's current distance in the queue is greater than its stored distance, we process unnecessary entries
2. **Using BFS instead of priority queue**: Using a regular queue results in incorrect shortest paths for weighted graphs
3. **Not handling disconnected components**: Nodes unreachable from the start will have infinite distance
4. **Incorrect distance comparisons**: Initialize distances to infinity, not -1 or other placeholders

## Mini-Challenge

1. Extend the implementation to reconstruct and return the shortest paths, not just distances
2. Modify Dijkstra's algorithm to stop once the shortest path to a specific target node is found
3. Implement a version that works with adjacency matrix representation instead of adjacency list
4. Create a visualization that shows how the algorithm progresses step by step

For the team:

- Compare Dijkstra's algorithm with BFS on the same graph (all weights = 1)
- Analyze how the algorithm performs on dense vs. sparse graphs
- Discuss how to handle cases where multiple paths have the same shortest distance
