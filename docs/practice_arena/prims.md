# Prim's Algorithm: Minimum Spanning Tree Construction

## Environment Setup

```bash
# Python 3.8+ with heapq module (built-in)
# Verify your Python installation
python --version

# No additional packages needed for basic implementation
```

## Visual Explanation

Prim's algorithm builds a minimum spanning tree (MST) by connecting vertices one by one:

```
Graph:
  A --- 4 --- B
  |           |
  1           6
  |           |
  C --- 5 --- D
  \           /
   \    2    /
    ---------

Step 1: Start at vertex A
MST: [A]

Step 2: Check all edges from A (to B:4, to C:1)
Select minimum edge A-C:1
MST: [A, C]

Step 3: Check all edges from A and C (to B:4, to D:2, to D:5)
Select minimum edge C-D:2
MST: [A, C, D]

Step 4: Check all edges from A, C, D (to B:4, to B:6)
Select minimum edge A-B:4
MST: [A, C, D, B]

Final MST: [(A,C,1), (C,D,2), (A,B,4)]
Total weight: 7
```

## Pseudocode

```
function prim(graph, start_vertex):
    priority_queue = [(0, start_vertex, -1)]  // (weight, vertex, from_vertex)
    visited = empty set
    mst = empty list

    while priority_queue is not empty:
        weight, vertex, from_vertex = extract_min(priority_queue)

        if vertex not in visited:
            visited.add(vertex)

            if from_vertex != -1:
                mst.append((from_vertex, vertex, weight))

            for each neighbor, edge_weight of vertex:
                if neighbor not in visited:
                    insert((edge_weight, neighbor, vertex), priority_queue)

    return mst
```

## Annotated Code Template

```python
import heapq

def prims_algorithm(graph, start_vertex):
    """
    Implement Prim's algorithm to find the minimum spanning tree.

    Args:
        graph: A dictionary where keys are vertices and values are lists of (neighbor, weight) tuples
        start_vertex: The vertex to start building the MST from

    Returns:
        A list of (source, destination, weight) representing the MST edges
    """
    # Initialize the priority queue with starting vertex
    # Format: (weight, vertex, parent)
    # TODO: Initialize priority queue with the starting vertex

    # Set to keep track of vertices in the MST
    # TODO: Initialize a set to track visited vertices

    # List to store MST edges
    # TODO: Initialize a list to store MST edges

    # Process vertices until priority queue is empty
    while priority_queue:
        # TODO: Extract the minimum weight edge from priority queue

        # TODO: Check if current vertex is already in MST

        # TODO: If not in MST, add the vertex to MST

        # TODO: Add edge to MST (except for the starting vertex)

        # TODO: Add all edges from current vertex to priority queue

    return mst
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm at each step

### Task

Working together, implement Prim's algorithm:

1. First, complete the initialization of data structures
2. Implement the main loop to extract minimum weight edges
3. Add code to add new edges to the priority queue
4. Test the algorithm on the example graph

### Complete Implementation

```python
import heapq

def prims_algorithm(graph, start_vertex):
    # Initialize priority queue with starting vertex (weight, vertex, parent)
    priority_queue = [(0, start_vertex, None)]

    # Set to keep track of vertices in the MST
    visited = set()

    # List to store MST edges
    mst = []

    # Keep track of total weight
    total_weight = 0

    # Process vertices until priority queue is empty
    while priority_queue:
        # Extract the minimum weight edge
        weight, current_vertex, parent = heapq.heappop(priority_queue)

        # Skip if vertex already in MST
        if current_vertex in visited:
            continue

        # Add vertex to MST
        visited.add(current_vertex)

        # Add edge to MST (except for starting vertex)
        if parent is not None:
            mst.append((parent, current_vertex, weight))
            total_weight += weight

        # Add all edges from current vertex to priority queue
        for neighbor, edge_weight in graph[current_vertex]:
            if neighbor not in visited:
                heapq.heappush(priority_queue, (edge_weight, neighbor, current_vertex))

    return mst, total_weight

# Example usage
graph = {
    'A': [('B', 4), ('C', 1)],
    'B': [('A', 4), ('D', 6)],
    'C': [('A', 1), ('D', 2), ('D', 5)],
    'D': [('C', 2), ('C', 5), ('B', 6)]
}

mst, total_weight = prims_algorithm(graph, 'A')
print(f"Minimum Spanning Tree Edges: {mst}")
print(f"Total MST Weight: {total_weight}")
```

## Peer Discussion Prompts

1. How does Prim's algorithm differ from Kruskal's algorithm in terms of approach?
2. What data structures are critical for an efficient implementation of Prim's algorithm?
3. In what scenarios might Prim's algorithm be preferable to Kruskal's algorithm?
4. How would you modify this algorithm to handle disconnected graphs?

## Checkpoint Questions

1. **Checkpoint 1**: What edge is added to the MST first in our example?
2. **Checkpoint 2**: At each step, how do we determine which edge to add next?
3. **Checkpoint 3**: Why do we need to check if a vertex is already visited before adding it to the MST?
4. **Checkpoint 4**: What is the time complexity of adding an edge to a priority queue in this implementation?

## Time and Space Complexity Walkthrough

### Time Complexity

- **With adjacency list and binary heap**: O(E log V)
  - Each edge can be added/extracted from the priority queue: O(log V)
  - We process potentially all E edges
- **With adjacency matrix and array**: O(V²)
  - Each vertex requires examining all other vertices

### Space Complexity

- **O(V + E)**:
  - Priority queue: O(E) in worst case
  - Visited set: O(V)
  - MST list: O(V-1) edges

## Common Implementation Mistakes

1. **Not handling cycles**: Forgetting to check if a vertex is already in the MST
2. **Incorrect priority queue usage**: Not updating priority when a shorter path is found
3. **Disconnected graphs**: Assuming the graph is fully connected
4. **Edge direction**: Not properly handling undirected graphs by adding edges in both directions
5. **Priority queue implementation**: Using the wrong ordering in the heap (max instead of min)

## Mini-Challenge

1. Modify the algorithm to return the MST as an adjacency list rather than a list of edges.
2. Implement a version that works with an adjacency matrix representation.
3. Extend the algorithm to handle weighted, directed graphs.
4. Create a visualization of the MST building process step by step.

For the team: Compare the performance of Prim's algorithm vs. Kruskal's algorithm on different types of graphs (sparse vs. dense).
