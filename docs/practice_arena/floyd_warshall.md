# Floyd-Warshall Algorithm: All-Pairs Shortest Paths

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Floyd-Warshall finds the shortest paths between all pairs of vertices in a weighted graph:

```
Initial graph:
    A ---3---> B
    ^          |
    |          |
    4          2
    |          |
    D <---1--- C

Step 1: Initialize distance matrix
   | A | B | C | D |
---+---+---+---+---+
 A | 0 | 3 | ∞ | ∞ |
 B | ∞ | 0 | 2 | ∞ |
 C | ∞ | ∞ | 0 | 1 |
 D | 4 | ∞ | ∞ | 0 |

Step 2: Using A as intermediate vertex
   | A | B | C | D |
---+---+---+---+---+
 A | 0 | 3 | ∞ | ∞ |
 B | ∞ | 0 | 2 | ∞ |
 C | ∞ | ∞ | 0 | 1 |
 D | 4 | 7 | ∞ | 0 |  (D→A→B = 4+3 = 7)

Step 3: Using B as intermediate vertex
   | A | B | C | D |
---+---+---+---+---+
 A | 0 | 3 | 5 | ∞ |  (A→B→C = 3+2 = 5)
 B | ∞ | 0 | 2 | ∞ |
 C | ∞ | ∞ | 0 | 1 |
 D | 4 | 7 | 9 | 0 |  (D→A→B→C = 7+2 = 9)

Step 4: Using C as intermediate vertex
   | A | B | C | D |
---+---+---+---+---+
 A | 0 | 3 | 5 | 6 |  (A→B→C→D = 5+1 = 6)
 B | ∞ | 0 | 2 | 3 |  (B→C→D = 2+1 = 3)
 C | ∞ | ∞ | 0 | 1 |
 D | 4 | 7 | 9 | 0 |

Step 5: Using D as intermediate vertex
   | A | B | C | D |
---+---+---+---+---+
 A | 0 | 3 | 5 | 6 |
 B | 7 | 0 | 2 | 3 |  (B→D→A = 3+4 = 7)
 C | 5 | 8 | 0 | 1 |  (C→D→A = 1+4 = 5)
 D | 4 | 7 | 9 | 0 |

Final shortest paths matrix:
   | A | B | C | D |
---+---+---+---+---+
 A | 0 | 3 | 5 | 6 |
 B | 7 | 0 | 2 | 3 |
 C | 5 | 8 | 0 | 1 |
 D | 4 | 7 | 9 | 0 |
```

## Pseudocode

```
function floyd_warshall(graph):
    // Initialize distance matrix from the graph
    let dist[i][j] = weight of edge (i,j) or ∞ if no edge exists
    for each vertex v:
        dist[v][v] = 0

    // Consider each vertex as an intermediate
    for k = 1 to |V|:
        for i = 1 to |V|:
            for j = 1 to |V|:
                if dist[i][k] + dist[k][j] < dist[i][j]:
                    dist[i][j] = dist[i][k] + dist[k][j]

    return dist
```

## Annotated Code Template

```python
def floyd_warshall(graph):
    """
    Implement the Floyd-Warshall algorithm to find all-pairs shortest paths.

    Args:
        graph: A dictionary where keys are tuples (u, v) of vertices and values are edge weights
               graph[(u, v)] = weight of edge from u to v

    Returns:
        A 2D dictionary where dist[u][v] is the shortest distance from u to v
    """
    # Extract all vertices from the graph
    vertices = set()
    for u, v in graph:
        vertices.add(u)
        vertices.add(v)

    # Initialize distance matrix
    # TODO: Initialize distances with direct edges from graph

    # Consider each vertex as an intermediate vertex
    # TODO: Implement the triple nested loop to update distances

    return dist
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm at each step

### Task

Working together, implement the Floyd-Warshall algorithm:

1. First, extract all vertices and initialize the distance matrix
2. Implement the three nested loops to consider each vertex as an intermediate
3. Add code to detect negative cycles
4. Test the algorithm on the example graph

### Complete Implementation

```python
def floyd_warshall(graph):
    # Extract all vertices from the graph
    vertices = set()
    for u, v in graph:
        vertices.add(u)
        vertices.add(v)

    # Convert to ordered list for matrix indexing
    vertices = sorted(list(vertices))
    n = len(vertices)

    # Create vertex to index mapping
    vertex_to_idx = {vertex: i for i, vertex in enumerate(vertices)}

    # Initialize distance matrix
    dist = [[float('infinity') for _ in range(n)] for _ in range(n)]

    # Set diagonal to 0 (distance from vertex to itself)
    for i in range(n):
        dist[i][i] = 0

    # Set initial distances from the graph
    for (u, v), weight in graph.items():
        i, j = vertex_to_idx[u], vertex_to_idx[v]
        dist[i][j] = weight

    # Floyd-Warshall algorithm
    for k in range(n):  # Intermediate vertex
        for i in range(n):  # Source vertex
            for j in range(n):  # Destination vertex
                if dist[i][k] != float('infinity') and dist[k][j] != float('infinity'):
                    dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])

    # Check for negative cycles
    for i in range(n):
        if dist[i][i] < 0:
            print("Graph contains a negative cycle")
            return None

    # Convert the result back to a dictionary
    result = {}
    for i in range(n):
        for j in range(n):
            if dist[i][j] != float('infinity'):
                result[(vertices[i], vertices[j])] = dist[i][j]

    return result

# Example usage
graph = {
    ('A', 'B'): 3,
    ('B', 'C'): 2,
    ('C', 'D'): 1,
    ('D', 'A'): 4
}

shortest_paths = floyd_warshall(graph)

if shortest_paths:
    # Print the distance matrix
    vertices = set()
    for u, v in shortest_paths:
        vertices.add(u)
        vertices.add(v)
    vertices = sorted(list(vertices))

    print("Shortest Paths Matrix:")
    print("   |", end="")
    for v in vertices:
        print(f" {v} |", end="")
    print("\n---+" + "---+".join("---" for _ in vertices))

    for u in vertices:
        print(f" {u} |", end="")
        for v in vertices:
            if (u, v) in shortest_paths:
                print(f" {shortest_paths[(u, v)]} |", end="")
            else:
                print(" ∞ |", end="")
        print()
```

## Peer Discussion Prompts

1. How does the Floyd-Warshall algorithm compare to running Dijkstra's algorithm from each vertex?
2. Why does the algorithm work with negative edge weights (but not negative cycles)?
3. What's the significance of the order of the three nested loops?
4. How could you modify the algorithm to reconstruct the actual shortest paths?

## Checkpoint Questions

1. **Checkpoint 1**: What does the distance matrix look like initially?
2. **Checkpoint 2**: After considering vertex A as an intermediate, what's the shortest path from D to B?
3. **Checkpoint 3**: Why do we set the diagonal elements to 0 in the initial matrix?
4. **Checkpoint 4**: How can we detect if there's a negative cycle in the graph?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(V³)**: where V is the number of vertices
  - Three nested loops, each iterating over all vertices
  - Each relaxation operation is O(1)

### Space Complexity

- **O(V²)**: for storing the distance matrix

## Common Implementation Mistakes

1. **Incorrect initialization**: Not setting the diagonal to 0 or not handling missing edges correctly
2. **Wrong loop order**: Putting the intermediate vertex (k) loop inside the i or j loops
3. **Negative cycle detection**: Forgetting to check for negative cycles on the diagonal
4. **Infinity handling**: Not properly handling infinity values during relaxation
5. **Vertex mapping**: Confusing vertex values with their indices in the matrix

## Mini-Challenge

1. Extend the algorithm to also return the predecessor matrix for path reconstruction.
2. Modify the implementation to handle disconnected graphs properly.
3. Implement a version that can identify all vertices affected by negative cycles.
4. Create a visualization that shows how the shortest paths evolve after each intermediate vertex.

For the team: Compare the performance of Floyd-Warshall with running Dijkstra's algorithm from each vertex on graphs of different sizes and densities.
