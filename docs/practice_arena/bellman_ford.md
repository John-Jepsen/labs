# Bellman-Ford Algorithm: Single-Source Shortest Paths with Negative Weights

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Bellman-Ford algorithm finds the shortest paths from a source vertex to all other vertices, even with negative edge weights:

```
Graph:
    A ---6---> B
    |          |
    |          |
    2          -1
    |          |
    V          V
    C ---3---> D
    ^          |
    |          |
    -5         2
    |          |
    E <---1--- F

Step 1: Initialize distances
dist[A] = 0, dist[B] = ∞, dist[C] = ∞, dist[D] = ∞, dist[E] = ∞, dist[F] = ∞

Step 2: Relax all edges |V|-1 times (5 times in our example)
Loop 1:
- Relax A→B: dist[B] = dist[A] + w(A,B) = 0 + 6 = 6
- Relax A→C: dist[C] = dist[A] + w(A,C) = 0 + 2 = 2
- Relax B→D: dist[D] = dist[B] + w(B,D) = 6 + (-1) = 5
- Relax C→D: dist[D] = min(dist[D], dist[C] + w(C,D)) = min(5, 2 + 3) = 5
- Relax D→F: dist[F] = dist[D] + w(D,F) = 5 + 2 = 7
- Relax F→E: dist[E] = dist[F] + w(F,E) = 7 + 1 = 8
- Relax E→C: dist[C] = min(dist[C], dist[E] + w(E,C)) = min(2, 8 + (-5)) = 2 (no change)

Loop 2-5: Continue relaxing... (eventually reaching the final values)

Final distances:
dist[A] = 0, dist[B] = 6, dist[C] = 2, dist[D] = 5, dist[E] = 8, dist[F] = 7

Step 3: Check for negative cycles
If any distance can still be reduced, there's a negative cycle.
```

## Pseudocode

```
function bellman_ford(graph, source):
    // Initialize distances from source to all other vertices as INFINITY
    distance[source] = 0

    // Relax all edges |V| - 1 times
    for i = 1 to |V| - 1:
        for each edge (u, v) with weight w:
            if distance[u] + w < distance[v]:
                distance[v] = distance[u] + w

    // Check for negative weight cycles
    for each edge (u, v) with weight w:
        if distance[u] + w < distance[v]:
            return "Graph contains a negative weight cycle"

    return distance
```

## Annotated Code Template

```python
def bellman_ford(graph, source):
    """
    Implement the Bellman-Ford algorithm to find shortest paths from a source vertex.

    Args:
        graph: A list of edges where each edge is (u, v, w) - from u to v with weight w
        source: The source vertex

    Returns:
        A dictionary of shortest distances from source to all vertices or None if there's a negative cycle
    """
    # Extract all vertices from the graph
    vertices = set()
    for u, v, w in graph:
        vertices.add(u)
        vertices.add(v)

    # Initialize distances
    # TODO: Set distance to source as 0 and all others as infinity

    # Relax all edges |V| - 1 times
    # TODO: Implement the main relaxation loop

    # Check for negative weight cycles
    # TODO: Implement the negative cycle detection

    return distances
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm at each step

### Task

Working together, implement the Bellman-Ford algorithm:

1. Start by initializing the distances for all vertices
2. Implement the edge relaxation process
3. Add the negative cycle detection
4. Test the algorithm on the example graph

### Complete Implementation

```python
def bellman_ford(graph, source):
    # Extract all vertices from the graph
    vertices = set()
    for u, v, w in graph:
        vertices.add(u)
        vertices.add(v)

    # Initialize distances
    distances = {vertex: float('infinity') for vertex in vertices}
    distances[source] = 0

    # Get the number of vertices
    V = len(vertices)

    # Relax all edges |V| - 1 times
    for i in range(V - 1):
        for u, v, w in graph:
            if distances[u] != float('infinity') and distances[u] + w < distances[v]:
                distances[v] = distances[u] + w

    # Check for negative weight cycles
    for u, v, w in graph:
        if distances[u] != float('infinity') and distances[u] + w < distances[v]:
            print("Graph contains a negative weight cycle")
            return None

    return distances

# Example usage
graph = [
    ('A', 'B', 6), ('A', 'C', 2),
    ('B', 'D', -1), ('C', 'D', 3),
    ('D', 'F', 2), ('F', 'E', 1),
    ('E', 'C', -5)
]

source = 'A'
distances = bellman_ford(graph, source)

if distances:
    print("Shortest distances from vertex", source)
    for vertex, distance in distances.items():
        print(f"{vertex}: {distance}")
```

## Peer Discussion Prompts

1. How does Bellman-Ford differ from Dijkstra's algorithm in terms of capabilities and limitations?
2. What real-world problems might involve negative edge weights?
3. Why is the relaxation step repeated exactly |V|-1 times?
4. How would you modify the algorithm to actually return the shortest paths (not just distances)?

## Checkpoint Questions

1. **Checkpoint 1**: What is the initial state of the distance array?
2. **Checkpoint 2**: After the first relaxation pass, what is the distance to vertex D?
3. **Checkpoint 3**: How can we detect if a negative cycle exists in the graph?
4. **Checkpoint 4**: What is the maximum number of edges in a shortest path without cycles?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(V·E)**: where V is the number of vertices and E is the number of edges
  - We relax each edge V-1 times: O(V·E)
  - Negative cycle check is O(E)

### Space Complexity

- **O(V)**: for storing distances to all vertices

## Common Implementation Mistakes

1. **Not handling disconnected vertices**: Forgetting to initialize all vertices
2. **Incorrect negative cycle detection**: The check must be done after the main relaxation loop
3. **Assuming directed graphs only**: Not adapting for undirected graphs (which need edges in both directions)
4. **Inefficient relaxation**: Not skipping relaxation when the source vertex has infinity distance
5. **Missing early termination**: Not adding an optimization to exit early when no relaxation occurs in a round

## Mini-Challenge

1. Modify the algorithm to also return the predecessor array so you can reconstruct the shortest paths.
2. Implement a version that can determine which vertices are affected by negative cycles.
3. Optimize the implementation to terminate early if no relaxations occur in a round.
4. Create a visualization that shows how distances change during each iteration.

For the team: Compare the performance of Bellman-Ford vs. Dijkstra's algorithm on graphs with and without negative edges, and analyze when each is more efficient.
