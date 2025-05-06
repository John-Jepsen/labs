# Kruskal's Algorithm: Finding Minimum Spanning Trees

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

Kruskal's algorithm finds a minimum spanning tree for a connected weighted graph by selecting edges in order of increasing weight:

```
Original Graph:
    A
   /|\
  / | \
 B--C--D
 |\ |  |
 | \|  |
 E--F--G

Edge weights:
A-B: 7, A-C: 8, A-D: 5
B-C: 3, B-E: 6, B-F: 2
C-F: 3, D-G: 2, E-F: 4, F-G: 5

Step 1: Sort all edges by weight:
B-F: 2, D-G: 2, B-C: 3, C-F: 3, E-F: 4, A-D: 5, F-G: 5, B-E: 6, A-B: 7, A-C: 8

Step 2: Initialize each vertex as a separate component
[A], [B], [C], [D], [E], [F], [G]

Step 3: Add edges in order of increasing weight, skipping those that form cycles:
Add B-F: 2 → Components: [A], [B,F], [C], [D], [E], [G]
Add D-G: 2 → Components: [A], [B,F], [C], [D,G], [E]
Add B-C: 3 → Components: [A], [B,F,C], [D,G], [E]
Add C-F: 3 → (Skip - would form cycle since B, F, and C are already connected)
Add E-F: 4 → Components: [A], [B,F,C,E], [D,G]
Add A-D: 5 → Components: [A,D,G], [B,F,C,E]
Add F-G: 5 → Components: [A,D,G,B,F,C,E]
Add B-E: 6 → (Skip - would form cycle)
Add A-B: 7 → (Skip - would form cycle)
Add A-C: 8 → (Skip - would form cycle)

Final MST (shown with selected edges):
    A
    |
    D
    |
B---C G
|  /
| /
E-F

Selected edges: B-F, D-G, B-C, E-F, A-D, F-G
Total weight: 2 + 2 + 3 + 4 + 5 + 5 = 21
```

## Pseudocode

```
function kruskal(graph, vertices):
    create empty set mst_edges
    initialize union-find data structure with vertices

    sort all edges in graph by weight (ascending)

    for each edge (u, v, weight) in sorted edges:
        if find_set(u) != find_set(v):  // Vertices not in same component
            add edge to mst_edges
            union_sets(u, v)

    return mst_edges

class UnionFind:
    function initialize(vertices):
        for each vertex:
            make_set(vertex)

    function find_set(vertex):
        if vertex != parent[vertex]:
            parent[vertex] = find_set(parent[vertex])  // Path compression
        return parent[vertex]

    function union_sets(u, v):
        root_u = find_set(u)
        root_v = find_set(v)

        if root_u != root_v:
            if rank[root_u] > rank[root_v]:
                parent[root_v] = root_u
            else:
                parent[root_u] = root_v
                if rank[root_u] == rank[root_v]:
                    rank[root_v] += 1
```

## Annotated Code Template

```python
class UnionFind:
    """
    Union-Find data structure for efficiently tracking connected components.
    """
    def __init__(self, size):
        """Initialize with 'size' separate components."""
        # TODO: Initialize parent array where each element is its own parent

        # TODO: Initialize rank array for union by rank optimization

    def find(self, x):
        """Find the representative (root) of the component containing x."""
        # TODO: Implement path compression for efficiency

    def union(self, x, y):
        """Merge components containing x and y, if they are different."""
        # TODO: Find the roots of x and y

        # TODO: If roots are different, merge components using rank


def kruskal(edges, n):
    """
    Find the minimum spanning tree using Kruskal's algorithm.

    Args:
        edges: List of edges as (u, v, weight) tuples
        n: Number of vertices in the graph

    Returns:
        List of edges in the minimum spanning tree
    """
    # TODO: Initialize union-find data structure

    # TODO: Sort edges by weight

    # TODO: Greedily select edges that don't form cycles
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm's progress and rationale at each step

### Task

Working together, complete the Kruskal's algorithm implementation. The driver will code, the navigator will guide the implementation, and the explainer will verbalize what's happening during each step of the algorithm.

1. Start with implementing the UnionFind data structure
2. Implement the Kruskal's algorithm function
3. Test the implementation with a sample graph
4. Trace through the execution and verify the result

### Complete Implementation

```python
class UnionFind:
    """
    Union-Find data structure for efficiently tracking connected components.
    """
    def __init__(self, size):
        """Initialize with 'size' separate components."""
        # Each element starts as its own parent
        self.parent = list(range(size))
        # Rank is used for union by rank optimization
        self.rank = [0] * size

    def find(self, x):
        """Find the representative (root) of the component containing x with path compression."""
        if self.parent[x] != x:
            # Path compression: point directly to the root
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]

    def union(self, x, y):
        """Merge components containing x and y, if they are different."""
        # Find the roots of x and y
        root_x = self.find(x)
        root_y = self.find(y)

        # If they're already in the same component, do nothing
        if root_x == root_y:
            return False

        # Union by rank: attach smaller rank tree under root of higher rank tree
        if self.rank[root_x] < self.rank[root_y]:
            self.parent[root_x] = root_y
        elif self.rank[root_x] > self.rank[root_y]:
            self.parent[root_y] = root_x
        else:
            # If ranks are same, make one the root and increment its rank
            self.parent[root_y] = root_x
            self.rank[root_x] += 1

        return True


def kruskal(edges, n):
    """
    Find the minimum spanning tree using Kruskal's algorithm.

    Args:
        edges: List of edges as (u, v, weight) tuples
        n: Number of vertices in the graph

    Returns:
        List of edges in the minimum spanning tree
    """
    # Initialize union-find data structure
    uf = UnionFind(n)

    # Sort edges by weight (ascending)
    sorted_edges = sorted(edges, key=lambda edge: edge[2])

    # Collect MST edges
    mst_edges = []

    # Greedily select edges that don't form cycles
    for u, v, weight in sorted_edges:
        if uf.find(u) != uf.find(v):  # Check if adding this edge would create a cycle
            uf.union(u, v)  # Merge the components
            mst_edges.append((u, v, weight))

            # Early termination: MST has n-1 edges
            if len(mst_edges) == n - 1:
                break

    return mst_edges


# Example usage with a sample graph
def print_graph_steps(edges, n):
    """Print the steps of Kruskal's algorithm execution."""
    print("Original edges:")
    for u, v, w in edges:
        print(f"  Edge {u}-{v} with weight {w}")

    print("\nSorted edges by weight:")
    sorted_edges = sorted(edges, key=lambda edge: edge[2])
    for i, (u, v, w) in enumerate(sorted_edges):
        print(f"  {i+1}. Edge {u}-{v} with weight {w}")

    print("\nKruskal's algorithm steps:")
    uf = UnionFind(n)
    mst_edges = []

    for i, (u, v, w) in enumerate(sorted_edges):
        print(f"  Examining edge {u}-{v} with weight {w}...")

        if uf.find(u) != uf.find(v):
            uf.union(u, v)
            mst_edges.append((u, v, w))
            print(f"    Added to MST (components merged)")
        else:
            print(f"    Skipped (would create cycle)")

        # Print current components
        components = {}
        for i in range(n):
            root = uf.find(i)
            if root not in components:
                components[root] = []
            components[root].append(i)

        print(f"    Current components: {list(components.values())}")

        if len(mst_edges) == n - 1:
            print("    MST complete!")
            break

    print("\nFinal MST edges:")
    total_weight = 0
    for u, v, w in mst_edges:
        print(f"  Edge {u}-{v} with weight {w}")
        total_weight += w

    print(f"Total MST weight: {total_weight}")


# Example graph (from visual explanation)
# Using numeric indices for vertices: A=0, B=1, C=2, D=3, E=4, F=5, G=6
edges = [
    (0, 1, 7),  # A-B
    (0, 2, 8),  # A-C
    (0, 3, 5),  # A-D
    (1, 2, 3),  # B-C
    (1, 4, 6),  # B-E
    (1, 5, 2),  # B-F
    (2, 5, 3),  # C-F
    (3, 6, 2),  # D-G
    (4, 5, 4),  # E-F
    (5, 6, 5),  # F-G
]
n = 7  # Number of vertices

mst = kruskal(edges, n)
print("Minimum Spanning Tree edges:")
total_weight = 0
for u, v, w in mst:
    print(f"  {u}-{v} with weight {w}")
    total_weight += w
print(f"Total MST weight: {total_weight}")

# Uncomment to see detailed steps:
# print("\nDetailed steps:")
# print_graph_steps(edges, n)
```

## Peer Discussion Prompts

1. How does Kruskal's algorithm compare to Prim's algorithm for finding minimum spanning trees?
2. In what scenarios might Kruskal's algorithm be preferred over Prim's algorithm?
3. Why is the Union-Find data structure particularly well-suited for Kruskal's algorithm?
4. How would the algorithm change if we wanted to find the maximum spanning tree instead?

## Checkpoint Questions

1. **Checkpoint 1**: Why do we need to sort the edges by weight at the beginning of Kruskal's algorithm?
2. **Checkpoint 2**: What is the purpose of the Union-Find data structure in this algorithm?
3. **Checkpoint 3**: How many edges are in a minimum spanning tree of a connected graph with n vertices?
4. **Checkpoint 4**: Why does adding edges that don't create cycles result in a spanning tree?

## Time and Space Complexity Walkthrough

### Time Complexity

- **Sorting edges**: O(E log E) where E is the number of edges
- **Union-Find operations**: O(E α(V)) where α is the inverse Ackermann function, which grows very slowly
- **Overall**: O(E log E) or O(E log V) since E ≤ V²

### Space Complexity

- **Union-Find data structure**: O(V) for parent and rank arrays
- **Sorted edges list**: O(E) for storing all edges
- **MST edges list**: O(V) as MST has V-1 edges
- **Overall**: O(E + V)

## Common Implementation Mistakes

1. **Incorrect Union-Find implementation**: Not using path compression or union by rank optimizations
2. **Not checking for cycles**: Failing to verify that an edge doesn't create a cycle
3. **Handling disconnected graphs**: If the graph is not connected, the MST will be a forest
4. **Early termination**: Forgetting to stop after selecting V-1 edges
5. **Not handling duplicate edges**: Ensure duplicate edges are handled correctly

## Mini-Challenge

1. Modify the algorithm to detect whether the input graph is connected
2. Implement a function to find the maximum spanning tree instead of the minimum
3. Extend the implementation to handle weighted edges with vertex names instead of indices
4. Create a visualization that shows the MST construction step by step

For the team:

- Compare the performance of Kruskal's algorithm with Prim's algorithm on different types of graphs
- Implement a solution to the "Minimum Cost to Connect All Points" problem using Kruskal's algorithm
- Discuss real-world applications of minimum spanning trees in network design
