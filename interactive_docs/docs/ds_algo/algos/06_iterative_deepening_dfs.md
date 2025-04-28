# Iterative Deepening Depth-First Search (IDDFS) Algorithm

## Objective

Master the Iterative Deepening Depth-First Search algorithm through collaborative implementation and analysis. By the end of this session, your team will understand how IDDFS combines the space efficiency of DFS with the level-by-level search benefits of BFS.

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

Iterative Deepening DFS performs multiple depth-limited DFS searches, incrementing the depth limit with each iteration until the goal is found.

```
    A
   / \
  B   C
 / \   \
D   E   F
```

IDDFS with increasing depth limits:

Depth 0: A
Depth 1: A → B, A → C
Depth 2: A → B → D, A → B → E, A → C → F

It combines the completeness and optimality of BFS with the space efficiency of DFS.

---

## Algorithm Walkthrough

### Pseudocode

```
function IDDFS(graph, start, goal):
    for depth from 0 to ∞:
        result = DepthLimitedSearch(graph, start, goal, depth)
        if result != "cutoff":
            return result

function DepthLimitedSearch(graph, node, goal, depth_limit):
    if node == goal:
        return node
    if depth_limit == 0:
        return "cutoff"

    cutoff_occurred = false

    for each neighbor of node:
        result = DepthLimitedSearch(graph, neighbor, goal, depth_limit - 1)

        if result == "cutoff":
            cutoff_occurred = true
        else if result != "failure":
            return result

    if cutoff_occurred:
        return "cutoff"
    else:
        return "failure"
```

### Time and Space Complexity

- **Time Complexity**: O(b^d) where b is the branching factor and d is the depth of the shallowest solution
- **Space Complexity**: O(d) for the depth-limited search, much better than BFS

---

## Annotated Code Template

```python
def depth_limited_search(graph, node, goal, depth_limit, visited=None, path=None):
    """
    Performs depth-limited search from node to goal with a specified depth limit.

    Args:
        graph: Dictionary representing an adjacency list graph
               {node: [neighbor1, neighbor2, ...]}
        node: Current node to explore
        goal: Target node to find
        depth_limit: Maximum depth to search
        visited: Set of visited nodes (initialized if None)
        path: Current path (initialized if None)

    Returns:
        Tuple (found, path):
            - found: True if goal found, False otherwise
            - path: Path to goal if found, None otherwise
    """
    if visited is None:
        visited = set()
    if path is None:
        path = [node]

    # TODO: Implement depth-limited search
    # 1. Check if we've found the goal
    # 2. Check if we've reached the depth limit
    # 3. Mark current node as visited
    # 4. Recursively visit all unvisited neighbors with reduced depth limit

    return False, None

def iterative_deepening_dfs(graph, start, goal, max_depth=float('inf')):
    """
    Performs iterative deepening DFS from start to goal up to a maximum depth.

    Args:
        graph: Dictionary representing an adjacency list graph
               {node: [neighbor1, neighbor2, ...]}
        start: Starting node
        goal: Target node to find
        max_depth: Maximum depth to search (default: infinity)

    Returns:
        Tuple (found, path, depth):
            - found: True if goal found, False otherwise
            - path: Path to goal if found, None otherwise
            - depth: Depth at which the goal was found
    """
    # TODO: Implement iterative deepening DFS
    # 1. For each depth from 0 to max_depth:
    #    a. Call depth_limited_search
    #    b. If goal found, return the result

    return False, None, -1
```

---

## Live Coding Group Activity

### Team Roles

- **Driver**: Types the code
- **Navigator**: Guides the implementation
- **Explainer**: Verbalizes what's happening at each step

### Task

Complete the IDDFS implementation by filling in the missing code in the template. Test it with the following graph:

```python
# Example graph as adjacency list
graph = {
    'A': ['B', 'C'],
    'B': ['A', 'D', 'E'],
    'C': ['A', 'F'],
    'D': ['B'],
    'E': ['B'],
    'F': ['C', 'G'],
    'G': ['F']
}

# Test various starting points and goals
test_cases = [
    ('A', 'G'),  # Should find at depth 3: A→C→F→G
    ('A', 'E'),  # Should find at depth 2: A→B→E
    ('D', 'C'),  # Should find at depth 2: D→B→A→C
    ('E', 'G'),  # Should find at depth 4: E→B→A→C→F→G
]

for start, goal in test_cases:
    found, path, depth = iterative_deepening_dfs(graph, start, goal)
    if found:
        print(f"Path from {start} to {goal}: {' → '.join(path)} (found at depth {depth})")
    else:
        print(f"No path found from {start} to {goal}")
```

### Checkpoint Questions

1. How does IDDFS combine the benefits of BFS and DFS?
2. Why is the space complexity of IDDFS better than BFS?
3. In what scenarios would IDDFS be preferable to either BFS or DFS?

---

## Complete Implementation

After your group discussion, compare your solution with this implementation:

```python
def depth_limited_search(graph, node, goal, depth_limit, visited=None, path=None):
    """
    Performs depth-limited search from node to goal with a specified depth limit.

    Args:
        graph: Dictionary representing an adjacency list graph
               {node: [neighbor1, neighbor2, ...]}
        node: Current node to explore
        goal: Target node to find
        depth_limit: Maximum depth to search
        visited: Set of visited nodes (initialized if None)
        path: Current path (initialized if None)

    Returns:
        Tuple (found, path):
            - found: True if goal found, False otherwise
            - path: Path to goal if found, None otherwise
    """
    if visited is None:
        visited = set([node])
    if path is None:
        path = [node]

    # Base case: found the goal
    if node == goal:
        return True, path

    # Base case: reached depth limit
    if depth_limit == 0:
        return False, None

    # Explore neighbors
    for neighbor in graph[node]:
        if neighbor not in visited:
            # Mark as visited and update path
            visited.add(neighbor)
            result, new_path = depth_limited_search(
                graph, neighbor, goal, depth_limit - 1,
                visited.copy(), path + [neighbor]
            )

            # If goal found in this branch, return the result
            if result:
                return True, new_path

    # Goal not found within depth limit
    return False, None

def iterative_deepening_dfs(graph, start, goal, max_depth=float('inf')):
    """
    Performs iterative deepening DFS from start to goal up to a maximum depth.

    Args:
        graph: Dictionary representing an adjacency list graph
               {node: [neighbor1, neighbor2, ...]}
        start: Starting node
        goal: Target node to find
        max_depth: Maximum depth to search (default: infinity)

    Returns:
        Tuple (found, path, depth):
            - found: True if goal found, False otherwise
            - path: Path to goal if found, None otherwise
            - depth: Depth at which the goal was found
    """
    # Iterate through increasing depth limits
    for depth in range(max_depth + 1):
        found, path = depth_limited_search(graph, start, goal, depth)

        # If goal found, return the result along with the depth
        if found:
            return True, path, depth

    # Goal not found within max_depth
    return False, None, -1

# Example usage
if __name__ == "__main__":
    graph = {
        'A': ['B', 'C'],
        'B': ['A', 'D', 'E'],
        'C': ['A', 'F'],
        'D': ['B'],
        'E': ['B'],
        'F': ['C', 'G'],
        'G': ['F']
    }

    start = 'A'
    goal = 'G'

    found, path, depth = iterative_deepening_dfs(graph, start, goal)

    if found:
        print(f"Path from {start} to {goal}: {' → '.join(path)}")
        print(f"Found at depth: {depth}")
    else:
        print(f"No path found from {start} to {goal}")
```

---

## Visualizing the Algorithm

Let's trace through the execution of IDDFS for finding a path from 'A' to 'G' in our example graph:

**Depth 0:**

- Explore A
- Goal not found

**Depth 1:**

- Explore A
- Explore B (neighbor of A)
- Explore C (neighbor of A)
- Goal not found

**Depth 2:**

- Explore A
- Explore B (neighbor of A)
  - Explore D (neighbor of B)
  - Explore E (neighbor of B)
- Explore C (neighbor of A)
  - Explore F (neighbor of C)
- Goal not found

**Depth 3:**

- Explore A
- Explore B (neighbor of A)
  - Explore D (neighbor of B)
  - Explore E (neighbor of B)
- Explore C (neighbor of A)
  - Explore F (neighbor of C)
    - Explore G (neighbor of F) - Goal found!
- Return path: A → C → F → G

---

## Common Implementation Mistakes

1. **Incorrect cycle detection**: IDDFS needs proper cycle detection to avoid infinite loops.
2. **Not resetting visited sets**: Each depth iteration needs a fresh visited set.
3. **Incorrect path tracking**: Ensure the path is properly updated during the search.
4. **Inefficient re-exploration**: The algorithm re-explores shallow nodes multiple times.
5. **Neglecting early termination**: Should stop as soon as the goal is found.

---

## Peer Discussion Prompts

1. How does IDDFS compare to BFS in terms of finding the shortest path?
2. What are the trade-offs between IDDFS, BFS, and DFS?
3. Can IDDFS be adapted to handle weighted graphs?
4. In what real-world scenarios would IDDFS be particularly useful?

---

## Mini-Challenge: Path Finding with Limited Memory

Modify your IDDFS implementation to work effectively when memory is limited by using an iterative (non-recursive) approach.

```python
def iterative_iddfs(graph, start, goal, max_depth=float('inf')):
    """
    Implements IDDFS without recursion to save memory.

    Returns:
        Tuple (found, path, depth)
    """
    # TODO: Implement iterative IDDFS
    # 1. Use a stack instead of recursion
    # 2. Keep track of node depth explicitly

    pass
```

### Extension for Fast Learners

Implement a bidirectional IDDFS that searches from both the start and goal nodes simultaneously.

---

## Applications of IDDFS

- Game playing algorithms
- Puzzle solving
- Route finding with limited memory
- Network packet routing
- Web crawling with depth limits
- Hierarchical data exploration

---

## IDDFS Space Optimization

One of the primary benefits of IDDFS is its space efficiency. Let's compare:

| Algorithm | Time Complexity | Space Complexity | Complete? | Optimal Path?    |
| :-------- | :-------------- | :--------------- | :-------- | :--------------- |
| BFS       | O(b^d)          | O(b^d)           | Yes       | Yes (unweighted) |
| DFS       | O(b^d)          | O(d)             | No        | No               |
| IDDFS     | O(b^d)          | O(d)             | Yes       | Yes (unweighted) |

Where:

- b = branching factor
- d = depth of shallowest solution

IDDFS may seem inefficient due to repeated exploration, but in practice, most of the work happens at the deepest level, making the overhead manageable.

---

## Team Reflection

As a group, discuss:

1. In what scenarios would the re-exploration overhead of IDDFS be worth the space savings?
2. How does the branching factor of the graph affect the efficiency of IDDFS?
3. Could IDDFS be combined with heuristics like A\* to create a more efficient algorithm?
4. What modifications would be needed to adapt IDDFS for trees instead of graphs?
