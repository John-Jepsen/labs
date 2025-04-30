# A\* Search Algorithm

## Objective

Master the A* (A-star) Search algorithm through collaborative implementation and analysis. By the end of this session, your team will understand how A* combines path cost with heuristics to find optimal paths efficiently and apply it to grid-based navigation problems.

---

## Environment Setup

| Requirements                                             |
| :------------------------------------------------------- |
| Python 3.8+                                              |
| Standard library only (using `heapq` for priority queue) |

Verify your environment:

```bash
python --version  # Should be 3.8+
```

---

## Visual Explanation

A\* Search efficiently finds the shortest path by combining:

- **g(n)**: The cost from the start node to the current node
- **h(n)**: A heuristic that estimates the cost from the current node to the goal
- **f(n)**: The total estimated cost: f(n) = g(n) + h(n)

A\* always expands the node with the lowest f(n) value.

```
Grid Navigation Example:
S = Start, G = Goal, # = Obstacle

+---+---+---+---+---+
| S |   |   |   |   |
+---+---+---+---+---+
|   | # | # |   |   |
+---+---+---+---+---+
|   | # |   |   |   |
+---+---+---+---+---+
|   | # | # | # |   |
+---+---+---+---+---+
|   |   |   |   | G |
+---+---+---+---+---+
```

A\* will find the shortest path around obstacles using a combination of actual movement cost and a heuristic (like Manhattan distance).

---

## Algorithm Walkthrough

### Pseudocode

```
function A_Star(graph, start, goal, heuristic):
    PriorityQueue open_set = new PriorityQueue()
    open_set.add(start, 0)  // (node, f_score)

    Map came_from = {}      // For path reconstruction
    Map g_score = {}        // Actual cost from start to current node
    Map f_score = {}        // Estimated total cost

    g_score[start] = 0
    f_score[start] = heuristic(start, goal)

    while open_set is not empty:
        current = open_set.pop()  // Gets node with lowest f_score

        if current == goal:
            return reconstruct_path(came_from, current)

        for each neighbor of current:
            // tentative_g is the distance from start to neighbor through current
            tentative_g = g_score[current] + cost(current, neighbor)

            if neighbor not in g_score OR tentative_g < g_score[neighbor]:
                // This path to neighbor is better than any previous one
                came_from[neighbor] = current
                g_score[neighbor] = tentative_g
                f_score[neighbor] = g_score[neighbor] + heuristic(neighbor, goal)

                if neighbor not in open_set:
                    open_set.add(neighbor, f_score[neighbor])

    return failure  // No path exists
```

### Time and Space Complexity

- **Time Complexity**: O(E log V) with a good heuristic, where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V) for the priority queue, visited set, and path tracking

---

## Annotated Code Template

```python
import heapq

def a_star_search(grid, start, goal):
    """
    Finds the shortest path from start to goal in a grid using A* Search.

    Args:
        grid: 2D array where 0 is open space and 1 is obstacle
        start: Tuple (row, col) representing start position
        goal: Tuple (row, col) representing goal position

    Returns:
        List of positions representing the path, or None if no path exists
    """
    # Define directions (up, right, down, left)
    directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]

    # TODO: Implement heuristic function (Manhattan distance)
    def heuristic(a, b):
        pass

    # TODO: Implement A* algorithm
    # 1. Initialize open set (priority queue), closed set, g_score, f_score
    # 2. While open set is not empty:
    #    a. Get node with lowest f_score
    #    b. If at goal, reconstruct path and return
    #    c. Move current node to closed set
    #    d. For each neighbor:
    #       i. Skip if in closed set or obstacle
    #       ii. Calculate tentative g_score
    #       iii. If new path is better, update g_score, f_score, and came_from

    return None  # No path found
```

---

## Live Coding Group Activity

### Team Roles

- **Driver**: Types the code
- **Navigator**: Guides the implementation
- **Explainer**: Verbalizes what's happening at each step

### Task

Complete the A\* Search implementation by filling in the missing code in the template. Test it with the following grid:

```python
# Example grid (0 = open, 1 = obstacle)
grid = [
    [0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0],
    [0, 1, 0, 0, 0],
    [0, 1, 1, 1, 0],
    [0, 0, 0, 0, 0]
]

start = (0, 0)  # Top-left
goal = (4, 4)   # Bottom-right

# Expected path: [(0,0), (1,0), (2,0), (3,0), (4,0), (4,1), (4,2), (4,3), (4,4)]
# or another valid shortest path
```

### Checkpoint Questions

1. How does the heuristic function affect A\* performance?
2. What happens if h(n) is always 0? How does A\* behave?
3. What makes a heuristic "admissible" and why is that important?

---

## Complete Implementation

After your group discussion, compare your solution with this implementation:

```python
import heapq

def a_star_search(grid, start, goal):
    """
    Finds the shortest path from start to goal in a grid using A* Search.

    Args:
        grid: 2D array where 0 is open space and 1 is obstacle
        start: Tuple (row, col) representing start position
        goal: Tuple (row, col) representing goal position

    Returns:
        List of positions representing the path, or None if no path exists
    """
    rows, cols = len(grid), len(grid[0])

    # Define directions (up, right, down, left)
    directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]

    # Manhattan distance heuristic
    def heuristic(a, b):
        return abs(a[0] - b[0]) + abs(a[1] - b[1])

    # Check if a position is valid
    def is_valid(pos):
        r, c = pos
        return 0 <= r < rows and 0 <= c < cols and grid[r][c] == 0

    # Initialize data structures
    open_set = []  # Priority queue
    heapq.heappush(open_set, (0, start))  # (f_score, position)

    came_from = {}  # To reconstruct path
    g_score = {start: 0}  # Cost from start to current
    f_score = {start: heuristic(start, goal)}  # Estimated total cost

    # For the priority queue, we need to track positions we've seen
    open_set_hash = {start}

    while open_set:
        # Get node with lowest f_score
        _, current = heapq.heappop(open_set)
        open_set_hash.remove(current)

        # If we reached the goal, reconstruct and return path
        if current == goal:
            path = []
            while current in came_from:
                path.append(current)
                current = came_from[current]
            path.append(start)
            return path[::-1]  # Reverse to get path from start to goal

        # Check neighbors
        for dr, dc in directions:
            neighbor = (current[0] + dr, current[1] + dc)

            # Skip invalid or obstacle positions
            if not is_valid(neighbor):
                continue

            # Calculate tentative g_score (cost is 1 for each step)
            tentative_g = g_score[current] + 1

            # If this path to neighbor is better than any previous one
            if neighbor not in g_score or tentative_g < g_score[neighbor]:
                came_from[neighbor] = current
                g_score[neighbor] = tentative_g
                f_score[neighbor] = tentative_g + heuristic(neighbor, goal)

                if neighbor not in open_set_hash:
                    heapq.heappush(open_set, (f_score[neighbor], neighbor))
                    open_set_hash.add(neighbor)

    return None  # No path found

# Example usage
if __name__ == "__main__":
    grid = [
        [0, 0, 0, 0, 0],
        [0, 1, 1, 0, 0],
        [0, 1, 0, 0, 0],
        [0, 1, 1, 1, 0],
        [0, 0, 0, 0, 0]
    ]

    start = (0, 0)  # Top-left
    goal = (4, 4)   # Bottom-right

    path = a_star_search(grid, start, goal)

    if path:
        print(f"Path found: {path}")

        # Visualize the path on the grid
        visual_grid = [['□' if cell == 0 else '■' for cell in row] for row in grid]
        visual_grid[start[0]][start[1]] = 'S'
        visual_grid[goal[0]][goal[1]] = 'G'

        for r, c in path:
            if (r, c) != start and (r, c) != goal:
                visual_grid[r][c] = '●'

        print("\nGrid Visualization:")
        for row in visual_grid:
            print(' '.join(row))
    else:
        print("No path found")
```

---

## Common Implementation Mistakes

1. **Incorrect heuristic**: Using a non-admissible heuristic that overestimates costs.
2. **Inefficient priority queue updates**: Not handling updates to nodes already in the open set.
3. **Missing edge cases**: Not checking grid boundaries or obstacles properly.
4. **Not tracking visited nodes**: Causing redundant exploration.
5. **Incorrect path reconstruction**: Failing to properly reconstruct the shortest path.

---

## Peer Discussion Prompts

1. How does A\* compare to Uniform Cost Search?
2. What makes a good heuristic for A\*?
3. How would you adapt A\* for navigation in a real-world map?
4. In what scenarios might A\* not be the best algorithm?

---

## Mini-Challenge: Diagonal Movement

Modify your A\* implementation to allow diagonal movement (8 directions instead of 4).

```python
def a_star_with_diagonals(grid, start, goal):
    """
    A* search with diagonal movement allowed.

    Returns:
        List of positions representing the path, or None if no path exists
    """
    # TODO: Implement A* with diagonal movement
    # 1. Add diagonal directions
    # 2. Update cost calculation (diagonal movement usually costs √2)
    # 3. Consider whether to allow "corner cutting"

    pass
```

### Extension for Fast Learners

Implement A* with a weighted heuristic, where f(n) = g(n) + w*h(n), to see how different weights affect the search.

---

## Heuristic Functions

Different heuristics can be used depending on the problem:

### Manhattan Distance (L1 Norm)

```python
def manhattan_distance(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])
```

### Euclidean Distance (L2 Norm)

```python
def euclidean_distance(a, b):
    return ((a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2) ** 0.5
```

### Diagonal Distance (Chebyshev Distance)

```python
def diagonal_distance(a, b):
    return max(abs(a[0] - b[0]), abs(a[1] - b[1]))
```

---

## Applications of A\*

- Pathfinding in video games
- Robot navigation
- GPS routing systems
- Network packet routing
- Puzzles like Sliding Puzzle or Tower of Hanoi
- Motion planning in robotics

---

## Grid Navigation With Weighted Costs

In many real scenarios, different terrain types have different movement costs:

```python
# Grid with terrain costs (e.g., road=1, grass=2, swamp=5)
terrain_grid = [
    [1, 1, 1, 2, 2],
    [1, 5, 5, 2, 2],
    [1, 5, 1, 1, 2],
    [1, 5, 5, 5, 1],
    [1, 1, 1, 1, 1]
]

def a_star_weighted_terrain(terrain_grid, start, goal):
    """A* search that takes terrain costs into account"""
    # Similar to regular A*, but use terrain costs for g-score calculation
    pass
```

---

## Team Reflection

As a group, discuss:

1. How does the choice of heuristic affect both the efficiency and the optimality of A\*?
2. When is A\* guaranteed to find the optimal path?
3. How would you handle scenarios where the environment changes during execution?
4. What are the trade-offs between speed and accuracy when implementing A\* in real-world applications?
