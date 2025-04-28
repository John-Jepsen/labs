# Uniform Cost Search (UCS) Algorithm

## Objective

Master the Uniform Cost Search algorithm through collaborative implementation and analysis. By the end of this session, your team will understand how UCS finds the lowest-cost path in a weighted graph and how it differs from other search algorithms.

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

Uniform Cost Search explores a weighted graph by always expanding the lowest-cost path first.

```
    A
   /|\
  / | \
 /  |  \
B   C   D
 \  |  /
  \ | /
    E
```

With costs:

- A → B: 4
- A → C: 1
- A → D: 5
- B → E: 1
- C → E: 5
- D → E: 2

Shortest path from A to E: A → C → D → E with total cost 8.

UCS uses a **priority queue** to track paths, prioritizing the lowest cumulative cost.

---

## Algorithm Walkthrough

### Pseudocode

```
function UniformCostSearch(graph, start, goal):
    PriorityQueue frontier = new PriorityQueue()
    frontier.add(start, 0)  // (node, priority/cost)

    Map came_from = {}      // For path reconstruction
    Map cost_so_far = {}    // To track lowest cost to each node

    came_from[start] = None
    cost_so_far[start] = 0

    while frontier is not empty:
        current = frontier.pop()  // Gets lowest cost node

        if current == goal:
            break

        for each neighbor of current:
            new_cost = cost_so_far[current] + cost(current, neighbor)

            if neighbor not in cost_so_far OR new_cost < cost_so_far[neighbor]:
                cost_so_far[neighbor] = new_cost
                priority = new_cost
                frontier.add(neighbor, priority)
                came_from[neighbor] = current

    return came_from, cost_so_far
```

### Time and Space Complexity

- **Time Complexity**: O(E + V log V) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V) for the priority queue, visited set, and path tracking

---

## Annotated Code Template

```python
import heapq

def uniform_cost_search(graph, start_node, goal_node):
    """
    Finds the lowest-cost path from start_node to goal_node using Uniform Cost Search.

    Args:
        graph: Dictionary representing an adjacency list with costs
               {node: [(neighbor1, cost1), (neighbor2, cost2), ...]}
        start_node: Starting node for UCS
        goal_node: Target node to reach

    Returns:
        (path, cost): Tuple containing the path as a list and the total cost
    """
    # Priority queue for (cost, node, path)
    # The cost is first in the tuple so heapq prioritizes by cost
    priority_queue = [(0, start_node, [start_node])]

    # Track visited nodes to avoid cycles
    visited = set()

    # TODO: Implement the UCS algorithm
    # While the priority queue is not empty:
    #   1. Pop the node with lowest cost so far
    #   2. If it's the goal, return the path and cost
    #   3. If we've seen it before, skip it
    #   4. Mark as visited
    #   5. Add all unvisited neighbors to the queue with cumulative cost

    # If we exit the loop, no path was found
    return None, float('inf')
```

---

## Live Coding Group Activity

### Team Roles

- **Driver**: Types the code
- **Navigator**: Guides the implementation
- **Explainer**: Verbalizes what's happening at each step

### Task

Complete the UCS implementation by filling in the missing code in the template. Test it with the following graph:

```python
# Example weighted graph as adjacency list with costs
graph = {
    'A': [('B', 4), ('C', 1), ('D', 5)],
    'B': [('A', 4), ('E', 1)],
    'C': [('A', 1), ('D', 3), ('E', 5)],
    'D': [('A', 5), ('C', 3), ('E', 2)],
    'E': [('B', 1), ('C', 5), ('D', 2)]
}

# Expected lowest-cost path from 'A' to 'E': ['A', 'C', 'D', 'E'] with cost 6
```

### Checkpoint Questions

1. How does UCS differ from BFS?
2. What happens if all edges have the same weight?
3. When would UCS be more appropriate than other search algorithms?

---

## Complete Implementation

After your group discussion, compare your solution with this implementation:

```python
import heapq

def uniform_cost_search(graph, start_node, goal_node):
    """
    Finds the lowest-cost path from start_node to goal_node using Uniform Cost Search.

    Args:
        graph: Dictionary representing an adjacency list with costs
               {node: [(neighbor1, cost1), (neighbor2, cost2), ...]}
        start_node: Starting node for UCS
        goal_node: Target node to reach

    Returns:
        (path, cost): Tuple containing the path as a list and the total cost
    """
    # Priority queue for (cost, node, path)
    # The cost is first in the tuple so heapq prioritizes by cost
    priority_queue = [(0, start_node, [start_node])]

    # Track visited nodes to avoid cycles
    visited = set()

    while priority_queue:
        # Pop the node with lowest cost so far
        current_cost, current_node, path = heapq.heappop(priority_queue)

        # If we've reached the goal, return the path and cost
        if current_node == goal_node:
            return path, current_cost

        # Skip if we've already visited this node
        if current_node in visited:
            continue

        # Mark as visited
        visited.add(current_node)

        # Explore neighbors
        for neighbor, cost in graph[current_node]:
            # Skip visited neighbors
            if neighbor not in visited:
                # Calculate new cost and path
                new_cost = current_cost + cost
                new_path = path + [neighbor]

                # Add to priority queue
                heapq.heappush(priority_queue, (new_cost, neighbor, new_path))

    # If we exit the loop, no path was found
    return None, float('inf')

# Example usage
if __name__ == "__main__":
    graph = {
        'A': [('B', 4), ('C', 1), ('D', 5)],
        'B': [('A', 4), ('E', 1)],
        'C': [('A', 1), ('D', 3), ('E', 5)],
        'D': [('A', 5), ('C', 3), ('E', 2)],
        'E': [('B', 1), ('C', 5), ('D', 2)]
    }

    start = 'A'
    goal = 'E'

    path, cost = uniform_cost_search(graph, start, goal)

    if path:
        print(f"Lowest-cost path from {start} to {goal}: {' → '.join(path)}")
        print(f"Total cost: {cost}")
    else:
        print(f"No path found from {start} to {goal}")
```

---

## Common Implementation Mistakes

1. **Not using a priority queue**: Using a regular queue loses the cost-based ordering.
2. **Incorrectly calculating cumulative costs**: Forgetting to add the current path cost.
3. **Inefficient visited node handling**: Checking visited status at the wrong point.
4. **Not tracking the path**: Only tracking the cost without the actual path.
5. **Not handling disconnected graphs**: Missing proper termination when no path exists.

---

## Peer Discussion Prompts

1. How does UCS relate to Dijkstra's algorithm?
2. In what scenarios would UCS outperform BFS?
3. What modifications would be needed to handle negative edge weights?
4. How does the priority queue implementation affect the algorithm's performance?

---

## Mini-Challenge: Optimized UCS

Modify your UCS implementation to be more efficient by:

1. Only adding a node to the priority queue when its cost decreases
2. Using a visited set more effectively

```python
def optimized_ucs(graph, start_node, goal_node):
    """
    Implements a more efficient version of UCS.

    Returns:
        (path, cost): Tuple containing the path as a list and the total cost
    """
    # TODO: Implement optimized UCS
    # 1. Use a dictionary to track cost to each node
    # 2. Use a dictionary to track the path to each node
    # 3. Only add to priority queue when cost decreases

    pass
```

### Extension for Fast Learners

Implement a bidirectional UCS that searches from both the start and goal nodes simultaneously.

---

## Applications of UCS

- Finding shortest paths in road networks
- Network routing algorithms
- Robot path planning
- Resource allocation
- Game pathfinding
- Transportation optimization

---

## UCS vs. Other Algorithms

| Algorithm | Uses                        | When to Use                                                |
| :-------- | :-------------------------- | :--------------------------------------------------------- |
| BFS       | Queue                       | Unweighted graphs, shortest path in terms of edges         |
| DFS       | Stack                       | Maze solving, puzzle solutions, complete graph exploration |
| UCS       | Priority Queue              | Weighted graphs, lowest-cost path                          |
| Dijkstra  | Priority Queue              | Single source shortest paths to all destinations           |
| A\*       | Priority Queue w/ Heuristic | Guided search with domain knowledge                        |

---

## Visualizing the Search Process

To better understand UCS, let's trace through our example:

```
Starting at A:
- Push (0, 'A', ['A']) to queue

Iteration 1:
- Pop (0, 'A', ['A'])
- Mark A as visited
- Push (1, 'C', ['A', 'C']) - Cost from A to C = 1
- Push (4, 'B', ['A', 'B']) - Cost from A to B = 4
- Push (5, 'D', ['A', 'D']) - Cost from A to D = 5

Iteration 2:
- Pop (1, 'C', ['A', 'C']) (lowest cost)
- Mark C as visited
- Push (4, 'D', ['A', 'C', 'D']) - Cost from A to C to D = 1 + 3 = 4
- Push (6, 'E', ['A', 'C', 'E']) - Cost from A to C to E = 1 + 5 = 6

Iteration 3:
- Pop (4, 'B', ['A', 'B']) (lowest cost)
- Mark B as visited
- Push (5, 'E', ['A', 'B', 'E']) - Cost from A to B to E = 4 + 1 = 5

Iteration 4:
- Pop (4, 'D', ['A', 'C', 'D']) (lowest cost)
- Mark D as visited
- Push (6, 'E', ['A', 'C', 'D', 'E']) - Cost from A to C to D to E = 1 + 3 + 2 = 6

Iteration 5:
- Pop (5, 'E', ['A', 'B', 'E']) (lowest cost)
- E is the goal! Return path ['A', 'B', 'E'] with cost 5
```

So the shortest path is A → B → E with cost 5.

---

## Team Reflection

As a group, discuss:

1. Why does UCS always find the optimal (lowest-cost) path?
2. How does the priority queue enable the "lowest-cost-first" exploration pattern?
3. What data structures would you use to implement UCS in a production system?
4. Can you think of a real-world problem where UCS would be the optimal solution?
