# Algorithm: A\* Search Algorithm

### Introduction to A\* Search Algorithm

Princess Elara, renowned for her strategic acumen, once embarked on a quest to find the most efficient path through a labyrinthine kingdom. To guide her through this complex journey, she employed the A\* (A-star) Search Algorithm, a powerful pathfinding and graph traversal technique used to find the shortest path between two nodes. This algorithm, widely recognized for its efficiency and accuracy, combines the strengths of Dijkstra's Algorithm and Greedy Best-First Search to navigate through intricate networks and maps.

### What is the A\* Search Algorithm?

The A* Search Algorithm is a widely used pathfinding and graph traversal algorithm that excels in finding the shortest path between a starting node and a target node. It works by maintaining a priority queue of nodes to be explored, prioritizing nodes based on a cost function (f(n) = g(n) + h(n)). Here, (g(n)) represents the cost of the path from the start node to the current node (n), and (h(n)) is a heuristic estimate of the cost from (n) to the goal node. By balancing these two factors, A* efficiently guides the search towards the target, ensuring both optimality and completeness. This makes A\* particularly suitable for applications such as route planning, game AI, and robotics, where finding the most efficient path is crucial.

#### Implementation:

```py
import heapq

def a_star(plains: Dict[int, List[Tuple[int, int]]], start: int, goal: int) -> List[int]:
    def heuristic(a, b):
        return abs(a - b)  # Example heuristic function (Manhattan distance)

    priority_queue = [(0, start)]
    came_from = {}
    cost_so_far = {start: 0}

    while priority_queue:
        current_cost, current = heapq.heappop(priority_queue)

        if current == goal:
            break

        for next_node, weight in plains[current]:
            new_cost = cost_so_far[current] + weight
            if next_node not in cost_so_far or new_cost < cost_so_far[next_node]:
                cost_so_far[next_node] = new_cost
                priority = new_cost + heuristic(next_node, goal)
                heapq.heappush(priority_queue, (priority, next_node))
                came_from[next_node] = current

    path = []
    node = goal
    while node != start:
        path.append(node)
        node = came_from[node]
    path.append(start)
    path.reverse()

    return path

# Example usage:
plains = {
    1: [(2, 1), (3, 4)],
    2: [(3, 2), (4, 5)],
    3: [(4, 1)],
    4: []
}
start, goal = 1, 4
print(a_star(plains, start, goal))  # Output: Shortest path from start to goal
```

#### Explanation:

Initialize:

- `priority_queue`: A magical compass to track the paths with the least fire.

Calculate Heuristic:

- `heuristic`: A map to estimate the distance to Princess Elara.

Find the Path:

- Sir Cedric moved through the plains, always choosing the path with the least estimated cost.

Retrieve the Result:

- The compass showed the shortest path to Princess Elara.
