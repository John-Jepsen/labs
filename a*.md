### Algorithm: A\* Search Algorithm

Sir Cedric and Ember faced the Inferno Plains, where they needed to find the shortest path to Princess Elara using the A\* Search Algorithm.

#### Initialize Data Structures:

- Sir Cedric used a magical compass (priority queue) to track the paths with the least fire.

#### Calculate Heuristic:

- Sir Cedric used a map (function) to estimate the distance to Princess Elara.

#### Find the Path:

- He moved through the plains, always choosing the path with the least estimated cost.

#### Retrieve the Result:

- The compass showed the shortest path to Princess Elara.

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
