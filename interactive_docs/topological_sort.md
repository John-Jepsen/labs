### Algorithm: Topological Sort

Topological sorting of a directed graph is a linear ordering of its vertices such that for every directed edge \(uv\), vertex \(u\) comes before \(v\) in the ordering.

#### Initialize Data Structures:

- Use a stack to store the topological order.
- Use a set to keep track of visited vertices.

#### Depth-First Search:

- Perform a depth-first search on each unvisited vertex, pushing vertices onto the stack after all their neighbors have been visited.

#### Retrieve the Result:

- The stack contains the topological order in reverse.

#### Implementation:

```py
def topological_sort(graph: Dict[int, List[int]]) -> List[int]:
    def dfs(node):
        visited.add(node)
        for neighbor in graph[node]:
            if neighbor not in visited:
                dfs(neighbor)
        stack.append(node)

    visited = set()
    stack = []

    for node in graph:
        if node not in visited:
            dfs(node)

    return stack[::-1]

# Example usage:
graph = {
    0: [1, 2],
    1: [3],
    2: [3],
    3: []
}
print(topological_sort(graph))  # Output: Topological order
```

#### Explanation:

Initialize:

- `stack`: A stack to store the topological order.
- `visited`: A set to keep track of visited vertices.

Depth-First Search:

- Perform a depth-first search on each unvisited vertex, pushing vertices onto the stack after all their neighbors have been visited.

Retrieve the Result:

- The stack contains the topological order in reverse.
