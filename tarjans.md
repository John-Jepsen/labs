### Algorithm: Tarjan's Algorithm

Princess Elara needed to find all the critical points in her underwater city, Aquapolis, to protect it from sea monster attacks. She used Tarjan's Algorithm to find strongly connected components in the city's infrastructure.

#### Initialize Data Structures:

- Princess Elara used a trident (stack) to keep track of nodes.
- She also used a magical compass (array) to keep track of discovery and low values.

#### Explore the City:

- She explored each part of the city, marking nodes and discovering strongly connected components.

#### Implementation:

```py
def tarjans_scc(city: Dict[int, List[int]]) -> List[List[int]]:
    def dfs(v):
        nonlocal index
        discovery[v] = low[v] = index
        index += 1
        stack.append(v)
        on_stack[v] = True

        for w in city[v]:
            if discovery[w] == -1:
                dfs(w)
                low[v] = min(low[v], low[w])
            elif on_stack[w]:
                low[v] = min(low[v], discovery[w])

        if low[v] == discovery[v]:
            component = []
            while True:
                w = stack.pop()
                on_stack[w] = False
                component.append(w)
                if w == v:
                    break
            scc.append(component)

    discovery = [-1] * len(city)
    low = [-1] * len(city)
    on_stack = [False] * len(city)
    stack = []
    scc = []
    index = 0

    for v in range(len(city)):
        if discovery[v] == -1:
            dfs(v)

    return scc

# Example usage:
city = {
    0: [1],
    1: [2],
    2: [0, 3],
    3: [4],
    4: [5],
    5: [3]
}
print(tarjans_scc(city))  # Output: Strongly connected components
```

#### Explanation:

Initialize:

- `discovery`, `low`: Arrays to track discovery and low values.
- `stack`, `on_stack`: A stack and boolean array to track nodes on the stack.

Explore the City:

- Princess Elara explored each part of the city, marking nodes and discovering strongly connected components.
