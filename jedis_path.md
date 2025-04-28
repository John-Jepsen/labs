# The Jedi's Path: Navigating with the Dijkstra's Algorithm

https://github.com/John-Jepsen/labs

## Problem:

In this coding challenge, you are tasked with implementing Dijkstra's algorithm to find the shortest path between two nodes in a weighted graph. Dijkstra's algorithm is fundamental in graph theory and is commonly used in various applications such as network routing protocols and GPS navigation systems.

You will be provided with a graph represented as an adjacency matrix, where each cell (i, j) represents the weight of the edge between nodes i and j. Your goal is to implement Dijkstra's algorithm to find the shortest path from a given source node to a target node in the graph.

Implement the following function in the programming language of your choice:

```python
def dijkstra(graph, source, target):
    """
    Finds the shortest path between a source node and a target node in a weighted graph.

    :param graph: Adjacency matrix representing the weighted graph.
    :type graph: List[List[int]]
    :param source: Source node.
    :type source: int
    :param target: Target node.
    :type target: int
    :return: Shortest path from source to target.
    :rtype: List[int] or None
    """
    # Your implementation here
    pass
```

### Input:

- graph: Adjacency matrix representing the weighted graph.
- source: Source node (integer).
- target: Target node (integer).

### Output:

- If a path exists from the source node to the target node, return a list of nodes representing the shortest path.
- If no path exists, return None.

### Constraints:

- The input graph is a square matrix where the number of rows and columns is equal to the number of nodes in the graph.
- All weights in the graph are non-negative integers.
- Nodes in the graph are labeled from 0 to N-1, where N is the number of nodes.

### Example:

```python
graph = [
    [0, 4, 2, 0, 0],
    [4, 0, 1, 5, 0],
    [2, 1, 0, 8, 10],
    [0, 5, 8, 0, 2],
    [0, 0, 10, 2, 0]
]
source = 0
target = 4
print(dijkstra(graph, source, target))  # Output: [0, 2, 4]
```
