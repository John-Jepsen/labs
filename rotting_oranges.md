```

 ____   ___  ______ ______ ____ ____   ____       ___  ____   ____ ____   ____   ___ _____
|    \ /   \|      |      |    |    \ /    |     /   \|    \ /    |    \ /    | /  _] ___/
|  D  )     |      |      ||  ||  _  |   __|    |     |  D  )  o  |  _  |   __|/  [(   \_
|    /|  O  |_|  |_|_|  |_||  ||  |  |  |  |    |  O  |    /|     |  |  |  |  |    _]__  |
|    \|     | |  |   |  |  |  ||  |  |  |_ |    |     |    \|  _  |  |  |  |_ |   [_/  \ |
|  .  \     | |  |   |  |  |  ||  |  |     |    |     |  .  \  |  |  |  |     |     \    |
|__|\_|\___/  |__|   |__| |____|__|__|___,_|     \___/|__|\_|__|__|__|__|___,_|_____|\___|


```

---

**Introduction**

Welcome, great Coding Assassin, to the intricate world of DataGridia! The inhabitants now face a critical challenge: ensuring the efficient propagation of rotting through a grid of oranges. Prepare your algorithms, for this mission requires your unparalleled skills in BFS and grid traversal. Only you can eliminate the inefficiencies and ensure the rot spreads as quickly as possible. Engage your coding prowess and complete the mission!

---

**Chapter 1: The Assassin's Survey**

The Great Coding Assassin begins by surveying the battlefield, identifying the locations of all fresh and rotten oranges.

```python
from collections import deque

class GreatCodingAssassin:
    def __init__(self, grid):
        self.grid = grid
        self.rows = len(grid)
        self.cols = len(grid[0])
        self.queue = deque()
        self.fresh_oranges = 0

    def prepare_mission(self):
        # The Great Coding Assassin surveys the battlefield
        for r in range(self.rows):
            for c in range(self.cols):
                if self.grid[r][c] == 2:
                    self.queue.append((r, c))
                elif self.grid[r][c] == 1:
                    self.fresh_oranges += 1
```

---

**Chapter 2: The Rot's Propagation**

With the battlefield surveyed, the Assassin executes the mission, spreading the rot with precision and efficiency.

```python
    def eliminate_obstacles(self):
        # The Assassin begins the mission
        directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]
        minutes_passed = 0
        while self.queue and self.fresh_oranges > 0:
            minutes_passed += 1
            for _ in range(len(self.queue)):
                x, y = self.queue.popleft()
                for dx, dy in directions:
                    nx, ny = x + dx, y + dy
                    if 0 <= nx < self.rows and 0 <= ny < self.cols and self.grid[nx][ny] == 1:
                        self.grid[nx][ny] = 2
                        self.queue.append((nx, ny))
                        self.fresh_oranges -= 1
        return minutes_passed if self.fresh_oranges == 0 else -1
```

---

**Chapter 3: Mission Report**

The mission concludes with the Assassin reporting the results of the rot's propagation.

```python
    def report_results(self):
        # The Assassin reports the mission outcome
        return self.eliminate_obstacles()

    def execute(self):
        self.prepare_mission()
        return self.report_results()
```

---

**Chapter 4: Undertaking the Mission**

The Coding Assassin is summoned with a new mission grid, ready to execute the task and ensure all oranges rot in the shortest time possible.

```python
def oranges_rotting(grid):
    assassin = GreatCodingAssassin(grid)
    return assassin.execute()

# Example mission grid
mission_grid = [
    [2, 1, 1],
    [1, 1, 0],
    [0, 1, 1]
]

# The Assassin undertakes the mission
result = oranges_rotting(mission_grid)
print(f"Mission result: {result}")
```

---

**Conclusion**

Congratulations, noble Coding Assassin! You have efficiently ensured the propagation of rot throughout DataGridia, optimizing the process with your advanced algorithmic skills. Your journey has enhanced your understanding of BFS and grid traversal, preparing you for even greater challenges. May your code always be efficient and your logic ever precise!

---
