```

 ▄▄▄        ██████ ▄▄▄█████▓▓█████  ██▀███   ▒█████   ██▓▓█████▄
▒████▄    ▒██    ▒ ▓  ██▒ ▓▒▓█   ▀ ▓██ ▒ ██▒▒██▒  ██▒▓██▒▒██▀ ██▌
▒██  ▀█▄  ░ ▓██▄   ▒ ▓██░ ▒░▒███   ▓██ ░▄█ ▒▒██░  ██▒▒██▒░██   █▌
░██▄▄▄▄██   ▒   ██▒░ ▓██▓ ░ ▒▓█  ▄ ▒██▀▀█▄  ▒██   ██░░██░░▓█▄   ▌
 ▓█   ▓██▒▒██████▒▒  ▒██▒ ░ ░▒████▒░██▓ ▒██▒░ ████▓▒░░██░░▒████▓
 ▒▒   ▓▒█░▒ ▒▓▒ ▒ ░  ▒ ░░   ░░ ▒░ ░░ ▒▓ ░▒▓░░ ▒░▒░▒░ ░▓   ▒▒▓  ▒
  ▒   ▒▒ ░░ ░▒  ░ ░    ░     ░ ░  ░  ░▒ ░ ▒░  ░ ▒ ▒░  ▒ ░ ░ ▒  ▒
  ░   ▒   ░  ░  ░    ░         ░     ░░   ░ ░ ░ ░ ▒   ▒ ░ ░ ░  ░
      ░  ░      ░              ░  ░   ░         ░ ░   ░     ░
                                                          ░

 ▄████▄   ▒█████   ██▓     ██▓     ██▓  ██████  ██▓ ▒█████   ███▄    █
▒██▀ ▀█  ▒██▒  ██▒▓██▒    ▓██▒    ▓██▒▒██    ▒ ▓██▒▒██▒  ██▒ ██ ▀█   █
▒▓█    ▄ ▒██░  ██▒▒██░    ▒██░    ▒██▒░ ▓██▄   ▒██▒▒██░  ██▒▓██  ▀█ ██▒
▒▓▓▄ ▄██▒▒██   ██░▒██░    ▒██░    ░██░  ▒   ██▒░██░▒██   ██░▓██▒  ▐▌██▒
▒ ▓███▀ ░░ ████▓▒░░██████▒░██████▒░██░▒██████▒▒░██░░ ████▓▒░▒██░   ▓██░
░ ░▒ ▒  ░░ ▒░▒░▒░ ░ ▒░▓  ░░ ▒░▓  ░░▓  ▒ ▒▓▒ ▒ ░░▓  ░ ▒░▒░▒░ ░ ▒░   ▒ ▒
  ░  ▒     ░ ▒ ▒░ ░ ░ ▒  ░░ ░ ▒  ░ ▒ ░░ ░▒  ░ ░ ▒ ░  ░ ▒ ▒░ ░ ░░   ░ ▒░
░        ░ ░ ░ ▒    ░ ░     ░ ░    ▒ ░░  ░  ░   ▒ ░░ ░ ░ ▒     ░   ░ ░
░ ░          ░ ░      ░  ░    ░  ░ ░        ░   ░      ░ ░           ░
░

```

**Introduction**

Welcome, brave coder, to the interstellar realm of Asteroidia! The universe is in turmoil, with asteroids hurtling through space, colliding with each other. As the esteemed Coding Astrophysicist, you must determine the final state of these celestial bodies after all collisions. Prepare your algorithms, for this cosmic challenge requires precision and logic!

---

**Chapter 1: The Astrophysicist's Analysis**

The Coding Astrophysicist begins by analyzing the array of asteroids, preparing to simulate their collisions.

```python
class CodingAstrophysicist:
    def __init__(self, asteroids):
        self.asteroids = asteroids

    def analyze_collisions(self):
        stack = []
        for asteroid in self.asteroids:
            while stack and asteroid < 0 < stack[-1]:
                if stack[-1] < -asteroid:
                    stack.pop()
                    continue
                elif stack[-1] == -asteroid:
                    stack.pop()
                break
            else:
                stack.append(asteroid)
        return stack

def final_state_of_asteroids(asteroids):
    astrophysicist = CodingAstrophysicist(asteroids)
    return astrophysicist.analyze_collisions()

# Example missions
missions = [
    [5, 10, -5],
    [8, -8],
    [10, 2, -5]
]

# The Astrophysicist undertakes the missions
for i, mission in enumerate(missions):
    result = final_state_of_asteroids(mission)
    print(f"Mission {i}: Final state of asteroids: {result}")
```

---

**Chapter 2: The Cosmic Collision**

With the array of asteroids analyzed, the Astrophysicist simulates the cosmic collisions, resolving each encounter with logic and precision.

```python
class CodingAstrophysicist:
    def __init__(self, asteroids):
        self.asteroids = asteroids

    def analyze_collisions(self):
        stack = []
        for asteroid in self.asteroids:
            while stack and asteroid < 0 < stack[-1]:
                if stack[-1] < -asteroid:
                    stack.pop()
                    continue
                elif stack[-1] == -asteroid:
                    stack.pop()
                break
            else:
                stack.append(asteroid)
        return stack

def final_state_of_asteroids(asteroids):
    astrophysicist = CodingAstrophysicist(asteroids)
    return astrophysicist.analyze_collisions()

# Example missions
missions = [
    [5, 10, -5],
    [8, -8],
    [10, 2, -5]
]

# The Astrophysicist undertakes the missions
for i, mission in enumerate(missions):
    result = final_state_of_asteroids(mission)
    print(f"Mission {i}: Final state of asteroids: {result}")
```

---

**Chapter 3: Mission Report**

The mission concludes with the Astrophysicist reporting the final state of the asteroids after all collisions.

```python
class CodingAstrophysicist:
    def __init__(self, asteroids):
        self.asteroids = asteroids

    def analyze_collisions(self):
        stack = []
        for asteroid in self.asteroids:
            while stack and asteroid < 0 < stack[-1]:
                if stack[-1] < -asteroid:
                    stack.pop()
                    continue
                elif stack[-1] == -asteroid:
                    stack.pop()
                break
            else:
                stack.append(asteroid)
        return stack

def final_state_of_asteroids(asteroids):
    astrophysicist = CodingAstrophysicist(asteroids)
    return astrophysicist.analyze_collisions()

# Example missions
missions = [
    [5, 10, -5],
    [8, -8],
    [10, 2, -5]
]

# The Astrophysicist undertakes the missions
for i, mission in enumerate(missions):
    result = final_state_of_asteroids(mission)
    print(f"Mission {i}: Final state of asteroids: {result}")
```

---

**Conclusion**

Congratulations, noble Coding Astrophysicist! You have successfully navigated the celestial challenge, determining the final state of the asteroids after all collisions. Your journey has enhanced your understanding of stack-based algorithms and collision resolution, preparing you for even greater cosmic challenges ahead. May your code always be efficient and your logic ever precise!

---

Happy coding, and may your adventures in the realm of algorithms be ever fruitful!
