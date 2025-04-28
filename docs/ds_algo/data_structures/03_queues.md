# Queues: First-In-First-Out (FIFO) Data Structures

## Learning Objectives

- Understand the FIFO (First-In-First-Out) principle of queues
- Implement standard, circular, and priority queues in Python
- Apply queue operations: enqueue, dequeue, peek, and isEmpty
- Analyze time and space complexity of queue operations
- Recognize application scenarios for different queue types

## Environment Setup

| Setup Steps                                                               |
| :------------------------------------------------------------------------ |
| 1. Ensure Python 3.8+ is installed (`python --version`)                   |
| 2. No external packages required (uses Python's built-in data structures) |
| 3. Create a working directory for your code                               |
| 4. Save code examples as `.py` files                                      |
| 5. Run with `python filename.py`                                          |

## Concept Overview

A queue is a linear data structure that follows the First-In-First-Out (FIFO) principle - the first element added is the first one removed.

### Queue Visualization

```
    ┌────┬────┬────┬────┬────┐
    │Item1│Item2│Item3│Item4│    │
    └────┴────┴────┴────┴────┘
      ↑                 ↑
    Front             Rear
   (Dequeue)        (Enqueue)
```

### Types of Queues

1. **Standard Queue**: Basic FIFO structure
2. **Circular Queue**: Efficient use of fixed-size array by wrapping around
3. **Priority Queue**: Elements have associated priorities and are dequeued in priority order

### Core Operations

- **enqueue(item)**: Add an item to the rear of the queue
- **dequeue()**: Remove and return the front item from the queue
- **peek()** or **front()**: Return the front item without removing it
- **isEmpty()**: Check if the queue is empty

## Starter Code with Gaps

Save this as `queue_implementations.py`:

```python
"""
Queue Implementations - Collaborative Learning Exercise
"""
from collections import deque

def main():
    print("===== STANDARD QUEUE USING LIST =====")
    # Warning: Using list as a queue is not efficient due to O(n) time complexity for dequeue
    queue = []

    # TODO: Implement standard queue operations using list
    # Enqueue: append(), Dequeue: pop(0)

    print("\n===== STANDARD QUEUE USING COLLECTIONS.DEQUE =====")
    # More efficient implementation using deque
    queue = deque()

    # TODO: Implement queue operations using deque
    # Enqueue: append(), Dequeue: popleft()

    print("\n===== CUSTOM QUEUE IMPLEMENTATION =====")
    # Using our custom implementation
    my_queue = Queue()

    # TODO: Use custom queue methods

    print("\n===== CIRCULAR QUEUE =====")
    # Fixed-size circular queue
    circular_queue = CircularQueue(5)  # Size 5

    # TODO: Demonstrate circular queue operations

    print("\n===== PRIORITY QUEUE =====")
    # Queue where items have priorities
    priority_queue = PriorityQueue()

    # TODO: Demonstrate priority queue operations

    print("\n===== QUEUE APPLICATIONS =====")
    # TODO: Implement breadth-first traversal of a simple graph
    print("Breadth-First Traversal:")
    graph = {
        'A': ['B', 'C'],
        'B': ['A', 'D', 'E'],
        'C': ['A', 'F'],
        'D': ['B'],
        'E': ['B', 'F'],
        'F': ['C', 'E']
    }
    breadth_first_traversal(graph, 'A')


class Queue:
    """A custom Queue implementation using a Python list"""

    def __init__(self):
        """Initialize an empty queue"""
        # TODO: Initialize the internal data structure

    def enqueue(self, item):
        """Add an item to the rear of the queue"""
        # TODO: Implement enqueue operation
        pass

    def dequeue(self):
        """Remove and return the front item from the queue"""
        # TODO: Implement dequeue operation with error handling
        pass

    def peek(self):
        """Return the front item without removing it"""
        # TODO: Implement peek operation with error handling
        pass

    def is_empty(self):
        """Check if the queue is empty"""
        # TODO: Implement isEmpty check
        pass

    def size(self):
        """Return the number of items in the queue"""
        # TODO: Return queue size
        pass

    def __str__(self):
        """Return a string representation of the queue"""
        # TODO: Implement string representation
        pass


class CircularQueue:
    """A circular queue implementation with fixed size"""

    def __init__(self, capacity):
        """Initialize an empty circular queue with given capacity"""
        # TODO: Initialize the circular queue with fixed capacity
        # Hint: Use an array of given size, plus front and rear pointers
        self.capacity = capacity
        self.queue = [None] * capacity
        self.front = self.rear = -1

    def enqueue(self, item):
        """Add an item to the queue"""
        # TODO: Implement circular enqueue with full queue detection
        # Remember to handle the case when queue is empty (front = rear = -1)
        # And the case when rear needs to wrap around to 0
        pass

    def dequeue(self):
        """Remove and return the front item from the queue"""
        # TODO: Implement circular dequeue
        # Remember to handle the case when queue becomes empty after dequeue
        # And the case when front needs to wrap around to 0
        pass

    def peek(self):
        """Return the front item without removing it"""
        # TODO: Implement peek operation with error handling
        pass

    def is_empty(self):
        """Check if the queue is empty"""
        # TODO: Implement isEmpty check
        pass

    def is_full(self):
        """Check if the queue is full"""
        # TODO: Implement isFull check
        # Hint: Queue is full when (rear + 1) % capacity == front
        pass

    def size(self):
        """Return the number of items in the queue"""
        # TODO: Calculate size in a circular queue
        pass

    def __str__(self):
        """Return a string representation of the circular queue"""
        # TODO: Implement string representation
        pass


class PriorityQueue:
    """A priority queue implementation using a list of (item, priority) tuples"""

    def __init__(self):
        """Initialize an empty priority queue"""
        # TODO: Initialize the internal data structure
        # Hint: Use a list of (item, priority) tuples

    def enqueue(self, item, priority):
        """Add an item with a priority (lower number = higher priority)"""
        # TODO: Implement priority enqueue
        # Hint: Either insert in order or use a simple list and sort on dequeue
        pass

    def dequeue(self):
        """Remove and return the highest priority item"""
        # TODO: Implement priority dequeue
        # Return the item with the highest priority (lowest priority number)
        pass

    def peek(self):
        """Return the highest priority item without removing it"""
        # TODO: Implement priority peek
        pass

    def is_empty(self):
        """Check if the priority queue is empty"""
        # TODO: Implement isEmpty check
        pass

    def size(self):
        """Return the number of items in the priority queue"""
        # TODO: Return priority queue size
        pass

    def __str__(self):
        """Return a string representation of the priority queue"""
        # TODO: Implement string representation
        pass


def breadth_first_traversal(graph, start_node):
    """Traverse a graph in breadth-first order using a queue"""
    # TODO: Implement BFS traversal using a queue
    # Hint: Use a queue to keep track of nodes to visit
    # and a set to keep track of visited nodes
    pass


if __name__ == "__main__":
    main()
```

## Live Coding Collaboration Tasks

### Task 1: Standard Queue Implementation

Working in pairs, implement the standard queue operations in the main function:

1. First person implements list-based queue operations
2. Second person implements deque-based queue operations
3. Together, compare the efficiency and discuss the differences

### Task 2: Complete the Queue Classes

In teams of 3 (or rotate roles in smaller teams):

1. **Person 1**: Complete the basic `Queue` class
2. **Person 2**: Complete the `CircularQueue` class
3. **Person 3**: Complete the `PriorityQueue` class
4. All review implementations together and test them

### Task 3: Implement BFS Algorithm

As a team, implement the `breadth_first_traversal` function using the queue data structure.

## Peer Discussion Questions

1. **Implementation Comparisons**:

   - What are the advantages of using deque over a list for a queue?
   - Compare the advantages and disadvantages of each queue implementation.
   - How does the circular queue optimize space usage?

2. **Use Case Analysis**:

   - When would you use a circular queue instead of a standard queue?
   - What real-world scenarios call for a priority queue?
   - How do queues enable breadth-first algorithms?

3. **Design Considerations**:
   - How would you implement a bounded queue with a maximum size?
   - What are the trade-offs of different priority queue implementations?
   - How would you implement a double-ended queue (deque)?

## Time and Space Complexity

| Operation | List-based Queue | deque-based Queue | Circular Queue | Priority Queue     |
| --------- | ---------------- | ----------------- | -------------- | ------------------ |
| Enqueue   | O(1)             | O(1)              | O(1)           | O(log n) or O(n)\* |
| Dequeue   | O(n)             | O(1)              | O(1)           | O(log n) or O(1)\* |
| Peek      | O(1)             | O(1)              | O(1)           | O(1)               |
| isEmpty   | O(1)             | O(1)              | O(1)           | O(1)               |
| Space     | O(n)             | O(n)              | O(n)           | O(n)               |

\*Note: Complexity for Priority Queue depends on implementation (heap vs. sorted list)

## Common Bugs and Debugging Tips

1. **Empty Queue Operations**

   - Error: Trying to dequeue or peek from an empty queue
   - Debug: Always check if the queue is empty before operations

   ```python
   if not queue.is_empty():
       item = queue.dequeue()
   else:
       print("Cannot dequeue from empty queue")
   ```

2. **Circular Queue Pointer Management**

   - Issue: Incorrect management of front and rear pointers
   - Debug: Carefully handle boundary conditions

   ```python
   # When enqueueing to a circular queue
   self.rear = (self.rear + 1) % self.capacity

   # When dequeueing from a circular queue
   self.front = (self.front + 1) % self.capacity
   ```

3. **Queue Full Condition**

   - Issue: Unable to detect when circular queue is full
   - Debug: Implement is_full method correctly

   ```python
   def is_full(self):
       return (self.rear + 1) % self.capacity == self.front
   ```

4. **Priority Confusion**
   - Issue: Confusion about priority ordering (higher number = higher priority or vice versa)
   - Debug: Clearly document and consistently implement priority ordering

## Group Challenge: Implementing a Service Queue System

### Challenge Task

Build a simple customer service queue simulation with multiple priority levels:

1. VIP customers (highest priority)
2. Regular customers with appointments (medium priority)
3. Walk-in customers (lowest priority)

The system should:

- Allow adding customers of different types
- Process customers in priority order
- Handle situations when the queue reaches capacity
- Provide estimated wait times based on processing speed
- Allow emergency priority override

### Approach Instructions

1. **Design Phase (10 minutes)**:

   - Decide on the queue implementation to use
   - Define customer object structure
   - Plan the simulation workflow

2. **Implementation Phase (15 minutes)**:

   - **Person 1**: Implement the customer class and priority queue
   - **Person 2**: Build the queue processing system
   - **Person 3**: Create simulation controls and statistics tracking

3. **Testing Phase (5 minutes)**:
   - Run simulations with different customer arrival patterns
   - Test edge cases (empty queue, full queue, priority ties)
   - Compare metrics like average wait time by priority level

### Starter Code for Service Queue

```python
class Customer:
    def __init__(self, name, customer_type, arrival_time):
        """Initialize a customer with type determining priority"""
        # TODO: Implement customer initialization
        # customer_type can be "vip", "appointment", or "walkin"
        pass

class ServiceQueue:
    def __init__(self, capacity=50, processing_time=5):
        """Initialize service queue with capacity and avg processing time"""
        # TODO: Implement service queue using a priority queue
        pass

    def add_customer(self, customer):
        """Add a customer to the queue based on their priority"""
        # TODO: Implement customer addition with priority based on type
        pass

    def process_next_customer(self):
        """Process the next highest priority customer"""
        # TODO: Implement customer processing
        pass

    def get_wait_time_estimate(self, customer_type):
        """Estimate wait time for a new customer of given type"""
        # TODO: Implement wait time estimation
        pass

def run_simulation(arrival_rate, service_time, simulation_duration):
    """Run a simulation of the customer service queue"""
    # TODO: Implement queue simulation
    pass
```

## Reflection Questions

After completing the exercises:

1. How does the choice of queue implementation affect the performance of the BFS algorithm?
2. What were the most challenging aspects of implementing the circular queue?
3. How would your priority queue implementation scale with very large numbers of items?
4. In what scenarios might you need to combine different queue types or implement a custom hybrid queue?
