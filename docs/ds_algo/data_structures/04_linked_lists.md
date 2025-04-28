# Linked Lists: Node-Based Sequential Data Structures

## Learning Objectives

- Understand the structure and behavior of singly and doubly linked lists
- Implement node creation, traversal, insertion, and deletion operations
- Compare linked lists with array-based structures for different use cases
- Analyze time and space complexity of linked list operations
- Solve common problems using linked list techniques

## Environment Setup

| Setup Steps                                                               |
| :------------------------------------------------------------------------ |
| 1. Ensure Python 3.8+ is installed (`python --version`)                   |
| 2. No external packages required (uses Python's built-in data structures) |
| 3. Create a working directory for your code                               |
| 4. Save code examples as `.py` files                                      |
| 5. Run with `python filename.py`                                          |

## Concept Overview

A linked list is a linear data structure where elements are stored in nodes, each containing data and a reference (pointer) to the next node.

### Types of Linked Lists

1. **Singly Linked List**: Each node points to the next node
2. **Doubly Linked List**: Each node points to both next and previous nodes
3. **Circular Linked List**: Last node points back to the first node

### Linked List Visualization

**Singly Linked List:**

```
┌─────┬─────┐    ┌─────┬─────┐    ┌─────┬─────┐    ┌─────┬─────┐
│ Data│ Next│───►│ Data│ Next│───►│ Data│ Next│───►│ Data│ NULL│
└─────┴─────┘    └─────┴─────┘    └─────┴─────┘    └─────┴─────┘
   Head                                               Tail
```

**Doubly Linked List:**

```
┌─────┬─────┬─────┐    ┌─────┬─────┬─────┐    ┌─────┬─────┬─────┐
│NULL │ Data│ Next│◄──►│ Prev│ Data│ Next│◄──►│ Prev│ Data│ NULL│
└─────┴─────┴─────┘    └─────┴─────┴─────┘    └─────┴─────┴─────┘
   Head                                           Tail
```

## Starter Code with Gaps

Save this as `linked_list_implementations.py`:

```python
"""
Linked List Implementations - Collaborative Learning Exercise
"""

def main():
    print("===== SINGLY LINKED LIST =====")
    # Create a singly linked list
    sll = SinglyLinkedList()

    # TODO: Add elements to the singly linked list

    # TODO: Demonstrate traversal, insertion, and deletion

    print("\n===== DOUBLY LINKED LIST =====")
    # Create a doubly linked list
    dll = DoublyLinkedList()

    # TODO: Add elements to the doubly linked list

    # TODO: Demonstrate traversal (forward and backward), insertion, and deletion

    print("\n===== LINKED LIST APPLICATIONS =====")
    # TODO: Implement and test linked list applications


class Node:
    """A basic node for singly linked list"""

    def __init__(self, data):
        """Initialize a node with data and null next pointer"""
        self.data = data
        self.next = None


class DoublyNode:
    """A node for doubly linked list"""

    def __init__(self, data):
        """Initialize a node with data, null next, and null prev pointers"""
        self.data = data
        self.next = None
        self.prev = None


class SinglyLinkedList:
    """A singly linked list implementation"""

    def __init__(self):
        """Initialize an empty linked list"""
        self.head = None
        self.tail = None
        self.size = 0

    def is_empty(self):
        """Check if the list is empty"""
        # TODO: Implement is_empty check
        pass

    def append(self, data):
        """Add a node with given data to the end of the list"""
        # TODO: Implement append operation
        # Remember to update both head and tail pointers
        pass

    def prepend(self, data):
        """Add a node with given data to the beginning of the list"""
        # TODO: Implement prepend operation
        pass

    def insert_after(self, target_data, data):
        """Insert a new node with data after the first node containing target_data"""
        # TODO: Implement insert_after operation
        pass

    def delete(self, data):
        """Delete the first node containing the given data"""
        # TODO: Implement delete operation
        # Remember to handle special cases (empty list, deleting head, etc.)
        pass

    def find(self, data):
        """Find and return the first node containing the given data"""
        # TODO: Implement find operation
        pass

    def display(self):
        """Display all elements in the list"""
        # TODO: Implement display operation
        pass

    def get_size(self):
        """Get the number of nodes in the list"""
        # TODO: Implement get_size (could use a counter or stored size)
        pass

    def reverse(self):
        """Reverse the linked list in-place"""
        # TODO: Implement reversal operation
        # This is a common interview question!
        pass


class DoublyLinkedList:
    """A doubly linked list implementation"""

    def __init__(self):
        """Initialize an empty doubly linked list"""
        self.head = None
        self.tail = None
        self.size = 0

    def is_empty(self):
        """Check if the list is empty"""
        # TODO: Implement is_empty check
        pass

    def append(self, data):
        """Add a node with given data to the end of the list"""
        # TODO: Implement append operation
        # Make sure to update prev pointers too!
        pass

    def prepend(self, data):
        """Add a node with given data to the beginning of the list"""
        # TODO: Implement prepend operation
        pass

    def insert_after(self, target_data, data):
        """Insert a new node with data after the first node containing target_data"""
        # TODO: Implement insert_after operation
        pass

    def delete(self, data):
        """Delete the first node containing the given data"""
        # TODO: Implement delete operation
        # Make sure to update prev pointers too!
        pass

    def display_forward(self):
        """Display all elements in forward direction"""
        # TODO: Implement forward display
        pass

    def display_backward(self):
        """Display all elements in backward direction (using prev pointers)"""
        # TODO: Implement backward display (starting from tail)
        pass

    def get_size(self):
        """Get the number of nodes in the list"""
        # TODO: Implement get_size
        pass


# Examples of common linked list problems
def detect_cycle(linked_list):
    """Detect if a linked list has a cycle using Floyd's cycle-finding algorithm"""
    # TODO: Implement cycle detection using slow and fast pointers
    # Return True if cycle exists, False otherwise
    pass


def find_middle(linked_list):
    """Find the middle node of a linked list using the slow and fast pointer technique"""
    # TODO: Implement middle node finding
    # Return the middle node (or the second middle node if even length)
    pass


def merge_sorted_lists(list1, list2):
    """Merge two sorted linked lists into a single sorted linked list"""
    # TODO: Implement sorted list merging
    # Return the merged list
    pass


if __name__ == "__main__":
    main()
```

## Live Coding Collaboration Tasks

### Task 1: Singly Linked List Implementation

Working in pairs, implement the core singly linked list operations:

1. First person implements `is_empty`, `append`, and `prepend`
2. Second person implements `insert_after`, `delete`, and `find`
3. Together implement `display` and `get_size`
4. Discuss and implement `reverse` collaboratively

### Task 2: Doubly Linked List Implementation

Working in pairs or small teams:

1. First person implements the doubly linked list node operations (`append`, `prepend`)
2. Second person implements traversal operations (`display_forward`, `display_backward`)
3. Third person (or back to first) implements insertion and deletion operations

### Task 3: Solving Linked List Problems

As a team, implement the three common linked list algorithms:

1. Cycle detection
2. Finding the middle node
3. Merging sorted lists

## Peer Discussion Questions

1. **Implementation Considerations**:

   - What are the trade-offs between singly and doubly linked lists?
   - When would you choose a linked list over an array or vice versa?
   - How does maintaining a tail pointer affect the complexity of operations?

2. **Algorithmic Thinking**:

   - How does the slow/fast pointer technique work in cycle detection?
   - What are the edge cases when implementing linked list operations?
   - How would you implement a linked list from scratch if pointers were not available?

3. **Real-world Applications**:
   - Where are linked lists used in real systems?
   - How might linked lists be used in an undo/redo feature?
   - What types of applications benefit most from linked list structures?

## Time and Space Complexity

| Operation         | Singly Linked List | Doubly Linked List  | Array (for comparison) |
| ----------------- | ------------------ | ------------------- | ---------------------- |
| Access by index   | O(n)               | O(n)                | O(1)                   |
| Insert at front   | O(1)               | O(1)                | O(n)                   |
| Insert at end     | O(1)\*             | O(1)                | O(1)\* or O(n)         |
| Insert in middle  | O(n)               | O(n)                | O(n)                   |
| Delete from front | O(1)               | O(1)                | O(n)                   |
| Delete from end   | O(n) or O(1)\*     | O(1)                | O(1)\* or O(n)         |
| Delete in middle  | O(n)               | O(n)                | O(n)                   |
| Search            | O(n)               | O(n)                | O(n)                   |
| Space per element | O(1) + extra ptr   | O(1) + 2 extra ptrs | O(1)                   |

\*With tail pointer for linked lists or dynamic arrays

## Common Bugs and Debugging Tips

1. **Null Pointer Exceptions**

   - Error: Trying to access fields of a null node
   - Debug: Always check if a node is null before accessing its fields

   ```python
   if current is not None:
       current = current.next
   ```

2. **Lost Nodes**

   - Issue: Nodes becoming unreachable due to improper pointer updates
   - Debug: Be careful when updating pointers, especially in delete operations

   ```python
   # Save references before updating pointers
   temp = current.next
   current.next = current.next.next
   # Now temp can be safely deleted or recycled
   ```

3. **Infinite Loops in Cycles**

   - Issue: Getting stuck in infinite loops when traversing a circular linked list
   - Debug: Use the slow/fast pointer technique or keep track of visited nodes

   ```python
   visited = set()
   while current:
       if current in visited:
           print("Cycle detected")
           break
       visited.add(current)
       current = current.next
   ```

4. **Boundary Conditions**
   - Issue: Not handling edge cases like empty lists or single-element lists
   - Debug: Always test your code with empty lists, single-element lists, and boundary cases

## Group Challenge: Implementing a Playlist Manager

### Challenge Task

Build a music playlist manager using linked lists that supports:

1. Adding songs to the front, end, or after a specific song
2. Removing songs by name
3. Moving songs up or down in the playlist
4. Creating a "shuffle" feature that randomizes the order
5. Implementing "repeat one", "repeat all", and "no repeat" modes using circular and non-circular linked lists

### Approach Instructions

1. **Design Phase (10 minutes)**:

   - Decide on data structure (singly vs. doubly linked list)
   - Define node structure (what information to store about each song)
   - Plan the user interface for playlist operations

2. **Implementation Phase (15 minutes)**:

   - **Person 1**: Implement basic playlist operations (add, remove, display)
   - **Person 2**: Implement movement operations (move up, move down)
   - **Person 3**: Implement special features (shuffle, repeat modes)

3. **Testing Phase (5 minutes)**:
   - Create test playlists with sample songs
   - Test all operations individually and in combination
   - Verify edge cases (empty playlist, single song, etc.)

### Starter Code for Playlist Manager

```python
class Song:
    def __init__(self, title, artist, duration):
        self.title = title
        self.artist = artist
        self.duration = duration  # in seconds

    def __str__(self):
        minutes = self.duration // 60
        seconds = self.duration % 60
        return f"{self.title} by {self.artist} ({minutes}:{seconds:02d})"


class Playlist:
    def __init__(self, name):
        self.name = name
        # TODO: Initialize the appropriate linked list structure
        # Hint: Consider what operations will be most efficient

    def add_song(self, song, position="end"):
        """Add a song to the playlist at the specified position (start, end, or after a song)"""
        # TODO: Implement add_song functionality
        pass

    def remove_song(self, title):
        """Remove a song from the playlist by title"""
        # TODO: Implement remove_song functionality
        pass

    def move_up(self, title):
        """Move a song up one position in the playlist"""
        # TODO: Implement move_up functionality
        pass

    def move_down(self, title):
        """Move a song down one position in the playlist"""
        # TODO: Implement move_down functionality
        pass

    def shuffle(self):
        """Randomly reorder the songs in the playlist"""
        # TODO: Implement shuffle functionality
        # Hint: Consider converting to a list, shuffling, then rebuilding the linked list
        pass

    def set_repeat_mode(self, mode):
        """Set repeat mode (none, one, all)"""
        # TODO: Implement repeat mode setting
        # Hint: "all" could use a circular linked list
        pass

    def display(self):
        """Display all songs in the playlist"""
        # TODO: Implement display functionality
        pass
```

## Reflection Questions

After completing the exercises:

1. How did the choice between singly and doubly linked lists affect your implementation?
2. What were the most challenging operations to implement and why?
3. How would you improve your implementation for better performance or more features?
4. In what real-world scenarios would linked lists be preferable to arrays or other data structures?
