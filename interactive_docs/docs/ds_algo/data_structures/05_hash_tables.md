# Hash Tables: Efficient Key-Value Data Structures

## Learning Objectives

- Understand the principles behind hash tables and hash functions
- Implement hash tables with different collision resolution strategies
- Analyze the time and space complexity of hash table operations
- Explore real-world applications of hash tables
- Compare hash table implementations with other data structures

## Environment Setup

| Setup Steps                                                               |
| :------------------------------------------------------------------------ |
| 1. Ensure Python 3.8+ is installed (`python --version`)                   |
| 2. No external packages required (uses Python's built-in data structures) |
| 3. Create a working directory for your code                               |
| 4. Save code examples as `.py` files                                      |
| 5. Run with `python filename.py`                                          |

## Concept Overview

A hash table (hash map) is a data structure that implements an associative array, which maps keys to values using a hash function.

### Hash Table Visualization

```
┌───────────────────────────────────┐
│ Hash Function: h(key) → index     │
└───────────────────────────────────┘
            │
            ▼
┌───┬───┬───┬───┬───┬───┬───┬───┬───┐
│ 0 │ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │...│  ← Buckets/Slots
├───┼───┼───┼───┼───┼───┼───┼───┼───┤
│   │ ▼ │   │ ▼ │   │ ▼ │   │   │   │
└───┴───┴───┴───┴───┴───┴───┴───┴───┘
      │       │       │
      ▼       ▼       ▼
    (k,v)   (k,v)→(k,v)  ← Entries (key-value pairs)
             Collision handling
             (e.g., chaining)
```

### Key Concepts

1. **Hash Function**: Converts keys into array indices
2. **Collision Resolution**: Handling when different keys hash to the same index
   - **Chaining**: Store multiple key-value pairs in the same slot using a linked list
   - **Open Addressing**: Find another empty slot (linear probing, quadratic probing, double hashing)
3. **Load Factor**: Ratio of filled slots to total slots (affects performance)
4. **Rehashing**: Increasing table size and redistributing entries when load factor gets too high

## Starter Code with Gaps

Save this as `hash_table_implementations.py`:

```python
"""
Hash Table Implementations - Collaborative Learning Exercise
"""
import hashlib

def main():
    print("===== PYTHON DICTIONARY =====")
    # Python's built-in dict is a hash table implementation
    py_dict = {}

    # TODO: Demonstrate dictionary operations

    print("\n===== CHAINING HASH TABLE =====")
    # Hash table using separate chaining for collision resolution
    chaining_table = HashTableChaining(size=10)

    # TODO: Demonstrate chaining hash table operations

    print("\n===== OPEN ADDRESSING HASH TABLE =====")
    # Hash table using linear probing for collision resolution
    open_addr_table = HashTableOpenAddressing(size=10)

    # TODO: Demonstrate open addressing hash table operations

    print("\n===== HASH TABLE APPLICATIONS =====")
    # TODO: Implement example applications of hash tables


class HashTableChaining:
    """
    Hash table implementation using separate chaining for collision resolution.
    Each slot contains a linked list of key-value pairs that hash to that slot.
    """

    class Node:
        """A node in a linked list for chaining"""
        def __init__(self, key, value):
            self.key = key
            self.value = value
            self.next = None

    def __init__(self, size=10):
        """Initialize a hash table with given size"""
        self.size = size
        self.buckets = [None] * size
        self.count = 0

    def _hash(self, key):
        """Hash function to convert key to index"""
        # TODO: Implement a hash function
        # Hint: Use built-in hash() or hashlib for strings, or implement your own
        # Make sure to handle different key types
        pass

    def put(self, key, value):
        """Insert or update a key-value pair"""
        # TODO: Implement insertion with chaining
        # If key exists, update its value
        # If key doesn't exist, add a new node to the chain
        pass

    def get(self, key):
        """Get value for a key"""
        # TODO: Implement retrieval
        # Return the value if key exists, otherwise return None
        pass

    def remove(self, key):
        """Remove a key-value pair"""
        # TODO: Implement removal
        # Return True if removed, False if key not found
        pass

    def contains(self, key):
        """Check if key exists"""
        # TODO: Implement contains check
        pass

    def keys(self):
        """Return all keys in the hash table"""
        # TODO: Implement keys collection
        pass

    def values(self):
        """Return all values in the hash table"""
        # TODO: Implement values collection
        pass

    def size(self):
        """Return number of key-value pairs"""
        # TODO: Return size
        pass

    def load_factor(self):
        """Calculate and return the load factor"""
        # TODO: Implement load factor calculation
        pass

    def _resize(self, new_size):
        """Resize the hash table"""
        # TODO: Implement resizing
        # Create a new larger table and rehash all existing entries
        pass

    def __str__(self):
        """String representation of the hash table"""
        # TODO: Implement string representation
        pass


class HashTableOpenAddressing:
    """
    Hash table implementation using open addressing with linear probing
    for collision resolution.
    """

    # Special marker for deleted slots
    _DELETED = object()

    def __init__(self, size=10):
        """Initialize a hash table with given size"""
        self.size = size
        self.keys = [None] * size
        self.values = [None] * size
        self.count = 0

    def _hash(self, key):
        """Hash function to convert key to index"""
        # TODO: Implement a hash function
        pass

    def _find_slot(self, key):
        """Find the slot for a key using linear probing"""
        # TODO: Implement slot finding
        # Return the index where key is found or should be inserted
        # Handle collisions with linear probing
        pass

    def put(self, key, value):
        """Insert or update a key-value pair"""
        # TODO: Implement insertion with linear probing
        # If load factor is too high, resize before insertion
        pass

    def get(self, key):
        """Get value for a key"""
        # TODO: Implement retrieval with linear probing
        pass

    def remove(self, key):
        """Remove a key-value pair"""
        # TODO: Implement removal
        # Use a special marker to indicate deleted slots
        pass

    def contains(self, key):
        """Check if key exists"""
        # TODO: Implement contains check
        pass

    def keys(self):
        """Return all keys in the hash table"""
        # TODO: Implement keys collection
        pass

    def values(self):
        """Return all values in the hash table"""
        # TODO: Implement values collection
        pass

    def size(self):
        """Return number of key-value pairs"""
        # TODO: Return size
        pass

    def load_factor(self):
        """Calculate and return the load factor"""
        # TODO: Implement load factor calculation
        pass

    def _resize(self, new_size):
        """Resize the hash table"""
        # TODO: Implement resizing
        pass

    def __str__(self):
        """String representation of the hash table"""
        # TODO: Implement string representation
        pass


def count_word_frequency(text):
    """Count frequency of each word in text using a hash table"""
    # TODO: Implement word frequency counter
    # Hint: Split text into words, use a hash table to count occurrences
    pass


def check_anagrams(word1, word2):
    """Check if two words are anagrams using a hash table"""
    # TODO: Implement anagram checker
    # Hint: Use a hash table to count character frequencies
    pass


def first_non_repeating_char(text):
    """Find the first non-repeating character in a string using a hash table"""
    # TODO: Implement first non-repeating character finder
    # Hint: First count all characters, then find first with count 1
    pass


if __name__ == "__main__":
    main()
```

## Live Coding Collaboration Tasks

### Task 1: Python Dictionary Operations

Working in pairs, implement the dictionary operations in the main function:

1. First person implements basic operations (put, get, remove)
2. Second person implements advanced operations (iteration, dictionary comprehensions)
3. Compare Python's built-in dictionary performance characteristics

### Task 2: Implement Hash Table with Chaining

In teams of 2-3:

1. First coder implements `_hash` method and `put` operation
2. Second coder implements `get` and `contains` operations
3. Third coder (or back to first) implements `remove` and collection methods
4. All implement resizing together
5. Test with various input types and edge cases

### Task 3: Implement Hash Table with Open Addressing

In teams of 2-3:

1. First coder implements `_hash`, `_find_slot`, and `put` methods
2. Second coder implements `get` and `contains` operations
3. Third coder (or back to first) implements `remove` and resizing
4. Test with various load factors and observe performance

### Task 4: Implement Hash Table Applications

Divide the utility functions among team members and implement them using hash tables.

## Peer Discussion Questions

1. **Hash Function Design**:

   - What makes a good hash function? What properties should it have?
   - How do hash collisions affect performance?
   - How can we create hash functions for custom objects?

2. **Collision Resolution**:

   - Compare the advantages and disadvantages of chaining vs. open addressing.
   - How does the load factor affect each collision resolution strategy?
   - When would you choose one strategy over another?

3. **Performance Analysis**:
   - What is the average and worst-case time complexity for hash table operations?
   - How does the load factor affect performance?
   - What trade-offs exist between memory usage and performance?

## Time and Space Complexity

| Operation | Average Case | Worst Case (without resize) | Amortized (with resize) |
| --------- | ------------ | --------------------------- | ----------------------- |
| Insert    | O(1)         | O(n)                        | O(1)                    |
| Lookup    | O(1)         | O(n)                        | O(1)                    |
| Delete    | O(1)         | O(n)                        | O(1)                    |
| Space     | O(n)         | O(n)                        | O(n)                    |

\*Worst case occurs when all keys hash to the same bucket or when there are many collisions

## Common Bugs and Debugging Tips

1. **Poor Hash Distribution**

   - Issue: Too many collisions due to a poorly designed hash function
   - Debug:
     - Test your hash function with various inputs to ensure even distribution
     - Visualize the distribution of keys across buckets
     ```python
     def analyze_distribution(hash_table):
         distribution = [0] * hash_table.size
         for i in range(hash_table.size):
             bucket = hash_table.buckets[i]
             count = 0
             while bucket:
                 count += 1
                 bucket = bucket.next
             distribution[i] = count
         return distribution
     ```

2. **Hash Collisions**

   - Issue: Same hash value for different keys
   - Debug: Ensure your collision resolution strategy works correctly

   ```python
   # Test with keys that you know will collide
   key1 = "abc"
   key2 = "cba"
   # Force collision by using a simple hash function like sum of character codes
   ```

3. **Incorrect Removal in Open Addressing**

   - Issue: Simply setting a slot to None breaks the search algorithm
   - Debug: Use a special marker for deleted slots rather than None

   ```python
   # Check if proper tombstone marking is used
   def test_delete_and_find(table):
       table.put("key1", "value1")
       table.put("key2", "value2")
       table.remove("key1")  # Should mark as deleted, not None
       assert table.get("key2") == "value2"  # Should still find key2
   ```

4. **Forgetting to Resize**
   - Issue: Performance degradation as table fills up
   - Debug: Monitor load factor and resize when it exceeds a threshold
   ```python
   def put(self, key, value):
       if self.load_factor() > 0.7:  # Threshold commonly used
           self._resize(self.size * 2)
       # Rest of put implementation
   ```

## Group Challenge: Building a Document Indexer

### Challenge Task

Implement a simple document indexing and search system using hash tables that:

1. Indexes multiple text documents by creating an inverted index
2. Allows searching for documents containing specific words
3. Ranks documents by relevance (frequency of search terms)
4. Supports basic boolean operators (AND, OR) in search queries
5. Implements a simple word stemming algorithm (e.g., removing common suffixes)

### Approach Instructions

1. **Design Phase (10 minutes)**:

   - Plan the structure of the inverted index
   - Define the document and term storage approach
   - Outline search algorithms for different query types

2. **Implementation Phase (15 minutes)**:

   - **Person 1**: Implement document parsing and word extraction
   - **Person 2**: Build the inverted index using hash tables
   - **Person 3**: Implement search functionality with ranking

3. **Testing Phase (5 minutes)**:
   - Create a small corpus of test documents
   - Try different search queries (single word, multiple words, boolean)
   - Evaluate whether document ranking makes sense

### Starter Code for Document Indexer

```python
class DocumentIndexer:
    def __init__(self):
        """Initialize the indexer with an empty inverted index"""
        # TODO: Initialize data structures
        # Inverted index: word -> {doc_id -> [positions]}
        self.inverted_index = {}
        self.documents = {}  # doc_id -> original text

    def add_document(self, doc_id, text):
        """Add a document to the index"""
        # TODO: Implement document addition
        # 1. Store the original document
        # 2. Tokenize the text into words
        # 3. Update the inverted index with word positions
        pass

    def search(self, query):
        """Search for documents matching the query"""
        # TODO: Implement search functionality
        # Parse query, find matching documents, and rank them
        pass

    def search_boolean(self, query):
        """Support AND/OR queries like 'word1 AND word2 OR word3'"""
        # TODO: Implement boolean search
        pass

    def _tokenize(self, text):
        """Convert text to tokens (words)"""
        # TODO: Implement tokenization
        # Remove punctuation, convert to lowercase, split into words
        pass

    def _stem(self, word):
        """Perform simple word stemming"""
        # TODO: Implement basic stemming
        # Remove common suffixes like -ing, -ed, -s
        pass

    def _rank_documents(self, matching_docs, query_terms):
        """Rank matching documents by relevance"""
        # TODO: Implement document ranking
        # Consider term frequency, document frequency, positions
        pass


# Utility functions for testing the index
def create_test_corpus():
    """Create a small test corpus of documents"""
    # TODO: Create 5-10 short documents about different topics
    pass

def test_indexer(indexer):
    """Test the indexer with various queries"""
    # TODO: Try different types of searches and print results
    pass
```

## Reflection Questions

After completing the exercises:

1. How do the performance characteristics of your hash table implementations compare to Python's built-in dictionaries?
2. What were the most challenging aspects of implementing collision resolution strategies?
3. How would you scale your hash table implementation to handle millions of entries efficiently?
4. In what real-world applications have you encountered hash tables, and how might your implementation be adapted for those scenarios?
