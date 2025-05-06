# Knuth-Morris-Pratt (KMP) Algorithm: Efficient Pattern Matching

## Environment Setup

```bash
# No special setup required - Python 3.8+ is all you need
# Verify your Python installation
python --version
```

## Visual Explanation

KMP algorithm efficiently finds occurrences of a pattern in a text by avoiding unnecessary character comparisons:

```
Text:    A B A B C A B A B D
Pattern: A B A B D

Step 1: Compute prefix function (LPS array)
Pattern: A B A B D
LPS:     0 0 1 2 0

Step 2: Pattern matching
i = 0, j = 0: Text[0] = 'A', Pattern[0] = 'A' → match, i++, j++
i = 1, j = 1: Text[1] = 'B', Pattern[1] = 'B' → match, i++, j++
i = 2, j = 2: Text[2] = 'A', Pattern[2] = 'A' → match, i++, j++
i = 3, j = 3: Text[3] = 'B', Pattern[3] = 'B' → match, i++, j++
i = 4, j = 4: Text[4] = 'C', Pattern[4] = 'D' → mismatch
  - Set j = LPS[j-1] = LPS[3] = 2
i = 4, j = 2: Text[4] = 'C', Pattern[2] = 'A' → mismatch
  - Set j = LPS[j-1] = LPS[1] = 0
i = 4, j = 0: Text[4] = 'C', Pattern[0] = 'A' → mismatch, i++, j = 0
i = 5, j = 0: Text[5] = 'A', Pattern[0] = 'A' → match, i++, j++
...and so on

When j reaches pattern length, we've found a match.
```

## Pseudocode

```
function compute_lps(pattern):
    m = length(pattern)
    lps = array of size m with all zeros
    length = 0  // length of the previous longest prefix suffix

    // the loop calculates lps[i] for i = 1 to m-1
    i = 1
    while i < m:
        if pattern[i] == pattern[length]:
            length += 1
            lps[i] = length
            i += 1
        else:
            if length != 0:
                length = lps[length - 1]
            else:
                lps[i] = 0
                i += 1
    return lps

function kmp_search(text, pattern):
    n = length(text)
    m = length(pattern)
    matches = empty list

    // If pattern is empty, return
    if m == 0:
        return matches

    // Preprocess the pattern - compute LPS array
    lps = compute_lps(pattern)

    i = 0  // index for text
    j = 0  // index for pattern

    while i < n:
        if pattern[j] == text[i]:
            i += 1
            j += 1

        if j == m:  // Found a match
            matches.append(i - j)
            j = lps[j - 1]
        else if i < n and pattern[j] != text[i]:
            if j != 0:
                j = lps[j - 1]
            else:
                i += 1

    return matches
```

## Annotated Code Template

```python
def compute_lps(pattern):
    """
    Compute the Longest Prefix Suffix (LPS) array for the KMP algorithm.

    Args:
        pattern: The pattern string to search for

    Returns:
        LPS array where lps[i] is the length of the longest proper prefix
        of pattern[0...i] which is also a suffix of pattern[0...i]
    """
    m = len(pattern)
    lps = [0] * m  # Initialize LPS array with zeros

    # TODO: Fill in the LPS array computation

    return lps

def kmp_search(text, pattern):
    """
    Implement the KMP algorithm to find all occurrences of pattern in text.

    Args:
        text: The text string to search in
        pattern: The pattern string to search for

    Returns:
        A list of indices where the pattern starts in the text
    """
    n = len(text)
    m = len(pattern)
    matches = []

    if m == 0:
        return matches

    # Compute the LPS array
    lps = compute_lps(pattern)

    # TODO: Implement the KMP search algorithm

    return matches
```

## Live Coding Group Activity

### Roles

- **Driver**: Types the code
- **Navigator**: Guides implementation strategy
- **Explainer**: Verbalizes the algorithm at each step

### Task

Working together, implement the KMP algorithm:

1. First, implement the LPS array computation function
2. Then, implement the main KMP search algorithm
3. Test the algorithm on multiple examples
4. Discuss how the LPS array helps avoid unnecessary comparisons

### Complete Implementation

```python
def compute_lps(pattern):
    """
    Compute the Longest Prefix Suffix (LPS) array for the KMP algorithm.
    """
    m = len(pattern)
    lps = [0] * m  # Initialize LPS array with zeros

    length = 0  # Length of the previous longest prefix suffix
    i = 1

    # Calculate lps[i] for i = 1 to m-1
    while i < m:
        if pattern[i] == pattern[length]:
            # If characters match, increment length and update LPS
            length += 1
            lps[i] = length
            i += 1
        else:
            # Characters don't match
            if length != 0:
                # Use the value of LPS for the previous character
                length = lps[length - 1]
            else:
                # No match found, set LPS to 0 and move to next character
                lps[i] = 0
                i += 1

    return lps

def kmp_search(text, pattern):
    """
    Implement the KMP algorithm to find all occurrences of pattern in text.
    """
    n = len(text)
    m = len(pattern)
    matches = []

    if m == 0:
        return matches

    # Compute the LPS array
    lps = compute_lps(pattern)
    print(f"LPS array for pattern '{pattern}': {lps}")

    i = 0  # Index for text
    j = 0  # Index for pattern

    while i < n:
        # Current characters match
        if pattern[j] == text[i]:
            i += 1
            j += 1

        # Found a complete match
        if j == m:
            matches.append(i - j)  # Record match position
            j = lps[j - 1]  # Look for the next match
        # Character mismatch after j matches
        elif i < n and pattern[j] != text[i]:
            if j != 0:
                j = lps[j - 1]  # Skip comparisons using LPS
            else:
                i += 1  # No match found, move to next character in text

    return matches

# Example usage
text = "ABABCABABD"
pattern = "ABABD"
matches = kmp_search(text, pattern)

print(f"Pattern '{pattern}' found at positions: {matches}")

# Additional example
text = "AAAAABAAAAABAAAAB"
pattern = "AAAAB"
matches = kmp_search(text, pattern)

print(f"Pattern '{pattern}' found at positions: {matches}")
```

## Peer Discussion Prompts

1. How does the KMP algorithm's time complexity compare to naive string matching?
2. What makes the LPS array so crucial to the efficiency of KMP?
3. Can you think of real-world applications where pattern matching is important?
4. How would you modify the algorithm to find the longest repeated substring?

## Checkpoint Questions

1. **Checkpoint 1**: What is the purpose of the LPS array in the KMP algorithm?
2. **Checkpoint 2**: For the pattern "ABABD", what is the value of LPS[3]?
3. **Checkpoint 3**: When a mismatch occurs, how does KMP determine where to continue matching?
4. **Checkpoint 4**: Why is the KMP algorithm especially efficient for patterns with repeated substrings?

## Time and Space Complexity Walkthrough

### Time Complexity

- **O(m + n)**: where m is the length of the pattern and n is the length of the text
  - Computing LPS array: O(m)
  - KMP search: O(n)
  - Each character in both strings is examined at most a constant number of times

### Space Complexity

- **O(m)**: for storing the LPS array
  - LPS array has the same length as the pattern

## Common Implementation Mistakes

1. **Incorrect LPS computation**: Not handling the case where length is non-zero correctly
2. **Improper mismatch handling**: Not setting j = lps[j-1] when there's a mismatch
3. **Index management**: Confusion about when to increment i and j
4. **Empty pattern handling**: Not handling empty patterns or texts
5. **Off-by-one errors**: Especially with empty strings or when recording match positions

## Mini-Challenge

1. Modify the algorithm to find the longest repeated substring in a text.
2. Implement a version that reports the number of partial matches before finding a complete match.
3. Extend the implementation to handle case-insensitive pattern matching.
4. Create a visualization of the KMP algorithm showing how the LPS array helps skip comparisons.

For the team: Compare the performance of KMP, Boyer-Moore, and Rabin-Karp algorithms on different text and pattern combinations.
