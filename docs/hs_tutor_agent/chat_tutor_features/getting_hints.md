# Getting Personalized Hints

Learning to code involves encountering challenges and working through them. The AI tutor provides a scaffolded hint system that helps you make progress without giving away complete solutions.

## How Hints Work

The AI tutor provides hints in progressive levels of detail:

1. **Level 1 - General Guidance**: Points you in the right direction without specific code
2. **Level 2 - Conceptual Reminder**: Refreshes relevant concepts and approaches
3. **Level 3 - Targeted Hint**: Identifies the specific issue in your code or thinking
4. **Level 4 - Code Framework**: Provides partial code structure while leaving key implementation to you

This progressive approach helps you develop problem-solving skills while still providing support when needed.

## Asking for Hints

There are several ways to request hints:

### Using the Hint Command

The simplest way is to use the `/hint` command:

```
/hint
```

This will provide a hint based on your current assignment and progress. For more specific help, you can add details:

```
/hint creating a loop to process the list
```

### Natural Language Requests

You can also ask for hints conversationally:

- "I'm stuck on this problem"
- "Can you give me a hint about how to start?"
- "I don't understand how to calculate the average"
- "I need help with the for loop"

### Sharing Your Current Code

For the most effective hints, share your current code attempt:

````
I'm trying to calculate the average of these numbers but I'm getting an error:

```python
numbers = [10, 20, 30, 40, 50]
total = 0
for num in numbers:
    total += number
average = total / len(numbers)
print(average)
````

```

This allows the AI tutor to identify specific issues and provide targeted guidance.

## Example Hint Progression

Here's an example of how the hint system progresses as you ask for more help:

### Problem: Create a function to find the largest number in a list

**Initial Request**:
```

I need a hint for finding the largest number in a list.

```

**Level 1 Hint** (General guidance):
```

Think about how you would find the largest number manually.
You'd need to look at each number and keep track of the largest
one you've seen so far. How might you translate this approach into code?

```

**Follow-up Request**:
```

I still don't know how to start coding this.

```

**Level 2 Hint** (Conceptual reminder):
```

This problem requires iteration and comparison. You'll need:

1. A loop to examine each number in the list
2. A variable to keep track of the largest number so far
3. Comparison logic to update your variable when you find a larger number

Do you remember which Python loop structures allow you to iterate through a list?

```

**Follow-up Request**:
```

I'm still confused about how to track the largest number.

```

**Level 3 Hint** (Targeted hint):
```

Start by assuming the first number in the list is the largest.
Then iterate through the rest of the list, comparing each number
with your current "largest" value. If you find a bigger number,
update your "largest" variable.

The comparison would look something like:
if current_number > largest_so_far:
largest_so_far = current_number

```

**Final Request**:
```

Can you show me more of the structure?

````

**Level 4 Hint** (Code framework):
```python
def find_largest(numbers):
    # Start by assuming the first number is the largest
    # (What should happen if the list is empty?)
    if not numbers:
        return None

    largest_so_far = numbers[0]

    # Check each number in the list
    for number in numbers:
        # Compare with current largest
        # Update largest_so_far if needed
        # Your code here

    # Return the largest number
    return largest_so_far
````

## Best Practices for Using Hints

### 1. Try Before Asking

Make a genuine attempt to solve the problem before asking for a hint. This helps you develop problem-solving skills and makes the hints more valuable.

### 2. Be Specific About Your Confusion

Instead of saying "I don't get it," explain what specific part is confusing you:

✅ "I understand I need to use a loop, but I'm not sure how to keep track of the count."  
❌ "This is too hard, I need help."

### 3. Share Your Current Approach

Always share your current code or thinking process:

✅ "Here's my attempt so far... I'm stuck on how to update the total."  
❌ "I need a hint for the averaging problem."

### 4. Reflect After Each Hint

After receiving a hint, take time to think about it before asking for more help:

✅ "I see, so I need to initialize a variable first. Let me try that..."  
❌ "I still don't get it, just tell me the answer."

### 5. Progress Through Hint Levels Gradually

Start with general hints and only ask for more specific guidance if you're still stuck:

✅ "Thanks for that hint. I tried implementing it but I'm getting an error about..."  
❌ "Just show me the code to solve this."

## When to Move Past Hints

Sometimes, after multiple hint attempts, you might need to move on. Consider:

1. **Requesting a walkthrough**: "Can you walk me through this step by step?"
2. **Asking for a simplified version**: "Can we solve a simpler version of this problem first?"
3. **Looking for alternative approaches**: "Is there another way to think about this problem?"

## Teacher Settings for Hints

Teachers can customize the hint system for their classroom:

- **Hint depth limit**: Restricting the level of detail in hints
- **Hint frequency**: Limiting the number of hints per assignment
- **Required attempt time**: Setting a minimum time students must spend before hints are available
- **Custom hint messages**: Adding specific guidance for common issues

To access these settings, teachers can use the classroom management dashboard.

## Next Steps

- [Learn about submitting code for review](code_review.md)
- [Explore special commands](special_commands.md)
- [Understand best practices for AI learning](best_practices.md)
