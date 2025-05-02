# Chat Tutor Features

This section explains how students interact with the AI tutor through the chat interface, including how to ask for help, get feedback, and engage in guided learning conversations.

## Chat Interface Overview

The chat interface is the primary way students communicate with their AI tutor. It provides:

- Real-time guidance and feedback
- Code evaluation and suggestions
- Concept explanations tailored to the student's level
- Progress tracking and next step recommendations

![Chat Interface Overview](../assets/chat_interface.png)

## Key Features

### Asking for Help

Students can ask for assistance in various ways:

- **Direct questions**: "How do I create a variable in Python?"
- **Code troubleshooting**: "Why am I getting this error?"
- **Conceptual clarification**: "Can you explain loops again?"
- **Next step guidance**: "What should I do next in this project?"

The AI tutor will respond with appropriately scaffolded guidance based on the student's progress level and the specific context of their question.

### Getting Hints

When stuck on a specific problem, students can request graduated hints:

1. **Ask for a hint**: Type "I need a hint" or "Help me with this problem"
2. **Specify the problem**: Describe which part you're struggling with
3. **Receive tiered assistance**: The AI provides increasingly specific guidance

Example hint progression:

````
Student: I need a hint for calculating the average.

AI Tutor: Think about what mathematical operation you use to find an average.
What values do you need to track?

Student: I still don't understand.

AI Tutor: To find an average, you need to sum all values and divide by the
count of values. What variables would you need to keep track of these?

Student: I'm still confused about the code.

AI Tutor: Here's a skeleton to help you get started:
```python
total = 0
count = 0
# For each number in your data:
#   Add the number to total
#   Increment count by 1
# Calculate average = total / count
````

### Code Feedback

Students can submit code for review by:

1. Pasting code directly in the chat
2. Clicking the "Review my code" button
3. Uploading a code file

The AI tutor evaluates the code and provides feedback on:

- **Syntax errors**: Identifying and explaining coding mistakes
- **Logical errors**: Highlighting issues in the solution approach
- **Style improvements**: Suggesting better coding practices
- **Conceptual understanding**: Reinforcing programming concepts

### Guided Exploration

The tutor supports self-directed learning through:

- **"Tell me more" requests**: Expanding on concepts students find interesting
- **"Why" questions**: Explaining the reasoning behind coding practices
- **Alternative approaches**: Showing different ways to solve the same problem
- **Real-world applications**: Connecting concepts to practical uses

### Reflection Prompts

The AI tutor encourages metacognition through reflection questions:

- "What was challenging about this exercise?"
- "How would you explain this concept to a friend?"
- "How might you use this technique in your project?"
- "What connections do you see to previous concepts we've covered?"

## Using Code Execution

Students can run and test code directly in the chat:

1. Write or paste code in the chat
2. Click the "Run Code" button or type "/run"
3. View the execution output in the chat
4. Get AI analysis of the results

The execution environment supports:

- Python (including visualization libraries)
- JavaScript/HTML/CSS (with preview)
- Simple database operations

## Progress Tracking

The chat interface provides progress indicators:

- **Concept mastery**: Visual indicators of concept understanding
- **Project completion**: Percentage of current project completed
- **Skill development**: Growth in specific programming skills
- **Learning path**: Position in the overall learning sequence

## Special Commands

Students can use special commands in the chat:

| Command     | Description                   | Example                     |
| ----------- | ----------------------------- | --------------------------- |
| `/help`     | List available commands       | `/help`                     |
| `/run`      | Execute code snippet          | `/run print("Hello world")` |
| `/debug`    | Debug code with explanation   | `/debug my_buggy_code.py`   |
| `/explain`  | Get detailed explanation      | `/explain for loops`        |
| `/hint`     | Request a hint                | `/hint`                     |
| `/next`     | Get next steps                | `/next`                     |
| `/glossary` | Define a term                 | `/glossary variable`        |
| `/save`     | Save chat for later reference | `/save`                     |

## Personalization Features

The AI tutor adapts to student preferences:

- **Explanation style**: Adjusts detail level and examples
- **Feedback approach**: Varies directness vs. guidance
- **Learning pace**: Adapts to faster or slower progression
- **Interest areas**: Incorporates student's programming interests

## Teacher Integration

Instructors can interact with the AI tutor system to:

- Review student conversations
- Add specific guidance notes for the AI
- Set learning goals and restrictions
- Override AI behavior for specific students

## Learn More

- [Asking Effective Questions](asking_questions.md)
- [Submitting Code for Review](code_review.md)
- [Getting Personalized Hints](getting_hints.md)
- [Using Special Commands](special_commands.md)
- [Best Practices for AI Learning](best_practices.md)
