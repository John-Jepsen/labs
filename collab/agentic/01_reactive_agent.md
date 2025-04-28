# Reactive Agent: Learning Through Implementation

## Objective

Build a reactive agent that responds directly to inputs without maintaining internal state, demonstrating the most basic agent architecture and its limitations.

---

## Environment Setup

| Setup Steps                                  |
| :------------------------------------------- |
| 1. Create and activate a virtual environment |
| 2. Install dependencies                      |
| 3. Configure API access                      |

```bash
# Create a virtual environment
python -m venv agentic-env
source agentic-env/bin/activate  # On Windows: agentic-env\Scripts\activate

# Install dependencies
pip install openai langchain pydantic python-dotenv

# Create a .env file for your API key
echo "OPENAI_API_KEY=your_api_key_here" > .env
```

### Verifying Setup

Run the following test code to ensure your environment is ready:

```python
import os
from dotenv import load_dotenv
from langchain.llms import OpenAI

# Load environment variables
load_dotenv()

# Test API connection
llm = OpenAI(temperature=0)
result = llm.invoke("Hello!")
print(f"LLM Response: {result}")
```

---

## Core Concept: Reactive Agents

Reactive agents follow the simplest agent paradigm:

1. **Perceive** the current environment through inputs
2. **Select an action** based on a predefined set of rules
3. **Execute** the action
4. Repeat for each new input

Key characteristics:

- **Stateless** - No memory of past interactions
- **Immediate response** - Direct mapping from input to output
- **No planning** - Cannot reason about future states
- **Rule-based** - Follow predetermined action patterns

### Real-world Examples

- Thermostats
- Basic chatbots
- Robotic vacuum cleaners (basic models)
- Traffic lights

---

## Modular Code Implementation

### 1. Agent Design

```python
import os
from typing import Dict, List, Any
from dotenv import load_dotenv
from langchain.llms import OpenAI
from langchain.prompts import PromptTemplate

class ReactiveAgent:
    """A simple reactive agent that responds to inputs without maintaining state."""

    def __init__(self, llm=None):
        """Initialize the reactive agent with an optional language model."""
        # YOUR CODE: Initialize the agent with a language model
        # If no model is provided, create a default one
        load_dotenv()
        self.llm = llm or OpenAI(temperature=0.7)

        # Define the basic prompt template for the agent
        self.prompt_template = PromptTemplate(
            input_variables=["input"],
            template="""
            You are a helpful assistant that responds directly to user requests.
            Respond to the following input:

            User Input: {input}

            Your response:
            """
        )

    def act(self, input_text: str) -> str:
        """Process the input and generate a response based on current input only."""
        # YOUR CODE: Generate a response using the prompt template and LLM
        prompt = self.prompt_template.format(input=input_text)
        response = self.llm.invoke(prompt)
        return response
```

### 2. Collaborative Coding Task

Each team member should take on one of the following roles:

- **Implementer**: Completes the code above and handles execution
- **Analyst**: Documents limitations and strengths observed during testing
- **Verifier**: Creates test cases and validates agent responses

### 3. Test Harness

```python
# YOUR GROUP TASK: Build a test harness to interact with your agent

def test_reactive_agent():
    """Test the reactive agent with various inputs and analyze responses."""
    agent = ReactiveAgent()

    test_inputs = [
        "What is the capital of France?",
        "I just told you I'm planning a trip to Paris.",
        "Could you recommend some attractions there?",
    ]

    print("=== Testing Reactive Agent ===")
    for input_text in test_inputs:
        print(f"\nInput: {input_text}")
        response = agent.act(input_text)
        print(f"Response: {response}")

        # GROUP DISCUSSION POINT: Does the agent remember previous context?
        # How does this affect the quality of responses?

if __name__ == "__main__":
    test_reactive_agent()
```

---

## Live Coding Collaboration

### Session Structure (45-60 minutes)

1. **Setup & Walkthrough** (10 min)

   - Configure environment
   - Review code structure

2. **Implementation Phase** (20 min)

   - Complete the `ReactiveAgent` class
   - Build the test harness
   - Create additional test scenarios

3. **Testing & Analysis** (15 min)

   - Run tests with varied inputs
   - Document observations
   - Identify strengths and limitations

4. **Extension** (15 min)
   - Implement one of the extension challenges

---

## Reflection Checkpoints

### Checkpoint 1: After Initial Testing

- What patterns do you notice in the agent's responses?
- When does the agent perform well? When does it struggle?
- How does the lack of memory impact user experience?

### Checkpoint 2: After Code Completion

- How would you rate the agent's:
  - Responsiveness
  - Accuracy
  - Consistency
  - Helpfulness
- What key limitations have you observed?

---

## Debugging and Extension Challenges

### Common Issues

- API rate limiting or authentication errors
- Prompt engineering challenges
- Response parsing issues

### Extension Ideas

1. **Rule-Based Enhancement**

   - Add explicit rules for common queries
   - Implement a "fallback" mechanism for unknown inputs

2. **Response Templating**

   - Create category-specific response templates
   - Implement basic output formatting

3. **Simple Classifier**
   - Add a pre-processing step that categorizes inputs
   - Customize prompts based on input category

---

## Agent Evaluation

### Effectiveness Metrics

- Response relevance to input
- Information accuracy
- Task completion rate

### Efficiency Metrics

- Response time
- Token usage
- Error rate

### Alignment Metrics

- Adherence to user instructions
- Tone consistency
- Safety and ethical considerations

### Evaluation Exercise

Create a standardized set of 5-10 inputs that cover different scenarios and evaluate your agent across all metrics above. Record your findings in a shared document.

---

## Discussion Questions

1. How does a reactive agent differ from how humans process information?
2. What types of applications are well-suited for reactive agents?
3. What is the fundamental limitation of reactive agents?
4. How might adding simple rules improve performance without adding memory?
5. If you were to redesign this agent, what would be your first improvement?

---

## Next Steps

- Explore how adding memory can enhance agent capabilities
- Investigate planning mechanisms for multi-step reasoning
- Consider how to maintain agent focus across complex interactions
