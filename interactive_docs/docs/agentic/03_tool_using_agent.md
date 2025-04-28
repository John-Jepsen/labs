# Tool-Using Agent: Extending Agent Capabilities

## Objective

Build an agent that can use external tools like web search and calculators to solve problems beyond its built-in knowledge, demonstrating how agent capabilities can be extended through tool integration.

---

## Environment Setup

| Setup Steps                                                        |
| :----------------------------------------------------------------- |
| 1. Create and activate a virtual environment (if not already done) |
| 2. Install dependencies (including tool-specific ones)             |
| 3. Configure API access                                            |

```bash
# Create a virtual environment (if not already created)
python -m venv agentic-env
source agentic-env/bin/activate  # On Windows: agentic-env\Scripts\activate

# Install dependencies
pip install openai langchain pydantic python-dotenv wikipedia duckduckgo-search

# Create a .env file for your API key (if not already done)
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

## Core Concept: Tool-Using Agents

Tool-using agents expand capabilities through:

1. **Tool recognition** - Identifying when external tools are needed
2. **Tool selection** - Choosing the appropriate tool for a task
3. **Tool usage** - Properly formatting inputs and interpreting outputs
4. **Result integration** - Incorporating tool outputs into responses

Key characteristics:

- **Extensible** - Can be enhanced with new tools
- **Specialized** - Uses purpose-built tools for specific tasks
- **Resource-aware** - Leverages external systems for efficiency
- **Capability-augmented** - Overcomes built-in limitations

### Real-world Examples

- Virtual assistants using weather APIs
- Programming assistants using code execution
- Research agents using search engines
- Math tutors using calculation tools

---

## Modular Code Implementation

### 1. Tool Definitions

```python
import os
import math
import json
from typing import Dict, List, Any, Optional, Callable
from dotenv import load_dotenv
from langchain.llms import OpenAI
from langchain.prompts import PromptTemplate

# Tool definition (simplified for learning purposes)
class Tool:
    """A tool that can be used by an agent to perform specific tasks."""

    def __init__(self, name: str, description: str, func: Callable):
        """Initialize a tool with a name, description, and function."""
        self.name = name
        self.description = description
        self.func = func

    def use(self, input_str: str) -> str:
        """Use the tool with the given input."""
        return self.func(input_str)

# Example tool implementations
def calculator_tool(expression: str) -> str:
    """Evaluates a mathematical expression and returns the result."""
    try:
        # Using eval is generally unsafe, but this is a simplified example
        # In production, use a proper mathematical expression parser
        cleaned_expression = expression.strip()
        result = eval(cleaned_expression, {"__builtins__": {}}, {"math": math})
        return f"Calculator result: {result}"
    except Exception as e:
        return f"Calculator error: {str(e)}"

def wikipedia_search(query: str) -> str:
    """Searches Wikipedia for information about the query."""
    try:
        import wikipedia
        # Search for the query
        search_results = wikipedia.search(query, results=3)
        if not search_results:
            return "No Wikipedia results found."

        # Get the summary of the first result
        try:
            page = wikipedia.page(search_results[0])
            summary = wikipedia.summary(search_results[0], sentences=3)
            return f"Wikipedia: {summary}\nURL: {page.url}"
        except wikipedia.DisambiguationError as e:
            # If there's a disambiguation, get the first option
            try:
                page = wikipedia.page(e.options[0])
                summary = wikipedia.summary(e.options[0], sentences=3)
                return f"Wikipedia: {summary}\nURL: {page.url}"
            except:
                return f"Multiple Wikipedia matches found: {', '.join(e.options[:5])}"
    except Exception as e:
        return f"Wikipedia search error: {str(e)}"

# YOUR GROUP TASK: Implement at least one additional tool
# Examples: Weather API, Unit converter, News search, etc.
```

### 2. Agent Design

````python
class ToolUsingAgent:
    """An agent that can use tools to complete tasks."""

    def __init__(self, llm=None):
        """Initialize the tool-using agent with an optional language model."""
        load_dotenv()
        self.llm = llm or OpenAI(temperature=0.7)

        # Initialize tools
        self.tools = [
            Tool("calculator", "Use this tool to perform mathematical calculations", calculator_tool),
            Tool("wikipedia", "Use this tool to search for information on Wikipedia", wikipedia_search),
            # Add your custom tool here
        ]

        # Build tool descriptions for the prompt
        self.tool_descriptions = "\n".join([
            f"- {tool.name}: {tool.description}" for tool in self.tools
        ])

        # Map tool names to tools for easy lookup
        self.tool_map = {tool.name: tool for tool in self.tools}

        # Define the main prompt template
        self.prompt_template = PromptTemplate(
            input_variables=["input", "tools", "tool_history"],
            template="""
            You are a helpful assistant that can use tools to answer questions.

            Available tools:
            {tools}

            Previous tool usage (if any):
            {tool_history}

            To use a tool, respond with a JSON object with the following structure:
            {{
                "reasoning": "Your step-by-step reasoning about what tool to use and why",
                "tool": "the_tool_name",
                "tool_input": "the input to pass to the tool"
            }}

            If you can answer without using a tool, or after you've received a tool result,
            respond with a JSON object with this structure:
            {{
                "reasoning": "Your reasoning process",
                "final_answer": "Your comprehensive answer to the question"
            }}

            User question: {input}

            Think carefully about whether you need to use a tool and which one is most appropriate.
            """
        )

    def _parse_response(self, response: str) -> Dict:
        """Parse the JSON response from the LLM."""
        # YOUR CODE: Parse the JSON response
        try:
            # Try to parse the entire response as JSON
            return json.loads(response)
        except json.JSONDecodeError:
            # If that fails, try to extract JSON from the response
            import re
            json_match = re.search(r'```json\n(.*?)```', response, re.DOTALL)
            if json_match:
                try:
                    return json.loads(json_match.group(1))
                except:
                    # If JSON extraction fails, create a default response
                    return {
                        "reasoning": "Failed to parse response",
                        "final_answer": "I encountered an error while processing your request."
                    }
            else:
                # If no JSON found, create a default response
                return {
                    "reasoning": "Failed to parse response",
                    "final_answer": "I encountered an error while processing your request."
                }

    def run(self, input_text: str, max_turns: int = 3) -> str:
        """Process the input using tools as needed and generate a response."""
        # YOUR CODE: Implement the main agent loop
        tool_history = []
        turns = 0

        while turns < max_turns:
            # Format the prompt with the input and tool history
            prompt = self.prompt_template.format(
                input=input_text,
                tools=self.tool_descriptions,
                tool_history="\n".join(tool_history) if tool_history else "None"
            )

            # Get a response from the LLM
            response = self.llm.invoke(prompt)
            parsed_response = self._parse_response(response)

            # Check if the response contains a final answer
            if "final_answer" in parsed_response:
                return parsed_response["final_answer"]

            # Check if the response requests a tool
            if "tool" in parsed_response and "tool_input" in parsed_response:
                tool_name = parsed_response["tool"]
                tool_input = parsed_response["tool_input"]

                # Check if the requested tool exists
                if tool_name in self.tool_map:
                    # Use the tool
                    tool_result = self.tool_map[tool_name].use(tool_input)

                    # Add the tool usage to the history
                    tool_history.append(f"Tool: {tool_name}\nInput: {tool_input}\nResult: {tool_result}")

                    # Continue to the next turn
                    turns += 1
                    continue
                else:
                    # If the tool doesn't exist, add an error to the history
                    tool_history.append(f"Error: Tool '{tool_name}' not found")
            else:
                # If the response is invalid, add an error to the history
                tool_history.append("Error: Invalid response format")

            turns += 1

        # If we've reached the maximum number of turns without a final answer
        return "I wasn't able to provide a complete answer within the allowed number of tool uses."
````

### 3. Test Harness

```python
def test_tool_using_agent():
    """Test the tool-using agent with various queries and analyze its tool usage."""
    agent = ToolUsingAgent()

    test_queries = [
        "What is the square root of 144?",
        "Tell me about the Python programming language.",
        "If I have 5 apples and give away 2, then buy 3 more, how many do I have?",
        # Add your own test queries here
    ]

    print("=== Testing Tool-Using Agent ===")

    for query in test_queries:
        print(f"\nQuery: {query}")
        result = agent.run(query)
        print(f"Response: {result}")

        # GROUP DISCUSSION POINT: Did the agent select the appropriate tool?
        # Could it have answered without using a tool? Was the tool usage effective?

if __name__ == "__main__":
    test_tool_using_agent()
```

---

## Live Coding Collaboration

### Session Structure (60-75 minutes)

1. **Setup & Tool Exploration** (15 min)

   - Configure environment
   - Understand available tools
   - Plan custom tool implementation

2. **Implementation Phase** (25 min)

   - Complete the agent's response parsing and run methods
   - Implement at least one custom tool
   - Enhance the main agent prompt

3. **Testing & Analysis** (20 min)

   - Test with varied queries requiring different tools
   - Analyze tool selection accuracy
   - Evaluate response quality with and without tools

4. **Extension & Refinement** (15 min)
   - Implement more sophisticated tool selection logic
   - Add error handling for tool failures
   - Create specialized prompts for tool result processing

---

## Reflection Checkpoints

### Checkpoint 1: After Tool Implementation

- How does each tool extend the agent's capabilities?
- What are the limitations of each tool?
- How might users misuse or misunderstand the tools?
- What additional tools would be most valuable to add?

### Checkpoint 2: After Agent Testing

- When does the agent correctly identify the need for a tool?
- When does it fail to use a tool when one would be helpful?
- How well does it interpret and incorporate tool outputs?
- What patterns emerge in successful vs. unsuccessful tool usage?

---

## Debugging and Extension Challenges

### Common Issues

- JSON parsing errors from malformed LLM responses
- Tool selection errors (choosing wrong tool or using unnecessarily)
- Tool input formatting problems
- Failure to properly incorporate tool results into final answers

### Extension Ideas

1. **Tool Chain Execution**

   - Allow the agent to use multiple tools in sequence
   - Implement a framework for passing outputs between tools

2. **Dynamic Tool Discovery**

   - Add the ability to register new tools at runtime
   - Implement a tool recommendation system

3. **Tool Result Validation**

   - Add checks to validate tool outputs
   - Implement fallback strategies for tool failures

4. **Specialized Tool Prompt**
   - Create custom prompts for specific tools
   - Optimize tool result interpretation

---

## Agent Evaluation

### Effectiveness Metrics

- Appropriate tool selection rate
- Tool usage success rate
- Answer accuracy with tools vs. without
- Problem-solving versatility

### Efficiency Metrics

- Number of tool calls needed
- Response time (including tool execution)
- Token usage
- Success rate within turn limit

### Alignment Metrics

- Transparency about tool usage
- Accurate representation of tool capabilities
- Proper attribution of information sources
- Error handling quality

### Evaluation Exercise

Create a test suite of 5-10 queries specifically designed to test boundary cases in tool selection. Record when the agent:

1. Uses a tool correctly when needed
2. Correctly answers without tools when possible
3. Uses a tool unnecessarily
4. Fails to use a tool when one would help

---

## Discussion Questions

1. How does tool usage parallel how humans use external resources?
2. What cognitive biases might affect an agent's tool selection?
3. How might you design an agent that learns which tools are most effective for different tasks?
4. What are the ethical considerations when using external tools (e.g., search, calculators)?
5. How would you design a system for the agent to request new tools when needed?
6. What are the security implications of allowing agents to use external tools?

---

## Next Steps

- Explore how memory can help agents learn which tools work best
- Investigate how planning can improve multi-step tool usage
- Consider how multiple agents might share and coordinate tool usage
