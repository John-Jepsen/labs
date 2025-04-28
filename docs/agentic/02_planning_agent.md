# Planning Agent: Goal-Directed Problem Solving

## Objective

Build a planning agent that breaks down complex tasks into manageable steps, demonstrates goal-setting capabilities, and creates/executes multi-step plans to achieve specified objectives.

---

## Environment Setup

| Setup Steps                                                        |
| :----------------------------------------------------------------- |
| 1. Create and activate a virtual environment (if not already done) |
| 2. Install dependencies                                            |
| 3. Configure API access                                            |

```bash
# Create a virtual environment (if not already created)
python -m venv agentic-env
source agentic-env/bin/activate  # On Windows: agentic-env\Scripts\activate

# Install dependencies
pip install openai langchain pydantic python-dotenv

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

## Core Concept: Planning Agents

Planning agents elevate agent capabilities by:

1. **Defining goals** based on user requests
2. **Breaking down** complex tasks into sequential steps
3. **Executing** each step in a logical order
4. **Adjusting** the plan as needed based on intermediate results

Key characteristics:

- **Goal-oriented** - Actions drive toward specific objectives
- **Sequential** - Follows step-by-step reasoning
- **Structured** - Organizes thoughts and actions before execution
- **Flexible** - Can replan when obstacles arise

### Real-world Examples

- GPS navigation systems
- Manufacturing robots
- Personal assistants
- Chess-playing algorithms

---

## Modular Code Implementation

### 1. Agent Design

````python
import os
from typing import Dict, List, Any
from dotenv import load_dotenv
from langchain.llms import OpenAI
from langchain.prompts import PromptTemplate
import json

class PlanningAgent:
    """A planning agent that breaks down complex tasks into steps and executes them."""

    def __init__(self, llm=None):
        """Initialize the planning agent with an optional language model."""
        load_dotenv()
        self.llm = llm or OpenAI(temperature=0.7)

        # Define the planning prompt template
        self.planning_template = PromptTemplate(
            input_variables=["goal"],
            template="""
            You are a helpful assistant that creates detailed plans.

            Goal: {goal}

            First, analyze this goal. Then, create a step-by-step plan to accomplish it.
            Format your response as a JSON object with:
            1. "analysis": Brief analysis of the goal
            2. "steps": Array of sequential steps, each with "step_number", "description", and "success_criteria"

            Your plan should be detailed, logical, and achievable.
            """
        )

        # Define the execution prompt template
        self.execution_template = PromptTemplate(
            input_variables=["goal", "plan", "current_step", "progress_so_far"],
            template="""
            You are executing a step in a plan.

            Goal: {goal}
            Overall Plan: {plan}
            Current Step: {current_step}
            Progress So Far: {progress_so_far}

            Execute this step and provide the result. Be thorough and detailed in your execution.
            """
        )

    def create_plan(self, goal: str) -> Dict:
        """Generate a structured plan for achieving the specified goal."""
        # YOUR CODE: Generate a plan using the planning template
        prompt = self.planning_template.format(goal=goal)
        response = self.llm.invoke(prompt)

        # Parse the JSON response
        try:
            plan = json.loads(response)
        except json.JSONDecodeError:
            # If the response is not valid JSON, try to extract it
            import re
            json_match = re.search(r'```json\n(.*?)```', response, re.DOTALL)
            if json_match:
                try:
                    plan = json.loads(json_match.group(1))
                except:
                    plan = {"analysis": "Failed to parse plan", "steps": []}
            else:
                plan = {"analysis": "Failed to parse plan", "steps": []}

        return plan

    def execute_step(self, goal: str, plan: Dict, step_index: int, progress_so_far: List[str]) -> str:
        """Execute a specific step in the plan and return the result."""
        # YOUR CODE: Execute the step using the execution template
        if step_index >= len(plan["steps"]):
            return "Error: Step index out of range"

        current_step = plan["steps"][step_index]
        prompt = self.execution_template.format(
            goal=goal,
            plan=str(plan["steps"]),
            current_step=str(current_step),
            progress_so_far="\n".join(progress_so_far)
        )

        result = self.llm.invoke(prompt)
        return result

    def execute_plan(self, goal: str) -> List[str]:
        """Create and execute a complete plan for the given goal."""
        # YOUR CODE: Implement complete plan execution
        plan = self.create_plan(goal)
        progress = []

        print(f"Goal: {goal}")
        print(f"Analysis: {plan['analysis']}")
        print("\nPlan:")
        for i, step in enumerate(plan["steps"]):
            print(f"Step {step['step_number']}: {step['description']}")

        print("\nExecution:")
        for i in range(len(plan["steps"])):
            print(f"\nExecuting Step {i+1}...")
            result = self.execute_step(goal, plan, i, progress)
            progress.append(f"Step {i+1} Result: {result}")
            print(result)

        return progress
````

### 2. Collaborative Coding Task

Each team member should take on one of the following roles:

- **Planner**: Completes the `create_plan` method and improves the planning prompt
- **Executor**: Completes the `execute_step` and `execute_plan` methods
- **Evaluator**: Creates test cases and evaluates plan quality and execution

### 3. Test Harness

```python
# YOUR GROUP TASK: Build a test harness to interact with your planning agent

def test_planning_agent():
    """Test the planning agent with various goals and analyze its plans and execution."""
    agent = PlanningAgent()

    test_goals = [
        "Write a blog post about artificial intelligence",
        "Plan a birthday party for a 10-year-old",
        "Research and compare three different smartphones",
    ]

    print("=== Testing Planning Agent ===")

    # Test detailed planning
    goal = test_goals[0]
    plan = agent.create_plan(goal)

    print(f"\nGoal: {goal}")
    print(f"Analysis: {plan['analysis']}")
    print("\nGenerated Plan:")
    for step in plan["steps"]:
        print(f"Step {step['step_number']}: {step['description']}")
        print(f"  Success Criteria: {step['success_criteria']}")

    # GROUP ACTIVITY: Select one of the goals and execute the full plan
    # execute_goal = test_goals[1]
    # progress = agent.execute_plan(execute_goal)

    # GROUP DISCUSSION POINT: Assess plan quality, execution effectiveness,
    # and how well the planning stage accounted for potential issues

if __name__ == "__main__":
    test_planning_agent()
```

---

## Live Coding Collaboration

### Session Structure (60-75 minutes)

1. **Setup & Planning** (15 min)

   - Configure environment
   - Review code structure
   - Understand the planning architecture

2. **Implementation Phase** (25 min)

   - Complete the agent's planning and execution methods
   - Enhance the planning and execution prompts
   - Implement test scenarios

3. **Testing & Execution** (20 min)

   - Test with varied goals
   - Analyze plan quality
   - Execute plans and observe results

4. **Analysis & Extension** (15 min)
   - Identify strengths and weaknesses
   - Implement one extension challenge

---

## Reflection Checkpoints

### Checkpoint 1: After Plan Generation

- How comprehensive is the generated plan?
- Are the steps logically ordered?
- Does the plan account for potential challenges?
- How could the planning prompt be improved?

### Checkpoint 2: After Plan Execution

- Did execution follow the plan faithfully?
- What unexpected issues arose during execution?
- How might the agent better handle unexpected outcomes?
- Was the plan sufficiently detailed for effective execution?

---

## Debugging and Extension Challenges

### Common Issues

- JSON parsing errors from malformed LLM output
- Plans that are too abstract or vague
- Plans with missing prerequisite steps
- Execution failures due to incomplete context

### Extension Ideas

1. **Dynamic Replanning**

   - Add logic to detect execution failures
   - Implement replanning when steps fail
   - Track overall goal progress

2. **Step Dependencies**

   - Enhance the plan structure to include step dependencies
   - Implement parallel execution for independent steps
   - Add resource tracking for steps

3. **User Feedback Loop**
   - Add checkpoints for user feedback
   - Incorporate user preferences into planning
   - Allow plan modification based on feedback

---

## Agent Evaluation

### Effectiveness Metrics

- Plan completeness
- Logical step ordering
- Success criteria clarity
- Task completion rate

### Efficiency Metrics

- Planning time
- Execution time
- Token usage
- Number of steps generated

### Alignment Metrics

- Adherence to user constraints
- Ethical consideration of actions
- Transparency of reasoning

### Evaluation Exercise

Select one complex goal and have each team member independently evaluate the agent's plan using the metrics above. Compare results and discuss differences in assessment.

---

## Discussion Questions

1. How does planning improve agent performance compared to reactive agents?
2. What cognitive limitations of humans does a planning agent help overcome?
3. When might a simple reactive agent outperform a planning agent?
4. How might you implement "common sense" checking in a planning agent?
5. What information should be tracked between planning and execution stages?
6. How would you redesign this agent to better handle uncertainty?

---

## Next Steps

- Explore how adding tools can extend the agent's capabilities
- Investigate how to combine planning with memory
- Consider how multiple planning agents might collaborate
