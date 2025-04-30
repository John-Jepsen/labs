# Multi-Agent Collaboration: Coordinated Problem Solving

## Objective

Build a system of multiple specialized agents that coordinate to solve complex problems, demonstrating how different agent roles, communication protocols, and collaborative frameworks can enhance collective intelligence.

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
pip install openai langchain pydantic python-dotenv asyncio

# Create a .env file for your API key (if not already done)
echo "OPENAI_API_KEY=your_api_key_here" > .env
```

### Verifying Setup

Run the following test code to ensure your environment is ready:

```python
import os
import asyncio
from dotenv import load_dotenv
from langchain.llms import OpenAI

# Load environment variables
load_dotenv()

# Test API connection
llm = OpenAI(temperature=0)
result = llm.invoke("Hello!")
print(f"LLM Response: {result}")

# Test async capabilities
async def test_async():
    return "Async is working!"

result = asyncio.run(test_async())
print(result)
```

---

## Core Concept: Multi-Agent Collaboration

Multi-agent systems enhance problem-solving through:

1. **Role specialization** - Agents focus on specific aspects of a problem
2. **Parallel processing** - Multiple agents work simultaneously
3. **Diverse perspectives** - Different agents approach problems differently
4. **Collective intelligence** - The system outperforms any individual agent

Key characteristics:

- **Coordinated** - Agents work together toward common goals
- **Communicative** - Information flows between agents
- **Modular** - Specialized agents can be added or removed
- **Scalable** - Systems can grow as problems become more complex

### Collaboration Patterns

- **Manager-Worker** - Central agent delegates tasks to specialists
- **Peer-to-Peer** - Agents communicate directly with each other
- **Blackboard** - Agents share information through a common workspace
- **Market-based** - Agents bid for tasks based on capabilities

### Real-world Examples

- Distributed task management systems
- Multi-disciplinary research teams
- Supply chain optimization
- Emergency response coordination

---

## Modular Code Implementation

### 1. Agent Base Class and Message System

```python
import os
import json
import uuid
import asyncio
from typing import Dict, List, Any, Optional, Callable
from enum import Enum
from dotenv import load_dotenv
from langchain.llms import OpenAI
from langchain.prompts import PromptTemplate

class MessageType(Enum):
    """Types of messages that can be exchanged between agents."""
    TASK = "task"
    RESULT = "result"
    QUESTION = "question"
    ANSWER = "answer"
    STATUS = "status"
    FINAL = "final"

class Message:
    """A message that can be sent between agents."""

    def __init__(self, sender: str, receiver: str, content: Any, msg_type: MessageType):
        """Initialize a message with sender, receiver, content, and type."""
        self.id = str(uuid.uuid4())
        self.sender = sender
        self.receiver = receiver
        self.content = content
        self.type = msg_type
        self.timestamp = asyncio.get_event_loop().time()

    def to_dict(self) -> Dict:
        """Convert the message to a dictionary."""
        return {
            "id": self.id,
            "sender": self.sender,
            "receiver": self.receiver,
            "content": self.content,
            "type": self.type.value,
            "timestamp": self.timestamp
        }

    @classmethod
    def from_dict(cls, data: Dict) -> 'Message':
        """Create a message from a dictionary."""
        return cls(
            sender=data["sender"],
            receiver=data["receiver"],
            content=data["content"],
            msg_type=MessageType(data["type"])
        )

class Blackboard:
    """A shared workspace where agents can read and write information."""

    def __init__(self):
        """Initialize an empty blackboard."""
        self.data = {}
        self.message_history = []
        self._subscribers = {}

    def write(self, key: str, value: Any, author: str):
        """Write data to the blackboard."""
        self.data[key] = {"value": value, "author": author, "timestamp": asyncio.get_event_loop().time()}

    def read(self, key: str) -> Optional[Any]:
        """Read data from the blackboard."""
        entry = self.data.get(key)
        return entry["value"] if entry else None

    def get_all(self) -> Dict:
        """Get all data from the blackboard."""
        return {k: v["value"] for k, v in self.data.items()}

    def post_message(self, message: Message):
        """Post a message to the blackboard and notify subscribers."""
        self.message_history.append(message)

        # Notify subscribers interested in this message
        receiver = message.receiver
        if receiver in self._subscribers:
            for callback in self._subscribers[receiver]:
                asyncio.create_task(callback(message))

        # Also notify subscribers interested in ALL messages
        if "*" in self._subscribers:
            for callback in self._subscribers["*"]:
                asyncio.create_task(callback(message))

    def subscribe(self, agent_id: str, callback: Callable):
        """Subscribe to messages for a specific agent or all messages."""
        if agent_id not in self._subscribers:
            self._subscribers[agent_id] = []
        self._subscribers[agent_id].append(callback)

    def get_messages(self, agent_id: str = None, msg_type: MessageType = None) -> List[Message]:
        """Get messages filtered by agent ID and/or message type."""
        filtered = self.message_history

        if agent_id:
            filtered = [msg for msg in filtered if msg.receiver == agent_id or msg.sender == agent_id]

        if msg_type:
            filtered = [msg for msg in filtered if msg.type == msg_type]

        return filtered

class Agent:
    """Base class for all agents in the multi-agent system."""

    def __init__(self, agent_id: str, name: str, role: str, blackboard: Blackboard, llm=None):
        """Initialize an agent with ID, name, role, and a reference to the shared blackboard."""
        load_dotenv()
        self.agent_id = agent_id
        self.name = name
        self.role = role
        self.blackboard = blackboard
        self.llm = llm or OpenAI(temperature=0.7)
        self.prompt_template = self._create_prompt_template()

        # Subscribe to messages addressed to this agent
        self.blackboard.subscribe(self.agent_id, self.handle_message)

    def _create_prompt_template(self) -> PromptTemplate:
        """Create the prompt template for this agent. Override in subclasses."""
        return PromptTemplate(
            input_variables=["input", "role", "context"],
            template="""
            You are a {role}.

            Context information:
            {context}

            Please process the following input:
            {input}
            """
        )

    async def process(self, input_text: str, context: str = "") -> str:
        """Process input and generate a response based on the agent's role."""
        prompt = self.prompt_template.format(
            input=input_text,
            role=self.role,
            context=context
        )

        response = self.llm.invoke(prompt)
        return response

    async def handle_message(self, message: Message):
        """Handle a message sent to this agent. Override in subclasses."""
        if message.receiver != self.agent_id and message.receiver != "*":
            return  # Message not for this agent

        print(f"{self.name} received message: {message.type.value} from {message.sender}")

        # Default implementation just processes the message content
        if message.type == MessageType.TASK:
            result = await self.process(message.content)

            # Send back a result message
            response = Message(
                sender=self.agent_id,
                receiver=message.sender,
                content=result,
                msg_type=MessageType.RESULT
            )
            self.blackboard.post_message(response)

    def send_message(self, receiver: str, content: Any, msg_type: MessageType):
        """Send a message to another agent via the blackboard."""
        message = Message(
            sender=self.agent_id,
            receiver=receiver,
            content=content,
            msg_type=msg_type
        )
        self.blackboard.post_message(message)
```

### 2. Specialized Agents

```python
class Manager(Agent):
    """Manager agent that coordinates the work of other agents."""

    def __init__(self, agent_id: str, name: str, blackboard: Blackboard, worker_ids: List[str], llm=None):
        """Initialize a manager agent with a list of worker agent IDs."""
        super().__init__(agent_id, name, "Task Manager", blackboard, llm)
        self.worker_ids = worker_ids
        self.pending_tasks = {}
        self.results = {}

    def _create_prompt_template(self) -> PromptTemplate:
        """Create the prompt template for the manager agent."""
        return PromptTemplate(
            input_variables=["input", "workers", "context"],
            template="""
            You are a Task Manager who coordinates multiple specialist workers.

            Your available workers are:
            {workers}

            Context information:
            {context}

            Based on this complex task, break it down into smaller tasks that can be assigned
            to your workers. Consider their specialties when making assignments.

            Task: {input}

            Respond with a JSON object containing task assignments:
            {{
                "analysis": "Your analysis of the overall task",
                "tasks": [
                    {{
                        "worker_id": "ID of the worker",
                        "task_description": "Detailed description of the subtask"
                    }}
                ]
            }}
            """
        )

    async def process_task(self, task: str, context: str = ""):
        """Process a complex task by breaking it down and assigning subtasks to workers."""
        # Format worker information for the prompt
        worker_info = "\n".join([f"- Worker ID: {worker_id}" for worker_id in self.worker_ids])

        # Generate the task breakdown
        prompt = self.prompt_template.format(
            input=task,
            workers=worker_info,
            context=context
        )

        response = self.llm.invoke(prompt)

        # Parse the response and assign tasks
        try:
            task_data = json.loads(response)
            print(f"Task Analysis: {task_data['analysis']}")

            for subtask in task_data["tasks"]:
                worker_id = subtask["worker_id"]
                task_description = subtask["task_description"]

                # Track the pending task
                task_id = str(uuid.uuid4())
                self.pending_tasks[task_id] = {
                    "worker_id": worker_id,
                    "description": task_description,
                    "status": "assigned"
                }

                # Assign the task to the worker
                self.send_message(
                    receiver=worker_id,
                    content={"task_id": task_id, "description": task_description},
                    msg_type=MessageType.TASK
                )

                print(f"Assigned task to {worker_id}: {task_description}")

            # Wait for all tasks to complete
            while any(task["status"] == "assigned" for task in self.pending_tasks.values()):
                await asyncio.sleep(0.1)

            # Compile the results
            final_result = await self.compile_results(task, task_data["analysis"])
            return final_result

        except json.JSONDecodeError:
            return "Error: Failed to parse task assignments."

    async def handle_message(self, message: Message):
        """Handle messages sent to the manager agent."""
        if message.type == MessageType.RESULT:
            # Handle task results from workers
            result = message.content
            task_id = result.get("task_id")

            if task_id in self.pending_tasks:
                self.pending_tasks[task_id]["status"] = "completed"
                self.results[task_id] = result.get("result")
                print(f"Received result from {message.sender} for task {task_id}")

    async def compile_results(self, original_task: str, analysis: str) -> str:
        """Compile all worker results into a final answer."""
        # Create a prompt to compile the results
        results_str = "\n".join([
            f"Task: {self.pending_tasks[task_id]['description']}\nResult: {result}"
            for task_id, result in self.results.items()
        ])

        compile_prompt = f"""
        You are a Task Manager compiling results from multiple workers.

        Original Task: {original_task}

        Task Analysis: {analysis}

        Worker Results:
        {results_str}

        Please compile these results into a comprehensive final answer that addresses the original task.
        Ensure your response is well-structured and integrates all the information provided by the workers.
        """

        final_result = self.llm.invoke(compile_prompt)
        return final_result

class Researcher(Agent):
    """Agent specialized in gathering and analyzing information."""

    def __init__(self, agent_id: str, name: str, blackboard: Blackboard, llm=None):
        """Initialize a researcher agent."""
        super().__init__(agent_id, name, "Research Specialist", blackboard, llm)

    def _create_prompt_template(self) -> PromptTemplate:
        """Create the prompt template for the researcher agent."""
        return PromptTemplate(
            input_variables=["input", "context"],
            template="""
            You are a Research Specialist who excels at gathering and analyzing information.

            Context information:
            {context}

            Research task: {input}

            Conduct thorough research on this topic. Focus on:
            1. Key facts and data
            2. Different perspectives or approaches
            3. Relevant background information
            4. Recent developments or trends

            Provide a comprehensive research report with properly organized findings.
            """
        )

    async def handle_message(self, message: Message):
        """Handle messages sent to the researcher agent."""
        if message.type == MessageType.TASK:
            task_data = message.content
            task_id = task_data["task_id"]
            description = task_data["description"]

            print(f"{self.name} researching: {description}")
            result = await self.process(description)

            # Send the result back
            self.send_message(
                receiver=message.sender,
                content={"task_id": task_id, "result": result},
                msg_type=MessageType.RESULT
            )

class Critic(Agent):
    """Agent specialized in critically evaluating ideas and identifying issues."""

    def __init__(self, agent_id: str, name: str, blackboard: Blackboard, llm=None):
        """Initialize a critic agent."""
        super().__init__(agent_id, name, "Critical Evaluator", blackboard, llm)

    def _create_prompt_template(self) -> PromptTemplate:
        """Create the prompt template for the critic agent."""
        return PromptTemplate(
            input_variables=["input", "context"],
            template="""
            You are a Critical Evaluator who specializes in analyzing ideas for flaws, limitations,
            and potential improvements.

            Context information:
            {context}

            Content to evaluate: {input}

            Critically evaluate this content. Focus on:
            1. Logical inconsistencies or fallacies
            2. Factual inaccuracies or missing context
            3. Potential biases or assumptions
            4. Limitations of the approach
            5. Areas for improvement or alternative perspectives

            Provide a balanced critique that acknowledges strengths while identifying areas for improvement.
            """
        )

    async def handle_message(self, message: Message):
        """Handle messages sent to the critic agent."""
        if message.type == MessageType.TASK:
            task_data = message.content
            task_id = task_data["task_id"]
            description = task_data["description"]

            print(f"{self.name} evaluating: {description}")
            result = await self.process(description)

            # Send the result back
            self.send_message(
                receiver=message.sender,
                content={"task_id": task_id, "result": result},
                msg_type=MessageType.RESULT
            )

# YOUR GROUP TASK: Implement at least one additional specialized agent
# Examples: Creative Agent, Data Analyst, Writer, Planner, etc.
```

### 3. Multi-Agent System

```python
class MultiAgentSystem:
    """A system that coordinates multiple agents to solve complex problems."""

    def __init__(self):
        """Initialize the multi-agent system with a shared blackboard."""
        self.blackboard = Blackboard()
        self.agents = {}

    def add_agent(self, agent: Agent):
        """Add an agent to the system."""
        self.agents[agent.agent_id] = agent
        print(f"Added agent: {agent.name} ({agent.role})")

    async def run(self, task: str) -> str:
        """Run the multi-agent system on a given task."""
        # Find the manager agent
        manager = next((agent for agent in self.agents.values() if isinstance(agent, Manager)), None)

        if not manager:
            return "Error: No manager agent found in the system."

        # Process the task
        result = await manager.process_task(task)
        return result

async def main():
    """Set up and run a multi-agent system."""
    # Create the multi-agent system
    system = MultiAgentSystem()

    # Create and add agents
    blackboard = system.blackboard

    # Add specialized worker agents
    researcher = Researcher("researcher_1", "Alex (Researcher)", blackboard)
    critic = Critic("critic_1", "Taylor (Critic)", blackboard)
    # Add your custom agent here

    # Add all worker agents to the system
    system.add_agent(researcher)
    system.add_agent(critic)
    # Add your custom agent to the system

    # Add manager agent (should be added last)
    manager = Manager(
        "manager",
        "Sam (Manager)",
        blackboard,
        worker_ids=["researcher_1", "critic_1"]  # Add your custom agent ID here
    )
    system.add_agent(manager)

    # Run the system on a task
    task = "Evaluate the potential impacts of artificial general intelligence on society in the next decade."
    print(f"\nProcessing task: {task}\n")

    result = await system.run(task)
    print("\nFinal Result:")
    print(result)

if __name__ == "__main__":
    asyncio.run(main())
```

---

## Live Coding Collaboration

### Session Structure (60-75 minutes)

1. **Setup & System Design** (15 min)

   - Configure environment
   - Understand agent roles and communication system
   - Plan custom agent implementation

2. **Implementation Phase** (25 min)

   - Complete the collaborative framework
   - Implement a specialized agent
   - Enhance the manager agent's task assignment logic

3. **Testing & Analysis** (20 min)

   - Test with complex problems requiring multiple agents
   - Analyze communication patterns
   - Evaluate collaboration effectiveness

4. **Extension & Refinement** (15 min)
   - Implement an improved task allocation strategy
   - Add more sophisticated agent coordination mechanisms
   - Create specialized roles for specific problem domains

---

## Reflection Checkpoints

### Checkpoint 1: After Agent Implementation

- How does each agent's specialization contribute to the overall system?
- What communication patterns are most effective for different tasks?
- What types of tasks benefit most from multi-agent collaboration?
- How might the system's performance change as more agents are added?

### Checkpoint 2: After System Testing

- How does the system handle task decomposition and reintegration?
- Where do agents collaborate effectively or struggle to coordinate?
- How does information flow through the system?
- What emergent behaviors do you observe in the multi-agent system?

---

## Debugging and Extension Challenges

### Common Issues

- Task allocation imbalances
- Communication bottlenecks
- Result integration challenges
- Circular dependencies between agents
- Response time variations

### Extension Ideas

1. **Dynamic Task Allocation**

   - Implement agent capability advertising
   - Create a bidding system for task assignment
   - Add priority-based task scheduling

2. **Advanced Communication Protocols**

   - Implement request-response patterns
   - Add broadcast messaging capabilities
   - Create agent status notification system

3. **Conflict Resolution**

   - Add voting mechanisms for decision making
   - Implement negotiation protocols
   - Create a consensus-building process

4. **System Monitoring**
   - Add performance metrics tracking
   - Create a visualization of agent interactions
   - Implement system health monitoring

---

## Agent Evaluation

### Effectiveness Metrics

- Task completion quality
- Appropriate specialization usage
- Knowledge integration
- Problem-solving versatility

### Efficiency Metrics

- End-to-end processing time
- Inter-agent communication volume
- Agent utilization balance
- Resource consumption

### Alignment Metrics

- Consistent goal pursuit across agents
- Coordination effectiveness
- System robustness to failures
- Appropriate task decomposition

### Evaluation Exercise

Design a complex problem that requires diverse expertise and analyze:

1. How the manager decomposes the problem
2. The quality of specialized contributions
3. The effectiveness of information sharing
4. The coherence of the final integrated solution

---

## Discussion Questions

1. How does multi-agent collaboration mirror human team dynamics?
2. What cognitive biases might affect different specialized agents?
3. How might you implement learning across the multi-agent system?
4. What are the tradeoffs between centralized vs. decentralized coordination?
5. How would you design the system to handle disagreements between agents?
6. What ethical considerations arise when multiple agents collaborate?

---

## Next Steps

- Explore how self-reflection can improve agent performance within a team
- Investigate how agent teams can learn from past collaborations
- Consider how multi-agent systems can adapt their structure to different problems
