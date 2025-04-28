# Self-Reflective Agent: Learning From Experience

## Objective

Build an agent capable of evaluating its own performance, identifying strengths and weaknesses, and adjusting its behavior based on past experiences to improve future interactions.

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

## Core Concept: Self-Reflective Agents

Self-reflective agents elevate capabilities through:

1. **Performance monitoring** - Tracking the outcomes of actions
2. **Self-evaluation** - Assessing strengths and weaknesses
3. **Strategic adaptation** - Modifying behavior based on insights
4. **Continuous improvement** - Learning from past experiences

Key characteristics:

- **Introspective** - Analyzes own thought processes
- **Self-aware** - Recognizes limitations and capabilities
- **Adaptive** - Changes strategies based on feedback
- **Growth-oriented** - Improves over time through experience

### Reflection Types

- **Outcome reflection** - Evaluating success or failure
- **Process reflection** - Analyzing the approach taken
- **Strategy reflection** - Considering alternative approaches
- **Knowledge reflection** - Identifying gaps in information

### Real-world Examples

- AI systems that track error rates to improve
- Recommendation systems that learn from user feedback
- Self-improving game-playing agents
- Learning-based robotics systems

---

## Modular Code Implementation

### 1. Reflection Framework

```python
import os
import json
import time
from typing import Dict, List, Any, Optional
from enum import Enum
from dotenv import load_dotenv
from langchain.llms import OpenAI
from langchain.prompts import PromptTemplate

class ActionResult(Enum):
    """Possible outcomes of an agent's actions."""
    SUCCESS = "success"
    PARTIAL_SUCCESS = "partial_success"
    FAILURE = "failure"
    UNKNOWN = "unknown"

class ExperienceRecord:
    """A record of an action taken by the agent and its outcome."""

    def __init__(self,
                 task: str,
                 action: str,
                 result: ActionResult,
                 feedback: Optional[str] = None,
                 context: Optional[Dict] = None):
        """
        Initialize an experience record.

        Args:
            task: The task or goal the agent was trying to achieve
            action: The specific action taken by the agent
            result: The outcome of the action
            feedback: Optional feedback provided (by user or environment)
            context: Optional contextual information relevant to the experience
        """
        self.task = task
        self.action = action
        self.result = result
        self.feedback = feedback
        self.context = context or {}
        self.timestamp = time.time()
        self.reflection = None  # Will be populated after reflection

    def to_dict(self) -> Dict:
        """Convert the experience record to a dictionary."""
        return {
            "task": self.task,
            "action": self.action,
            "result": self.result.value,
            "feedback": self.feedback,
            "context": self.context,
            "timestamp": self.timestamp,
            "reflection": self.reflection
        }

    @classmethod
    def from_dict(cls, data: Dict) -> 'ExperienceRecord':
        """Create an experience record from a dictionary."""
        record = cls(
            task=data["task"],
            action=data["action"],
            result=ActionResult(data["result"]),
            feedback=data["feedback"],
            context=data["context"]
        )
        record.timestamp = data["timestamp"]
        record.reflection = data["reflection"]
        return record

    def __str__(self) -> str:
        """String representation of the experience record."""
        return f"Task: {self.task}\nAction: {self.action}\nResult: {self.result.value}\nFeedback: {self.feedback or 'None'}"

class ExperienceMemory:
    """A collection of experience records with retrieval capabilities."""

    def __init__(self, max_experiences: int = 100):
        """Initialize the experience memory with a maximum size."""
        self.experiences = []
        self.max_experiences = max_experiences

    def add(self, experience: ExperienceRecord):
        """Add an experience record to memory."""
        self.experiences.append(experience)
        # Trim if exceeding maximum size
        if len(self.experiences) > self.max_experiences:
            self.experiences = self.experiences[-self.max_experiences:]

    def get_all(self) -> List[ExperienceRecord]:
        """Get all experience records."""
        return self.experiences

    def get_by_result(self, result: ActionResult) -> List[ExperienceRecord]:
        """Get experiences with a specific result."""
        return [exp for exp in self.experiences if exp.result == result]

    def get_by_task_similarity(self, task: str, k: int = 5) -> List[ExperienceRecord]:
        """Get experiences with similar tasks (simplified implementation)."""
        # In a real implementation, this would use embeddings and semantic search
        # For this educational example, we'll use simple string matching
        # Sort experiences by the length of the longest common substring with the task
        def common_substring_length(s1: str, s2: str) -> int:
            # Simple implementation of longest common substring length
            s1, s2 = s1.lower(), s2.lower()
            m = [[0] * (len(s2) + 1) for _ in range(len(s1) + 1)]
            longest = 0
            for i in range(1, len(s1) + 1):
                for j in range(1, len(s2) + 1):
                    if s1[i-1] == s2[j-1]:
                        m[i][j] = m[i-1][j-1] + 1
                        longest = max(longest, m[i][j])
            return longest

        sorted_experiences = sorted(
            self.experiences,
            key=lambda exp: common_substring_length(exp.task, task),
            reverse=True
        )
        return sorted_experiences[:k]

    def save_to_file(self, filename: str):
        """Save experiences to a file."""
        with open(filename, 'w') as f:
            json.dump([exp.to_dict() for exp in self.experiences], f, indent=2)

    def load_from_file(self, filename: str):
        """Load experiences from a file."""
        if os.path.exists(filename):
            with open(filename, 'r') as f:
                data = json.load(f)
                self.experiences = [ExperienceRecord.from_dict(item) for item in data]
```

### 2. Reflection Engine

```python
class ReflectionEngine:
    """Engine for generating reflections on agent experiences."""

    def __init__(self, llm=None):
        """Initialize the reflection engine with an optional language model."""
        load_dotenv()
        self.llm = llm or OpenAI(temperature=0.7)

        # Define the reflection prompt template
        self.reflection_template = PromptTemplate(
            input_variables=["experience", "similar_experiences"],
            template="""
            You are a self-reflective AI analyzing your past performance to improve.

            Current experience to reflect on:
            {experience}

            Similar past experiences (if any):
            {similar_experiences}

            Please reflect on this experience by considering:
            1. What went well and why?
            2. What could have been improved and how?
            3. What patterns do you notice across similar experiences?
            4. What lessons can be applied to future situations?
            5. What specific changes in approach would lead to better outcomes?

            Provide a thoughtful, specific reflection that will help improve future performance.
            """
        )

        # Define the meta-reflection prompt template
        self.meta_reflection_template = PromptTemplate(
            input_variables=["reflections"],
            template="""
            You are a self-reflective AI analyzing multiple reflections to identify patterns
            and extract general principles for improvement.

            Recent reflections:
            {reflections}

            Based on these reflections, please:
            1. Identify recurring themes and patterns
            2. Extract 3-5 key principles or strategies for improvement
            3. Develop specific, actionable rules to guide future behavior
            4. Note any areas where your understanding seems incomplete

            Format your response as a structured summary of insights and action items.
            """
        )

    def reflect_on_experience(self,
                              experience: ExperienceRecord,
                              similar_experiences: List[ExperienceRecord] = None) -> str:
        """Generate a reflection on a single experience."""
        # Format the experience for the prompt
        experience_str = str(experience)

        # Format similar experiences if provided
        if similar_experiences and len(similar_experiences) > 0:
            similar_exp_str = "\n\n".join([str(exp) for exp in similar_experiences])
        else:
            similar_exp_str = "No similar past experiences."

        # Generate the reflection
        prompt = self.reflection_template.format(
            experience=experience_str,
            similar_experiences=similar_exp_str
        )

        reflection = self.llm.invoke(prompt)
        return reflection

    def meta_reflect(self, experiences: List[ExperienceRecord], k: int = 5) -> str:
        """Generate a meta-reflection across multiple experiences."""
        # Select the k most recent experiences with reflections
        recent_reflections = [exp for exp in experiences if exp.reflection is not None]
        recent_reflections = sorted(recent_reflections, key=lambda x: x.timestamp, reverse=True)[:k]

        if not recent_reflections:
            return "No reflections available for meta-reflection."

        # Format the reflections for the prompt
        reflections_str = "\n\n".join([
            f"Task: {exp.task}\nReflection: {exp.reflection}"
            for exp in recent_reflections
        ])

        # Generate the meta-reflection
        prompt = self.meta_reflection_template.format(reflections=reflections_str)
        meta_reflection = self.llm.invoke(prompt)

        return meta_reflection
```

### 3. Self-Reflective Agent

```python
class SelfReflectiveAgent:
    """An agent that learns from experience through reflection."""

    def __init__(self, llm=None):
        """Initialize the self-reflective agent with memory and reflection systems."""
        load_dotenv()
        self.llm = llm or OpenAI(temperature=0.7)
        self.experience_memory = ExperienceMemory()
        self.reflection_engine = ReflectionEngine(llm)
        self.strategies = []  # Learned strategies from meta-reflection

        # Define the decision-making prompt template
        self.decision_template = PromptTemplate(
            input_variables=["task", "context", "similar_experiences", "strategies"],
            template="""
            You are a self-reflective AI that learns from past experiences.

            Current task: {task}

            Context: {context}

            Relevant past experiences:
            {similar_experiences}

            Strategies learned from reflection:
            {strategies}

            Based on your past experiences and learned strategies, determine the best
            approach to complete this task. Explain your reasoning and how it incorporates
            lessons from previous experiences.

            Your response should be structured as follows:
            1. Analysis of the task
            2. Relevant past experiences and lessons
            3. Approach chosen with explanation
            4. Step-by-step plan
            """
        )

    def load_experiences(self, filename: str):
        """Load past experiences from a file."""
        self.experience_memory.load_from_file(filename)

    def save_experiences(self, filename: str):
        """Save experiences to a file."""
        self.experience_memory.save_to_file(filename)

    def _format_experiences(self, experiences: List[ExperienceRecord]) -> str:
        """Format a list of experiences for inclusion in prompts."""
        if not experiences:
            return "No relevant past experiences."

        return "\n\n".join([
            f"Task: {exp.task}\nAction: {exp.action}\nResult: {exp.result.value}\nReflection: {exp.reflection or 'None'}"
            for exp in experiences
        ])

    def decide_approach(self, task: str, context: str = "") -> str:
        """Decide on an approach for a given task based on past experiences."""
        # Retrieve similar past experiences
        similar_experiences = self.experience_memory.get_by_task_similarity(task)

        # Format the experiences and strategies for the prompt
        similar_exp_str = self._format_experiences(similar_experiences)
        strategies_str = "\n".join(self.strategies) if self.strategies else "No established strategies yet."

        # Generate the decision
        prompt = self.decision_template.format(
            task=task,
            context=context,
            similar_experiences=similar_exp_str,
            strategies=strategies_str
        )

        decision = self.llm.invoke(prompt)
        return decision

    def act(self, task: str, context: str = "") -> str:
        """Perform an action based on the decided approach."""
        # In a full implementation, this would execute the action
        # For this educational example, we'll simply return the planned approach
        return self.decide_approach(task, context)

    def record_experience(self,
                         task: str,
                         action: str,
                         result: ActionResult,
                         feedback: Optional[str] = None,
                         context: Optional[Dict] = None):
        """Record an experience and generate a reflection on it."""
        # Create and store the experience
        experience = ExperienceRecord(task, action, result, feedback, context)

        # Retrieve similar past experiences for context in reflection
        similar_experiences = self.experience_memory.get_by_task_similarity(task)

        # Generate a reflection
        reflection = self.reflection_engine.reflect_on_experience(experience, similar_experiences)
        experience.reflection = reflection

        # Store the experience with its reflection
        self.experience_memory.add(experience)

        # If we have enough experiences, perform a meta-reflection
        if len(self.experience_memory.experiences) >= 5:
            meta_reflection = self.reflection_engine.meta_reflect(self.experience_memory.experiences)

            # Extract strategies from meta-reflection (in a real implementation,
            # this would use more sophisticated parsing)
            strategies = meta_reflection.split("\n")
            strategies = [s for s in strategies if s.strip().startswith("- ") or s.strip().startswith("* ")]

            if strategies:
                self.strategies = strategies

        return reflection
```

### 4. Test Harness

```python
def test_self_reflective_agent():
    """Test the self-reflective agent with a series of experiences and tasks."""
    agent = SelfReflectiveAgent()

    # First, let's create some simulated past experiences
    print("=== Creating Simulated Past Experiences ===\n")

    # Experience 1: A successful explanation
    agent.record_experience(
        task="Explain the concept of machine learning to a beginner",
        action="Provided an analogy comparing machine learning to how humans learn from experience",
        result=ActionResult.SUCCESS,
        feedback="The explanation was clear and relatable. The analogy really helped."
    )

    # Experience 2: A partial success
    agent.record_experience(
        task="Explain the difference between supervised and unsupervised learning",
        action="Gave technical definitions with mathematical formulations",
        result=ActionResult.PARTIAL_SUCCESS,
        feedback="The explanation was accurate but too technical for the audience."
    )

    # Experience 3: A failure
    agent.record_experience(
        task="Explain neural networks to a non-technical manager",
        action="Used detailed technical descriptions of neuron activations and backpropagation",
        result=ActionResult.FAILURE,
        feedback="The explanation was too complex and used too much jargon."
    )

    # Experience 4: Another success with a different approach
    agent.record_experience(
        task="Explain the concept of overfitting to a data scientist",
        action="Used technical language with relevant examples from real-world projects",
        result=ActionResult.SUCCESS,
        feedback="The explanation was appropriately technical and the examples were helpful."
    )

    # Now, let's test the agent on a new task
    print("\n=== Testing Agent on New Task ===\n")

    new_task = "Explain how GPT models work to a high school student"
    print(f"Task: {new_task}\n")

    # Get the agent's approach
    approach = agent.decide_approach(new_task)
    print("Agent's Planned Approach:")
    print(approach)

    # Simulate the execution of this approach
    # In a real application, this would be an actual implementation
    simulated_result = ActionResult.SUCCESS
    simulated_feedback = "The student understood the concept well and could explain it back in their own words."

    # Record this new experience
    print("\n=== Recording New Experience and Reflection ===\n")
    reflection = agent.record_experience(
        task=new_task,
        action=approach,
        result=simulated_result,
        feedback=simulated_feedback
    )

    print("Agent's Reflection:")
    print(reflection)

    # Display learned strategies
    print("\n=== Agent's Learned Strategies ===\n")
    if agent.strategies:
        for i, strategy in enumerate(agent.strategies, 1):
            print(f"{i}. {strategy}")
    else:
        print("No strategies learned yet.")

    # Test on another related but different task
    print("\n=== Testing Agent on Another Task ===\n")

    another_task = "Explain the ethical implications of AI to a group of policy makers"
    print(f"Task: {another_task}\n")

    # Get the agent's approach for this new task
    approach = agent.decide_approach(another_task)
    print("Agent's Planned Approach:")
    print(approach)

if __name__ == "__main__":
    test_self_reflective_agent()
```

---

## Live Coding Collaboration

### Session Structure (60-75 minutes)

1. **Setup & Framework Design** (15 min)

   - Configure environment
   - Understand the reflection architecture
   - Plan implementation approach

2. **Implementation Phase** (25 min)

   - Complete the agent's reflection capabilities
   - Enhance the decision-making system
   - Implement the experience recording and retrieval

3. **Testing & Analysis** (20 min)

   - Test with varied scenarios
   - Analyze reflection quality
   - Evaluate how past experiences influence decisions

4. **Extension & Refinement** (15 min)
   - Implement improved experience retrieval
   - Add more sophisticated reflection analysis
   - Create specialized reflection patterns for different tasks

---

## Reflection Checkpoints

### Checkpoint 1: After Framework Implementation

- How does the reflection system capture different aspects of performance?
- What types of experiences provide the most valuable learning?
- How should reflections be structured to maximize their utility?
- What patterns might emerge from different types of experiences?

### Checkpoint 2: After Agent Testing

- How do reflections change as the agent accumulates more experiences?
- When does the agent effectively incorporate past lessons?
- How does reflection quality impact future decision-making?
- What meta-patterns emerge from reflecting on reflections?

---

## Debugging and Extension Challenges

### Common Issues

- Overgeneralizing from limited experiences
- Underfitting (not learning from experiences)
- Reflection paradox (overthinking vs. acting)
- Context blindness in applying past lessons
- Strategy conflicts between different task types

### Extension Ideas

1. **Emotion-Informed Reflection**

   - Add emotional assessment to experiences
   - Implement reflection on emotional responses
   - Create affect-aware decision making

2. **Counterfactual Reflection**

   - Add "what if" analysis to reflections
   - Implement alternative scenario exploration
   - Create decision trees from counterfactuals

3. **Collaborative Reflection**

   - Share reflections across multiple agents
   - Implement peer feedback on reflections
   - Create consensus-building for strategies

4. **User-Guided Reflection**
   - Add user feedback on reflections
   - Implement adjustable reflection parameters
   - Create personalized reflection styles

---

## Agent Evaluation

### Effectiveness Metrics

- Improvement rate over time
- Strategy quality and applicability
- Adaptation to new situations
- Learning transfer across domains

### Efficiency Metrics

- Reflection relevance to future tasks
- Strategy extraction accuracy
- Experience retrieval precision
- Reflection generation time

### Alignment Metrics

- Self-awareness of limitations
- Appropriate confidence calibration
- Ethical considerations in reflections
- Value alignment in strategy formation

### Evaluation Exercise

Design a sequence of related tasks that test the agent's ability to:

1. Learn from initial mistakes
2. Apply lessons to similar situations
3. Transfer insights to new domains
4. Integrate potentially conflicting feedback
5. Develop nuanced strategies over time

---

## Discussion Questions

1. How does self-reflection in AI agents compare to human metacognition?
2. What biases might affect an agent's reflection process?
3. How might reflection capabilities enhance or hinder agent transparency?
4. What are the ethical implications of agents that continuously self-improve?
5. How might self-reflection impact an agent's explainability?
6. What role should human feedback play in agent reflection?
7. How would you balance reflection time vs. action time in resource-constrained environments?

---

## Next Steps

- Explore how combining reflection with planning can create more strategic agents
- Investigate how reflection can enhance multi-agent collaboration
- Consider how tool-using agents can reflect on tool efficacy
