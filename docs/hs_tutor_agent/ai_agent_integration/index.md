# AI Agent Integration

This section explores how the AI tutoring system works behind the scenes, including the agent architecture, prompt engineering, memory systems, and feedback mechanisms.

## Overview

The AI Tutor platform uses a multi-agent system powered by Large Language Models (LLMs) to provide personalized coding instruction. The system is designed to:

1. Understand student questions and code
2. Provide contextually relevant guidance
3. Adapt to different learning paces
4. Offer targeted feedback and encouragement
5. Maintain conversational context over time

## Key Components

- [Agent Architecture](agent_architecture.md): How different agents collaborate
- [Prompt Engineering](prompt_engineering.md): Crafting effective agent instructions
- [Memory Systems](memory_systems.md): Maintaining context across conversations
- [Routing Logic](routing_logic.md): Directing questions to specialized agents
- [Feedback Mechanisms](feedback_mechanisms.md): Providing helpful corrections
- [Code Evaluation](code_evaluation.md): Assessing student code submissions
- [Personalization](personalization.md): Adapting to individual learning styles

## Agent Types

The platform employs several specialized agents:

| Agent Type            | Purpose                    | Capabilities                              |
| --------------------- | -------------------------- | ----------------------------------------- |
| **Coding Tutor**      | Main instructional agent   | Explains concepts, guides problem-solving |
| **Hint Provider**     | Offers graduated hints     | Provides escalating levels of assistance  |
| **Code Reviewer**     | Evaluates code submissions | Identifies bugs, suggests improvements    |
| **Encourager**        | Provides motivation        | Celebrates progress, maintains engagement |
| **Concept Explainer** | Clarifies core concepts    | Breaks down complex topics with analogies |

## Conversation Flow

When a student interacts with the AI Tutor, the following process occurs:

1. **Input Processing**: The student's message is analyzed to determine intent
2. **Router Selection**: The message is directed to the appropriate specialized agent
3. **Context Gathering**: Relevant context from memory is retrieved
4. **Response Generation**: The agent generates a response using the LLM
5. **Memory Update**: The conversation is stored for future context
6. **Response Delivery**: The formatted response is sent to the student

![Agent Conversation Flow](../assets/agent_flow.png)

## LLM Integration

The platform supports multiple LLM backends:

- OpenAI GPT models (default)
- Local LLM options (e.g., Llama, Mistral)
- Custom fine-tuned models
- Hybrid approaches

## Implementation Examples

### Basic Agent Definition

```python
from langchain.chat_models import ChatOpenAI
from langchain.schema import SystemMessage, HumanMessage

class CodingTutorAgent:
    def __init__(self, model_name="gpt-3.5-turbo"):
        self.llm = ChatOpenAI(model_name=model_name)
        self.system_prompt = SystemMessage(
            content="You are a patient, encouraging coding tutor for high school students."
                   "Explain concepts simply, provide examples, and guide without giving answers."
        )

    async def respond(self, message, conversation_history=None):
        history = conversation_history or []
        messages = [self.system_prompt] + history + [HumanMessage(content=message)]
        response = await self.llm.agenerate([messages])
        return response.generations[0][0].text
```

### Router Chain Example

```python
from langchain.chains import LLMChain
from langchain.prompts import PromptTemplate

class AgentRouter:
    def __init__(self, llm):
        self.llm = llm
        self.router_template = PromptTemplate(
            input_variables=["message"],
            template="""
            Given the student's message, determine which specialized agent should handle it:

            1. Coding Tutor: General coding help and instruction
            2. Hint Provider: When the student is stuck and needs a hint
            3. Code Reviewer: When the student wants code review
            4. Encourager: When the student seems frustrated or needs motivation
            5. Concept Explainer: When the student asks about a specific concept

            Student message: {message}

            Agent to use (respond with just the number):
            """
        )
        self.router_chain = LLMChain(llm=self.llm, prompt=self.router_template)

    async def route(self, message):
        result = await self.router_chain.arun(message=message)
        agent_mapping = {
            "1": self.coding_tutor,
            "2": self.hint_provider,
            "3": self.code_reviewer,
            "4": self.encourager,
            "5": self.concept_explainer
        }
        selected_agent = agent_mapping.get(result.strip(), self.coding_tutor)
        return selected_agent
```

## Next Steps

To learn more about specific components of the AI Tutor system:

- [Explore Agent Architecture](agent_architecture.md)
- [Study Prompt Engineering](prompt_engineering.md)
- [Understand Memory Systems](memory_systems.md)
- [Configure the AI Tutor](configuration.md)
