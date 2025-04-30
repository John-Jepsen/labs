# Memory-Augmented Agent: Persistent Context Across Interactions

## Objective

Build an agent capable of maintaining context across multiple interactions by implementing memory systems, allowing for more natural conversations and incremental problem solving.

---

## Environment Setup

| Setup Steps                                                        |
| :----------------------------------------------------------------- |
| 1. Create and activate a virtual environment (if not already done) |
| 2. Install dependencies (including memory-specific ones)           |
| 3. Configure API access                                            |

```bash
# Create a virtual environment (if not already created)
python -m venv agentic-env
source agentic-env/bin/activate  # On Windows: agentic-env\Scripts\activate

# Install dependencies
pip install openai langchain pydantic python-dotenv chromadb tiktoken

# Create a .env file for your API key (if not already done)
echo "OPENAI_API_KEY=your_api_key_here" > .env
```

### Verifying Setup

Run the following test code to ensure your environment is ready:

```python
import os
from dotenv import load_dotenv
from langchain.llms import OpenAI
from langchain.embeddings import OpenAIEmbeddings
import chromadb

# Load environment variables
load_dotenv()

# Test API connection
llm = OpenAI(temperature=0)
result = llm.invoke("Hello!")
print(f"LLM Response: {result}")

# Test embedding creation
embeddings = OpenAIEmbeddings()
test_embedding = embeddings.embed_query("Test query")
print(f"Embedding length: {len(test_embedding)}")

# Test ChromaDB
client = chromadb.Client()
print("ChromaDB connection successful")
```

---

## Core Concept: Memory-Augmented Agents

Memory-augmented agents enhance capabilities through:

1. **Short-term memory** - Maintaining immediate context within a conversation
2. **Long-term memory** - Storing and retrieving information across separate sessions
3. **Memory organization** - Structuring knowledge for efficient retrieval
4. **Memory-aware reasoning** - Using past experiences to inform decisions

Key characteristics:

- **Contextual** - Maintains conversational thread across turns
- **Personalized** - Remembers user preferences and past interactions
- **Progressive** - Builds knowledge over time
- **Adaptive** - Learns from past experiences

### Memory Types

- **Conversation memory** - Recent dialogue history
- **Vector memory** - Semantic similarity-based retrieval
- **Episodic memory** - Time-ordered sequence of interactions
- **Declarative memory** - Explicit facts and information

### Real-world Examples

- Personal assistants remembering preferences
- Customer service bots with conversation history
- Educational tutors tracking student progress
- Smart home systems adapting to user habits

---

## Modular Code Implementation

### 1. Memory Systems

```python
import os
import json
import time
from typing import Dict, List, Any, Optional
from dotenv import load_dotenv
from langchain.llms import OpenAI
from langchain.prompts import PromptTemplate
from langchain.embeddings import OpenAIEmbeddings
import chromadb

class ConversationMemory:
    """Simple buffer-based memory that stores recent conversation turns."""

    def __init__(self, max_turns: int = 10):
        """Initialize the conversation memory with a maximum number of turns."""
        self.max_turns = max_turns
        self.buffer = []

    def add(self, role: str, content: str):
        """Add a new message to the conversation memory."""
        self.buffer.append({"role": role, "content": content, "timestamp": time.time()})
        # Trim the buffer if it exceeds the maximum size
        if len(self.buffer) > self.max_turns:
            self.buffer = self.buffer[-self.max_turns:]

    def get(self) -> List[Dict]:
        """Get the conversation history."""
        return self.buffer

    def get_formatted(self) -> str:
        """Get the conversation history as a formatted string."""
        formatted = []
        for message in self.buffer:
            formatted.append(f"{message['role'].capitalize()}: {message['content']}")
        return "\n".join(formatted)

    def clear(self):
        """Clear the conversation memory."""
        self.buffer = []

class VectorMemory:
    """Vector-based memory system for semantic retrieval of information."""

    def __init__(self, collection_name: str = "agent_memory"):
        """Initialize the vector memory with a collection name."""
        load_dotenv()
        self.embeddings = OpenAIEmbeddings()
        self.client = chromadb.Client()

        # Create or get the collection
        try:
            self.collection = self.client.get_or_create_collection(collection_name)
        except:
            # If the collection exists but with different settings, recreate it
            self.client.delete_collection(collection_name)
            self.collection = self.client.create_collection(collection_name)

        self.next_id = 1

    def add(self, content: str, metadata: Optional[Dict] = None):
        """Add a new memory to the vector store."""
        if metadata is None:
            metadata = {}

        # Add timestamp if not present
        if "timestamp" not in metadata:
            metadata["timestamp"] = time.time()

        # Convert content to embedding and store
        embedding = self.embeddings.embed_query(content)

        # Store in ChromaDB
        self.collection.add(
            ids=[f"mem_{self.next_id}"],
            embeddings=[embedding],
            documents=[content],
            metadatas=[metadata]
        )

        self.next_id += 1

    def retrieve(self, query: str, k: int = 3) -> List[Dict]:
        """Retrieve the k most relevant memories for a given query."""
        # Convert query to embedding
        query_embedding = self.embeddings.embed_query(query)

        # Query the collection
        results = self.collection.query(
            query_embeddings=[query_embedding],
            n_results=k
        )

        # Format the results
        memories = []
        if results["documents"] and len(results["documents"]) > 0:
            for i, doc in enumerate(results["documents"][0]):
                memories.append({
                    "content": doc,
                    "metadata": results["metadatas"][0][i] if i < len(results["metadatas"][0]) else {}
                })

        return memories

    def clear(self):
        """Clear all memories from the collection."""
        self.collection.delete(ids=self.collection.get()["ids"])
        self.next_id = 1

# YOUR GROUP TASK: Implement at least one additional memory system
# Examples: Structured knowledge base, Episodic memory, etc.
```

### 2. Agent Design

```python
class MemoryAugmentedAgent:
    """An agent that uses memory systems to maintain context across interactions."""

    def __init__(self, llm=None):
        """Initialize the memory-augmented agent with memory systems and an optional language model."""
        load_dotenv()
        self.llm = llm or OpenAI(temperature=0.7)

        # Initialize memory systems
        self.conversation_memory = ConversationMemory()
        self.vector_memory = VectorMemory()

        # Define the prompt template
        self.prompt_template = PromptTemplate(
            input_variables=["input", "conversation_history", "relevant_memories"],
            template="""
            You are a helpful assistant with both conversation memory and long-term memory.

            Conversation history:
            {conversation_history}

            Relevant information from your long-term memory:
            {relevant_memories}

            User's current input: {input}

            Respond to the user's current input, taking into account both the conversation history
            and any relevant information from your long-term memory. If you learn any new important
            information that should be remembered, note it with [REMEMBER: information to remember].
            """
        )

    def _format_memories(self, memories: List[Dict]) -> str:
        """Format retrieved memories for inclusion in the prompt."""
        # YOUR CODE: Format the memories for the prompt
        if not memories:
            return "No relevant memories found."

        result = []
        for i, memory in enumerate(memories):
            timestamp = memory["metadata"].get("timestamp", "Unknown time")
            if isinstance(timestamp, (int, float)):
                from datetime import datetime
                timestamp = datetime.fromtimestamp(timestamp).strftime("%Y-%m-%d %H:%M:%S")

            result.append(f"Memory {i+1} [{timestamp}]: {memory['content']}")

        return "\n".join(result)

    def _extract_memories(self, response: str) -> List[str]:
        """Extract new memories to store from the agent's response."""
        # YOUR CODE: Extract information marked for remembering
        import re
        memory_pattern = r"\[REMEMBER: (.*?)\]"
        return re.findall(memory_pattern, response)

    def process(self, input_text: str) -> str:
        """Process the user input using memory systems and generate a response."""
        # YOUR CODE: Implement the main agent processing logic

        # Retrieve relevant memories
        relevant_memories = self.vector_memory.retrieve(input_text)
        formatted_memories = self._format_memories(relevant_memories)

        # Get conversation history
        conversation_history = self.conversation_memory.get_formatted()

        # Generate response
        prompt = self.prompt_template.format(
            input=input_text,
            conversation_history=conversation_history if conversation_history else "No conversation history yet.",
            relevant_memories=formatted_memories
        )

        response = self.llm.invoke(prompt)

        # Extract and store new memories
        new_memories = self._extract_memories(response)
        for memory in new_memories:
            self.vector_memory.add(memory, {"source": "conversation"})

        # Clean the response by removing memory markers
        clean_response = response.replace("[REMEMBER: ", "[Noted: ").replace("]", "]")

        # Update conversation memory
        self.conversation_memory.add("user", input_text)
        self.conversation_memory.add("assistant", clean_response)

        return clean_response
```

### 3. Test Harness

```python
def test_memory_agent():
    """Test the memory-augmented agent with a multi-turn conversation."""
    agent = MemoryAugmentedAgent()

    # Pre-populate some memories (optional)
    agent.vector_memory.add("The user's name is Alex.", {"type": "user_info"})
    agent.vector_memory.add("Alex likes hiking and photography.", {"type": "user_preference"})
    agent.vector_memory.add("Today's weather is sunny with a high of 75°F.", {"type": "environment"})

    # Test conversation
    conversation = [
        "Hi there! How are you today?",
        "Can you tell me what you remember about me?",
        "I recently went on a trip to Japan. It was amazing!",
        "What was my favorite activity again?",
        "I'm planning another trip soon. Any suggestions based on what I like?"
    ]

    print("=== Testing Memory-Augmented Agent ===\n")

    for i, user_input in enumerate(conversation):
        print(f"Turn {i+1}:")
        print(f"User: {user_input}")
        response = agent.process(user_input)
        print(f"Agent: {response}\n")

        # GROUP DISCUSSION POINT: How does the agent's memory affect its responses?
        # What information is being remembered correctly? What is being forgotten?

if __name__ == "__main__":
    test_memory_agent()
```

---

## Live Coding Collaboration

### Session Structure (60-75 minutes)

1. **Setup & Memory Design** (15 min)

   - Configure environment
   - Understand memory systems
   - Plan custom memory implementation

2. **Implementation Phase** (25 min)

   - Complete the agent's memory processing methods
   - Implement memory extraction and formatting
   - Create test scenarios with multi-turn conversations

3. **Testing & Analysis** (20 min)

   - Test with varied conversation flows
   - Analyze memory retrieval effectiveness
   - Evaluate response quality with memory vs. without memory

4. **Extension & Refinement** (15 min)
   - Implement more sophisticated memory prioritization
   - Add memory decay or importance weighting
   - Create a custom memory type for specific information

---

## Reflection Checkpoints

### Checkpoint 1: After Memory System Implementation

- What types of information should be stored in each memory system?
- How should memories be organized for effective retrieval?
- What metadata is important to track for each memory?
- How might different memory structures affect agent behavior?

### Checkpoint 2: After Agent Testing

- When does the agent effectively retrieve relevant memories?
- When does it fail to recall important information?
- How does conversation quality change as memory accumulates?
- What patterns of forgetting or misremembering do you observe?

---

## Debugging and Extension Challenges

### Common Issues

- Memory retrieval misses relevant information
- Memory overload (too many irrelevant memories)
- Temporal confusion (mixing up when information was learned)
- Contradictory memories causing inconsistent responses

### Extension Ideas

1. **Memory Forgetting**

   - Implement memory decay based on time
   - Add importance scoring to prioritize memories
   - Create a memory consolidation process

2. **Structured Knowledge**

   - Implement entity-relationship based memory
   - Add schema validation for memory objects
   - Create domain-specific memory structures

3. **Active Memory Management**

   - Add memory summarization to compress information
   - Implement memory contradiction detection
   - Create memory reconstruction capabilities

4. **Memory Visualization**
   - Create a timeline view of memories
   - Implement knowledge graph visualization
   - Add memory influence tracing

---

## Agent Evaluation

### Effectiveness Metrics

- Information recall accuracy
- Contextual relevance of responses
- Conversation coherence across turns
- Memory relevance to current query

### Efficiency Metrics

- Response time as memory grows
- Memory storage requirements
- Retrieval precision and recall
- Context window utilization

### Alignment Metrics

- Consistency in factual responses
- Appropriate recall of personal information
- Privacy considerations in memory storage
- Transparency about remembered information

### Evaluation Exercise

Design a challenging multi-turn conversation scenario that tests the agent's ability to:

1. Remember factual information provided by the user
2. Recall preferences and apply them in recommendations
3. Maintain conversational context over 5+ turns
4. Recognize and resolve contradictions in information

---

## Discussion Questions

1. How does memory augmentation parallel human memory processes?
2. What are the ethical implications of agents remembering personal information?
3. How might different memory architectures affect agent personality and behavior?
4. What are the tradeoffs between different memory systems (vector vs. conversation vs. structured)?
5. How would you design memory systems for an agent that needs to learn and evolve over time?
6. What privacy and security considerations exist for memory-augmented agents?

---

## Next Steps

- Explore how planning can use memory to improve future interactions
- Investigate how tool usage can be enhanced with memory of past effectiveness
- Consider how multiple agents might share and build collective memory
