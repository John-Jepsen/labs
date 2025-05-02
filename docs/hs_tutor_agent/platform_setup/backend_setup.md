# Backend Setup

The AI Tutor's backend is built with FastAPI and handles API requests, LLM interactions, and database operations. This guide will walk you through setting up the backend environment.

## Prerequisites

- Python 3.11 or higher installed
- Pip (Python package manager)
- Git (for cloning the repository)
- A text editor or IDE
- Terminal or command prompt

## Setup Instructions

### 1. Navigate to the Backend Directory

After cloning the repository, navigate to the backend directory:

```bash
cd ai-tutor/backend
```

### 2. Create a Virtual Environment

Creating a virtual environment isolates your project dependencies:

```bash
# On Windows
python -m venv venv

# On macOS/Linux
python3 -m venv venv
```

### 3. Activate the Virtual Environment

```bash
# On Windows
venv\Scripts\activate

# On macOS/Linux
source venv/bin/activate
```

You should see `(venv)` appear at the beginning of your command prompt, indicating the virtual environment is active.

### 4. Install Dependencies

Install the required Python packages:

```bash
pip install -r requirements.txt
```

The main dependencies include:

- FastAPI: Web framework
- Uvicorn: ASGI server
- LangChain: For LLM integrations
- SQLAlchemy: ORM for database operations
- Pydantic: Data validation
- Python-dotenv: Environment variable management

### 5. Create an Environment File

Create a `.env` file in the backend directory:

```bash
touch .env
```

Open the file and add the following environment variables:

```
# API Keys
OPENAI_API_KEY=your_openai_api_key_here

# Database Configuration
DATABASE_URL=sqlite:///./ai_tutor.db  # for development
# DATABASE_URL=postgresql://user:password@localhost/ai_tutor  # for production

# Server Settings
DEBUG=True
HOST=localhost
PORT=8000
```

### 6. Initialize the Database

Run the database initialization script:

```bash
python scripts/init_db.py
```

This will create the necessary database tables and populate them with initial data.

## Backend Structure

The backend follows a modular structure:

```
backend/
├── agents/                # AI agent definitions
│   ├── base.py            # Base agent class
│   ├── coding_tutor.py    # Coding tutor agent
│   ├── hint_provider.py   # Hint provider agent
│   └── code_reviewer.py   # Code review agent
├── chains/                # LangChain components
│   ├── router_chain.py    # Agent router chain
│   ├── memory.py          # Conversation memory
│   └── prompts.py         # System prompts
├── database/
│   ├── models.py          # SQLAlchemy models
│   ├── crud.py            # CRUD operations
│   └── session.py         # Database session
├── routes/                # API endpoints
│   ├── auth.py            # Authentication routes
│   ├── chat.py            # Chat API routes
│   ├── code.py            # Code execution routes
│   └── progress.py        # Student progress routes
├── utils/                 # Utility functions
├── main.py                # FastAPI application
├── config.py              # Configuration settings
└── requirements.txt       # Dependencies
```

## Key Components

### Agent System

The agent system is built on top of LangChain and uses a router pattern to direct student questions to the appropriate specialized agent:

```python
# Example from agents/base.py
from langchain.chat_models import ChatOpenAI
from langchain.schema import HumanMessage, SystemMessage

class BaseAgent:
    def __init__(self, model_name="gpt-3.5-turbo"):
        self.llm = ChatOpenAI(model_name=model_name)
        self.system_prompt = SystemMessage(content="You are a helpful AI tutor.")

    async def process(self, message, history=None):
        history = history or []
        messages = [self.system_prompt] + history + [HumanMessage(content=message)]
        response = await self.llm.agenerate([messages])
        return response.generations[0][0].text
```

### API Routes

The API routes handle HTTP requests from the frontend:

```python
# Example from routes/chat.py
from fastapi import APIRouter, Depends, HTTPException
from ..database.session import get_db
from ..agents.router import AgentRouter

router = APIRouter()
agent_router = AgentRouter()

@router.post("/chat")
async def chat(message: str, session_id: str, db = Depends(get_db)):
    try:
        # Route message to appropriate agent
        response = await agent_router.route(message, session_id)
        # Store message and response in database
        # ...
        return {"response": response}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
```

## Running the Backend

To start the backend server:

```bash
uvicorn main:app --reload
```

The API will be available at `http://localhost:8000`. The interactive API documentation (Swagger UI) will be available at `http://localhost:8000/docs`.

## Troubleshooting

### Common Issues

1. **Missing dependencies**: If you encounter import errors, ensure you've installed all dependencies:

   ```bash
   pip install -r requirements.txt
   ```

2. **Database connection errors**: Verify your database URL in the `.env` file.

3. **API key issues**: Ensure your OpenAI API key is valid and properly set in the `.env` file.

4. **Port conflicts**: If port 8000 is already in use, you can specify a different port:
   ```bash
   uvicorn main:app --reload --port 8080
   ```

## Next Steps

After setting up the backend:

1. [Set up the frontend](frontend_setup.md)
2. [Configure the environment](env_setup.md)
3. [Set up the database](database_setup.md)
