# Platform Setup for Developers

This section provides detailed instructions for developers to set up the complete AI Tutor platform for development, testing, or deployment.

## Prerequisites

Before starting the setup process, ensure you have:

- Git installed (version 2.20+)
- Python installed (version 3.11+)
- Node.js installed (version 18+)
- VS Code or your preferred IDE
- Basic familiarity with command line interfaces
- OpenAI API key (or access to a local LLM)

## Setup Steps

1. [Cloning the Repository](clone_repo.md)
2. [Backend Setup](backend_setup.md)
3. [Frontend Setup](frontend_setup.md)
4. [Environment Configuration](env_setup.md)
5. [Database Configuration](database_setup.md)
6. [Running the Development Environment](dev_environment.md)
7. [Deployment Options](deployment_options.md)

## Architecture Overview

The AI Tutor platform consists of the following components:

- **FastAPI Backend**: Handles API requests, LLM interactions, and database operations
- **React Frontend**: Provides the user interface for students and teachers
- **LangChain Integration**: Orchestrates the AI tutor's behavior and capabilities
- **Database**: Stores student progress, chat history, and system configurations
- **Optional Docker Container**: For simplified deployment

## Project Structure

```
ai-tutor/
├── backend/               # FastAPI server and API endpoints
│   ├── agents/            # AI agent definitions and prompts
│   ├── chains/            # LangChain components
│   ├── database/          # Database models and operations
│   ├── routes/            # API routes
│   └── utils/             # Helper functions
├── frontend/              # React application
│   ├── public/            # Static assets
│   └── src/               # Source code
│       ├── components/    # React components
│       ├── contexts/      # React contexts
│       ├── hooks/         # Custom React hooks
│       ├── pages/         # Page components
│       └── utils/         # Frontend utilities
├── lessons/               # Curriculum content
│   ├── beginner/          # Beginner coding lessons
│   ├── intermediate/      # Intermediate lessons
│   └── advanced/          # Advanced projects
└── docs/                  # Documentation
```

## Required API Keys

The platform requires the following API keys:

- OpenAI API key (for ChatGPT integration)
- Optional: GitHub API key (for project management features)
- Optional: Replit API key (for browser-based coding environment)

## Next Steps

After completing the setup process, you may want to:

- [Understand the AI Agent Integration](../ai_agent_integration/index.md)
- [Explore the Lesson Structure](../lesson_structure/index.md)
- [Customize the Chat Tutor Features](../chat_tutor_features/index.md)
