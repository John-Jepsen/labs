# Frontend Setup

This guide will walk you through setting up the React frontend for the AI Tutor platform.

## Prerequisites

Before setting up the frontend, ensure you have:

- Node.js installed (version 18.0.0 or higher)
- npm or yarn package manager
- The AI Tutor repository cloned to your local machine
- A code editor like VS Code

## Navigating to the Frontend Directory

First, navigate to the frontend directory from the root of the project:

```bash
cd ai-tutor/frontend
```

## Installing Dependencies

Install all the required frontend dependencies:

```bash
# Using npm
npm install

# Using yarn
yarn install
```

This will install all the packages defined in the `package.json` file.

<!-- TODO: Add screenshot of successful installation -->

## Environment Configuration

The frontend requires some environment variables to be set up correctly:

1. Create a `.env` file in the frontend directory:

```bash
cp .env.example .env
```

2. Open the `.env` file in your editor and update the values as needed:

```
REACT_APP_API_URL=http://localhost:8000
REACT_APP_AUTH_DOMAIN=example.auth0.com
REACT_APP_AUTH_CLIENT_ID=your_client_id
```

<!-- TODO: Add detailed explanation of each environment variable -->

## Running the Development Server

Start the frontend development server:

```bash
# Using npm
npm start

# Using yarn
yarn start
```

This will start the development server on port 3000, and your browser should automatically open to `http://localhost:3000`.

<!-- TODO: Add screenshot of the running frontend -->

## Building for Production

To create a production build:

```bash
# Using npm
npm run build

# Using yarn
yarn build
```

This will create optimized files in the `build` directory, ready for deployment.

## Frontend Structure

The frontend follows a modern React application structure:

```
frontend/
├── public/                # Static files
│   ├── index.html        # HTML template
│   └── assets/           # Static assets (images, etc.)
├── src/                  # Source code
│   ├── components/       # Reusable UI components
│   ├── contexts/         # React contexts for state management
│   ├── hooks/            # Custom React hooks
│   ├── pages/            # Page components
│   ├── services/         # API and service functions
│   ├── styles/           # CSS and style files
│   ├── utils/            # Utility functions
│   ├── App.js            # Main app component
│   └── index.js          # Entry point
├── .env                  # Environment variables
├── package.json          # Project dependencies and scripts
└── README.md             # Frontend documentation
```

## Key Components

The frontend includes several key components:

### Chat Interface

The main interaction point for students with the AI tutor:

```jsx
// src/components/ChatInterface/ChatInterface.js
import React, { useState, useEffect } from "react";

function ChatInterface() {
	// Implementation details
}
```

<!-- TODO: Add more details about the chat interface component -->

### Code Editor

An integrated code editor for writing and running code:

```jsx
// src/components/CodeEditor/CodeEditor.js
import React from "react";
import { Editor } from "some-editor-library";

function CodeEditor() {
	// Implementation details
}
```

<!-- TODO: Add more details about the code editor component -->

### Lesson Navigator

Component for browsing and selecting lessons:

```jsx
// src/components/LessonNavigator/LessonNavigator.js
import React from "react";

function LessonNavigator() {
	// Implementation details
}
```

<!-- TODO: Add more details about the lesson navigator component -->

## Custom Hooks

The frontend uses several custom hooks:

### useChat

```jsx
// src/hooks/useChat.js
import { useState, useEffect } from "react";

function useChat() {
	// Implementation details
}
```

<!-- TODO: Add more details about the useChat hook -->

## Customizing the UI

You can customize the UI by modifying the theme in:

```jsx
// src/styles/theme.js
export const theme = {
	colors: {
		primary: "#4285F4",
		secondary: "#34A853",
		// More colors
	},
	// More theme properties
};
```

<!-- TODO: Add more details about UI customization -->

## Testing the Frontend

Run tests to ensure the frontend is working correctly:

```bash
# Using npm
npm test

# Using yarn
yarn test
```

<!-- TODO: Add details about the testing framework and test files -->

## Troubleshooting

Common issues when setting up the frontend:

### Node Version Issues

If you encounter errors related to Node.js versions:

```bash
nvm use 18
```

Or install the correct Node.js version.

### Dependency Conflicts

If you see dependency conflicts:

```bash
# Using npm
npm clean-install

# Using yarn
yarn install --force
```

<!-- TODO: Add more common issues and solutions -->

## Next Steps

After setting up the frontend, you should [configure the environment](env_setup.md) to connect all components of the platform.
