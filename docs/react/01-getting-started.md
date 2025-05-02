# Getting Started with Modern React

This section will guide you through setting up your development environment, scaffolding a new React project with Next.js, and understanding the basic project structure.

## Environment Setup

### System Requirements

- **Node.js**: v20.0.0 or later
- **npm**: v10.0.0 or later (comes with Node.js)
- **Git**: Latest version recommended
- **Code Editor**: We recommend Visual Studio Code with the following extensions:
  - ESLint
  - Prettier
  - Tailwind CSS IntelliSense
  - TypeScript JSX Snippets

### Installing Node.js

#### macOS

Using Homebrew:

```bash
brew install node@20
```

#### Windows

Download and install from [nodejs.org](https://nodejs.org/)

#### Linux (Ubuntu/Debian)

```bash
curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt-get install -y nodejs
```

### Verifying Installation

Ensure Node.js and npm are correctly installed:

```bash
node --version
# v20.x.x (or higher)

npm --version
# v10.x.x (or higher)
```

## Project Scaffolding with create-next-app

Next.js provides a tool called `create-next-app` that sets up a new project with all the modern configurations you need.

### Creating a New Project

Run the following command in your terminal:

```bash
npx create-next-app@latest my-modern-app
```

During the setup, you'll be prompted with several options. Here are our recommended selections:

```
Would you like to use TypeScript? › Yes
Would you like to use ESLint? › Yes
Would you like to use Tailwind CSS? › Yes
Would you like to use `src/` directory? › Yes
Would you like to use App Router? (recommended) › Yes
Would you like to customize the default import alias? › Yes, use @/*
```

### Installing Additional Dependencies

Once your project is created, navigate to the project directory and install the additional dependencies we'll need:

```bash
cd my-modern-app

# Install form handling and validation libraries
npm install react-hook-form @hookform/resolvers zod

# Install UI utilities
npm install clsx

# Install Radix UI components (as needed)
npm install @radix-ui/react-dialog @radix-ui/react-dropdown-menu @radix-ui/react-toast @radix-ui/react-tabs

# Install OpenAI SDK (for AI features)
npm install openai
```

## Project Structure Overview

After setup, your project structure should look like this:

```
my-modern-app/
├── .eslintrc.json           # ESLint configuration
├── .gitignore               # Git ignore rules
├── next.config.js           # Next.js configuration
├── package.json             # Project dependencies
├── postcss.config.js        # PostCSS configuration
├── tailwind.config.ts       # Tailwind CSS configuration
├── tsconfig.json            # TypeScript configuration
├── public/                  # Static assets
└── src/
    ├── app/                 # App Router pages
    │   ├── layout.tsx       # Root layout
    │   ├── page.tsx         # Home page
    │   └── favicon.ico      # Favicon
    ├── components/          # Reusable components
    │   ├── ui/              # UI components
    │   └── shared/          # Shared components
    ├── lib/                 # Utilities and shared code
    │   ├── utils.ts         # Utility functions
    │   └── types.ts         # TypeScript types
    └── styles/              # Global styles
        └── globals.css      # Global CSS
```

### Enhanced Project Structure

For more complex applications, we recommend expanding the structure as follows:

```
src/
├── app/                     # App Router pages
├── components/              # Reusable components
│   ├── ui/                  # Basic UI components
│   ├── forms/               # Form-related components
│   ├── layout/              # Layout components
│   └── shared/              # Shared components
├── lib/                     # Utilities and shared code
│   ├── utils/               # Utility functions
│   ├── api/                 # API clients
│   ├── validation/          # Zod schemas
│   └── hooks/               # Custom React hooks
├── types/                   # TypeScript types
├── styles/                  # Global styles
└── server/                  # Server-only code
    ├── actions/             # Server actions
    ├── api/                 # API routes
    └── db/                  # Database utilities
```

## Configuring TailwindCSS

The `create-next-app` command should have set up TailwindCSS for you. Let's verify and enhance the configuration.

Your `tailwind.config.ts` file should look similar to this:

```typescript
import type { Config } from "tailwindcss";

const config: Config = {
	content: [
		"./src/pages/**/*.{js,ts,jsx,tsx,mdx}",
		"./src/components/**/*.{js,ts,jsx,tsx,mdx}",
		"./src/app/**/*.{js,ts,jsx,tsx,mdx}",
	],
	theme: {
		extend: {
			colors: {
				primary: {
					50: "#f0f9ff",
					100: "#e0f2fe",
					200: "#bae6fd",
					300: "#7dd3fc",
					400: "#38bdf8",
					500: "#0ea5e9",
					600: "#0284c7",
					700: "#0369a1",
					800: "#075985",
					900: "#0c4a6e",
					950: "#082f49",
				},
			},
		},
	},
	plugins: [],
};
export default config;
```

If you want to set up a dark mode toggle, modify your configuration to include it:

```typescript
import type { Config } from "tailwindcss";

const config: Config = {
	content: [
		"./src/pages/**/*.{js,ts,jsx,tsx,mdx}",
		"./src/components/**/*.{js,ts,jsx,tsx,mdx}",
		"./src/app/**/*.{js,ts,jsx,tsx,mdx}",
	],
	// Add dark mode support
	darkMode: "class",
	theme: {
		extend: {
			colors: {
				// Your color palette
				primary: {
					50: "#f0f9ff",
					100: "#e0f2fe",
					// ...other shades
				},
			},
		},
	},
	plugins: [],
};
export default config;
```

## Setting Up the Base CSS

Ensure your `globals.css` file includes the Tailwind directives:

```css
@tailwind base;
@tailwind components;
@tailwind utilities;

/* Add any global styles below this line */
:root {
	--foreground-rgb: 0, 0, 0;
	--background-rgb: 255, 255, 255;
}

@media (prefers-color-scheme: dark) {
	:root {
		--foreground-rgb: 255, 255, 255;
		--background-rgb: 0, 0, 0;
	}
}

body {
	color: rgb(var(--foreground-rgb));
	background: rgb(var(--background-rgb));
}

/* Add any custom utilities here */
@layer utilities {
	.flex-center {
		@apply flex items-center justify-center;
	}
}
```

## Starting the Development Server

To start the development server, run:

```bash
npm run dev
```

Your application should now be running on [http://localhost:3000](http://localhost:3000).

In the next section, we'll explore modern React fundamentals, focusing on the distinction between Server Components and Client Components in the Next.js App Router.
