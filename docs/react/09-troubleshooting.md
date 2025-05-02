# Troubleshooting

This section covers common issues and solutions you might encounter when building modern React applications, along with debugging techniques and workflow tips.

## Common React Server Components Issues

### "Error: useState is not defined" or "Error: useEffect is not defined"

**Symptom:** Errors about React hooks not being defined when using them in a component.

**Cause:** You're trying to use React hooks in a Server Component. Hooks can only be used in Client Components.

**Solution:** Add the `'use client'` directive at the top of your file:

```tsx
"use client";

import { useState, useEffect } from "react";

export default function MyComponent() {
	const [count, setCount] = useState(0);

	useEffect(() => {
		console.log("Component mounted");
	}, []);

	return <div>Count: {count}</div>;
}
```

### "Error: Event handlers cannot be passed to Client Component props"

**Symptom:** Error when passing an event handler from a Server Component to a Client Component.

**Cause:** Server Components cannot define or pass event handlers to Client Components.

**Solution:** Move the event handling logic to a Client Component:

```tsx
// Before (Error)
// app/page.tsx (Server Component)
export default function Page() {
	function handleClick() {
		console.log("Clicked");
	}

	return <Button onClick={handleClick}>Click me</Button>;
}

// After (Fixed)
// app/page.tsx (Server Component)
import ButtonWithHandler from "@/components/client/ButtonWithHandler";

export default function Page() {
	return <ButtonWithHandler>Click me</ButtonWithHandler>;
}

// components/client/ButtonWithHandler.tsx (Client Component)
("use client");

export default function ButtonWithHandler({ children }) {
	function handleClick() {
		console.log("Clicked");
	}

	return <button onClick={handleClick}>{children}</button>;
}
```

### "Error: Objects are not valid as a React child"

**Symptom:** Error when trying to render an object directly in your JSX.

**Cause:** React cannot automatically convert objects to strings.

**Solution:** Convert objects to strings or extract specific properties:

```tsx
// Before (Error)
return <div>{user}</div>;

// After (Fixed)
return <div>{user.name}</div>;

// Or using JSON.stringify for debugging
return <pre>{JSON.stringify(user, null, 2)}</pre>;
```

## Server-Side Rendering Issues

### "Hydration Failed" Errors

**Symptom:** Errors in the console about hydration mismatch between server and client.

**Cause:** The HTML generated on the server doesn't match what React expects to render on the client.

**Solution:**

1. Ensure consistent rendering between server and client:

```tsx
// Avoid code like this
function Component() {
	// This will generate different outputs on server vs client
	return <div>{new Date().toLocaleTimeString()}</div>;
}

// Use this instead
function Component() {
	const [time, setTime] = useState("");

	useEffect(() => {
		// Update the time only on the client
		setTime(new Date().toLocaleTimeString());

		const interval = setInterval(() => {
			setTime(new Date().toLocaleTimeString());
		}, 1000);

		return () => clearInterval(interval);
	}, []);

	return <div>{time}</div>;
}
```

2. Use `suppressHydrationWarning` for intentional differences:

```tsx
<div suppressHydrationWarning>
	Current time: {new Date().toLocaleTimeString()}
</div>
```

### "Text content does not match server-rendered HTML" Warning

**Symptom:** Warning about text content not matching server-rendered HTML.

**Cause:** Text content is different between server and client renders.

**Solution:** Make sure your rendering is consistent or use `suppressHydrationWarning`:

```tsx
<p suppressHydrationWarning>Random number: {Math.random()}</p>
```

## TypeScript Issues

### Type Errors with Props

**Symptom:** TypeScript errors about missing or incorrect props.

**Cause:** Component props are not properly typed or required props are missing.

**Solution:** Define proper interfaces for your props and provide default values:

```tsx
interface ButtonProps {
	text: string;
	onClick: () => void;
	variant?: "primary" | "secondary" | "outline";
}

function Button({ text, onClick, variant = "primary" }: ButtonProps) {
	// Component implementation
}
```

### "Property does not exist on type" Errors

**Symptom:** TypeScript errors about properties not existing on certain types.

**Cause:** Accessing properties that TypeScript doesn't know exist on an object.

**Solution:** Use proper type assertions or type guards:

```tsx
// Type assertion
const result = data as { id: string; name: string };

// Type guard
function isUser(obj: any): obj is User {
	return obj && typeof obj.id === "string" && typeof obj.name === "string";
}

if (isUser(data)) {
	console.log(data.name); // TypeScript knows data is a User here
}
```

### Type Errors with API Responses

**Symptom:** TypeScript errors when working with data from APIs.

**Cause:** TypeScript doesn't know the structure of the API response.

**Solution:** Define interfaces for your API responses and use proper typing:

```tsx
interface User {
	id: string;
	name: string;
	email: string;
}

async function fetchUser(id: string): Promise<User> {
	const response = await fetch(`/api/users/${id}`);

	if (!response.ok) {
		throw new Error("Failed to fetch user");
	}

	return response.json();
}

// In your component
const [user, setUser] = useState<User | null>(null);

useEffect(() => {
	fetchUser("123")
		.then((data) => setUser(data))
		.catch((error) => console.error(error));
}, []);
```

## Next.js App Router Issues

### "Error: Unsupported Server Component type"

**Symptom:** Error about unsupported Server Component types.

**Cause:** You're trying to use a component or feature that's not supported in Server Components.

**Solution:** Move the component to a Client Component:

```tsx
"use client";

import { useState } from "react";

export default function ClientComponent() {
	// This is now allowed because we're in a Client Component
	const [state, setState] = useState(0);

	return <div>{state}</div>;
}
```

### Routes Not Working as Expected

**Symptom:** Routes are not behaving as expected or showing "404 Not Found" errors.

**Cause:** Incorrect file structure in the app directory.

**Solution:** Ensure your file structure follows Next.js App Router conventions:

- Place page components in `page.tsx` files
- Place layouts in `layout.tsx` files
- Place loading states in `loading.tsx` files
- Use folder names for route segments
- Use dynamic segments with `[param]` syntax

```
app/
├── page.tsx              # Home page: /
├── about/
│   └── page.tsx          # About page: /about
├── blog/
│   ├── page.tsx          # Blog index: /blog
│   └── [slug]/
│       └── page.tsx      # Blog post: /blog/:slug
└── dashboard/
    ├── layout.tsx        # Dashboard layout
    ├── page.tsx          # Dashboard index: /dashboard
    └── settings/
        └── page.tsx      # Dashboard settings: /dashboard/settings
```

### Server Actions Not Working

**Symptom:** Server Actions not working or throwing errors.

**Cause:** Incorrect implementation or usage of Server Actions.

**Solution:** Ensure you're using Server Actions correctly:

```tsx
// Form with Server Action
export default function ContactForm() {
	async function submitForm(formData: FormData) {
		"use server"; // This marks the function as a Server Action

		const name = formData.get("name") as string;
		const email = formData.get("email") as string;
		const message = formData.get("message") as string;

		// Process form data on the server
		// ...
	}

	return (
		<form action={submitForm}>
			<input name="name" required />
			<input name="email" type="email" required />
			<textarea name="message" required></textarea>
			<button type="submit">Submit</button>
		</form>
	);
}
```

## Performance Issues

### Slow Initial Load Times

**Symptom:** The application takes a long time to load initially.

**Cause:** Large bundle sizes, unoptimized images, or unnecessary client-side JavaScript.

**Solution:**

1. Use the Next.js bundle analyzer to identify large dependencies:

```bash
npm install --save-dev @next/bundle-analyzer
```

```js
// next.config.js
const withBundleAnalyzer = require("@next/bundle-analyzer")({
	enabled: process.env.ANALYZE === "true",
});

module.exports = withBundleAnalyzer({
	// Your Next.js config
});
```

Run with:

```bash
ANALYZE=true npm run build
```

2. Optimize images with next/image:

```tsx
import Image from "next/image";

export default function OptimizedImage() {
	return (
		<Image
			src="/large-image.jpg"
			alt="Description"
			width={800}
			height={600}
			priority={true} // For LCP images
		/>
	);
}
```

3. Use code splitting and lazy loading:

```tsx
import { lazy, Suspense } from "react";

// Lazy load heavy components
const HeavyComponent = lazy(() => import("@/components/HeavyComponent"));

export default function Page() {
	return (
		<div>
			<h1>My Page</h1>
			<Suspense fallback={<div>Loading...</div>}>
				<HeavyComponent />
			</Suspense>
		</div>
	);
}
```

### React Component Re-Rendering Too Often

**Symptom:** UI feels sluggish, components re-render frequently.

**Cause:** Inefficient rendering patterns, missing memoization.

**Solution:**

1. Use React DevTools Profiler to identify unnecessary re-renders.

2. Memoize components with `React.memo`:

```tsx
import { memo } from "react";

function ExpensiveComponent({ data }) {
	// Complex rendering logic
	return <div>{/* ... */}</div>;
}

export default memo(ExpensiveComponent);
```

3. Memoize callbacks and derived values:

```tsx
import { useMemo, useCallback } from "react";

function SearchResults({ items, query }) {
	// Memoize filtered results
	const filteredItems = useMemo(() => {
		return items.filter((item) =>
			item.name.toLowerCase().includes(query.toLowerCase())
		);
	}, [items, query]);

	// Memoize callback
	const handleSelect = useCallback((id) => {
		console.log(`Selected item: ${id}`);
	}, []);

	return (
		<ul>
			{filteredItems.map((item) => (
				<li key={item.id} onClick={() => handleSelect(item.id)}>
					{item.name}
				</li>
			))}
		</ul>
	);
}
```

4. Use appropriate state management to avoid prop drilling.

### Memory Leaks

**Symptom:** Application becomes slower over time, browser tab uses increasing memory.

**Cause:** Uncleaned event listeners, timers, or observers.

**Solution:** Properly clean up effects:

```tsx
useEffect(() => {
	const intervalId = setInterval(() => {
		// Do something periodically
	}, 1000);

	// Clean up when component unmounts
	return () => clearInterval(intervalId);
}, []);

useEffect(() => {
	window.addEventListener("resize", handleResize);

	// Clean up when component unmounts
	return () => window.removeEventListener("resize", handleResize);
}, []);
```

## API and Data Fetching Issues

### "TypeError: Failed to fetch" Errors

**Symptom:** Errors in the console when trying to fetch data.

**Cause:** Network issues, CORS problems, or invalid request configuration.

**Solution:**

1. Check network connectivity and server status.

2. Implement proper error handling in fetch requests:

```tsx
async function fetchData() {
	try {
		const response = await fetch("/api/data");

		if (!response.ok) {
			throw new Error(`HTTP error ${response.status}`);
		}

		const data = await response.json();
		return data;
	} catch (error) {
		console.error("Fetch error:", error);
		// Handle the error appropriately
		return null;
	}
}
```

3. For CORS issues, configure your API server to allow cross-origin requests.

### Data Not Updating After Mutations

**Symptom:** UI doesn't reflect changes after creating, updating, or deleting data.

**Cause:** Missing cache invalidation or refetching.

**Solution:** Implement proper cache management:

1. With React Query / TanStack Query:

```tsx
import { useMutation, useQueryClient } from "@tanstack/react-query";

function TodoList() {
	const queryClient = useQueryClient();

	const mutation = useMutation({
		mutationFn: (newTodo) => {
			return fetch("/api/todos", {
				method: "POST",
				body: JSON.stringify(newTodo),
			});
		},
		onSuccess: () => {
			// Invalidate the todos query to refetch
			queryClient.invalidateQueries({ queryKey: ["todos"] });
		},
	});
}
```

2. With tRPC:

```tsx
const utils = trpc.useContext();

const createPost = trpc.post.create.useMutation({
	onSuccess: () => {
		// Invalidate the posts query
		utils.post.getAll.invalidate();
	},
});
```

### State Not Persisting Between Page Navigations

**Symptom:** State resets when navigating between pages.

**Cause:** React components remount on page changes.

**Solution:**

1. Use a global state management solution:

```tsx
// With Zustand
import { create } from "zustand";

interface AppState {
	count: number;
	increment: () => void;
}

export const useAppStore = create<AppState>((set) => ({
	count: 0,
	increment: () => set((state) => ({ count: state.count + 1 })),
}));

// In component
function Counter() {
	const { count, increment } = useAppStore();

	return (
		<div>
			<p>Count: {count}</p>
			<button onClick={increment}>Increment</button>
		</div>
	);
}
```

2. Store state in localStorage or cookies:

```tsx
function usePersistedState(key, defaultValue) {
	const [state, setState] = useState(() => {
		if (typeof window !== "undefined") {
			const stored = localStorage.getItem(key);
			return stored ? JSON.parse(stored) : defaultValue;
		}
		return defaultValue;
	});

	useEffect(() => {
		if (typeof window !== "undefined") {
			localStorage.setItem(key, JSON.stringify(state));
		}
	}, [key, state]);

	return [state, setState];
}

// In component
function Counter() {
	const [count, setCount] = usePersistedState("counter", 0);

	return (
		<div>
			<p>Count: {count}</p>
			<button onClick={() => setCount(count + 1)}>Increment</button>
		</div>
	);
}
```

## Form Handling Issues

### Form Data Not Being Captured Correctly

**Symptom:** Form submissions include missing or incorrect data.

**Cause:** Improper form control setup or event handling.

**Solution:** Use a form library like react-hook-form:

```tsx
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { z } from "zod";

const schema = z.object({
	name: z.string().min(2, "Name is too short"),
	email: z.string().email("Invalid email format"),
});

type FormData = z.infer<typeof schema>;

function ContactForm() {
	const {
		register,
		handleSubmit,
		formState: { errors },
	} = useForm<FormData>({
		resolver: zodResolver(schema),
	});

	const onSubmit = (data: FormData) => {
		console.log(data);
		// Process form data
	};

	return (
		<form onSubmit={handleSubmit(onSubmit)}>
			<div>
				<label htmlFor="name">Name</label>
				<input id="name" {...register("name")} />
				{errors.name && <p>{errors.name.message}</p>}
			</div>

			<div>
				<label htmlFor="email">Email</label>
				<input id="email" type="email" {...register("email")} />
				{errors.email && <p>{errors.email.message}</p>}
			</div>

			<button type="submit">Submit</button>
		</form>
	);
}
```

### Form Validation Errors

**Symptom:** Forms submit invalid data or don't display validation errors correctly.

**Cause:** Missing or incorrect validation logic.

**Solution:** Implement comprehensive validation with Zod:

```tsx
const userSchema = z
	.object({
		username: z
			.string()
			.min(3, "Username must be at least 3 characters")
			.max(20, "Username cannot exceed 20 characters")
			.regex(
				/^[a-z0-9_-]+$/,
				"Username can only contain lowercase letters, numbers, underscores, and hyphens"
			),

		email: z.string().email("Invalid email address"),

		password: z
			.string()
			.min(8, "Password must be at least 8 characters")
			.regex(/[A-Z]/, "Password must contain at least one uppercase letter")
			.regex(/[a-z]/, "Password must contain at least one lowercase letter")
			.regex(/[0-9]/, "Password must contain at least one number"),

		confirmPassword: z.string(),
	})
	.refine((data) => data.password === data.confirmPassword, {
		message: "Passwords don't match",
		path: ["confirmPassword"],
	});
```

## Styling Issues

### Styles Not Applying as Expected

**Symptom:** CSS classes or styles don't seem to apply correctly.

**Cause:** CSS specificity issues, TailwindCSS purging, or class naming conflicts.

**Solution:**

1. Use the browser DevTools to inspect the applied styles.

2. For TailwindCSS specificity issues, use the `!important` modifier:

```html
<div class="bg-blue-500 !bg-red-500">
	<!-- This will have a red background -->
</div>
```

3. Ensure your TailwindCSS content paths include all files:

```js
// tailwind.config.js
module.exports = {
	content: ["./src/**/*.{js,ts,jsx,tsx,mdx}"],
	// ...
};
```

4. Use more specific selectors or the `@apply` directive for complex cases:

```css
/* styles/globals.css */
@tailwind base;
@tailwind components;
@tailwind utilities;

@layer components {
	.custom-button {
		@apply bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded-md;
	}
}
```

### Dark Mode Not Working

**Symptom:** Dark mode toggle doesn't affect the UI or works inconsistently.

**Cause:** Incorrect setup of dark mode or hydration issues.

**Solution:**

1. Make sure your `tailwind.config.js` is configured for dark mode:

```js
// tailwind.config.js
module.exports = {
	darkMode: "class", // or 'media' for media query based
	// ...
};
```

2. Ensure you're using next-themes correctly:

```tsx
// _app.tsx or providers.tsx
import { ThemeProvider } from "next-themes";

export function Providers({ children }) {
	return (
		<ThemeProvider attribute="class" defaultTheme="system" enableSystem>
			{children}
		</ThemeProvider>
	);
}

// In a component that toggles the theme
("use client");

import { useTheme } from "next-themes";
import { useEffect, useState } from "react";

export function ThemeToggle() {
	const { theme, setTheme } = useTheme();
	const [mounted, setMounted] = useState(false);

	// Prevent hydration mismatch
	useEffect(() => {
		setMounted(true);
	}, []);

	if (!mounted) return null;

	return (
		<button onClick={() => setTheme(theme === "dark" ? "light" : "dark")}>
			{theme === "dark" ? "Light Mode" : "Dark Mode"}
		</button>
	);
}
```

## Debugging Workflow Tips

### Using Browser DevTools

1. **React DevTools Extension**: Install the React Developer Tools extension for Chrome or Firefox.

2. **Components Tab**: Inspect component props, state, and hooks.

3. **Profiler Tab**: Analyze component rendering performance.

4. **Network Tab**: Monitor API requests and responses.

5. **Console**: Use `console.log`, `console.error`, or `console.table` for debugging.

```tsx
// Better console logging
console.log("User data:", {
	id: user.id,
	name: user.name,
	isAdmin: user.isAdmin,
});

// Group related logs
console.group("Authentication Process");
console.log("Checking credentials...");
console.log("Generating token...");
console.log("Token:", token);
console.groupEnd();

// Measure performance
console.time("fetchData");
await fetchData();
console.timeEnd("fetchData");
```

### Using VS Code for Debugging

1. **Set breakpoints**: Click in the gutter next to line numbers.

2. **Configure launch.json**:

```json
{
	"version": "0.2.0",
	"configurations": [
		{
			"name": "Next.js: debug server-side",
			"type": "node-terminal",
			"request": "launch",
			"command": "npm run dev"
		},
		{
			"name": "Next.js: debug client-side",
			"type": "chrome",
			"request": "launch",
			"url": "http://localhost:3000"
		}
	]
}
```

3. **Use the debug console**: View variables and execute code.

### Debugging Server Components

For debugging server components, use `console.log` statements in your server code. These will appear in your terminal where Next.js is running.

You can also create a debug utility:

```tsx
// lib/debug.ts
export function debug(label: string, value: any) {
	if (process.env.NODE_ENV !== "production") {
		console.log(`[DEBUG] ${label}:`, value);
	}
}

// In your server component
import { debug } from "@/lib/debug";

export default async function Page() {
	const data = await fetchData();
	debug("Fetched data", data);

	// Rest of the component
}
```

## Key Takeaways

1. **Understand Server/Client Boundaries**: Most issues with modern React stem from confusion about what can run where.

2. **Use TypeScript Effectively**: Properly typing your components and data prevents many common errors.

3. **Follow App Router Conventions**: Next.js App Router has specific file and directory conventions that must be followed.

4. **Optimize Performance**: Regularly test and debug performance issues using the built-in tools.

5. **Implement Error Boundaries**: Catch and handle errors gracefully to prevent entire app crashes.

```tsx
"use client";

import { Component, ErrorInfo, ReactNode } from "react";

interface ErrorBoundaryProps {
	fallback: ReactNode;
	children: ReactNode;
}

interface ErrorBoundaryState {
	hasError: boolean;
}

class ErrorBoundary extends Component<ErrorBoundaryProps, ErrorBoundaryState> {
	constructor(props: ErrorBoundaryProps) {
		super(props);
		this.state = { hasError: false };
	}

	static getDerivedStateFromError(_: Error): ErrorBoundaryState {
		return { hasError: true };
	}

	componentDidCatch(error: Error, errorInfo: ErrorInfo) {
		console.error("Error caught by boundary:", error, errorInfo);
	}

	render() {
		if (this.state.hasError) {
			return this.props.fallback;
		}

		return this.props.children;
	}
}

export default ErrorBoundary;
```

This concludes our comprehensive guide to modern React development. We've covered everything from project setup to deployment and troubleshooting. Use these best practices and techniques to create performant, maintainable, and user-friendly React applications.
