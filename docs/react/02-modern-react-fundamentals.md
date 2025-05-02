# Modern React Fundamentals

This section covers the fundamental concepts of modern React development, with a focus on React Server Components (RSC) and the crucial distinction between server and client components.

## Understanding React Server Components

React Server Components represent a paradigm shift in how we build React applications. Unlike traditional client-rendered React, RSC allows components to run on the server, reducing the JavaScript sent to the client and improving performance.

### Key Benefits of Server Components

1. **Reduced Client-Side JavaScript**: Server components don't send their component code to the client
2. **Direct Backend Access**: Can directly access backend resources (databases, file systems)
3. **Automatic Code Splitting**: More efficient than manual imports
4. **Improved SEO**: Content is rendered on the server
5. **Better Performance**: Faster page loads and improved core web vitals

## Server Components vs. Client Components

In Next.js App Router, all components are Server Components by default. Let's understand the key differences:

| Feature                     | Server Component     | Client Component                    |
| --------------------------- | -------------------- | ----------------------------------- |
| Rendering Location          | Server               | Client (browser)                    |
| JavaScript Bundle           | Not included         | Included                            |
| Access to Browser APIs      | No                   | Yes                                 |
| Access to React Hooks       | No                   | Yes                                 |
| Access to Backend Resources | Yes                  | No (requires API calls)             |
| File Convention             | Regular `.tsx` files | Files with `'use client'` directive |

### When to Use Server Components

Use Server Components when:

- Fetching data from a database or API
- Accessing backend resources directly
- Content is primarily static or SEO-critical
- The component doesn't need interactivity or browser APIs
- You want to keep bundle size small

Example Server Component:

```tsx
// app/products/page.tsx
// This is a Server Component by default
import { getProducts } from "@/lib/api";

export default async function ProductsPage() {
	// Direct data fetching without useEffect or useState
	const products = await getProducts();

	return (
		<div className="grid grid-cols-3 gap-4">
			{products.map((product) => (
				<div key={product.id} className="border p-4 rounded-lg">
					<h2 className="text-xl font-bold">{product.name}</h2>
					<p className="text-gray-600">{product.description}</p>
					<p className="text-green-600 font-bold">${product.price}</p>
				</div>
			))}
		</div>
	);
}
```

### When to Use Client Components

Use Client Components when:

- You need interactivity (click handlers, form inputs)
- You need to use React hooks like `useState` or `useEffect`
- You need access to browser-only APIs
- You need to use browser events
- You're using client-side libraries that rely on the DOM

Example Client Component:

```tsx
// components/ui/Counter.tsx
"use client"; // This directive marks this as a Client Component

import { useState } from "react";

export default function Counter() {
	const [count, setCount] = useState(0);

	return (
		<div className="border p-4 rounded-lg">
			<p className="text-xl mb-2">Count: {count}</p>
			<button
				onClick={() => setCount(count + 1)}
				className="bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded"
			>
				Increment
			</button>
		</div>
	);
}
```

## Understanding the Server/Client Boundary

The boundary between Server and Client Components is crucial to understand for efficient applications.

### Rules for Composing Components

1. Server Components can import and render Client Components
2. Client Components cannot import and use Server Components directly
3. Client Components can render Server Components through props (children pattern)

### The "Children as Props" Pattern

This pattern allows Client Components to receive Server Components as children:

```tsx
// components/ui/ClientWrapper.tsx
"use client";

import { useState } from "react";

export default function ClientWrapper({
	children, // This can be a Server Component
}: {
	children: React.ReactNode;
}) {
	const [isOpen, setIsOpen] = useState(false);

	return (
		<div className="border p-4">
			<button
				onClick={() => setIsOpen(!isOpen)}
				className="bg-gray-200 px-4 py-2 rounded"
			>
				Toggle Content
			</button>

			{isOpen && <div className="mt-4">{children}</div>}
		</div>
	);
}
```

Using it in a page:

```tsx
// app/example/page.tsx
import ClientWrapper from "@/components/ui/ClientWrapper";
import { getServerData } from "@/lib/api";

// This is a Server Component
async function ServerContent() {
	const data = await getServerData();

	return (
		<div>
			<h2 className="text-2xl font-bold mb-4">Server Data</h2>
			<pre>{JSON.stringify(data, null, 2)}</pre>
		</div>
	);
}

export default function ExamplePage() {
	return (
		<div className="p-4">
			<h1 className="text-2xl font-bold mb-4">
				Server/Client Boundary Example
			</h1>

			<ClientWrapper>
				{/* Server Component passed as children to a Client Component */}
				<ServerContent />
			</ClientWrapper>
		</div>
	);
}
```

### Recommended Component Organization

To clearly separate Server and Client components, we recommend organizing your project as follows:

```
src/
├── components/
│   ├── server/        # Server-only components
│   │   └── ...
│   ├── client/        # Client-only components
│   │   └── ...
│   ├── ui/            # Shared UI components (mostly client)
│   │   └── ...
│   └── shared/        # Components that can be both
│       └── ...
```

## Server Component Data Fetching

One of the biggest advantages of Server Components is the ability to fetch data directly without useEffect:

```tsx
// app/dashboard/page.tsx
import { getUser, getUserStats } from "@/lib/api";

export default async function DashboardPage() {
	// Parallel data fetching
	const userPromise = getUser();
	const statsPromise = getUserStats();

	// Wait for both promises to resolve
	const [user, stats] = await Promise.all([userPromise, statsPromise]);

	return (
		<div className="p-6">
			<h1 className="text-2xl font-bold mb-4">Dashboard for {user.name}</h1>

			<div className="grid grid-cols-3 gap-4">
				<div className="border p-4 rounded-lg">
					<h2 className="text-lg font-semibold">Total Orders</h2>
					<p className="text-3xl font-bold">{stats.orders}</p>
				</div>
				<div className="border p-4 rounded-lg">
					<h2 className="text-lg font-semibold">Revenue</h2>
					<p className="text-3xl font-bold">${stats.revenue}</p>
				</div>
				<div className="border p-4 rounded-lg">
					<h2 className="text-lg font-semibold">Customers</h2>
					<p className="text-3xl font-bold">{stats.customers}</p>
				</div>
			</div>
		</div>
	);
}
```

## Client-Side Data Fetching

When you need to fetch data on the client side:

```tsx
"use client";

import { useState, useEffect } from "react";

export default function ClientSideFetchExample() {
	const [data, setData] = useState(null);
	const [loading, setLoading] = useState(true);
	const [error, setError] = useState(null);

	useEffect(() => {
		async function fetchData() {
			try {
				const response = await fetch("/api/data");
				if (!response.ok) throw new Error("Failed to fetch");
				const result = await response.json();
				setData(result);
			} catch (err) {
				setError(err.message);
			} finally {
				setLoading(false);
			}
		}

		fetchData();
	}, []);

	if (loading) return <div>Loading...</div>;
	if (error) return <div>Error: {error}</div>;

	return (
		<div>
			<h2>Client-side Data</h2>
			<pre>{JSON.stringify(data, null, 2)}</pre>
		</div>
	);
}
```

## Server Actions for Form Handling

React Server Components work seamlessly with Server Actions to handle form submissions:

```tsx
// app/contact/page.tsx
export default function ContactPage() {
	// Server Action (new React feature)
	async function submitForm(formData: FormData) {
		"use server"; // This marks the function as a Server Action

		const name = formData.get("name");
		const email = formData.get("email");
		const message = formData.get("message");

		// Server-side validation, database access, etc.
		await saveContactMessage({ name, email, message });

		// No need for redirect - the server will handle the response
	}

	return (
		<div className="max-w-md mx-auto p-6">
			<h1 className="text-2xl font-bold mb-4">Contact Us</h1>

			<form action={submitForm} className="space-y-4">
				<div>
					<label htmlFor="name" className="block text-sm font-medium">
						Name
					</label>
					<input
						type="text"
						id="name"
						name="name"
						className="mt-1 block w-full rounded-md border-gray-300 shadow-sm"
						required
					/>
				</div>

				<div>
					<label htmlFor="email" className="block text-sm font-medium">
						Email
					</label>
					<input
						type="email"
						id="email"
						name="email"
						className="mt-1 block w-full rounded-md border-gray-300 shadow-sm"
						required
					/>
				</div>

				<div>
					<label htmlFor="message" className="block text-sm font-medium">
						Message
					</label>
					<textarea
						id="message"
						name="message"
						rows={4}
						className="mt-1 block w-full rounded-md border-gray-300 shadow-sm"
						required
					></textarea>
				</div>

				<button
					type="submit"
					className="bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded"
				>
					Submit
				</button>
			</form>
		</div>
	);
}
```

## Key Takeaways

1. **Default to Server Components** when building new components, and only switch to Client Components when necessary for interactivity or browser APIs.

2. **Keep Client Components Lean** by moving as much logic as possible to Server Components.

3. **Use the Children Pattern** to compose Server and Client Components together.

4. **Fetch Data in Server Components** whenever possible to reduce client-side JavaScript and improve performance.

5. **Use Server Actions** for form handling and data mutations to keep sensitive logic on the server.

In the next section, we'll explore edge computing and streaming for even better performance.
