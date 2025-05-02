# AI Integration

This section explores how to integrate AI capabilities into your React application, focusing on OpenAI API integration, building AI-powered UX features, and implementing polling agents for enhanced user experiences.

## Understanding AI Integration in Modern Apps

AI integration allows your applications to leverage machine learning capabilities without requiring expertise in AI/ML. Modern apps increasingly use AI for features like:

1. **Content Generation**: Automated text, image, or code creation
2. **Natural Language Processing**: Understanding and responding to user text
3. **Recommendation Systems**: Personalized content and product suggestions
4. **Intelligent Search**: Semantic search beyond keyword matching
5. **User Experience Enhancement**: Proactive assistance and contextual help

## Setting Up OpenAI SDK

First, install the OpenAI SDK:

```bash
npm install openai
```

### Configuring the OpenAI Client

```typescript
// lib/ai/openai.ts
import OpenAI from "openai";

// Create and export the OpenAI client
export const openai = new OpenAI({
	apiKey: process.env.OPENAI_API_KEY,
	organization: process.env.OPENAI_ORGANIZATION, // optional
});

// You can create helper functions for common operations
export async function generateText(
	prompt: string,
	options?: { max_tokens?: number; temperature?: number }
) {
	try {
		const response = await openai.chat.completions.create({
			model: "gpt-4o",
			messages: [{ role: "user", content: prompt }],
			max_tokens: options?.max_tokens || 500,
			temperature: options?.temperature || 0.7,
		});

		return {
			success: true,
			text: response.choices[0].message.content,
		};
	} catch (error) {
		console.error("Error generating text:", error);
		return {
			success: false,
			error: error instanceof Error ? error.message : "Unknown error",
		};
	}
}
```

### Environment Variables Setup

Create or update your `.env.local` file:

```
OPENAI_API_KEY=your_api_key_here
OPENAI_ORGANIZATION=your_org_id_here  # Optional
```

Update your Next.js configuration to expose these variables to the server:

```typescript
// next.config.js
/** @type {import('next').NextConfig} */
const nextConfig = {
	// Your other configurations...
	env: {
		OPENAI_API_KEY: process.env.OPENAI_API_KEY,
		OPENAI_ORGANIZATION: process.env.OPENAI_ORGANIZATION,
	},
};

module.exports = nextConfig;
```

## Creating AI-Powered API Endpoints

### Text Generation Endpoint

```typescript
// app/api/ai/generate/route.ts
import { openai } from "@/lib/ai/openai";
import { NextResponse } from "next/server";

export async function POST(request: Request) {
	try {
		const { prompt, maxTokens = 500, temperature = 0.7 } = await request.json();

		if (!prompt) {
			return NextResponse.json(
				{ error: "Prompt is required" },
				{ status: 400 }
			);
		}

		const response = await openai.chat.completions.create({
			model: "gpt-4o",
			messages: [{ role: "user", content: prompt }],
			max_tokens: maxTokens,
			temperature: temperature,
		});

		return NextResponse.json({
			text: response.choices[0].message.content,
		});
	} catch (error) {
		console.error("OpenAI API error:", error);
		return NextResponse.json(
			{ error: "Failed to generate text" },
			{ status: 500 }
		);
	}
}
```

### AI Text Completion Component

```tsx
// components/client/AiTextCompletion.tsx
"use client";

import { useState } from "react";
import { Button } from "@/components/ui/Button";

export default function AiTextCompletion() {
	const [prompt, setPrompt] = useState("");
	const [result, setResult] = useState("");
	const [loading, setLoading] = useState(false);
	const [error, setError] = useState<string | null>(null);

	async function handleSubmit(e: React.FormEvent) {
		e.preventDefault();
		setLoading(true);
		setError(null);

		try {
			const response = await fetch("/api/ai/generate", {
				method: "POST",
				headers: {
					"Content-Type": "application/json",
				},
				body: JSON.stringify({ prompt }),
			});

			if (!response.ok) {
				throw new Error("Failed to generate text");
			}

			const data = await response.json();
			setResult(data.text);
		} catch (err) {
			setError(err instanceof Error ? err.message : "An error occurred");
		} finally {
			setLoading(false);
		}
	}

	return (
		<div className="max-w-2xl mx-auto p-6 bg-white dark:bg-gray-800 rounded-lg shadow-md">
			<h2 className="text-2xl font-bold mb-4">AI Text Completion</h2>

			<form onSubmit={handleSubmit} className="space-y-4">
				<div>
					<label htmlFor="prompt" className="block text-sm font-medium mb-1">
						Enter a prompt:
					</label>
					<textarea
						id="prompt"
						value={prompt}
						onChange={(e) => setPrompt(e.target.value)}
						className="w-full rounded-md border border-gray-300 dark:border-gray-700 p-3 min-h-24"
						placeholder="Describe a futuristic city in the year 2150..."
						required
					/>
				</div>

				<Button type="submit" isLoading={loading}>
					Generate
				</Button>
			</form>

			{error && (
				<div className="mt-4 p-3 bg-red-100 dark:bg-red-900/30 text-red-700 dark:text-red-300 rounded-md">
					{error}
				</div>
			)}

			{result && (
				<div className="mt-6">
					<h3 className="text-lg font-medium mb-2">Generated Text:</h3>
					<div className="bg-gray-100 dark:bg-gray-900 p-4 rounded-md whitespace-pre-wrap">
						{result}
					</div>
				</div>
			)}
		</div>
	);
}
```

## Server-Side AI Processing

For AI processing that requires access to data or environment variables only available on the server:

```typescript
// app/api/ai/summarize/route.ts
import { openai } from "@/lib/ai/openai";
import { NextResponse } from "next/server";
import { getArticleById } from "@/lib/api"; // Hypothetical function to get article data

export async function GET(request: Request) {
	const url = new URL(request.url);
	const articleId = url.searchParams.get("articleId");

	if (!articleId) {
		return NextResponse.json(
			{ error: "Article ID is required" },
			{ status: 400 }
		);
	}

	try {
		// Get article data from database or API
		const article = await getArticleById(articleId);

		if (!article) {
			return NextResponse.json({ error: "Article not found" }, { status: 404 });
		}

		// Generate summary with OpenAI
		const response = await openai.chat.completions.create({
			model: "gpt-4o",
			messages: [
				{
					role: "system",
					content:
						"You are a helpful assistant that summarizes articles concisely.",
				},
				{
					role: "user",
					content: `Summarize the following article in 3-4 sentences: ${article.title}\n\n${article.content}`,
				},
			],
			max_tokens: 150,
			temperature: 0.5,
		});

		return NextResponse.json({
			summary: response.choices[0].message.content,
			articleTitle: article.title,
		});
	} catch (error) {
		console.error("Error summarizing article:", error);
		return NextResponse.json(
			{ error: "Failed to summarize article" },
			{ status: 500 }
		);
	}
}
```

## Implementing AI-Powered UX Features

### Smart Search Suggestions

```tsx
// components/client/SmartSearchBar.tsx
"use client";

import { useState, useEffect } from "react";
import { useDebounce } from "@/lib/hooks/useDebounce";

export default function SmartSearchBar() {
	const [query, setQuery] = useState("");
	const [suggestions, setSuggestions] = useState<string[]>([]);
	const [loading, setLoading] = useState(false);

	// Debounce the search query to avoid too many API calls
	const debouncedQuery = useDebounce(query, 500);

	useEffect(() => {
		async function fetchSuggestions() {
			if (debouncedQuery.length < 3) {
				setSuggestions([]);
				return;
			}

			setLoading(true);

			try {
				const response = await fetch(
					`/api/ai/search-suggestions?query=${encodeURIComponent(
						debouncedQuery
					)}`
				);

				if (!response.ok) {
					throw new Error("Failed to fetch suggestions");
				}

				const data = await response.json();
				setSuggestions(data.suggestions);
			} catch (error) {
				console.error("Error fetching suggestions:", error);
				setSuggestions([]);
			} finally {
				setLoading(false);
			}
		}

		fetchSuggestions();
	}, [debouncedQuery]);

	return (
		<div className="relative max-w-md mx-auto">
			<div className="relative">
				<input
					type="text"
					value={query}
					onChange={(e) => setQuery(e.target.value)}
					className="w-full px-4 py-2 pr-10 border border-gray-300 dark:border-gray-700 rounded-md focus:outline-none focus:ring-2 focus:ring-primary-500"
					placeholder="Search..."
				/>
				{loading && (
					<div className="absolute right-3 top-2.5">
						<div className="animate-spin h-5 w-5 border-2 border-gray-500 rounded-full border-t-transparent"></div>
					</div>
				)}
			</div>

			{suggestions.length > 0 && (
				<div className="absolute w-full mt-1 bg-white dark:bg-gray-800 shadow-lg rounded-md border border-gray-200 dark:border-gray-700 z-10">
					<ul className="py-1">
						{suggestions.map((suggestion, index) => (
							<li
								key={index}
								className="px-4 py-2 hover:bg-gray-100 dark:hover:bg-gray-700 cursor-pointer"
								onClick={() => {
									setQuery(suggestion);
									setSuggestions([]);
								}}
							>
								{suggestion}
							</li>
						))}
					</ul>
				</div>
			)}
		</div>
	);
}
```

### AI-Powered Content Generator

```tsx
// components/client/ContentGenerator.tsx
"use client";

import { useState } from "react";
import { Button } from "@/components/ui/Button";

// Types for the form fields
interface FormState {
	topic: string;
	tone: "professional" | "casual" | "humorous";
	length: "short" | "medium" | "long";
}

export default function ContentGenerator() {
	const [form, setForm] = useState<FormState>({
		topic: "",
		tone: "professional",
		length: "medium",
	});

	const [content, setContent] = useState("");
	const [loading, setLoading] = useState(false);

	function handleChange(
		e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>
	) {
		const { name, value } = e.target;
		setForm((prev) => ({ ...prev, [name]: value }));
	}

	async function handleSubmit(e: React.FormEvent) {
		e.preventDefault();
		setLoading(true);

		try {
			const response = await fetch("/api/ai/generate-content", {
				method: "POST",
				headers: {
					"Content-Type": "application/json",
				},
				body: JSON.stringify(form),
			});

			if (!response.ok) {
				throw new Error("Failed to generate content");
			}

			const data = await response.json();
			setContent(data.content);
		} catch (error) {
			console.error("Error generating content:", error);
		} finally {
			setLoading(false);
		}
	}

	return (
		<div className="max-w-3xl mx-auto p-6 bg-white dark:bg-gray-800 rounded-lg shadow-md">
			<h2 className="text-2xl font-bold mb-6">AI Content Generator</h2>

			<form onSubmit={handleSubmit} className="space-y-4">
				<div>
					<label htmlFor="topic" className="block text-sm font-medium mb-1">
						Topic:
					</label>
					<input
						id="topic"
						name="topic"
						value={form.topic}
						onChange={handleChange}
						className="w-full rounded-md border border-gray-300 dark:border-gray-700 p-2"
						placeholder="E.g., Benefits of React Server Components"
						required
					/>
				</div>

				<div className="grid grid-cols-1 md:grid-cols-2 gap-4">
					<div>
						<label htmlFor="tone" className="block text-sm font-medium mb-1">
							Tone:
						</label>
						<select
							id="tone"
							name="tone"
							value={form.tone}
							onChange={handleChange}
							className="w-full rounded-md border border-gray-300 dark:border-gray-700 p-2"
						>
							<option value="professional">Professional</option>
							<option value="casual">Casual</option>
							<option value="humorous">Humorous</option>
						</select>
					</div>

					<div>
						<label htmlFor="length" className="block text-sm font-medium mb-1">
							Length:
						</label>
						<select
							id="length"
							name="length"
							value={form.length}
							onChange={handleChange}
							className="w-full rounded-md border border-gray-300 dark:border-gray-700 p-2"
						>
							<option value="short">Short (100-200 words)</option>
							<option value="medium">Medium (300-500 words)</option>
							<option value="long">Long (600-800 words)</option>
						</select>
					</div>
				</div>

				<Button type="submit" isLoading={loading} className="mt-2">
					Generate Content
				</Button>
			</form>

			{content && (
				<div className="mt-8">
					<h3 className="text-lg font-medium mb-3">Generated Content:</h3>
					<div className="bg-gray-100 dark:bg-gray-900 p-6 rounded-md whitespace-pre-wrap">
						{content}
					</div>

					<div className="mt-4 flex justify-end">
						<Button
							variant="outline"
							onClick={() => navigator.clipboard.writeText(content)}
						>
							Copy to Clipboard
						</Button>
					</div>
				</div>
			)}
		</div>
	);
}
```

## Implementing Polling Agents for AI Responses

For long-running AI tasks, implement a polling mechanism:

### Server-Side Implementation

```typescript
// app/api/ai/tasks/route.ts
import { NextResponse } from "next/server";
import { openai } from "@/lib/ai/openai";
import { createId } from "@paralleldrive/cuid2";

// In-memory store for tasks (use Redis or a database in production)
const tasks = new Map<
	string,
	{
		status: "pending" | "processing" | "completed" | "failed";
		prompt: string;
		result?: string;
		error?: string;
		createdAt: Date;
	}
>();

export async function POST(request: Request) {
	try {
		const { prompt, model = "gpt-4o" } = await request.json();

		if (!prompt) {
			return NextResponse.json(
				{ error: "Prompt is required" },
				{ status: 400 }
			);
		}

		// Create a task ID
		const taskId = createId();

		// Store the task
		tasks.set(taskId, {
			status: "pending",
			prompt,
			createdAt: new Date(),
		});

		// Start processing in the background
		processTask(taskId, prompt, model);

		return NextResponse.json({ taskId });
	} catch (error) {
		console.error("Error creating task:", error);
		return NextResponse.json(
			{ error: "Failed to create task" },
			{ status: 500 }
		);
	}
}

export async function GET(request: Request) {
	const url = new URL(request.url);
	const taskId = url.searchParams.get("taskId");

	if (!taskId) {
		return NextResponse.json({ error: "Task ID is required" }, { status: 400 });
	}

	const task = tasks.get(taskId);

	if (!task) {
		return NextResponse.json({ error: "Task not found" }, { status: 404 });
	}

	return NextResponse.json({
		taskId,
		status: task.status,
		result: task.result,
		error: task.error,
	});
}

// Function to process the task asynchronously
async function processTask(taskId: string, prompt: string, model: string) {
	const task = tasks.get(taskId);

	if (!task) return;

	tasks.set(taskId, { ...task, status: "processing" });

	try {
		const response = await openai.chat.completions.create({
			model,
			messages: [{ role: "user", content: prompt }],
			max_tokens: 1000,
		});

		const result = response.choices[0].message.content;

		// Update task with result
		tasks.set(taskId, {
			...task,
			status: "completed",
			result,
		});

		// In a real app, you might want to clean up completed tasks after some time
		setTimeout(() => {
			if (tasks.has(taskId)) {
				tasks.delete(taskId);
			}
		}, 1000 * 60 * 10); // Delete after 10 minutes
	} catch (error) {
		console.error(`Error processing task ${taskId}:`, error);

		// Update task with error
		tasks.set(taskId, {
			...task,
			status: "failed",
			error: error instanceof Error ? error.message : "Unknown error",
		});
	}
}
```

### Client-Side Polling Component

```tsx
// components/client/AiTaskPoller.tsx
"use client";

import { useState, useEffect } from "react";
import { Button } from "@/components/ui/Button";

interface TaskState {
	taskId: string | null;
	status: "idle" | "pending" | "processing" | "completed" | "failed";
	result: string | null;
	error: string | null;
}

export default function AiTaskPoller() {
	const [prompt, setPrompt] = useState("");
	const [task, setTask] = useState<TaskState>({
		taskId: null,
		status: "idle",
		result: null,
		error: null,
	});

	// Function to create a new task
	async function createTask() {
		if (!prompt.trim()) return;

		setTask({
			taskId: null,
			status: "pending",
			result: null,
			error: null,
		});

		try {
			const response = await fetch("/api/ai/tasks", {
				method: "POST",
				headers: {
					"Content-Type": "application/json",
				},
				body: JSON.stringify({ prompt }),
			});

			if (!response.ok) {
				throw new Error("Failed to create task");
			}

			const data = await response.json();

			setTask((prev) => ({
				...prev,
				taskId: data.taskId,
				status: "processing",
			}));
		} catch (error) {
			setTask((prev) => ({
				...prev,
				status: "failed",
				error: error instanceof Error ? error.message : "Failed to create task",
			}));
		}
	}

	// Poll for task status
	useEffect(() => {
		if (task.status !== "processing" || !task.taskId) return;

		const pollInterval = setInterval(async () => {
			try {
				const response = await fetch(`/api/ai/tasks?taskId=${task.taskId}`);

				if (!response.ok) {
					throw new Error("Failed to fetch task status");
				}

				const data = await response.json();

				if (data.status === "completed") {
					setTask((prev) => ({
						...prev,
						status: "completed",
						result: data.result,
					}));
					clearInterval(pollInterval);
				} else if (data.status === "failed") {
					setTask((prev) => ({
						...prev,
						status: "failed",
						error: data.error || "Task processing failed",
					}));
					clearInterval(pollInterval);
				}
			} catch (error) {
				console.error("Error polling task:", error);
			}
		}, 2000); // Poll every 2 seconds

		return () => clearInterval(pollInterval);
	}, [task.status, task.taskId]);

	function handleReset() {
		setTask({
			taskId: null,
			status: "idle",
			result: null,
			error: null,
		});
	}

	return (
		<div className="max-w-2xl mx-auto p-6 bg-white dark:bg-gray-800 rounded-lg shadow-md">
			<h2 className="text-2xl font-bold mb-4">AI Task Processing</h2>

			<div className="space-y-4">
				<div>
					<label htmlFor="prompt" className="block text-sm font-medium mb-1">
						Enter your prompt:
					</label>
					<textarea
						id="prompt"
						value={prompt}
						onChange={(e) => setPrompt(e.target.value)}
						className="w-full rounded-md border border-gray-300 dark:border-gray-700 p-3 min-h-24"
						placeholder="Ask the AI to generate something complex..."
						disabled={task.status === "processing" || task.status === "pending"}
					/>
				</div>

				<div className="flex gap-2">
					<Button
						onClick={createTask}
						disabled={
							!prompt.trim() ||
							task.status === "processing" ||
							task.status === "pending"
						}
					>
						Generate with AI
					</Button>

					{(task.status === "completed" || task.status === "failed") && (
						<Button variant="outline" onClick={handleReset}>
							Reset
						</Button>
					)}
				</div>

				{task.status === "pending" && (
					<div className="text-gray-600 dark:text-gray-400">
						Initializing task...
					</div>
				)}

				{task.status === "processing" && (
					<div className="flex items-center space-x-2 text-blue-600 dark:text-blue-400">
						<div className="animate-spin h-4 w-4 border-2 border-current rounded-full border-t-transparent"></div>
						<span>Processing your request...</span>
					</div>
				)}

				{task.status === "failed" && (
					<div className="p-4 bg-red-100 dark:bg-red-900/30 text-red-700 dark:text-red-300 rounded-md">
						Error: {task.error}
					</div>
				)}

				{task.status === "completed" && task.result && (
					<div className="mt-6">
						<h3 className="text-lg font-medium mb-2">Result:</h3>
						<div className="bg-gray-100 dark:bg-gray-900 p-4 rounded-md whitespace-pre-wrap">
							{task.result}
						</div>
					</div>
				)}
			</div>
		</div>
	);
}
```

## AI-Enhanced Search with Embeddings

For more advanced AI features like semantic search, you can use embeddings:

```typescript
// app/api/ai/vector-search/route.ts
import { openai } from "@/lib/ai/openai";
import { NextResponse } from "next/server";
import { getDocuments, calculateCosineSimilarity } from "@/lib/db"; // Hypothetical helper functions

export async function GET(request: Request) {
	const url = new URL(request.url);
	const query = url.searchParams.get("query");

	if (!query) {
		return NextResponse.json({ error: "Query is required" }, { status: 400 });
	}

	try {
		// Generate embedding for the query
		const embeddingResponse = await openai.embeddings.create({
			model: "text-embedding-ada-002",
			input: query,
		});

		const queryEmbedding = embeddingResponse.data[0].embedding;

		// Fetch documents (in a real app, these would be pre-embedded and stored in a vector DB)
		const documents = await getDocuments();

		// Find documents with similar embeddings
		const results = documents
			.map((doc) => ({
				...doc,
				similarity: calculateCosineSimilarity(queryEmbedding, doc.embedding),
			}))
			.filter((doc) => doc.similarity > 0.7) // Threshold
			.sort((a, b) => b.similarity - a.similarity)
			.slice(0, 5); // Top 5 results

		return NextResponse.json({ results });
	} catch (error) {
		console.error("Error performing vector search:", error);
		return NextResponse.json(
			{ error: "Failed to perform search" },
			{ status: 500 }
		);
	}
}
```

## Key Takeaways

1. **Start with Server-Side AI** to keep API keys secure and leverage server computing power.

2. **Implement Async Processing** with polling for long-running AI tasks to avoid timeouts.

3. **Add Contextual AI Features** that enhance rather than replace your application's core functionality.

4. **Consider User Experience** by providing appropriate loading states and error handling for AI features.

5. **Protect Your API Keys** by handling sensitive operations on the server and never exposing keys on the client.

In the next section, we'll explore backend integration with tRPC and Prisma for a fully type-safe application.
