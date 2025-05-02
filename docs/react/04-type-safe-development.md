# Type-Safe Development

This section covers modern type-safe development practices for React applications, focusing on TypeScript, Zod for schema validation, and building robust forms with react-hook-form.

## Understanding TypeScript in React

TypeScript provides static type checking for JavaScript, catching errors early in development rather than at runtime. In React applications, TypeScript helps ensure component props are used correctly, state is managed properly, and API responses match expected formats.

### Key Benefits of TypeScript

1. **Better Developer Experience**: Autocompletion, inline documentation, and type checking
2. **Enhanced Code Quality**: Catch bugs early during development
3. **Safer Refactoring**: TypeScript validates changes across your codebase
4. **Self-Documenting Code**: Types serve as documentation for functions and components
5. **Better Team Collaboration**: Easier to understand others' code through type definitions

## TypeScript Fundamentals for React

### Basic Types

```typescript
// Basic types
const isActive: boolean = true;
const count: number = 42;
const name: string = "John Doe";
const items: string[] = ["apple", "banana", "orange"];
const user: { id: number; name: string } = { id: 1, name: "Alice" };

// Union types
type Status = "pending" | "success" | "error";
const requestStatus: Status = "pending";

// Optional properties
type User = {
	id: number;
	name: string;
	email?: string; // Optional email
};
```

### Type vs Interface

```typescript
// Interface (can be extended, good for objects)
interface User {
	id: number;
	name: string;
}

interface AdminUser extends User {
	permissions: string[];
}

// Type (more flexible, can use unions and intersections)
type ID = string | number;

type CommonFields = {
	createdAt: Date;
	updatedAt: Date;
};

type Post = CommonFields & {
	id: ID;
	title: string;
	content: string;
};
```

## Typing React Components

### Function Component with Props

```tsx
// Basic component with typed props
interface ButtonProps {
	text: string;
	onClick: () => void;
	variant?: "primary" | "secondary" | "outline";
	disabled?: boolean;
}

function Button({
	text,
	onClick,
	variant = "primary",
	disabled = false,
}: ButtonProps) {
	return (
		<button
			onClick={onClick}
			disabled={disabled}
			className={`btn btn-${variant}`}
		>
			{text}
		</button>
	);
}
```

### Using React's Built-in Types

```tsx
import { ReactNode, MouseEvent, ChangeEvent } from "react";

interface CardProps {
	title: string;
	children: ReactNode; // Can accept any valid JSX
	onClose?: (e: MouseEvent<HTMLButtonElement>) => void;
}

function Card({ title, children, onClose }: CardProps) {
	return (
		<div className="border rounded-lg p-4">
			<div className="flex justify-between items-center mb-4">
				<h2 className="text-xl font-bold">{title}</h2>
				{onClose && (
					<button onClick={onClose} className="text-gray-500">
						&times;
					</button>
				)}
			</div>
			<div>{children}</div>
		</div>
	);
}
```

### Typing Hooks

```tsx
import { useState, useEffect } from "react";

interface User {
	id: number;
	name: string;
	email: string;
}

function UserProfile({ userId }: { userId: number }) {
	// Type the state
	const [user, setUser] = useState<User | null>(null);
	const [loading, setLoading] = useState<boolean>(true);
	const [error, setError] = useState<string | null>(null);

	useEffect(() => {
		async function fetchUser() {
			try {
				setLoading(true);
				const response = await fetch(`/api/users/${userId}`);

				if (!response.ok) {
					throw new Error("Failed to fetch user");
				}

				const userData: User = await response.json();
				setUser(userData);
			} catch (err) {
				setError(
					err instanceof Error ? err.message : "An unknown error occurred"
				);
			} finally {
				setLoading(false);
			}
		}

		fetchUser();
	}, [userId]);

	if (loading) return <div>Loading...</div>;
	if (error) return <div>Error: {error}</div>;
	if (!user) return <div>User not found</div>;

	return (
		<div>
			<h1>{user.name}</h1>
			<p>{user.email}</p>
		</div>
	);
}
```

## Typing API Responses

When working with APIs, define types for your API responses:

```typescript
// types/api.ts
export interface ApiResponse<T> {
	data: T;
	meta: {
		total: number;
		page: number;
		limit: number;
	};
}

export interface User {
	id: number;
	name: string;
	email: string;
	role: "user" | "admin";
	createdAt: string;
}

export interface Post {
	id: number;
	title: string;
	content: string;
	authorId: number;
	published: boolean;
	tags: string[];
	createdAt: string;
	updatedAt: string;
}

// Using these types
async function fetchUsers(): Promise<ApiResponse<User[]>> {
	const response = await fetch("/api/users");
	return response.json();
}

async function fetchPosts(userId: number): Promise<ApiResponse<Post[]>> {
	const response = await fetch(`/api/users/${userId}/posts`);
	return response.json();
}
```

## Zod for Runtime Validation

While TypeScript provides static type checking during development, Zod enables runtime validation, ensuring data conforms to your expected schema at runtime.

### Installing Zod

```bash
npm install zod
```

### Basic Zod Schema

```typescript
import { z } from "zod";

// Define a schema for user data
const userSchema = z.object({
	id: z.number(),
	name: z.string().min(2).max(50),
	email: z.string().email(),
	age: z.number().int().positive().optional(),
	role: z.enum(["user", "admin", "editor"]),
	settings: z.object({
		newsletter: z.boolean(),
		theme: z.enum(["light", "dark", "system"]).default("system"),
	}),
	tags: z.array(z.string()),
});

// Infer TypeScript type from Zod schema
type User = z.infer<typeof userSchema>;

// Validate data at runtime
function processUserData(data: unknown): User {
	// This will throw if validation fails
	const validatedUser = userSchema.parse(data);
	return validatedUser;
}

// Safe parsing that doesn't throw
function safeProcessUserData(data: unknown) {
	const result = userSchema.safeParse(data);

	if (result.success) {
		// result.data is typed as User
		return { success: true, data: result.data };
	} else {
		// result.error contains validation errors
		return { success: false, errors: result.error.format() };
	}
}
```

### Advanced Zod Features

```typescript
import { z } from "zod";

// Custom error messages
const passwordSchema = z
	.string()
	.min(8, "Password must be at least 8 characters")
	.max(100, "Password too long")
	.regex(/[A-Z]/, "Need at least one uppercase letter")
	.regex(/[a-z]/, "Need at least one lowercase letter")
	.regex(/[0-9]/, "Need at least one number");

// Transformations
const userInputSchema = z.object({
	name: z.string().transform((val) => val.trim()),
	email: z.string().email().toLowerCase(),
	birthYear: z.number().transform((year) => new Date().getFullYear() - year), // Calculate age
});

// Optional fields with defaults
const configSchema = z.object({
	theme: z.enum(["light", "dark"]).default("light"),
	notificationsEnabled: z.boolean().default(true),
	itemsPerPage: z.number().int().positive().default(10),
});

// Union types
const responseSchema = z.union([
	z.object({ status: z.literal("success"), data: z.any() }),
	z.object({ status: z.literal("error"), message: z.string() }),
]);
```

## Type-Safe API Routes

For Next.js API routes, ensure type safety:

```typescript
// app/api/users/route.ts
import { z } from "zod";
import { NextResponse } from "next/server";

// Input validation schema
const createUserSchema = z.object({
	name: z.string().min(2),
	email: z.string().email(),
	password: z.string().min(8),
});

export async function POST(request: Request) {
	try {
		// Parse request body as JSON
		const body = await request.json();

		// Validate against schema
		const validatedData = createUserSchema.parse(body);

		// Process the valid data
		// ... (create user in database)

		return NextResponse.json({ success: true, message: "User created" });
	} catch (error) {
		if (error instanceof z.ZodError) {
			// Return validation errors
			return NextResponse.json(
				{ success: false, errors: error.format() },
				{ status: 400 }
			);
		}

		// Handle other errors
		return NextResponse.json(
			{ success: false, message: "Internal server error" },
			{ status: 500 }
		);
	}
}
```

## Building Type-Safe Forms with React Hook Form

React Hook Form combined with Zod provides a powerful solution for building type-safe forms with validation.

### Setting Up a Type-Safe Form

```tsx
// app/register/page.tsx
"use client";

import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { z } from "zod";

// Define form schema with Zod
const registerSchema = z
	.object({
		name: z.string().min(2, "Name must be at least 2 characters"),
		email: z.string().email("Please enter a valid email"),
		password: z
			.string()
			.min(8, "Password must be at least 8 characters")
			.regex(/[A-Z]/, "Password must contain at least one uppercase letter")
			.regex(/[0-9]/, "Password must contain at least one number"),
		confirmPassword: z.string(),
	})
	.refine((data) => data.password === data.confirmPassword, {
		message: "Passwords do not match",
		path: ["confirmPassword"],
	});

// Infer form data type from schema
type RegisterFormData = z.infer<typeof registerSchema>;

export default function RegisterPage() {
	const {
		register,
		handleSubmit,
		formState: { errors, isSubmitting },
		reset,
	} = useForm<RegisterFormData>({
		resolver: zodResolver(registerSchema),
		defaultValues: {
			name: "",
			email: "",
			password: "",
			confirmPassword: "",
		},
	});

	async function onSubmit(data: RegisterFormData) {
		try {
			// Submit data to API
			const response = await fetch("/api/register", {
				method: "POST",
				headers: { "Content-Type": "application/json" },
				body: JSON.stringify(data),
			});

			if (!response.ok) {
				throw new Error("Registration failed");
			}

			// Reset form on success
			reset();
			alert("Registration successful!");
		} catch (error) {
			console.error(error);
			alert(error instanceof Error ? error.message : "Something went wrong");
		}
	}

	return (
		<div className="max-w-md mx-auto p-6">
			<h1 className="text-2xl font-bold mb-6">Register</h1>

			<form onSubmit={handleSubmit(onSubmit)} className="space-y-4">
				<div>
					<label htmlFor="name" className="block text-sm font-medium mb-1">
						Name
					</label>
					<input
						id="name"
						type="text"
						className="w-full rounded-md border-gray-300 shadow-sm"
						{...register("name")}
					/>
					{errors.name && (
						<p className="mt-1 text-sm text-red-600">{errors.name.message}</p>
					)}
				</div>

				<div>
					<label htmlFor="email" className="block text-sm font-medium mb-1">
						Email
					</label>
					<input
						id="email"
						type="email"
						className="w-full rounded-md border-gray-300 shadow-sm"
						{...register("email")}
					/>
					{errors.email && (
						<p className="mt-1 text-sm text-red-600">{errors.email.message}</p>
					)}
				</div>

				<div>
					<label htmlFor="password" className="block text-sm font-medium mb-1">
						Password
					</label>
					<input
						id="password"
						type="password"
						className="w-full rounded-md border-gray-300 shadow-sm"
						{...register("password")}
					/>
					{errors.password && (
						<p className="mt-1 text-sm text-red-600">
							{errors.password.message}
						</p>
					)}
				</div>

				<div>
					<label
						htmlFor="confirmPassword"
						className="block text-sm font-medium mb-1"
					>
						Confirm Password
					</label>
					<input
						id="confirmPassword"
						type="password"
						className="w-full rounded-md border-gray-300 shadow-sm"
						{...register("confirmPassword")}
					/>
					{errors.confirmPassword && (
						<p className="mt-1 text-sm text-red-600">
							{errors.confirmPassword.message}
						</p>
					)}
				</div>

				<button
					type="submit"
					disabled={isSubmitting}
					className="w-full bg-blue-500 hover:bg-blue-600 text-white py-2 px-4 rounded-md disabled:opacity-50"
				>
					{isSubmitting ? "Registering..." : "Register"}
				</button>
			</form>
		</div>
	);
}
```

## Creating Reusable Form Components

To avoid repetition, create reusable form components:

```tsx
// components/forms/FormField.tsx
"use client";

import { RegisterOptions, useFormContext } from "react-hook-form";

interface FormFieldProps {
	name: string;
	label: string;
	type?: string;
	placeholder?: string;
	registerOptions?: RegisterOptions;
}

export default function FormField({
	name,
	label,
	type = "text",
	placeholder = "",
	registerOptions = {},
}: FormFieldProps) {
	const {
		register,
		formState: { errors },
	} = useFormContext();

	const error = errors[name];

	return (
		<div className="mb-4">
			<label htmlFor={name} className="block text-sm font-medium mb-1">
				{label}
			</label>
			<input
				id={name}
				type={type}
				placeholder={placeholder}
				className={`w-full rounded-md border ${
					error ? "border-red-500" : "border-gray-300"
				} shadow-sm px-3 py-2`}
				{...register(name, registerOptions)}
			/>
			{error && (
				<p className="mt-1 text-sm text-red-600">{error.message as string}</p>
			)}
		</div>
	);
}
```

Using the reusable form components:

```tsx
// app/contact/page.tsx
"use client";

import { useForm, FormProvider } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { z } from "zod";
import FormField from "@/components/forms/FormField";

const contactSchema = z.object({
	name: z.string().min(2, "Name is required"),
	email: z.string().email("Please enter a valid email"),
	subject: z.string().min(5, "Subject is required"),
	message: z.string().min(10, "Message is too short"),
});

type ContactFormData = z.infer<typeof contactSchema>;

export default function ContactPage() {
	const methods = useForm<ContactFormData>({
		resolver: zodResolver(contactSchema),
	});

	const onSubmit = async (data: ContactFormData) => {
		// Form submission logic
		console.log(data);
	};

	return (
		<div className="max-w-md mx-auto p-6">
			<h1 className="text-2xl font-bold mb-6">Contact Us</h1>

			<FormProvider {...methods}>
				<form onSubmit={methods.handleSubmit(onSubmit)} className="space-y-4">
					<FormField name="name" label="Name" placeholder="Your name" />

					<FormField
						name="email"
						label="Email"
						type="email"
						placeholder="your.email@example.com"
					/>

					<FormField
						name="subject"
						label="Subject"
						placeholder="What's this about?"
					/>

					<div className="mb-4">
						<label htmlFor="message" className="block text-sm font-medium mb-1">
							Message
						</label>
						<textarea
							id="message"
							className="w-full rounded-md border border-gray-300 shadow-sm px-3 py-2"
							rows={4}
							placeholder="Your message"
							{...methods.register("message")}
						/>
						{methods.formState.errors.message && (
							<p className="mt-1 text-sm text-red-600">
								{methods.formState.errors.message.message}
							</p>
						)}
					</div>

					<button
						type="submit"
						disabled={methods.formState.isSubmitting}
						className="w-full bg-blue-500 hover:bg-blue-600 text-white py-2 px-4 rounded-md disabled:opacity-50"
					>
						{methods.formState.isSubmitting ? "Sending..." : "Send Message"}
					</button>
				</form>
			</FormProvider>
		</div>
	);
}
```

## Working with Dynamic Forms

For forms with dynamic fields:

```tsx
// components/forms/DynamicForm.tsx
"use client";

import { useFieldArray, useForm, FormProvider } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { z } from "zod";
import FormField from "@/components/forms/FormField";

const itemSchema = z.object({
	name: z.string().min(1, "Name is required"),
	quantity: z.number().int().positive("Quantity must be positive"),
});

const orderSchema = z.object({
	customerName: z.string().min(2, "Customer name is required"),
	email: z.string().email("Valid email is required"),
	items: z.array(itemSchema).min(1, "Add at least one item"),
});

type OrderFormData = z.infer<typeof orderSchema>;

export default function DynamicOrderForm() {
	const methods = useForm<OrderFormData>({
		resolver: zodResolver(orderSchema),
		defaultValues: {
			customerName: "",
			email: "",
			items: [{ name: "", quantity: 1 }],
		},
	});

	const { fields, append, remove } = useFieldArray({
		control: methods.control,
		name: "items",
	});

	const onSubmit = (data: OrderFormData) => {
		console.log("Order data:", data);
		// Process order...
	};

	return (
		<div className="max-w-lg mx-auto p-6">
			<h1 className="text-2xl font-bold mb-6">Create Order</h1>

			<FormProvider {...methods}>
				<form onSubmit={methods.handleSubmit(onSubmit)} className="space-y-6">
					<FormField name="customerName" label="Customer Name" />

					<FormField name="email" label="Email" type="email" />

					<div>
						<h2 className="text-lg font-medium mb-3">Order Items</h2>

						{fields.map((field, index) => (
							<div key={field.id} className="flex gap-3 mb-3 items-end">
								<div className="flex-1">
									<label
										htmlFor={`items.${index}.name`}
										className="block text-sm font-medium mb-1"
									>
										Item Name
									</label>
									<input
										id={`items.${index}.name`}
										{...methods.register(`items.${index}.name` as const)}
										className="w-full rounded-md border border-gray-300 px-3 py-2"
									/>
									{methods.formState.errors.items?.[index]?.name && (
										<p className="text-sm text-red-600 mt-1">
											{methods.formState.errors.items[index]?.name?.message}
										</p>
									)}
								</div>

								<div className="w-24">
									<label
										htmlFor={`items.${index}.quantity`}
										className="block text-sm font-medium mb-1"
									>
										Qty
									</label>
									<input
										id={`items.${index}.quantity`}
										type="number"
										{...methods.register(`items.${index}.quantity` as const, {
											valueAsNumber: true,
										})}
										className="w-full rounded-md border border-gray-300 px-3 py-2"
									/>
									{methods.formState.errors.items?.[index]?.quantity && (
										<p className="text-sm text-red-600 mt-1">
											{methods.formState.errors.items[index]?.quantity?.message}
										</p>
									)}
								</div>

								<button
									type="button"
									onClick={() => remove(index)}
									className="bg-red-500 text-white p-2 rounded-md mb-1"
									disabled={fields.length === 1}
								>
									&times;
								</button>
							</div>
						))}

						{methods.formState.errors.items?.message && (
							<p className="text-sm text-red-600 mt-1">
								{methods.formState.errors.items.message}
							</p>
						)}

						<button
							type="button"
							onClick={() => append({ name: "", quantity: 1 })}
							className="mt-2 bg-gray-200 px-3 py-1 rounded-md text-sm"
						>
							Add Item
						</button>
					</div>

					<button
						type="submit"
						className="w-full bg-blue-500 hover:bg-blue-600 text-white py-2 px-4 rounded-md"
					>
						Submit Order
					</button>
				</form>
			</FormProvider>
		</div>
	);
}
```

## Server-Side Validation

For server actions, validate data using Zod:

```tsx
// app/contact/actions.ts
"use server";

import { z } from "zod";

const contactSchema = z.object({
	name: z.string().min(2, "Name is required"),
	email: z.string().email("Valid email is required"),
	message: z.string().min(10, "Message is too short"),
});

export async function submitContactForm(formData: FormData) {
	// Extract data from FormData
	const rawData = {
		name: formData.get("name"),
		email: formData.get("email"),
		message: formData.get("message"),
	};

	// Validate with Zod
	const result = contactSchema.safeParse(rawData);

	if (!result.success) {
		// Return validation errors
		return {
			success: false,
			errors: result.error.format(),
		};
	}

	// Process the validated data
	const validData = result.data;

	try {
		// Save to database, send email, etc.
		await saveContactMessage(validData);

		return {
			success: true,
			message: "Message sent successfully!",
		};
	} catch (error) {
		return {
			success: false,
			message: "Failed to send message. Please try again.",
		};
	}
}

// Example usage in a form component
// <form action={submitContactForm}>
//   ...form fields
// </form>
```

## Key Takeaways

1. **Use TypeScript** for static type checking to catch errors early in development.

2. **Combine TypeScript with Zod** for runtime validation to ensure data integrity.

3. **Create Type-Safe Forms** with react-hook-form and Zod to provide excellent user experience.

4. **Build Reusable Form Components** to reduce repetition and ensure consistency.

5. **Validate on Both Client and Server** to guarantee data integrity throughout your application.

In the next section, we'll explore styling modern React applications with Tailwind CSS and component libraries.
