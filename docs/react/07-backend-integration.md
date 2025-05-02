# Backend Integration

This section covers integrating backend services with your React application, with a focus on tRPC for end-to-end type safety, Prisma for database access, and effective API route patterns.

## End-to-End Type Safety

One of the major challenges in full-stack development is maintaining type safety between your frontend and backend. With modern tools like tRPC and Prisma, we can achieve a fully type-safe application stack.

### Benefits of End-to-End Type Safety

1. **Catch Errors Early**: Compiler flags type mismatches before runtime
2. **Improved Developer Experience**: Autocomplete and inline documentation for APIs
3. **Safer Refactoring**: Automatically detect breaking changes
4. **Reduced Need for Documentation**: Types serve as living documentation
5. **Faster Development**: Less time debugging type-related issues

## Setting Up tRPC

[tRPC](https://trpc.io/) enables you to create fully type-safe APIs without schemas or code generation.

### Installing tRPC Dependencies

```bash
npm install @trpc/server @trpc/client @trpc/react-query @trpc/next @tanstack/react-query zod
```

### Basic tRPC Setup

First, create a tRPC router:

```typescript
// src/server/trpc/router.ts
import { initTRPC } from "@trpc/server";
import { z } from "zod";

// Create a tRPC instance
const t = initTRPC.create();

// Define router and procedure helpers
export const router = t.router;
export const publicProcedure = t.procedure;

// Define a basic router with procedures
export const appRouter = router({
	hello: publicProcedure
		.input(z.object({ name: z.string() }).optional())
		.query(({ input }) => {
			return {
				greeting: `Hello ${input?.name ?? "World"}!`,
			};
		}),

	createUser: publicProcedure
		.input(
			z.object({
				name: z.string().min(2),
				email: z.string().email(),
			})
		)
		.mutation(async ({ input }) => {
			// In a real app, you would save to a database here
			console.log("Creating user:", input);

			return {
				id: "user-123",
				name: input.name,
				email: input.email,
				createdAt: new Date(),
			};
		}),
});

// Type definition for your API
export type AppRouter = typeof appRouter;
```

Set up a tRPC API handler in Next.js:

```typescript
// src/app/api/trpc/[trpc]/route.ts
import { fetchRequestHandler } from "@trpc/server/adapters/fetch";
import { appRouter } from "@/server/trpc/router";

const handler = (req: Request) =>
	fetchRequestHandler({
		endpoint: "/api/trpc",
		req,
		router: appRouter,
		createContext: () => ({}),
	});

export { handler as GET, handler as POST };
```

Create a client-side provider:

```tsx
// src/app/providers.tsx
"use client";

import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { httpBatchLink } from "@trpc/client";
import { useState } from "react";
import { trpc } from "@/lib/trpc";
import { ThemeProvider } from "next-themes";

export function Providers({ children }: { children: React.ReactNode }) {
	const [queryClient] = useState(() => new QueryClient());
	const [trpcClient] = useState(() =>
		trpc.createClient({
			links: [
				httpBatchLink({
					url: `${window.location.origin}/api/trpc`,
				}),
			],
		})
	);

	return (
		<trpc.Provider client={trpcClient} queryClient={queryClient}>
			<QueryClientProvider client={queryClient}>
				<ThemeProvider attribute="class" defaultTheme="system" enableSystem>
					{children}
				</ThemeProvider>
			</QueryClientProvider>
		</trpc.Provider>
	);
}
```

Set up the tRPC client:

```typescript
// src/lib/trpc.ts
import { createTRPCReact } from "@trpc/react-query";
import type { AppRouter } from "@/server/trpc/router";

export const trpc = createTRPCReact<AppRouter>();
```

Update your layout to use the providers:

```tsx
// src/app/layout.tsx
import { Providers } from "./providers";

export default function RootLayout({
	children,
}: {
	children: React.ReactNode;
}) {
	return (
		<html lang="en" suppressHydrationWarning>
			<body>
				<Providers>{children}</Providers>
			</body>
		</html>
	);
}
```

### Using tRPC in Components

```tsx
// src/app/trpc-example/page.tsx
"use client";

import { useState } from "react";
import { trpc } from "@/lib/trpc";

export default function TrpcExamplePage() {
	const [name, setName] = useState("");

	// Use the tRPC query (fully typed!)
	const hello = trpc.hello.useQuery({ name: name || undefined });

	// Use the tRPC mutation
	const createUser = trpc.createUser.useMutation({
		onSuccess: (data) => {
			console.log("User created:", data);
			alert(`User created with ID: ${data.id}`);
		},
	});

	return (
		<div className="max-w-md mx-auto p-6">
			<h1 className="text-2xl font-bold mb-6">tRPC Example</h1>

			<div className="mb-8">
				<h2 className="text-lg font-semibold mb-2">Greeting</h2>
				<div className="flex gap-2 mb-4">
					<input
						type="text"
						value={name}
						onChange={(e) => setName(e.target.value)}
						placeholder="Enter your name"
						className="px-3 py-2 border rounded-md flex-1"
					/>
				</div>

				{hello.isLoading ? (
					<p>Loading...</p>
				) : hello.error ? (
					<p className="text-red-500">Error: {hello.error.message}</p>
				) : (
					<p>{hello.data?.greeting}</p>
				)}
			</div>

			<div>
				<h2 className="text-lg font-semibold mb-2">Create User</h2>
				<form
					onSubmit={(e) => {
						e.preventDefault();
						const formData = new FormData(e.currentTarget);
						createUser.mutate({
							name: formData.get("name") as string,
							email: formData.get("email") as string,
						});
					}}
					className="space-y-4"
				>
					<div>
						<label htmlFor="name" className="block text-sm mb-1">
							Name
						</label>
						<input
							id="name"
							name="name"
							type="text"
							required
							className="px-3 py-2 border rounded-md w-full"
						/>
					</div>

					<div>
						<label htmlFor="email" className="block text-sm mb-1">
							Email
						</label>
						<input
							id="email"
							name="email"
							type="email"
							required
							className="px-3 py-2 border rounded-md w-full"
						/>
					</div>

					<button
						type="submit"
						disabled={createUser.isLoading}
						className="bg-blue-500 text-white px-4 py-2 rounded-md hover:bg-blue-600 disabled:opacity-50"
					>
						{createUser.isLoading ? "Creating..." : "Create User"}
					</button>
				</form>
			</div>
		</div>
	);
}
```

## Setting Up Prisma

[Prisma](https://www.prisma.io/) is a modern ORM for TypeScript that provides a type-safe API for your database.

### Installing Prisma

```bash
npm install prisma @prisma/client
npx prisma init
```

### Defining Your Schema

Edit your `prisma/schema.prisma` file:

```prisma
// prisma/schema.prisma
generator client {
  provider = "prisma-client-js"
}

datasource db {
  provider = "postgresql" // or "mysql", "sqlite"
  url      = env("DATABASE_URL")
}

model User {
  id        String   @id @default(cuid())
  email     String   @unique
  name      String?
  password  String
  createdAt DateTime @default(now())
  updatedAt DateTime @updatedAt
  posts     Post[]
}

model Post {
  id        String   @id @default(cuid())
  title     String
  content   String?
  published Boolean  @default(false)
  createdAt DateTime @default(now())
  updatedAt DateTime @updatedAt
  author    User     @relation(fields: [authorId], references: [id])
  authorId  String
}
```

### Setting Up the Prisma Client

Create a singleton instance of the Prisma client:

```typescript
// src/lib/db.ts
import { PrismaClient } from "@prisma/client";

// PrismaClient is attached to the `global` object in development to prevent
// exhausting your database connection limit.
const globalForPrisma = global as unknown as { prisma: PrismaClient };

export const prisma =
	globalForPrisma.prisma ||
	new PrismaClient({
		log:
			process.env.NODE_ENV === "development"
				? ["query", "error", "warn"]
				: ["error"],
	});

if (process.env.NODE_ENV !== "production") globalForPrisma.prisma = prisma;
```

### Integrating Prisma with tRPC

Update your tRPC router to use Prisma:

```typescript
// src/server/trpc/router.ts
import { initTRPC } from "@trpc/server";
import { z } from "zod";
import { prisma } from "@/lib/db";

// Create a tRPC instance
const t = initTRPC.create();

// Define router and procedure helpers
export const router = t.router;
export const publicProcedure = t.procedure;

// Define a basic router with procedures
export const appRouter = router({
	getUsers: publicProcedure.query(async () => {
		return prisma.user.findMany({
			select: {
				id: true,
				name: true,
				email: true,
				createdAt: true,
			},
		});
	}),

	getUserById: publicProcedure
		.input(z.object({ id: z.string() }))
		.query(async ({ input }) => {
			return prisma.user.findUnique({
				where: { id: input.id },
				select: {
					id: true,
					name: true,
					email: true,
					posts: {
						select: {
							id: true,
							title: true,
							published: true,
						},
					},
					createdAt: true,
				},
			});
		}),

	createUser: publicProcedure
		.input(
			z.object({
				name: z.string().min(2),
				email: z.string().email(),
				password: z.string().min(8),
			})
		)
		.mutation(async ({ input }) => {
			// In a real app, you would hash the password here
			return prisma.user.create({
				data: input,
				select: {
					id: true,
					name: true,
					email: true,
					createdAt: true,
				},
			});
		}),

	createPost: publicProcedure
		.input(
			z.object({
				title: z.string().min(1),
				content: z.string().optional(),
				authorId: z.string(),
				published: z.boolean().default(false),
			})
		)
		.mutation(async ({ input }) => {
			return prisma.post.create({
				data: input,
			});
		}),
});

// Type definition for your API
export type AppRouter = typeof appRouter;
```

## Creating a More Complex tRPC Router

For larger applications, organize your tRPC router by features:

```typescript
// src/server/trpc/routers/user.ts
import { z } from "zod";
import { router, publicProcedure } from "../trpc";
import { prisma } from "@/lib/db";

export const userRouter = router({
	getAll: publicProcedure.query(async () => {
		return prisma.user.findMany({
			select: {
				id: true,
				name: true,
				email: true,
				createdAt: true,
			},
		});
	}),

	getById: publicProcedure
		.input(z.object({ id: z.string() }))
		.query(async ({ input }) => {
			return prisma.user.findUnique({
				where: { id: input.id },
				select: {
					id: true,
					name: true,
					email: true,
					posts: {
						select: {
							id: true,
							title: true,
							published: true,
						},
					},
					createdAt: true,
				},
			});
		}),

	create: publicProcedure
		.input(
			z.object({
				name: z.string().min(2),
				email: z.string().email(),
				password: z.string().min(8),
			})
		)
		.mutation(async ({ input }) => {
			return prisma.user.create({
				data: input,
				select: {
					id: true,
					name: true,
					email: true,
					createdAt: true,
				},
			});
		}),
});

// src/server/trpc/routers/post.ts
import { z } from "zod";
import { router, publicProcedure } from "../trpc";
import { prisma } from "@/lib/db";

export const postRouter = router({
	getAll: publicProcedure
		.input(
			z
				.object({
					published: z.boolean().optional(),
				})
				.optional()
		)
		.query(async ({ input }) => {
			return prisma.post.findMany({
				where: input ? { published: input.published } : undefined,
				include: {
					author: {
						select: {
							id: true,
							name: true,
						},
					},
				},
				orderBy: {
					createdAt: "desc",
				},
			});
		}),

	getById: publicProcedure
		.input(z.object({ id: z.string() }))
		.query(async ({ input }) => {
			return prisma.post.findUnique({
				where: { id: input.id },
				include: {
					author: {
						select: {
							id: true,
							name: true,
						},
					},
				},
			});
		}),

	create: publicProcedure
		.input(
			z.object({
				title: z.string().min(1),
				content: z.string().optional(),
				authorId: z.string(),
				published: z.boolean().default(false),
			})
		)
		.mutation(async ({ input }) => {
			return prisma.post.create({
				data: input,
				include: {
					author: {
						select: {
							id: true,
							name: true,
						},
					},
				},
			});
		}),

	update: publicProcedure
		.input(
			z.object({
				id: z.string(),
				title: z.string().min(1).optional(),
				content: z.string().optional(),
				published: z.boolean().optional(),
			})
		)
		.mutation(async ({ input }) => {
			const { id, ...data } = input;
			return prisma.post.update({
				where: { id },
				data,
			});
		}),

	delete: publicProcedure
		.input(z.object({ id: z.string() }))
		.mutation(async ({ input }) => {
			return prisma.post.delete({
				where: { id: input.id },
			});
		}),
});

// src/server/trpc/router.ts
import { initTRPC } from "@trpc/server";
import { userRouter } from "./routers/user";
import { postRouter } from "./routers/post";

// Create a tRPC instance
const t = initTRPC.create();

// Define router and procedure helpers
export const router = t.router;
export const publicProcedure = t.procedure;

// Merge all routers
export const appRouter = router({
	user: userRouter,
	post: postRouter,
});

// Type definition for your API
export type AppRouter = typeof appRouter;
```

## Adding Authentication Context

For authenticated routes, create a context with the user:

```typescript
// src/server/trpc/context.ts
import { prisma } from "@/lib/db";
import { getServerSession } from "next-auth/next"; // Assumes you're using NextAuth
import { authOptions } from "@/lib/auth"; // Your NextAuth configuration

export async function createContext(opts: { headers: Headers }) {
	const session = await getServerSession(authOptions);

	return {
		prisma,
		session,
		userId: session?.user?.id,
	};
}

export type Context = Awaited<ReturnType<typeof createContext>>;
```

Update your tRPC instance to use the context:

```typescript
// src/server/trpc/trpc.ts
import { initTRPC, TRPCError } from "@trpc/server";
import { type Context } from "./context";

const t = initTRPC.context<Context>().create();

export const router = t.router;
export const publicProcedure = t.procedure;

// Create a middleware for protected routes
const isAuthed = t.middleware(({ next, ctx }) => {
	if (!ctx.userId) {
		throw new TRPCError({ code: "UNAUTHORIZED" });
	}
	return next({
		ctx: {
			...ctx,
			// Add user ID to context
			userId: ctx.userId,
		},
	});
});

// Create a procedure that requires authentication
export const protectedProcedure = t.procedure.use(isAuthed);
```

Update your API handler to use the context:

```typescript
// src/app/api/trpc/[trpc]/route.ts
import { fetchRequestHandler } from "@trpc/server/adapters/fetch";
import { appRouter } from "@/server/trpc/router";
import { createContext } from "@/server/trpc/context";

const handler = (req: Request) =>
	fetchRequestHandler({
		endpoint: "/api/trpc",
		req,
		router: appRouter,
		createContext: () => createContext({ headers: req.headers }),
	});

export { handler as GET, handler as POST };
```

## Implementing API Route Patterns

For features not using tRPC, implement traditional Next.js API routes:

### RESTful API Routes

```typescript
// src/app/api/posts/route.ts
import { NextResponse } from "next/server";
import { prisma } from "@/lib/db";
import { getServerSession } from "next-auth/next";
import { authOptions } from "@/lib/auth";

// GET /api/posts
export async function GET(request: Request) {
	const { searchParams } = new URL(request.url);
	const published = searchParams.get("published");

	try {
		const posts = await prisma.post.findMany({
			where: published ? { published: published === "true" } : undefined,
			include: {
				author: {
					select: {
						id: true,
						name: true,
					},
				},
			},
			orderBy: {
				createdAt: "desc",
			},
		});

		return NextResponse.json(posts);
	} catch (error) {
		console.error("Error fetching posts:", error);
		return NextResponse.json(
			{ error: "Failed to fetch posts" },
			{ status: 500 }
		);
	}
}

// POST /api/posts
export async function POST(request: Request) {
	const session = await getServerSession(authOptions);

	if (!session?.user?.id) {
		return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
	}

	try {
		const body = await request.json();

		const post = await prisma.post.create({
			data: {
				title: body.title,
				content: body.content,
				published: body.published ?? false,
				author: {
					connect: { id: session.user.id },
				},
			},
		});

		return NextResponse.json(post, { status: 201 });
	} catch (error) {
		console.error("Error creating post:", error);
		return NextResponse.json(
			{ error: "Failed to create post" },
			{ status: 500 }
		);
	}
}
```

### Dynamic Route Handlers

```typescript
// src/app/api/posts/[id]/route.ts
import { NextResponse } from "next/server";
import { prisma } from "@/lib/db";
import { getServerSession } from "next-auth/next";
import { authOptions } from "@/lib/auth";

// GET /api/posts/:id
export async function GET(
	request: Request,
	{ params }: { params: { id: string } }
) {
	try {
		const post = await prisma.post.findUnique({
			where: { id: params.id },
			include: {
				author: {
					select: {
						id: true,
						name: true,
					},
				},
			},
		});

		if (!post) {
			return NextResponse.json({ error: "Post not found" }, { status: 404 });
		}

		return NextResponse.json(post);
	} catch (error) {
		console.error("Error fetching post:", error);
		return NextResponse.json(
			{ error: "Failed to fetch post" },
			{ status: 500 }
		);
	}
}

// PATCH /api/posts/:id
export async function PATCH(
	request: Request,
	{ params }: { params: { id: string } }
) {
	const session = await getServerSession(authOptions);

	if (!session?.user?.id) {
		return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
	}

	try {
		// Verify ownership
		const post = await prisma.post.findUnique({
			where: { id: params.id },
			select: { authorId: true },
		});

		if (!post) {
			return NextResponse.json({ error: "Post not found" }, { status: 404 });
		}

		if (post.authorId !== session.user.id) {
			return NextResponse.json({ error: "Forbidden" }, { status: 403 });
		}

		const body = await request.json();

		const updatedPost = await prisma.post.update({
			where: { id: params.id },
			data: {
				title: body.title,
				content: body.content,
				published: body.published,
			},
		});

		return NextResponse.json(updatedPost);
	} catch (error) {
		console.error("Error updating post:", error);
		return NextResponse.json(
			{ error: "Failed to update post" },
			{ status: 500 }
		);
	}
}

// DELETE /api/posts/:id
export async function DELETE(
	request: Request,
	{ params }: { params: { id: string } }
) {
	const session = await getServerSession(authOptions);

	if (!session?.user?.id) {
		return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
	}

	try {
		// Verify ownership
		const post = await prisma.post.findUnique({
			where: { id: params.id },
			select: { authorId: true },
		});

		if (!post) {
			return NextResponse.json({ error: "Post not found" }, { status: 404 });
		}

		if (post.authorId !== session.user.id) {
			return NextResponse.json({ error: "Forbidden" }, { status: 403 });
		}

		await prisma.post.delete({
			where: { id: params.id },
		});

		return NextResponse.json({ success: true });
	} catch (error) {
		console.error("Error deleting post:", error);
		return NextResponse.json(
			{ error: "Failed to delete post" },
			{ status: 500 }
		);
	}
}
```

## Key Takeaways

1. **Use tRPC** for end-to-end type safety between your frontend and backend.

2. **Implement Prisma** for type-safe database access with automatic migrations.

3. **Organize Code by Feature** to maintain a clean, scalable architecture.

4. **Add Authentication Context** to protect sensitive routes and operations.

5. **Balance REST and tRPC** depending on your project needs.

In the next section, we'll cover deploying your application to production.
