# Edge Computing & Streaming

This section covers two advanced performance optimization techniques in modern Next.js applications: Edge Functions and Streaming. These features enable faster page loads, reduced Time to First Byte (TTFB), and improved user experience.

## Understanding Edge Computing

Edge computing moves processing closer to the user by running code at the "edge" of the network (in CDN locations worldwide) rather than in a centralized data center.

### Benefits of Edge Computing

1. **Lower Latency**: Reduced round-trip time to servers
2. **Global Distribution**: Serve users from nearby locations
3. **Cost Efficiency**: Lower bandwidth costs for providers
4. **Improved Reliability**: Less dependence on a single region
5. **Enhanced Security**: Distributed traffic handling

## Edge Functions in Next.js

Next.js allows you to run your React Server Components at the edge through Edge Runtime.

### Edge Runtime vs. Node.js Runtime

| Feature             | Edge Runtime               | Node.js Runtime                        |
| ------------------- | -------------------------- | -------------------------------------- |
| Startup Time        | Cold start in milliseconds | Cold start in hundreds of milliseconds |
| Available APIs      | Web APIs (fetch, etc.)     | Full Node.js APIs                      |
| Bundle Size         | Limited (< 4MB)            | Unlimited                              |
| Execution Duration  | Limited (typically < 30s)  | Extended (minutes)                     |
| Global Distribution | Yes                        | No (regional)                          |
| File System Access  | No                         | Yes                                    |
| Database Drivers    | Limited                    | Full support                           |

### Setting Up Edge Runtime

To run a page at the edge, export a runtime configuration:

```tsx
// app/edge-example/page.tsx
export const runtime = "edge";

export default async function EdgePage() {
	// This page will run at the edge
	const response = await fetch("https://api.example.com/data");
	const data = await response.json();

	return (
		<div className="p-6">
			<h1 className="text-2xl font-bold mb-4">Edge-rendered Page</h1>
			<div className="bg-gray-100 p-4 rounded">
				<pre>{JSON.stringify(data, null, 2)}</pre>
			</div>
		</div>
	);
}
```

## API Routes at the Edge

You can also run API routes at the edge:

```tsx
// app/api/edge/route.ts
export const runtime = "edge";

export async function GET(request: Request) {
	const { searchParams } = new URL(request.url);
	const query = searchParams.get("query") || "";

	// Fetch data from another API
	const response = await fetch(`https://api.example.com/search?q=${query}`);
	const data = await response.json();

	// Process the data at the edge
	const results = data.items.map((item) => ({
		id: item.id,
		title: item.title,
		snippet: item.description.substring(0, 100) + "...",
	}));

	return Response.json({ results });
}
```

## Edge Function Limitations

When using the Edge Runtime, be aware of these limitations:

1. **No Node.js APIs**: Native Node.js APIs like `fs` are not available
2. **Limited NPM Packages**: Some NPM packages that depend on Node.js won't work
3. **Bundle Size Limit**: Your code and dependencies must fit within size limits
4. **Execution Time Limits**: Edge functions have stricter timeout limits

### Compatible Database Access

For database access at the edge, use HTTP-based APIs:

```tsx
// app/api/users/route.ts
export const runtime = "edge";

export async function GET() {
	// Using data API instead of direct database driver
	const response = await fetch("https://api.supabase.com/rest/v1/users", {
		headers: {
			apikey: process.env.SUPABASE_API_KEY!,
			Authorization: `Bearer ${process.env.SUPABASE_API_KEY}`,
		},
	});

	const users = await response.json();
	return Response.json({ users });
}
```

## Streaming in Next.js

Streaming allows you to progressively render UI from the server. Instead of waiting for all data to load before sending HTML to the client, streaming sends chunks of HTML as they become ready.

### Benefits of Streaming

1. **Faster Initial Page Load**: Show UI while data is still being fetched
2. **Improved TTFB**: First bytes arrive sooner
3. **Progressive Enhancement**: Core content arrives first
4. **Prioritized Loading**: Load critical UI first, defer less important parts

### Implementing Streaming with Suspense

Wrap data-fetching components in `<Suspense>` to enable streaming:

```tsx
// app/dashboard/page.tsx
import { Suspense } from "react";
import Loading from "./loading";
import UserProfile from "@/components/server/UserProfile";
import RevenueMetrics from "@/components/server/RevenueMetrics";
import RecentOrders from "@/components/server/RecentOrders";

export default function DashboardPage() {
	return (
		<div className="p-6">
			<h1 className="text-2xl font-bold mb-6">Dashboard</h1>

			<div className="grid grid-cols-12 gap-6">
				{/* User profile loads first */}
				<div className="col-span-12 md:col-span-4">
					<Suspense fallback={<Loading text="Loading profile..." />}>
						<UserProfile />
					</Suspense>
				</div>

				{/* Metrics load next */}
				<div className="col-span-12 md:col-span-8">
					<Suspense fallback={<Loading text="Loading metrics..." />}>
						<RevenueMetrics />
					</Suspense>
				</div>

				{/* Orders can take longer to load */}
				<div className="col-span-12">
					<Suspense fallback={<Loading text="Loading recent orders..." />}>
						<RecentOrders />
					</Suspense>
				</div>
			</div>
		</div>
	);
}
```

The `Loading` component:

```tsx
// app/dashboard/loading.tsx
export default function Loading({ text = "Loading..." }: { text?: string }) {
	return (
		<div className="border border-gray-200 rounded-lg p-4 h-full">
			<div className="flex items-center justify-center h-full min-h-[200px]">
				<div className="flex flex-col items-center">
					<div className="animate-spin h-8 w-8 border-4 border-blue-500 rounded-full border-t-transparent"></div>
					<p className="mt-2 text-gray-500">{text}</p>
				</div>
			</div>
		</div>
	);
}
```

The data-fetching components:

```tsx
// components/server/UserProfile.tsx
import { getUser } from "@/lib/api";

export default async function UserProfile() {
	// This data fetch suspends the component
	const user = await getUser();

	return (
		<div className="border border-gray-200 rounded-lg p-4">
			<div className="flex items-center">
				<div className="w-16 h-16 rounded-full bg-gray-200 flex items-center justify-center text-gray-600">
					{user.name.charAt(0)}
				</div>
				<div className="ml-4">
					<h2 className="text-xl font-semibold">{user.name}</h2>
					<p className="text-gray-600">{user.email}</p>
				</div>
			</div>

			<div className="mt-4">
				<p className="text-sm text-gray-500">
					Member since {new Date(user.createdAt).toLocaleDateString()}
				</p>
			</div>
		</div>
	);
}
```

### Using `loading.tsx` for Automatic Suspense Boundaries

Next.js provides a special `loading.tsx` file that automatically wraps the page in a Suspense boundary:

```tsx
// app/dashboard/loading.tsx
export default function DashboardLoading() {
	return (
		<div className="p-6">
			<h1 className="text-2xl font-bold mb-6">Dashboard</h1>

			<div className="grid grid-cols-12 gap-6">
				<div className="col-span-12 md:col-span-4">
					<div className="border border-gray-200 rounded-lg p-4 animate-pulse">
						<div className="h-32 bg-gray-200 rounded"></div>
					</div>
				</div>

				<div className="col-span-12 md:col-span-8">
					<div className="border border-gray-200 rounded-lg p-4 animate-pulse">
						<div className="h-32 bg-gray-200 rounded"></div>
					</div>
				</div>

				<div className="col-span-12">
					<div className="border border-gray-200 rounded-lg p-4 animate-pulse">
						<div className="h-64 bg-gray-200 rounded"></div>
					</div>
				</div>
			</div>
		</div>
	);
}
```

## Strategic Loading States

For a sophisticated user experience, implement nested Suspense boundaries with different loading states:

```tsx
// app/products/page.tsx
import { Suspense } from "react";
import ProductFilters from "@/components/server/ProductFilters";
import ProductGrid from "@/components/server/ProductGrid";
import ProductSkeleton from "@/components/ui/ProductSkeleton";
import FiltersSkeleton from "@/components/ui/FiltersSkeleton";

export default function ProductsPage() {
	return (
		<div className="container mx-auto px-4 py-8">
			<h1 className="text-3xl font-bold mb-8">Products</h1>

			<div className="flex flex-col md:flex-row gap-8">
				{/* Filters section */}
				<div className="w-full md:w-1/4">
					<Suspense fallback={<FiltersSkeleton />}>
						<ProductFilters />
					</Suspense>
				</div>

				{/* Product grid */}
				<div className="w-full md:w-3/4">
					<Suspense fallback={<ProductSkeleton count={8} />}>
						<ProductGrid />
					</Suspense>
				</div>
			</div>
		</div>
	);
}
```

## Parallel Data Fetching for Improved Performance

To optimize loading, fetch data in parallel rather than sequentially:

```tsx
// components/server/Dashboard.tsx
import { getUser, getStats, getOrders } from '@/lib/api';

export default async function Dashboard() {
  // Start all data fetches in parallel
  const userPromise = getUser();
  const statsPromise = getStats();
  const ordersPromise = getOrders();

  // Wait for all promises to resolve
  const [user, stats, orders] = await Promise.all([
    userPromise,
    statsPromise,
    ordersPromise
  ]);

  return (
    // Render with all data available
  );
}
```

## Streaming with Edge Functions

Combining edge functions with streaming provides the best performance:

```tsx
// app/realtime-dashboard/page.tsx
export const runtime = "edge";

import { Suspense } from "react";
import RealTimeMetrics from "@/components/server/RealTimeMetrics";
import MetricsSkeleton from "@/components/ui/MetricsSkeleton";

export default function RealTimeDashboardPage() {
	return (
		<div className="p-6">
			<h1 className="text-2xl font-bold mb-6">Real-Time Dashboard</h1>

			<div className="space-y-6">
				<Suspense fallback={<MetricsSkeleton />}>
					<RealTimeMetrics />
				</Suspense>
			</div>
		</div>
	);
}
```

## Server-Side Waterfall Anti-Pattern

Avoid sequential data fetching that creates "waterfalls":

```tsx
// ❌ BAD: Sequential fetching creates a waterfall
async function BadComponent() {
	const user = await getUser(); // Waits for this to complete
	const posts = await getUserPosts(user.id); // Then starts this
	const comments = await getPostComments(posts[0].id); // Then starts this

	return <div>...</div>;
}

// ✅ GOOD: Parallel fetching
async function GoodComponent() {
	const user = await getUser();

	// Start these fetches in parallel
	const postsPromise = getUserPosts(user.id);
	const likesPromise = getUserLikes(user.id);

	// Wait for both to complete
	const [posts, likes] = await Promise.all([postsPromise, likesPromise]);

	return <div>...</div>;
}
```

## Performance Optimization Techniques

### 1. Strategic Component Splitting

Split components to optimize what gets streamed first:

```tsx
// app/page.tsx
import { Suspense } from "react";
import MainContent from "@/components/server/MainContent";
import Sidebar from "@/components/server/Sidebar";
import Footer from "@/components/server/Footer";

export default function HomePage() {
	return (
		<div className="container mx-auto px-4 py-8">
			{/* Critical content (loads first) */}
			<header className="mb-8">
				<h1 className="text-4xl font-bold">Welcome to Our Platform</h1>
			</header>

			<div className="flex flex-col lg:flex-row gap-8">
				{/* Main content (high priority) */}
				<div className="lg:w-2/3">
					<Suspense
						fallback={
							<div className="animate-pulse h-96 bg-gray-100 rounded-lg"></div>
						}
					>
						<MainContent />
					</Suspense>
				</div>

				{/* Sidebar (medium priority) */}
				<div className="lg:w-1/3">
					<Suspense
						fallback={
							<div className="animate-pulse h-96 bg-gray-100 rounded-lg"></div>
						}
					>
						<Sidebar />
					</Suspense>
				</div>
			</div>

			{/* Footer (low priority) */}
			<div className="mt-12">
				<Suspense
					fallback={
						<div className="animate-pulse h-24 bg-gray-100 rounded-lg"></div>
					}
				>
					<Footer />
				</Suspense>
			</div>
		</div>
	);
}
```

### 2. Static vs. Dynamic Content

Optimize by separating static and dynamic content:

```tsx
// app/blog/[slug]/page.tsx
import { Suspense } from "react";
import { getPostBySlug } from "@/lib/api";
import StaticPostContent from "@/components/blog/StaticPostContent";
import DynamicComments from "@/components/blog/DynamicComments";
import CommentsSkeleton from "@/components/ui/CommentsSkeleton";

export default async function BlogPostPage({
	params,
}: {
	params: { slug: string };
}) {
	// This data should load quickly
	const post = await getPostBySlug(params.slug);

	return (
		<div className="max-w-3xl mx-auto px-4 py-8">
			{/* Static content renders immediately */}
			<StaticPostContent post={post} />

			{/* Dynamic content streams in */}
			<div className="mt-8">
				<h2 className="text-2xl font-bold mb-4">Comments</h2>
				<Suspense fallback={<CommentsSkeleton />}>
					<DynamicComments postId={post.id} />
				</Suspense>
			</div>
		</div>
	);
}
```

## Key Takeaways

1. **Use Edge Functions** for globally distributed, low-latency applications, especially for API routes and pages that don't require Node.js-specific features.

2. **Implement Streaming** with Suspense to improve perceived performance by showing UI progressively.

3. **Fetch Data in Parallel** whenever possible to avoid waterfalls and reduce loading times.

4. **Prioritize Content** by streaming critical UI first and deferring less important parts.

5. **Combine Edge + Streaming** for the best possible performance in global applications.

In the next section, we'll explore type-safe development with TypeScript, Zod, and react-hook-form.
