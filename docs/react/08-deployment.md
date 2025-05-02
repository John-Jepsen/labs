# Deployment

This section covers deploying your modern React application to production, with a focus on Vercel deployment, environment variables, and monitoring.

## Preparing for Deployment

Before deploying your application, ensure it's production-ready:

### 1. Performance Optimizations

- Run Lighthouse tests and address critical issues
- Ensure images are optimized with next/image
- Check that your bundles are properly split
- Analyze your bundle size with `next/bundle-analyzer`

### 2. Environment Variables

Create a `.env.example` file that lists all required environment variables without actual values:

```
# .env.example
# Database
DATABASE_URL=

# Authentication
NEXTAUTH_URL=
NEXTAUTH_SECRET=

# Third-party services
OPENAI_API_KEY=
```

Make sure all environment variables are properly set in your production environment.

### 3. Database Migrations

If you're using Prisma, make sure to run migrations in your deployment pipeline:

```bash
npx prisma migrate deploy
```

### 4. Security Checks

- Ensure API keys and secrets are not exposed in client-side code
- Implement proper CORS policies
- Set appropriate Content Security Policy headers
- Validate user inputs on both client and server

## Deploying to Vercel

[Vercel](https://vercel.com/) is the preferred hosting platform for Next.js applications, offering an excellent developer experience and global edge network.

### Setting Up Vercel CLI

Install the Vercel CLI:

```bash
npm install -g vercel
```

### Configuring Vercel for Your Project

Create a `vercel.json` file in your project root:

```json
{
	"version": 2,
	"buildCommand": "npm run build",
	"devCommand": "npm run dev",
	"installCommand": "npm install",
	"framework": "nextjs",
	"regions": ["sfo1", "cdg1"],
	"env": {
		"NEXTAUTH_URL": "https://your-app-name.vercel.app"
	}
}
```

### Deploying from the CLI

Log in to Vercel:

```bash
vercel login
```

Deploy to production:

```bash
vercel --prod
```

### Deploying from Git

1. Push your code to GitHub, GitLab, or Bitbucket
2. Connect your repository in the Vercel dashboard
3. Configure build settings and environment variables
4. Deploy your application

### Configuring Environment Variables in Vercel

Add your environment variables in the Vercel dashboard:

1. Go to your project in the Vercel dashboard
2. Navigate to Settings > Environment Variables
3. Add each environment variable with the appropriate value
4. Specify whether it should be available in Development, Preview, or Production environments

### Automatic Deployments

Vercel automatically deploys your application when you push to your default branch. You can also configure:

- Preview deployments for pull requests
- Deployment protection with password or team access
- Custom domains and SSL certificates

## Optimizing for the Edge

Next.js on Vercel can leverage the Edge Runtime for improved performance:

### Configuring Edge Functions

```typescript
// app/api/edge-example/route.ts
export const runtime = "edge";

export async function GET(request: Request) {
	const start = Date.now();
	// Simulate some processing
	await new Promise((resolve) => setTimeout(resolve, 100));
	const duration = Date.now() - start;

	return Response.json({
		message: "Hello from the Edge!",
		timestamp: new Date().toISOString(),
		duration: `${duration}ms`,
		region: process.env.VERCEL_REGION || "unknown",
	});
}
```

### Global Edge Config

For applications that can run entirely on the Edge:

```typescript
// next.config.js
/** @type {import('next').NextConfig} */
const nextConfig = {
	experimental: {
		runtime: "edge",
	},
};

module.exports = nextConfig;
```

## Setting Up Custom Domains

To configure a custom domain for your application:

1. Navigate to your project in the Vercel dashboard
2. Go to Settings > Domains
3. Add your domain and follow the DNS configuration instructions
4. Vercel will automatically provision an SSL certificate

## Monitoring and Analytics

### Setting Up Vercel Analytics

Enable Vercel Analytics for performance monitoring:

```typescript
// next.config.js
/** @type {import('next').NextConfig} */
const nextConfig = {
	experimental: {
		webVitalsAttribution: ["CLS", "LCP"],
	},
};

module.exports = nextConfig;
```

Enable Speed Insights in the Vercel dashboard:

1. Go to your project settings
2. Navigate to Analytics tab
3. Enable Speed Insights

### Implementing Error Monitoring

Integrate an error monitoring service like Sentry:

```bash
npm install @sentry/nextjs
```

Initialize Sentry in your project:

```typescript
// sentry.server.config.ts
import * as Sentry from "@sentry/nextjs";

Sentry.init({
	dsn: process.env.SENTRY_DSN,
	tracesSampleRate: 1.0,
	environment: process.env.NODE_ENV,
});

// sentry.client.config.ts
import * as Sentry from "@sentry/nextjs";

Sentry.init({
	dsn: process.env.SENTRY_DSN,
	tracesSampleRate: 1.0,
	environment: process.env.NODE_ENV,
	integrations: [new Sentry.BrowserTracing()],
});

// next.config.js
const { withSentryConfig } = require("@sentry/nextjs");

/** @type {import('next').NextConfig} */
const nextConfig = {
	// Your Next.js config
};

module.exports = withSentryConfig(
	nextConfig,
	{
		// For all available options, see:
		// https://github.com/getsentry/sentry-webpack-plugin#options
		silent: true,
	},
	{
		// For all available options, see:
		// https://docs.sentry.io/platforms/javascript/guides/nextjs/
		widenClientFileUpload: true,
		transpileClientSDK: true,
		hideSourceMaps: true,
		disableLogger: true,
	}
);
```

## Database Deployment

For applications using a database, you'll need to provision a database service.

### Setting Up a Database on Vercel

Vercel partners with various database providers for seamless integrations:

1. Go to your project in the Vercel dashboard
2. Navigate to Storage tab
3. Choose a database provider (e.g., Vercel Postgres, NeonDB)
4. Follow the setup instructions

### Using External Database Services

For services like Supabase, PlanetScale, or MongoDB Atlas:

1. Create a database in your chosen service
2. Get the connection string or credentials
3. Add them to your Vercel environment variables

Example for Prisma with external database:

```
# In Vercel environment variables
DATABASE_URL=postgresql://username:password@hostname:port/database
```

## Implementing CI/CD

### GitHub Actions for Testing

Create a `.github/workflows/test.yml` file:

```yaml
name: Run Tests

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Set up Node.js
        uses: actions/setup-node@v3
        with:
          node-version: "20"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run linting
        run: npm run lint

      - name: Run tests
        run: npm test
```

### Vercel GitHub Integration

For automatic deployments with Vercel:

1. Connect your GitHub repository in the Vercel dashboard
2. Configure automatic deployments for pull requests and merges
3. Set up deployment protection rules if needed

## Managing Deployments

### Deployment Environments

Vercel supports multiple environments:

- **Production**: Your live application
- **Preview**: Automatically deployed for pull requests
- **Development**: Local development environment

### Environment Variables Per Environment

Configure different variables for each environment:

1. Go to your project in the Vercel dashboard
2. Navigate to Settings > Environment Variables
3. Use the checkboxes to apply variables to specific environments

### Rollbacks

If something goes wrong, you can roll back to a previous deployment:

1. Go to your project in the Vercel dashboard
2. Navigate to Deployments tab
3. Find a stable deployment and click "..." > "Promote to Production"

## Optimizing for Production

### Caching Strategies

Set up caching headers in Next.js:

```typescript
// app/api/data/route.ts
export function GET() {
	return Response.json(
		{ data: "Some data" },
		{
			headers: {
				"Cache-Control": "public, s-maxage=10, stale-while-revalidate=59",
			},
		}
	);
}
```

### Image Optimization

Next.js Image component is automatically optimized on Vercel:

```tsx
import Image from "next/image";

export default function OptimizedImage() {
	return (
		<Image
			src="/large-image.jpg"
			alt="Description"
			width={800}
			height={600}
			quality={90}
			priority
		/>
	);
}
```

### ISR (Incremental Static Regeneration)

Use ISR for dynamic content that doesn't change frequently:

```tsx
// app/blog/[slug]/page.tsx
export async function generateStaticParams() {
	const posts = await getPosts();

	return posts.map((post) => ({
		slug: post.slug,
	}));
}

export const revalidate = 3600; // Revalidate at most once per hour

export default async function BlogPost({
	params,
}: {
	params: { slug: string };
}) {
	const post = await getPostBySlug(params.slug);

	// Render the post...
}
```

## Key Takeaways

1. **Choose the Right Platform**: Vercel is optimized for Next.js applications.

2. **Manage Environment Variables** securely and configure them for different environments.

3. **Implement Monitoring** to track performance and errors in production.

4. **Set Up CI/CD Pipelines** for automatic testing and deployment.

5. **Optimize for Production** with caching, ISR, and edge functions.

In the next section, we'll cover troubleshooting common issues in React applications.
