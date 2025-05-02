# Styling Modern React Applications

This section covers modern approaches to styling React applications, with a focus on TailwindCSS, component libraries, and implementing features like dark mode. We'll explore how to create visually appealing, responsive interfaces without sacrificing performance.

## TailwindCSS Overview

TailwindCSS is a utility-first CSS framework that allows you to build designs directly in your markup by applying pre-defined utility classes. Rather than writing custom CSS, you compose designs using utility classes that handle spacing, colors, typography, and more.

### Key Benefits of TailwindCSS

1. **Development Speed**: Rapid UI development without context switching to CSS files
2. **Consistency**: Predefined design system with consistent spacing, colors, etc.
3. **Responsive Design**: Built-in responsive utilities for different screen sizes
4. **Customizability**: Easily extend the default configuration
5. **Performance**: Automatic purging of unused styles in production

## TailwindCSS Configuration

If you followed the setup in the first section, TailwindCSS should already be configured in your project. Let's explore how to customize it further.

### Customizing the Theme

The `tailwind.config.ts` file is where you define your project's design system:

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
				// Custom color palette
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
				secondary: {
					// ...secondary colors
				},
				// Add more custom colors
			},
			fontFamily: {
				sans: ["var(--font-inter)", "system-ui", "sans-serif"],
				heading: ["var(--font-montserrat)", "system-ui", "sans-serif"],
			},
			spacing: {
				// Custom spacing values
				"128": "32rem",
				"144": "36rem",
			},
			borderRadius: {
				// Custom border radius values
				"4xl": "2rem",
			},
			boxShadow: {
				// Custom shadows
				soft: "0 2px 15px rgba(0, 0, 0, 0.05)",
			},
			animation: {
				// Custom animations
				"bounce-slow": "bounce 3s infinite",
			},
		},
	},
	plugins: [],
};
export default config;
```

### Setting Up Plugins

Tailwind has a rich ecosystem of plugins that extend its functionality:

```typescript
import type { Config } from "tailwindcss";
import typography from "@tailwindcss/typography";
import forms from "@tailwindcss/forms";
import aspectRatio from "@tailwindcss/aspect-ratio";

const config: Config = {
	// ... other configurations
	plugins: [
		typography, // Adds prose classes for beautiful typographic defaults
		forms, // Better styling for form elements
		aspectRatio, // Utilities for aspect ratios
	],
};
export default config;
```

Install these plugins:

```bash
npm install -D @tailwindcss/typography @tailwindcss/forms @tailwindcss/aspect-ratio
```

## Basic TailwindCSS Usage

### Layout and Spacing

```tsx
// Simple layout with Tailwind classes
export default function ProfileCard() {
	return (
		<div className="bg-white rounded-lg shadow-md p-6 max-w-md mx-auto">
			<div className="flex items-center space-x-4">
				<div className="h-16 w-16 rounded-full bg-gray-200"></div>
				<div>
					<h2 className="text-xl font-bold">Jane Doe</h2>
					<p className="text-gray-600">Product Designer</p>
				</div>
			</div>

			<div className="mt-6">
				<p className="text-gray-700">
					User experience designer focused on creating intuitive, accessible
					products.
				</p>
			</div>

			<div className="mt-6 pt-6 border-t border-gray-100 flex justify-between">
				<div className="text-center">
					<div className="font-bold">142</div>
					<div className="text-xs text-gray-500">Posts</div>
				</div>
				<div className="text-center">
					<div className="font-bold">2.4k</div>
					<div className="text-xs text-gray-500">Followers</div>
				</div>
				<div className="text-center">
					<div className="font-bold">268</div>
					<div className="text-xs text-gray-500">Following</div>
				</div>
			</div>
		</div>
	);
}
```

### Responsive Design

Tailwind makes responsive design straightforward with breakpoint prefixes:

```tsx
export default function ResponsiveGrid() {
	return (
		<div className="container mx-auto px-4 py-8">
			<h1 className="text-2xl md:text-3xl lg:text-4xl font-bold mb-8">
				Our Team
			</h1>

			<div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-6">
				{Array.from({ length: 8 }).map((_, i) => (
					<div key={i} className="bg-white rounded-lg shadow p-6">
						<div className="h-40 bg-gray-200 rounded-md mb-4"></div>
						<h2 className="font-bold text-lg">Team Member {i + 1}</h2>
						<p className="text-gray-600 text-sm">Position</p>
					</div>
				))}
			</div>
		</div>
	);
}
```

### Hover, Focus, and Other States

Tailwind provides modifiers for different states:

```tsx
// Button with hover, focus, active states
export default function Button({ children }: { children: React.ReactNode }) {
	return (
		<button
			className="
      bg-blue-500 
      hover:bg-blue-600 
      focus:bg-blue-700 
      active:bg-blue-800 
      text-white 
      px-4 
      py-2 
      rounded-md
      focus:outline-none 
      focus:ring-2 
      focus:ring-blue-300 
      focus:ring-offset-2
      transition-colors
    "
		>
			{children}
		</button>
	);
}
```

## Creating a Design System with TailwindCSS

### Consistent Spacing

Use Tailwind's spacing scale consistently:

```tsx
// Consistent spacing in a card component
export function Card({ title, content }: { title: string; content: string }) {
	return (
		<div className="bg-white rounded-lg shadow-md overflow-hidden">
			<div className="p-6">
				<h3 className="text-xl font-bold mb-4">{title}</h3>
				<p className="text-gray-600 mb-6">{content}</p>
				<button className="bg-primary-500 text-white px-4 py-2 rounded-md">
					Learn More
				</button>
			</div>
		</div>
	);
}
```

### Typography System

Create a consistent typography system:

```tsx
// components/ui/Typography.tsx
// Define reusable typography components

export function Heading1({ children }: { children: React.ReactNode }) {
	return <h1 className="text-3xl font-bold text-gray-900 mb-6">{children}</h1>;
}

export function Heading2({ children }: { children: React.ReactNode }) {
	return <h2 className="text-2xl font-bold text-gray-900 mb-4">{children}</h2>;
}

export function Heading3({ children }: { children: React.ReactNode }) {
	return (
		<h3 className="text-xl font-semibold text-gray-900 mb-3">{children}</h3>
	);
}

export function Body({ children }: { children: React.ReactNode }) {
	return <p className="text-base text-gray-700 mb-4">{children}</p>;
}

export function Small({ children }: { children: React.ReactNode }) {
	return <p className="text-sm text-gray-500">{children}</p>;
}
```

### Color System

Use your custom color palette consistently:

```tsx
// Alert component with semantic colors
type AlertVariant = "info" | "success" | "warning" | "error";

const variantClasses: Record<AlertVariant, string> = {
	info: "bg-blue-50 text-blue-800 border-blue-200",
	success: "bg-green-50 text-green-800 border-green-200",
	warning: "bg-yellow-50 text-yellow-800 border-yellow-200",
	error: "bg-red-50 text-red-800 border-red-200",
};

export function Alert({
	variant = "info",
	title,
	children,
}: {
	variant?: AlertVariant;
	title: string;
	children: React.ReactNode;
}) {
	return (
		<div className={`rounded-md p-4 border ${variantClasses[variant]}`}>
			<div className="flex">
				<div className="flex-shrink-0">{/* Icon would go here */}</div>
				<div className="ml-3">
					<h3 className="text-sm font-medium">{title}</h3>
					<div className="mt-2 text-sm">{children}</div>
				</div>
			</div>
		</div>
	);
}
```

## Using TailwindCSS with Component Libraries

While Tailwind provides utility classes, you might want to use it with component libraries for more complex UI elements. Let's explore using Tailwind with Radix UI.

### Setting Up Radix UI

Radix UI provides unstyled, accessible components that you can style with Tailwind:

```bash
npm install @radix-ui/react-dialog @radix-ui/react-dropdown-menu @radix-ui/react-tabs
```

### Styling Radix Dialog with Tailwind

```tsx
// components/ui/Dialog.tsx
"use client";

import React from "react";
import * as DialogPrimitive from "@radix-ui/react-dialog";
import { X } from "lucide-react"; // Assuming you're using Lucide icons

export function Dialog({ children, ...props }: DialogPrimitive.DialogProps) {
	return <DialogPrimitive.Root {...props}>{children}</DialogPrimitive.Root>;
}

export function DialogTrigger({
	children,
	...props
}: DialogPrimitive.DialogTriggerProps) {
	return (
		<DialogPrimitive.Trigger {...props}>{children}</DialogPrimitive.Trigger>
	);
}

export function DialogContent({
	children,
	...props
}: DialogPrimitive.DialogContentProps) {
	return (
		<DialogPrimitive.Portal>
			<DialogPrimitive.Overlay className="fixed inset-0 bg-black/40 backdrop-blur-sm data-[state=open]:animate-in data-[state=closed]:animate-out data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0 z-40" />
			<DialogPrimitive.Content
				className="fixed left-[50%] top-[50%] z-50 max-h-[85vh] w-full max-w-md translate-x-[-50%] translate-y-[-50%] bg-white p-6 shadow-lg rounded-lg data-[state=open]:animate-in data-[state=closed]:animate-out data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0 data-[state=closed]:zoom-out-95 data-[state=open]:zoom-in-95 data-[state=closed]:slide-out-to-left-1/2 data-[state=closed]:slide-out-to-top-[48%] data-[state=open]:slide-in-from-left-1/2 data-[state=open]:slide-in-from-top-[48%] duration-200"
				{...props}
			>
				{children}
				<DialogPrimitive.Close className="absolute right-4 top-4 rounded-sm opacity-70 ring-offset-white transition-opacity hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-gray-400 focus:ring-offset-2 disabled:pointer-events-none">
					<X className="h-4 w-4" />
					<span className="sr-only">Close</span>
				</DialogPrimitive.Close>
			</DialogPrimitive.Content>
		</DialogPrimitive.Portal>
	);
}

export function DialogHeader({
	children,
	...props
}: React.HTMLAttributes<HTMLDivElement>) {
	return (
		<div className="mb-4" {...props}>
			{children}
		</div>
	);
}

export function DialogTitle({
	children,
	...props
}: DialogPrimitive.DialogTitleProps) {
	return (
		<DialogPrimitive.Title className="text-lg font-semibold" {...props}>
			{children}
		</DialogPrimitive.Title>
	);
}

export function DialogDescription({
	children,
	...props
}: DialogPrimitive.DialogDescriptionProps) {
	return (
		<DialogPrimitive.Description className="text-sm text-gray-600" {...props}>
			{children}
		</DialogPrimitive.Description>
	);
}

export function DialogFooter({
	children,
	...props
}: React.HTMLAttributes<HTMLDivElement>) {
	return (
		<div className="mt-6 flex justify-end space-x-2" {...props}>
			{children}
		</div>
	);
}
```

### Using the Dialog Component

```tsx
// Example usage of the Dialog component
"use client";

import {
	Dialog,
	DialogTrigger,
	DialogContent,
	DialogHeader,
	DialogTitle,
	DialogDescription,
	DialogFooter,
} from "@/components/ui/Dialog";
import { useState } from "react";

export default function DialogExample() {
	const [open, setOpen] = useState(false);

	return (
		<Dialog open={open} onOpenChange={setOpen}>
			<DialogTrigger asChild>
				<button className="bg-primary-500 hover:bg-primary-600 text-white px-4 py-2 rounded-md">
					Open Dialog
				</button>
			</DialogTrigger>

			<DialogContent>
				<DialogHeader>
					<DialogTitle>Edit Profile</DialogTitle>
					<DialogDescription>
						Make changes to your profile here. Click save when you're done.
					</DialogDescription>
				</DialogHeader>

				<div className="py-4">
					<div className="grid gap-4">
						<div>
							<label
								htmlFor="name"
								className="block text-sm font-medium text-gray-700 mb-1"
							>
								Name
							</label>
							<input
								id="name"
								className="w-full border border-gray-300 rounded-md px-3 py-2 focus:outline-none focus:ring-2 focus:ring-primary-500"
								placeholder="Your name"
							/>
						</div>

						<div>
							<label
								htmlFor="email"
								className="block text-sm font-medium text-gray-700 mb-1"
							>
								Email
							</label>
							<input
								id="email"
								type="email"
								className="w-full border border-gray-300 rounded-md px-3 py-2 focus:outline-none focus:ring-2 focus:ring-primary-500"
								placeholder="your.email@example.com"
							/>
						</div>
					</div>
				</div>

				<DialogFooter>
					<button
						onClick={() => setOpen(false)}
						className="bg-gray-200 hover:bg-gray-300 text-gray-800 px-4 py-2 rounded-md"
					>
						Cancel
					</button>
					<button
						onClick={() => setOpen(false)}
						className="bg-primary-500 hover:bg-primary-600 text-white px-4 py-2 rounded-md"
					>
						Save Changes
					</button>
				</DialogFooter>
			</DialogContent>
		</Dialog>
	);
}
```

## Building a Component Library

For a consistent design system, build a library of reusable components:

### Button Component

```tsx
// components/ui/Button.tsx
import { forwardRef } from "react";
import { VariantProps, cva } from "class-variance-authority";
import { cn } from "@/lib/utils";

// Define button variants using class-variance-authority
const buttonVariants = cva(
	"inline-flex items-center justify-center rounded-md font-medium transition-colors focus:outline-none focus:ring-2 focus:ring-offset-2 disabled:opacity-50 disabled:pointer-events-none",
	{
		variants: {
			variant: {
				default:
					"bg-primary-500 text-white hover:bg-primary-600 focus:ring-primary-500",
				outline:
					"border border-gray-300 bg-transparent hover:bg-gray-50 focus:ring-gray-500",
				ghost: "bg-transparent hover:bg-gray-100 focus:ring-gray-500",
				destructive:
					"bg-red-500 text-white hover:bg-red-600 focus:ring-red-500",
			},
			size: {
				sm: "h-8 px-3 text-xs",
				md: "h-10 px-4 text-sm",
				lg: "h-12 px-6 text-base",
			},
		},
		defaultVariants: {
			variant: "default",
			size: "md",
		},
	}
);

// Type for button props
export interface ButtonProps
	extends React.ButtonHTMLAttributes<HTMLButtonElement>,
		VariantProps<typeof buttonVariants> {
	isLoading?: boolean;
}

// Button component
const Button = forwardRef<HTMLButtonElement, ButtonProps>(
	({ className, variant, size, isLoading, children, ...props }, ref) => {
		return (
			<button
				className={cn(buttonVariants({ variant, size }), className)}
				ref={ref}
				disabled={isLoading || props.disabled}
				{...props}
			>
				{isLoading ? (
					<span className="mr-2">
						<svg
							className="animate-spin h-4 w-4 text-current"
							xmlns="http://www.w3.org/2000/svg"
							fill="none"
							viewBox="0 0 24 24"
						>
							<circle
								className="opacity-25"
								cx="12"
								cy="12"
								r="10"
								stroke="currentColor"
								strokeWidth="4"
							></circle>
							<path
								className="opacity-75"
								fill="currentColor"
								d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
							></path>
						</svg>
					</span>
				) : null}
				{children}
			</button>
		);
	}
);
Button.displayName = "Button";

export { Button, buttonVariants };
```

To use this component, we need a utility function:

```typescript
// lib/utils.ts
import { clsx, type ClassValue } from "clsx";
import { twMerge } from "tailwind-merge";

export function cn(...inputs: ClassValue[]) {
	return twMerge(clsx(inputs));
}
```

Install the required packages:

```bash
npm install class-variance-authority clsx tailwind-merge
```

### Card Component

```tsx
// components/ui/Card.tsx
import { cn } from "@/lib/utils";

interface CardProps extends React.HTMLAttributes<HTMLDivElement> {}

export function Card({ className, ...props }: CardProps) {
	return (
		<div
			className={cn("bg-white rounded-lg shadow-md overflow-hidden", className)}
			{...props}
		/>
	);
}

interface CardHeaderProps extends React.HTMLAttributes<HTMLDivElement> {}

export function CardHeader({ className, ...props }: CardHeaderProps) {
	return (
		<div
			className={cn("px-6 py-4 border-b border-gray-100", className)}
			{...props}
		/>
	);
}

interface CardTitleProps extends React.HTMLAttributes<HTMLHeadingElement> {}

export function CardTitle({ className, ...props }: CardTitleProps) {
	return <h3 className={cn("text-lg font-semibold", className)} {...props} />;
}

interface CardDescriptionProps
	extends React.HTMLAttributes<HTMLParagraphElement> {}

export function CardDescription({ className, ...props }: CardDescriptionProps) {
	return (
		<p className={cn("text-sm text-gray-500 mt-1", className)} {...props} />
	);
}

interface CardContentProps extends React.HTMLAttributes<HTMLDivElement> {}

export function CardContent({ className, ...props }: CardContentProps) {
	return <div className={cn("px-6 py-4", className)} {...props} />;
}

interface CardFooterProps extends React.HTMLAttributes<HTMLDivElement> {}

export function CardFooter({ className, ...props }: CardFooterProps) {
	return <div className={cn("px-6 py-4 bg-gray-50", className)} {...props} />;
}
```

## Implementing Dark Mode

TailwindCSS supports dark mode either automatically based on system preferences or manually with a toggle switch.

### Setting Up Dark Mode

First, configure dark mode in your `tailwind.config.ts`:

```typescript
// tailwind.config.ts
import type { Config } from "tailwindcss";

const config: Config = {
	darkMode: "class", // Use 'media' for system preference or 'class' for manual toggle
	// ... other configuration
};
export default config;
```

### Creating a Dark Mode Toggle

```tsx
// components/ThemeToggle.tsx
"use client";

import { useTheme } from "next-themes";
import { useEffect, useState } from "react";
import { Moon, Sun } from "lucide-react";

export function ThemeToggle() {
	const { theme, setTheme } = useTheme();
	const [mounted, setMounted] = useState(false);

	// Prevent hydration mismatch
	useEffect(() => {
		setMounted(true);
	}, []);

	if (!mounted) {
		return null;
	}

	return (
		<button
			onClick={() => setTheme(theme === "dark" ? "light" : "dark")}
			className="p-2 rounded-md bg-gray-200 dark:bg-gray-800"
			aria-label="Toggle theme"
		>
			{theme === "dark" ? (
				<Sun className="h-5 w-5 text-yellow-500" />
			) : (
				<Moon className="h-5 w-5 text-gray-700" />
			)}
		</button>
	);
}
```

### Setting Up ThemeProvider

Install and set up next-themes:

```bash
npm install next-themes
```

Add the ThemeProvider to your layout:

```tsx
// src/app/providers.tsx
"use client";

import { ThemeProvider as NextThemesProvider } from "next-themes";
import { type ThemeProviderProps } from "next-themes/dist/types";

export function ThemeProvider({ children, ...props }: ThemeProviderProps) {
	return <NextThemesProvider {...props}>{children}</NextThemesProvider>;
}

// src/app/layout.tsx
import { ThemeProvider } from "./providers";

export default function RootLayout({
	children,
}: {
	children: React.ReactNode;
}) {
	return (
		<html lang="en" suppressHydrationWarning>
			<body>
				<ThemeProvider attribute="class" defaultTheme="system" enableSystem>
					{children}
				</ThemeProvider>
			</body>
		</html>
	);
}
```

### Dark Mode Styling

When using the 'class' strategy, use dark: modifiers to style elements:

```tsx
// components/ui/Card.tsx with dark mode support
export function Card({ className, ...props }: CardProps) {
	return (
		<div
			className={cn(
				"bg-white dark:bg-gray-800 rounded-lg shadow-md overflow-hidden",
				className
			)}
			{...props}
		/>
	);
}

export function CardHeader({ className, ...props }: CardHeaderProps) {
	return (
		<div
			className={cn(
				"px-6 py-4 border-b border-gray-100 dark:border-gray-700",
				className
			)}
			{...props}
		/>
	);
}

export function CardTitle({ className, ...props }: CardTitleProps) {
	return (
		<h3
			className={cn(
				"text-lg font-semibold text-gray-900 dark:text-gray-100",
				className
			)}
			{...props}
		/>
	);
}

export function CardDescription({ className, ...props }: CardDescriptionProps) {
	return (
		<p
			className={cn("text-sm text-gray-500 dark:text-gray-400 mt-1", className)}
			{...props}
		/>
	);
}
```

## Advanced Styling Techniques

### Composing Utility Classes with clsx and tailwind-merge

The `cn` utility function we defined earlier makes it easy to compose class names conditionally:

```tsx
// Example component with conditional class composition
import { cn } from "@/lib/utils";

interface BadgeProps extends React.HTMLAttributes<HTMLSpanElement> {
	variant?: "default" | "outline" | "secondary" | "destructive";
}

export function Badge({
	className,
	variant = "default",
	...props
}: BadgeProps) {
	return (
		<span
			className={cn(
				"inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-semibold",
				{
					"bg-primary-100 text-primary-800": variant === "default",
					"bg-gray-100 text-gray-800 border border-gray-200":
						variant === "outline",
					"bg-gray-100 text-gray-800": variant === "secondary",
					"bg-red-100 text-red-800": variant === "destructive",
				},
				className
			)}
			{...props}
		/>
	);
}
```

### Adding Animations

TailwindCSS can be extended with custom animations:

```typescript
// tailwind.config.ts
import type { Config } from "tailwindcss";

const config: Config = {
	// ... other configuration
	theme: {
		extend: {
			// ... other extensions
			keyframes: {
				shimmer: {
					"0%": { backgroundPosition: "-200% 0" },
					"100%": { backgroundPosition: "200% 0" },
				},
				fadeIn: {
					from: { opacity: "0" },
					to: { opacity: "1" },
				},
				fadeOut: {
					from: { opacity: "1" },
					to: { opacity: "0" },
				},
				slideInFromTop: {
					from: { transform: "translateY(-10px)", opacity: "0" },
					to: { transform: "translateY(0)", opacity: "1" },
				},
			},
			animation: {
				shimmer: "shimmer 2s linear infinite",
				fadeIn: "fadeIn 0.3s ease-out",
				fadeOut: "fadeOut 0.3s ease-out",
				slideIn: "slideInFromTop 0.3s ease-out",
			},
		},
	},
};
export default config;
```

### Creating a Skeleton Loading Component

```tsx
// components/ui/Skeleton.tsx
import { cn } from "@/lib/utils";

interface SkeletonProps extends React.HTMLAttributes<HTMLDivElement> {}

export function Skeleton({ className, ...props }: SkeletonProps) {
	return (
		<div
			className={cn(
				"animate-pulse rounded-md bg-gray-200 dark:bg-gray-700",
				className
			)}
			{...props}
		/>
	);
}

export function CardSkeleton() {
	return (
		<div className="bg-white dark:bg-gray-800 rounded-lg shadow-md overflow-hidden">
			<div className="p-6">
				<Skeleton className="h-6 w-1/3 mb-4" />
				<Skeleton className="h-4 w-full mb-2" />
				<Skeleton className="h-4 w-3/4 mb-2" />
				<Skeleton className="h-4 w-5/6" />
				<div className="mt-6">
					<Skeleton className="h-10 w-1/3" />
				</div>
			</div>
		</div>
	);
}
```

## Key Takeaways

1. **Use TailwindCSS** for rapid development of consistent, responsive interfaces.

2. **Create a Component Library** with reusable UI components styled with Tailwind.

3. **Implement Dark Mode** early in your development process using next-themes.

4. **Compose Classes with Utilities** like clsx and tailwind-merge for cleaner code.

5. **Extend Tailwind's Configuration** to match your design system requirements.

In the next section, we'll explore AI integration in React applications.
