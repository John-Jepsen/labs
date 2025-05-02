# Installing Node.js

This guide will walk you through installing Node.js, a JavaScript runtime that allows you to run JavaScript code outside of a web browser.

## What is Node.js?

Node.js is a runtime environment that:

- Allows you to run JavaScript on your computer (not just in browsers)
- Enables server-side JavaScript applications
- Provides a package manager (npm) for installing JavaScript libraries
- Is essential for modern web development

## Downloading Node.js

<!-- TODO: Add screenshots of Node.js download page -->

### For Windows

1. Go to the official Node.js website at [nodejs.org](https://nodejs.org)
2. Download the LTS (Long Term Support) version
3. Run the installer

<!-- TODO: Add detailed steps with screenshots -->

### For macOS

There are multiple ways to install Node.js on macOS:

#### Using Homebrew (Recommended)

1. Open Terminal
2. Install Homebrew if not already installed
3. Run `brew install node`

#### Using the Node.js Installer

1. Go to the official Node.js website at [nodejs.org](https://nodejs.org)
2. Download the LTS version for macOS
3. Run the installer package

<!-- TODO: Add detailed steps with screenshots -->

### For Chromebooks

1. Enable Linux on your Chromebook
2. Open the Terminal app
3. Run `sudo apt install nodejs npm`

<!-- TODO: Add detailed steps with screenshots -->

## Verifying Your Installation

To verify that Node.js and npm were installed correctly:

1. Open a terminal or command prompt
2. Type `node --version`
3. Type `npm --version`
4. You should see version numbers for both

<!-- TODO: Add screenshot of successful verification -->

## Basic Node.js Usage

Here's a simple example of running JavaScript with Node.js:

1. Create a file named `hello.js` with the following content:

   ```javascript
   console.log("Hello, World!");
   ```

2. Run it using:

   ```bash
   node hello.js
   ```

3. You should see "Hello, World!" printed in the terminal

<!-- TODO: Add more examples and use cases -->

## Introduction to npm

npm (Node Package Manager) comes with Node.js and allows you to install libraries and tools.

### Basic npm Commands

- `npm init`: Initialize a new Node.js project
- `npm install <package>`: Install a package
- `npm install -g <package>`: Install a package globally
- `npm update`: Update installed packages

<!-- TODO: Add examples of using these commands -->

## Troubleshooting

If you encounter issues during installation:

<!-- TODO: Add common issues and solutions -->

## Next Steps

Now that you have Node.js installed, you're ready to [create your first Python program](first_python.md)!
