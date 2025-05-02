# Installing Git

This guide will walk you through installing Git, a version control system that helps you track changes to your code and collaborate with others.

## What is Git?

Git is a version control system that allows you to:

- Track changes to your code
- Revert to previous versions if something goes wrong
- Collaborate with others on the same codebase
- Back up your code to remote repositories like GitHub

## Downloading Git

<!-- TODO: Add screenshots of Git download page -->

### For Windows

1. Go to the official Git website at [git-scm.com](https://git-scm.com)
2. Download the latest version of Git for Windows
3. Run the installer

<!-- TODO: Add detailed steps with screenshots -->

### For macOS

There are multiple ways to install Git on macOS:

#### Using Homebrew (Recommended)

1. Open Terminal
2. Install Homebrew if not already installed
3. Run `brew install git`

#### Using the Git Installer

1. Go to the official Git website at [git-scm.com](https://git-scm.com)
2. Download the latest version of Git for macOS
3. Run the installer package

<!-- TODO: Add detailed steps with screenshots -->

### For Chromebooks

1. Enable Linux on your Chromebook
2. Open the Terminal app
3. Run `sudo apt install git`

<!-- TODO: Add detailed steps with screenshots -->

## Verifying Your Installation

To verify that Git was installed correctly:

1. Open a terminal or command prompt
2. Type `git --version`
3. You should see the Git version number displayed

<!-- TODO: Add screenshot of successful verification -->

## Configuring Git

Before you start using Git, you need to configure your identity:

```bash
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"
```

<!-- TODO: Add additional configuration options -->

## Basic Git Commands

Here are some essential Git commands to get started:

- `git init`: Initialize a new Git repository
- `git clone`: Clone an existing repository
- `git add`: Add files to the staging area
- `git commit`: Commit changes to the repository
- `git status`: Check the status of your repository

<!-- TODO: Add examples of using these commands -->

## Troubleshooting

If you encounter issues during installation:

<!-- TODO: Add common issues and solutions -->

## Next Steps

Now that you have Git installed, you're ready to [install Node.js](install_nodejs.md)!
