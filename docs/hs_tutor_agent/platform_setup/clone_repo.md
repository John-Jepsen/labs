# Cloning the Repository

This guide will walk you through cloning the AI Tutor platform repository to your local machine, which is the first step in setting up the development environment.

## Prerequisites

Before cloning the repository, ensure you have:

- Git installed on your machine (version 2.20+)
- A GitHub account (if you need to access private repositories)
- Sufficient disk space (at least 1GB recommended)
- Terminal or Command Prompt access

## Getting the Repository URL

The AI Tutor platform code is hosted on GitHub. You'll need the repository URL to clone it.

<!-- TODO: Add the actual repository URL when available -->

```
https://github.com/example/ai-tutor-platform.git
```

## Cloning the Repository

### Using HTTPS (Recommended for Beginners)

1. Open your terminal or command prompt
2. Navigate to the directory where you want to store the project
3. Run the clone command:

```bash
git clone https://github.com/example/ai-tutor-platform.git
```

<!-- TODO: Add screenshot of successful clone -->

### Using SSH (For developers with SSH keys configured)

If you have SSH keys set up with GitHub (for enhanced security):

```bash
git clone git@github.com:example/ai-tutor-platform.git
```

<!-- TODO: Add instructions for setting up SSH keys -->

## Navigating to the Project Directory

After cloning, navigate to the project directory:

```bash
cd ai-tutor-platform
```

## Repository Structure

The repository has the following structure:

```
ai-tutor-platform/
├── backend/               # FastAPI server code
├── frontend/              # React frontend code
├── lessons/               # Curriculum content
├── docs/                  # Documentation
├── scripts/               # Utility scripts
├── .env.example           # Example environment variables
├── README.md              # Project overview
└── docker-compose.yml     # Docker configuration
```

## Checking Out a Specific Branch

If you need to work on a specific branch:

```bash
git checkout <branch-name>
```

For example:

```bash
git checkout development
```

## Updating Your Local Copy

To ensure you have the latest code:

```bash
git pull
```

## Troubleshooting Clone Issues

### Permission Denied

If you see "Permission denied" errors:

- Verify you have the correct access rights to the repository
- Ensure your GitHub credentials are correctly configured

### SSL Certificate Error

If you encounter SSL certificate issues:

```bash
git config --global http.sslVerify false
```

Note: This is not recommended for security reasons, but can be a temporary fix.

<!-- TODO: Add more common issues and solutions -->

## Next Steps

Now that you've cloned the repository, you're ready to [set up the backend](backend_setup.md).
