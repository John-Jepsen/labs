#!/bin/bash

# Example usage:
# ./manage_github_pages.sh on    # Enable GitHub Pages
# ./manage_github_pages.sh off   # Disable GitHub Pages

# Check if gh CLI is installed
if ! command -v gh &>/dev/null; then
    echo "Error: GitHub CLI (gh) is not installed"
    exit 1
fi

# Check if user is authenticated with gh
if ! gh auth status &>/dev/null; then
    echo "Error: Not authenticated with GitHub CLI"
    echo "Please run 'gh auth login' first"
    exit 1
fi

# Check for correct number of arguments
if [ $# -ne 1 ]; then
    echo "Usage: $0 [on|off]"
    exit 1
fi

# Set variables
USERNAME="John-Jepsen"
REPO="labs"
ACTION=$1

# Function to check API response
check_response() {
    if [ $? -eq 0 ]; then
        echo "Operation successful"
    else
        echo "Operation failed"
        exit 1
    fi
}

# Main logic
case $ACTION in
"on")
    echo "Enabling GitHub Pages..."
    gh api --method PATCH "/repos/$USERNAME/$REPO/pages" \
        --field source.branch='gh-pages' \
        --field source.path='/'
    check_response
    ;;
"off")
    echo "Disabling GitHub Pages..."
    gh api --method DELETE "/repos/$USERNAME/$REPO/pages"
    check_response
    ;;
*)
    echo "Invalid action. Use 'on' or 'off'"
    exit 1
    ;;
esac

echo "GitHub Pages status for $USERNAME/$REPO has been set to: $ACTION"
