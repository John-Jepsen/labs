#!/bin/bash

# serve_docs.sh - Script to serve the MkDocs site locally

# Display banner
echo "======================================================="
echo "        MkDocs Documentation Server                    "
echo "======================================================="
echo

# Check for Python
if ! command -v python3 &>/dev/null; then
    echo "Error: Python 3 is required but not installed."
    exit 1
fi

# Check for MkDocs
if ! command -v mkdocs &>/dev/null; then
    echo "Installing required Python packages..."
    pip install -r requirements.txt
fi

# Start the MkDocs server
echo "Starting MkDocs server..."
echo "The documentation will be available at http://127.0.0.1:8000"
echo "Press Ctrl+C to stop the server"
echo

mkdocs serve
