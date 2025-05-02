#!/bin/bash

# update_docs_site.sh - Script to update the MkDocs site with content from docs/ directory

set -e

# Display banner
echo "======================================================="
echo "         MkDocs Documentation Site Updater             "
echo "======================================================="
echo

# Check for Python
if ! command -v python3 &>/dev/null; then
    echo "Error: Python 3 is required but not installed."
    exit 1
fi

# Check for necessary Python packages
required_packages=("mkdocs" "pyyaml")
missing_packages=()

for package in "${required_packages[@]}"; do
    if ! python3 -c "import $package" 2>/dev/null; then
        missing_packages+=("$package")
    fi
done

if [ ${#missing_packages[@]} -gt 0 ]; then
    echo "Installing required Python packages..."
    pip install -r requirements.txt
fi

# Run the updater script
echo "Updating MkDocs configuration and building site..."
python3 update_mkdocs.py

# Check if the update was successful
if [ $? -eq 0 ]; then
    echo
    echo "======================================================="
    echo "Documentation site updated successfully!"
    echo "Site is available in the 'site/' directory"
    echo "To serve the documentation locally, run 'mkdocs serve'"
    echo "======================================================="
else
    echo
    echo "======================================================="
    echo "Error: Documentation site update failed!"
    echo "Check the log output above for details."
    echo "======================================================="
    exit 1
fi
