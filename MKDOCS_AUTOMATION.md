# MkDocs Automation System

This document provides an overview of the MkDocs automation system implemented in this repository.

## Automation Components

The system includes the following components:

1. **update_mkdocs.py** - A Python script that:

   - Scans the `docs/` directory
   - Builds a navigation structure
   - Updates the `mkdocs.yml` file
   - Builds the site

2. **update_docs_site.sh** - A shell script that:

   - Checks for dependencies
   - Runs `update_mkdocs.py`
   - Provides user-friendly output

3. **serve_docs.sh** - A shell script that:

   - Checks for dependencies
   - Starts a local MkDocs server

4. **mkdocs.yml** - The configuration file:

   - Auto-updated by the scripts
   - Contains the generated navigation structure

5. **docs/maintainer_guide.md** - Documentation for:
   - How to use the automation system
   - Best practices for organizing content

## Key Features

- **Automatic Navigation** - The system automatically builds a hierarchical navigation structure based on the directory structure in `docs/`.
- **Special Handling for index.md** - Files named `index.md` are treated as section landing pages.
- **Alphabetical Ordering** - Items are ordered alphabetically by default.
- **Numerical Prefix Support** - Files can be prefixed with numbers (e.g., `01_introduction.md`) for explicit ordering.
- **Change Tracking** - The system reports which files were added or removed from the navigation.
- **Error Reporting** - Clear error messages when issues occur.

## Usage

To update the site with new content:

```bash
./update_docs_site.sh
```

To serve the site locally:

```bash
./serve_docs.sh
```

## How It Works

1. The system scans all markdown files in `docs/` recursively
2. It builds a nested dictionary representing the file hierarchy
3. Special handling is applied for `index.md` files
4. The dictionary is converted to MkDocs navigation format
5. The `nav` section of `mkdocs.yml` is updated
6. The site is built using `mkdocs build --clean`

## Extending the System

To extend the automation system:

1. Modify `update_mkdocs.py` to change how navigation is generated
2. Update the shell scripts to add new functionality
3. Add new documentation in `docs/maintainer_guide.md`

## Implementation Notes

- The system preserves the basic structure of `mkdocs.yml` while updating only the `nav` section
- It uses PyYAML with `sort_keys=False` to maintain the order of sections
- It converts relative file paths to use forward slashes for cross-platform compatibility
