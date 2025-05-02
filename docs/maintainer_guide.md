# Documentation Maintainer Guide

This guide explains how to maintain and update the documentation site when new content is added to the `docs/` directory.

## Quick Start

To update the site with new content:

```bash
./update_docs_site.sh
```

This will automatically scan all markdown files in the `docs/` directory, update the navigation structure in `mkdocs.yml`, and build the site.

## How It Works

The update process follows these steps:

1. Scans all markdown files in the `docs/` directory
2. Builds a hierarchical navigation structure from the file paths
3. Updates the `nav` section in `mkdocs.yml`
4. Rebuilds the site with `mkdocs build --clean`
5. Reports which files were added or removed

## Adding New Content

To add new content to the site:

1. Create your markdown files in the appropriate location in the `docs/` directory
2. Run `./update_docs_site.sh` to update the site
3. Check the site to ensure your content appears correctly

### Best Practices

- Use `index.md` files for section landing pages
- Organize related content in subdirectories
- Use meaningful filenames that describe the content
- Begin filenames with numbers for explicit ordering (e.g., `01_introduction.md`)

## Troubleshooting

If you encounter issues:

1. Check the error output from the update script
2. Verify your markdown files have correct formatting
3. Ensure file and directory names don't contain special characters
4. Check for broken links or missing resources

## Manual Navigation Structure

The script automatically creates the navigation structure based on the file hierarchy, but you can still manually edit `mkdocs.yml` after running the script if you need a custom structure.

However, note that running `update_docs_site.sh` again will overwrite your manual changes.

## Building and Serving Locally

To build and serve the documentation locally:

```bash
# Build the site
mkdocs build

# Serve the site locally
mkdocs serve
```

Then open your browser to `http://127.0.0.1:8000` to view the documentation.

## Deployment

The documentation is automatically deployed to GitHub Pages when changes are pushed to the main branch.
