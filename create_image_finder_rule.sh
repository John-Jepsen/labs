#!/bin/bash

cat >.cursor/rules/012-markdown-image-finder.mdc <<'EOF'
---
description: Tool for finding and inserting relevant images into markdown files
globs: ["*.md", "**/*.md"]
alwaysApply: false
---
# Markdown Image Finder

This rule provides a tool to search for images online and insert them directly into your markdown files.

## Features

- Search for images by description
- Preview images before insertion
- Properly format image links for markdown
- Support for attribution and captions
- Save images locally or link to online sources

## Usage

To find and insert an image:

1. Type `/find-image [description]` in your markdown file
2. Select from image search results
3. Choose between local saving or online linking
4. Add optional caption or attribution

## Image Search Integration

The tool integrates with multiple image sources:
- Unsplash for high-quality free images
- Pixabay for royalty-free images
- Pexels for professionally shot photos

## Implementation

```javascript
// Image finder implementation
cursor.commands.registerCommand('markdown.findImage', async function(context) {
  const editor = context.editor;
  const query = await context.prompt('Enter image search term:');
  
  if (!query) return;
  
  try {
    // Search for images using configured API
    const results = await searchImages(query);
    
    if (!results.length) {
      context.showMessage('No images found for: ' + query);
      return;
    }
    
    // Show image previews and selection UI
    const selection = await context.showQuickPick(
      results.map(img => ({
        label: img.title || 'Image',
        description: img.source,
        imageUrl: img.thumbnailUrl
      }))
    );
    
    if (!selection) return;
    
    const selectedImage = results[results.findIndex(img => 
      img.thumbnailUrl === selection.imageUrl)];
    
    // Ask for storage preference
    const storageType = await context.showQuickPick([
      { label: 'Link to online image', value: 'remote' },
      { label: 'Save image locally', value: 'local' }
    ]);
    
    if (!storageType) return;
    
    // Get caption information
    const caption = await context.prompt('Add caption (optional):');
    const altText = await context.prompt('Alt text:', query);
    
    // Generate markdown for image
    let markdownText;
    
    if (storageType.value === 'local') {
      // Save image locally
      const imagePath = await saveImageLocally(selectedImage, query);
      markdownText = "![" + altText + "](" + imagePath + " \"" + (selectedImage.title || query) + "\")";
    } else {
      markdownText = "![" + altText + "](" + selectedImage.url + " \"" + (selectedImage.title || query) + "\")";
    }
    
    // Add caption if provided
    if (caption) {
      markdownText += "\n*" + caption + "*";
      
      // Add attribution if available
      if (selectedImage.attribution) {
        markdownText += " [Source](" + selectedImage.attributionUrl + ")";
      }
    }
    
    // Insert at cursor position
    editor.insertText(markdownText);
    
  } catch (error) {
    context.showError('Error finding images: ' + error.message);
  }
});
```

## Configuration

Configure preferred image sources in your Cursor settings:

```json
{
  "markdown.imageFinder": {
    "preferredSources": ["unsplash", "pixabay", "pexels"],
    "defaultStorage": "local",
    "apiKeys": {
      "unsplash": "YOUR_UNSPLASH_API_KEY",
      "pixabay": "YOUR_PIXABAY_API_KEY",
      "pexels": "YOUR_PEXELS_API_KEY"
    }
  }
}
```
EOF

echo "Rule file created at .cursor/rules/012-markdown-image-finder.mdc"
