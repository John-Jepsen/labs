#!/bin/bash

RULE_FILE=".cursor/rules/011-chat-history-saver.mdc"

# Create the frontmatter
cat >"$RULE_FILE" <<'EOF'
---
description: Automatically saves all chat history to maintain a record of all conversations
globs: "**/*"
---

# Chat History Saver

This rule ensures that all chat conversations with Cursor are automatically saved to a dedicated history folder. Chat histories include timestamps, user queries, and assistant responses for future reference and documentation.

## Implementation Details

- Every new chat session will be saved as a Markdown file in the `.chat_history` folder
- File names include date and time of the conversation start
- Chat content includes complete prompts and responses
- Each interaction is timestamped for easy reference
- Files are organized chronologically

## Benefits

- Maintains a searchable archive of all conversations 
- Prevents loss of valuable problem-solving insights
- Allows review of past approaches to similar problems
- Enables tracking of project evolution through conversations
- Provides documentation of decision-making processes

## Privacy Note

Be mindful that chat histories may contain sensitive information or credentials. Review chat history files before sharing repositories with others.
EOF

# Create the chat history directory if it doesn't exist
mkdir -p .chat_history

echo "Chat history rule created at $RULE_FILE"
echo "Chat history directory created at .chat_history"
