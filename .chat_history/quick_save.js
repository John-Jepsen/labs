const fs = require('fs');
const path = require('path');

// Function to save the current chat
function saveCurrentChat() {
  try {
    // Get current directory
    const workspacePath = process.cwd();
    
    // Create chat history directory if it doesn't exist
    const historyDir = path.join(workspacePath, '.chat_history');
    if (!fs.existsSync(historyDir)) {
      fs.mkdirSync(historyDir, { recursive: true });
      console.log(`Created directory: ${historyDir}`);
    } else {
      console.log(`Directory already exists: ${historyDir}`);
    }
    
    // Generate filename with timestamp
    const now = new Date();
    const timestamp = now.toISOString().replace(/[:.]/g, '-');
    const filename = `quick_chat_${timestamp}.md`;
    const filePath = path.join(historyDir, filename);
    
    // Current conversation (manual reconstruction)
    const messages = [
      {
        role: 'user',
        content: 'run a test to see if the chat history rule is working correctly i just opened a new chat and it didnt work',
        timestamp: now.toISOString()
      },
      {
        role: 'assistant',
        content: 'I\'ll help you test if the chat history rule is working correctly. Let\'s first check if the chat history directory exists and then see if any files have been created for this conversation...(assistant response content)',
        timestamp: now.toISOString()
      },
      {
        role: 'user',
        content: 'lets do this \n\nConsider adding a manual action button in the rule that you can trigger to save the chat history\n\nchange the to exictue when I type in "save_chat" into the chat. at that moment is will copy the chat and save it to the file',
        timestamp: now.toISOString()
      },
      {
        role: 'assistant',
        content: 'I\'ll modify the rule to add a manual action button that saves the chat history when you type "save_chat" in the chat...(assistant response content)',
        timestamp: now.toISOString()
      },
      {
        role: 'user',
        content: 'save_chat',
        timestamp: now.toISOString()
      }
    ];
    
    // Format the conversation
    let content = `# Chat Session: ${now.toLocaleString()}\n\n`;
    
    for (const message of messages) {
      const role = message.role === 'user' ? '## User' : '## Assistant';
      const time = new Date(message.timestamp).toLocaleString();
      
      content += `${role} (${time}):\n\n${message.content}\n\n---\n\n`;
    }
    
    // Write to file
    fs.writeFileSync(filePath, content);
    console.log(`\nChat history saved to ${filePath}`);
    
    return { success: true, filePath };
  } catch (error) {
    console.error("Error saving chat:", error);
    return { success: false, error: error.message };
  }
}

// Run the save function
saveCurrentChat(); 