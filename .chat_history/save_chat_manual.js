const fs = require('fs');
const path = require('path');
const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

// Function to create a chat history with manual input
async function createManualChatHistory() {
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
    const filename = `manual_chat_${timestamp}.md`;
    const filePath = path.join(historyDir, filename);
    
    // Initialize content
    let content = `# Manual Chat Session: ${now.toLocaleString()}\n\n`;
    
    // Ask for chat title
    const title = await promptUser('Enter a title for this chat session (optional): ');
    if (title) {
      content = `# ${title}: ${now.toLocaleString()}\n\n`;
    }
    
    // Array to store messages
    const messages = [];
    
    console.log('\nEnter chat messages (type "done" on a new line when finished)');
    
    let adding = true;
    while (adding) {
      // Get user message
      const userMessage = await promptUser('\nEnter your message: ');
      if (userMessage.toLowerCase() === 'done') {
        adding = false;
        continue;
      }
      
      messages.push({
        role: 'user',
        content: userMessage,
        timestamp: new Date().toISOString()
      });
      
      // Get assistant message
      const assistantMessage = await promptUser('Enter assistant\'s response: ');
      if (assistantMessage.toLowerCase() === 'done') {
        adding = false;
        continue;
      }
      
      messages.push({
        role: 'assistant',
        content: assistantMessage,
        timestamp: new Date().toISOString()
      });
    }
    
    // Format the conversation
    for (const message of messages) {
      const role = message.role === 'user' ? '## User' : '## Assistant';
      const time = new Date(message.timestamp).toLocaleString();
      
      content += `${role} (${time}):\n\n${message.content}\n\n---\n\n`;
    }
    
    // Write to file
    fs.writeFileSync(filePath, content);
    console.log(`\nChat history saved to ${filePath}`);
    
    // Ask if the user wants to view the saved file
    const viewFile = await promptUser('Would you like to view the saved file? (y/n): ');
    if (viewFile.toLowerCase() === 'y') {
      console.log('\n--- File Contents ---\n');
      console.log(fs.readFileSync(filePath, 'utf8'));
      console.log('\n--- End of File ---');
    }
    
    return { success: true, filePath };
  } catch (error) {
    console.error("Error in manual chat save:", error);
    return { success: false, error: error.message };
  } finally {
    rl.close();
  }
}

// Helper function to prompt for input
function promptUser(question) {
  return new Promise((resolve) => {
    rl.question(question, (answer) => {
      resolve(answer);
    });
  });
}

// Run the manual chat history creator
createManualChatHistory(); 