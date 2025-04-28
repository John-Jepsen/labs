const fs = require('fs');
const path = require('path');

// Test function to simulate chat history saving
function testChatHistorySave() {
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
    const filename = `test_chat_${timestamp}.md`;
    const filePath = path.join(historyDir, filename);
    
    // Mock conversation data
    const mockConversation = {
      messages: [
        {
          role: 'user',
          content: 'This is a test message from the user',
          timestamp: new Date().toISOString()
        },
        {
          role: 'assistant',
          content: 'This is a test response from the assistant',
          timestamp: new Date().toISOString()
        }
      ]
    };
    
    // Format the conversation
    let content = `# Test Chat Session: ${now.toLocaleString()}\n\n`;
    
    for (const message of mockConversation.messages) {
      const role = message.role === 'user' ? '## User' : '## Assistant';
      const time = message.timestamp ? new Date(message.timestamp).toLocaleString() : now.toLocaleString();
      
      content += `${role} (${time}):\n\n${message.content}\n\n---\n\n`;
    }
    
    // Write to file
    fs.writeFileSync(filePath, content);
    console.log(`Test chat history saved to ${filePath}`);
    return { success: true, filePath };
  } catch (error) {
    console.error("Error in test script:", error);
    return { success: false, error: error.message };
  }
}

// Run the test
testChatHistorySave(); 