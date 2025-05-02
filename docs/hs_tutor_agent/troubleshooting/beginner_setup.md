# Beginner Setup Troubleshooting

This guide helps resolve common issues encountered when setting up your coding environment for the first time.

## Python Installation Issues

### Python Not Found After Installation

**Symptoms**:

- Command `python` or `python3` returns "not found" or "not recognized"
- VS Code shows "Python interpreter not found"

**Solutions for Windows**:

1. **Check if Python is installed**:

   - Search for "Python" in your Start menu
   - If found, Python is installed but not in your PATH

2. **Add Python to PATH**:

   - Run the installer again
   - Select "Modify"
   - Check "Add Python to environment variables"
   - Complete the installation

   ![Add Python to PATH](../assets/python_path_windows.png)

3. **Manually add to PATH**:
   - Find your Python installation path (typically `C:\Users\YourName\AppData\Local\Programs\Python\Python311`)
   - Open System Properties > Advanced > Environment Variables
   - Edit the PATH variable and add the Python installation path
   - Add both the main folder and the `Scripts` subfolder

**Solutions for macOS**:

1. **Check installation**:

   - Open Terminal and run `which python3`
   - If no output, Python is not properly installed

2. **Use Homebrew**:

   - Install Homebrew if not already installed: `/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`
   - Install Python: `brew install python`
   - Restart Terminal

3. **Update PATH**:
   - Open `~/.zshrc` or `~/.bash_profile` with a text editor
   - Add: `export PATH="/usr/local/bin:$PATH"`
   - Save and run `source ~/.zshrc` or `source ~/.bash_profile`

**Solutions for Chromebooks**:

1. **Verify Linux is enabled**:

   - Go to Settings > Developers > Linux development environment
   - If not enabled, turn it on

2. **Reinstall Python**:

   ```
   sudo apt update
   sudo apt install python3 python3-pip
   ```

3. **Check version**:
   ```
   python3 --version
   ```

### pip Not Working

**Symptoms**:

- `pip` command not found
- Unable to install packages

**Solutions**:

1. **Verify pip installation**:

   - Windows: `python -m pip --version`
   - macOS/Linux: `python3 -m pip --version`

2. **Install pip if missing**:

   - Download [get-pip.py](https://bootstrap.pypa.io/get-pip.py)
   - Run: `python get-pip.py` or `python3 get-pip.py`

3. **Update pip**:
   - Windows: `python -m pip install --upgrade pip`
   - macOS/Linux: `python3 -m pip install --upgrade pip`

### Permission Errors When Installing Packages

**Symptoms**:

- "Permission denied" when using pip
- "Access is denied" messages

**Solutions**:

1. **Windows**:

   - Run Command Prompt as Administrator
   - Try installing again

2. **macOS/Linux**:
   - Use `pip install --user package_name`
   - Or create a virtual environment (recommended)

## VS Code Setup Problems

### Python Extension Not Working

**Symptoms**:

- No code completion
- No syntax highlighting for Python
- "Python interpreter not found" message

**Solutions**:

1. **Verify extension installation**:

   - Open Extensions view (Ctrl+Shift+X or Cmd+Shift+X)
   - Search for Python
   - If not installed, install it
   - If installed, try disabling and re-enabling

2. **Select Python interpreter**:

   - Press Ctrl+Shift+P or Cmd+Shift+P
   - Type "Python: Select Interpreter"
   - Choose your Python installation

   ![Select Python Interpreter](../assets/vscode_select_interpreter.png)

3. **Restart VS Code**:
   - Close VS Code completely
   - Reopen and check if the extension works

### Can't Open Terminal in VS Code

**Symptoms**:

- Terminal fails to open
- Error message when trying to open terminal

**Solutions**:

1. **Check terminal shell settings**:

   - Go to Settings (Ctrl+, or Cmd+,)
   - Search for "terminal.integrated.shell"
   - Make sure it's set correctly:
     - Windows: `C:\Windows\System32\cmd.exe` or PowerShell
     - macOS: `/bin/zsh` or `/bin/bash`
     - Linux: `/bin/bash`

2. **Reinstall VS Code**:
   - Uninstall VS Code
   - Download fresh copy from [code.visualstudio.com](https://code.visualstudio.com/)
   - Install and try again

### Can't Run Python Files

**Symptoms**:

- No Run button appears above Python files
- Error when trying to run Python code

**Solutions**:

1. **Check file extension**:

   - Make sure your file ends with `.py`
   - Save the file if you haven't already

2. **Verify Python path**:

   - Open VS Code settings
   - Search for "python.pythonPath"
   - Ensure it points to your Python installation

3. **Try running from terminal**:
   - Open terminal in VS Code
   - Navigate to your file location
   - Run: `python filename.py` or `python3 filename.py`

## Git Installation Issues

### Git Not Found After Installation

**Symptoms**:

- `git` command not found
- VS Code shows "Git not found"

**Solutions for Windows**:

1. **Verify installation**:

   - Search for "Git Bash" in Start menu
   - If found, Git is installed but not in PATH

2. **Add Git to PATH**:

   - Run the Git installer again
   - Choose "Modify"
   - Select "Use Git from the Windows Command Prompt"
   - Complete installation

3. **Manually add to PATH**:
   - Find Git installation (typically `C:\Program Files\Git\bin`)
   - Add to PATH environment variable

**Solutions for macOS**:

1. **Install via Homebrew**:

   ```
   brew install git
   ```

2. **Install Xcode Command Line Tools**:

   ```
   xcode-select --install
   ```

3. **Verify installation**:
   ```
   git --version
   ```

### Authentication Issues with Git

**Symptoms**:

- "Authentication failed" when pushing/pulling
- Requests for username/password repeatedly

**Solutions**:

1. **Check your credentials**:

   - Verify your username and email are set:
     ```
     git config --global user.name
     git config --global user.email
     ```
   - Set them if needed:
     ```
     git config --global user.name "Your Name"
     git config --global user.email "your.email@example.com"
     ```

2. **Use credential manager**:

   - Windows: Git Credential Manager is installed with Git
   - macOS:
     ```
     git config --global credential.helper osxkeychain
     ```
   - Linux:
     ```
     git config --global credential.helper store
     ```

3. **Generate SSH key** (alternative approach):
   - Generate key:
     ```
     ssh-keygen -t ed25519 -C "your.email@example.com"
     ```
   - Add to your GitHub/GitLab account
   - Clone using SSH URL instead of HTTPS

## Node.js Installation Issues

### Node.js Command Not Found

**Symptoms**:

- `node` or `npm` commands not found
- "Not recognized" errors

**Solutions for Windows**:

1. **Verify installation**:

   - Check Programs and Features for Node.js
   - If present, add to PATH manually

2. **Reinstall Node.js**:
   - Download LTS version from [nodejs.org](https://nodejs.org/)
   - During installation, check "Automatically install necessary tools"
   - Complete installation and restart computer

**Solutions for macOS**:

1. **Install via Homebrew**:

   ```
   brew install node
   ```

2. **Verify PATH**:

   - Check if `/usr/local/bin` is in your PATH
   - If not, add it to `~/.zshrc` or `~/.bash_profile`

3. **Verify installation**:
   ```
   node --version
   npm --version
   ```

### npm Install Errors

**Symptoms**:

- Error messages during package installation
- Failed dependencies

**Solutions**:

1. **Clear npm cache**:

   ```
   npm cache clean --force
   ```

2. **Update npm**:

   ```
   npm install -g npm@latest
   ```

3. **Fix permissions** (macOS/Linux):
   ```
   sudo chown -R $(whoami) ~/.npm
   ```

## General Troubleshooting Tips

### File Path Issues

**Symptoms**:

- "File not found" errors when running code
- Issues opening or saving files

**Solutions**:

1. **Check for spaces in filenames**:

   - Avoid spaces in file and folder names
   - Use underscores or hyphens instead

2. **Check file paths**:

   - Use forward slashes `/` in code, even on Windows
   - Use relative paths when possible

3. **Check current working directory**:
   - Print current directory in your code:
     ```python
     import os
     print(os.getcwd())
     ```
   - Make sure you're in the right folder when running commands

### Permission Issues

**Symptoms**:

- "Permission denied" errors
- Can't save or modify files

**Solutions**:

1. **Check file permissions**:

   - Windows: Right-click > Properties > Security
   - macOS/Linux: `ls -la` to view permissions

2. **Run as administrator/use sudo**:

   - Windows: Run terminal as Administrator
   - macOS/Linux: Use `sudo` for commands requiring privileges

3. **Change file ownership** (macOS/Linux):
   ```
   sudo chown -R username:group directory
   ```

### Internet Connection Issues

**Symptoms**:

- Timeouts when downloading packages
- Can't connect to GitHub or other services

**Solutions**:

1. **Check internet connection**:

   - Try opening a website in your browser
   - Check your Wi-Fi connection

2. **Check proxy settings**:

   - If you're behind a proxy (common in schools):
     ```
     git config --global http.proxy http://proxy-server:port
     ```
     ```
     npm config set proxy http://proxy-server:port
     ```

3. **Try using a different network**:
   - Mobile hotspot
   - Home network instead of school network

## Getting More Help

If you're still experiencing issues:

1. **Ask your AI tutor**:

   - Provide the exact error message
   - Explain what you've already tried
   - Share screenshots if possible

2. **Check documentation**:

   - [Python documentation](https://docs.python.org/)
   - [VS Code documentation](https://code.visualstudio.com/docs)
   - [Git documentation](https://git-scm.com/doc)

3. **School IT support**:

   - Many school-provided devices have restrictions
   - IT support may need to approve software installation

4. **Online communities**:
   - [Stack Overflow](https://stackoverflow.com/)
   - [Reddit r/learnprogramming](https://www.reddit.com/r/learnprogramming/)
   - [GitHub Community Forum](https://github.community/)
