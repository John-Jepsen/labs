# Installing Visual Studio Code

Visual Studio Code (VS Code) is a free, lightweight code editor that we'll use to write and run our code. It has many helpful features that make coding easier and more enjoyable.

## Download VS Code

1. Open your web browser and go to [https://code.visualstudio.com/](https://code.visualstudio.com/)

2. The website should automatically detect your operating system and show the appropriate download button:

   ![VS Code download page](../assets/vscode_download.png)

3. Click the download button to start downloading the installer

## Install VS Code on Windows

1. Once the download is complete, open the installer file (typically named something like `VSCodeSetup-x64-1.xx.x.exe`)

2. Accept the license agreement and click "Next"

3. Leave the destination location as default and click "Next"

4. On the Select Start Menu Folder screen, leave as default and click "Next"

5. On the Select Additional Tasks screen, make sure these options are checked:

   - Create a desktop icon
   - Add "Open with Code" action to Windows Explorer file context menu
   - Add "Open with Code" action to Windows Explorer directory context menu
   - Register Code as an editor for supported file types

   ![VS Code Windows installation options](../assets/vscode_windows_options.png)

6. Click "Next" and then "Install"

7. Once installation is complete, leave "Launch Visual Studio Code" checked and click "Finish"

## Install VS Code on macOS

1. Once the download is complete, open the downloaded file (typically named something like `VSCode-darwin-universal.zip`)

2. Drag the Visual Studio Code application to your Applications folder

   ![VS Code macOS installation](../assets/vscode_mac_install.png)

3. Open VS Code from your Applications folder or Launchpad

## Install VS Code on Chromebook

If you're using a Chromebook, you can install VS Code through Linux:

1. First, [enable Linux on your Chromebook](https://support.google.com/chromebook/answer/9145439)

2. Open the Terminal app

3. Update your package list:

   ```
   sudo apt update
   ```

4. Install VS Code:

   ```
   sudo apt install code
   ```

5. Once installed, you can launch VS Code from your app launcher

## First Launch and Setup

When you first open VS Code, you'll see a welcome screen like this:

![VS Code first launch](../assets/vscode_first_launch.png)

Take a moment to explore the interface:

1. The left sidebar contains icons for different views (Explorer, Search, Source Control, etc.)
2. The main area is where you'll write and edit code
3. The bottom panel can show a terminal, problems, output, and more

## Install Essential Extensions

VS Code becomes even more powerful with extensions. Let's install a few that will help us with Python programming:

1. Click on the Extensions icon in the left sidebar (or press `Ctrl+Shift+X` on Windows/Linux or `Cmd+Shift+X` on macOS)

2. Search for "Python" and install the official Python extension by Microsoft

   ![VS Code Python extension](../assets/vscode_python_extension.png)

3. Also search for and install:
   - "Python Indent" (helps with proper indentation)
   - "Pylance" (provides intelligent code suggestions)

## Troubleshooting

### Common Issues on Windows

- **Error during installation**: Make sure you have administrator privileges
- **VS Code doesn't start**: Try right-clicking and selecting "Run as administrator"

### Common Issues on macOS

- **"App is damaged"**: Go to System Preferences > Security & Privacy and click "Open Anyway"
- **Can't drag to Applications**: Make sure you have permission to write to the Applications folder

### Common Issues on Chromebooks

- **Linux not available**: Your Chromebook may not support Linux apps
- **Installation fails**: Try running `sudo apt update && sudo apt upgrade` first

## Next Steps

Now that you have VS Code installed, you're ready to [install Python](install_python.md)!
