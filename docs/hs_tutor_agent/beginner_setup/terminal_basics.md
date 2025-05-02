# Running Code in the Terminal

This guide will teach you how to use the terminal (also called command line or console) to run your Python programs and perform basic operations.

## What is the Terminal?

The terminal is a text-based interface that allows you to interact with your computer by typing commands. It's a powerful tool that programmers use to:

- Navigate through files and folders
- Run programs
- Install software
- Perform tasks that might be difficult or impossible with a graphical interface

## Opening the Terminal

### On Windows

1. Press `Win + R` to open the Run dialog
2. Type `cmd` and press Enter
3. Alternatively, search for "Command Prompt" in the Start menu

<!-- TODO: Add screenshot of Windows Command Prompt -->

### On macOS

1. Press `Cmd + Space` to open Spotlight Search
2. Type "Terminal" and press Enter
3. Alternatively, go to Applications > Utilities > Terminal

<!-- TODO: Add screenshot of macOS Terminal -->

### On Chromebook

1. Click on the Launcher (circle icon)
2. Search for "Terminal"
3. If you're using Linux on your Chromebook, open the Linux terminal

<!-- TODO: Add screenshot of Chromebook Terminal -->

### In VS Code

1. Open VS Code
2. Click on "Terminal" in the top menu
3. Select "New Terminal"

<!-- TODO: Add screenshot of VS Code Terminal -->

## Basic Terminal Commands

Here are some essential commands to help you navigate:

| Command           | Windows             | macOS/Linux         | Description                      |
| ----------------- | ------------------- | ------------------- | -------------------------------- |
| List files        | `dir`               | `ls`                | Shows files in current directory |
| Change directory  | `cd folder_name`    | `cd folder_name`    | Moves to specified folder        |
| Move up one level | `cd ..`             | `cd ..`             | Goes to parent directory         |
| Clear screen      | `cls`               | `clear`             | Clears terminal output           |
| Current path      | `cd`                | `pwd`               | Shows current directory path     |
| Create directory  | `mkdir folder_name` | `mkdir folder_name` | Creates a new folder             |

<!-- TODO: Add examples of using these commands -->

## Running Python Programs in the Terminal

To run a Python program from the terminal:

1. Navigate to the directory containing your Python file using `cd`
2. Type one of the following commands:
   ```
   python filename.py
   ```
   or
   ```
   python3 filename.py
   ```

### Example

If your `hello_world.py` file is in a folder called "python_projects":

#### Windows

```
cd python_projects
python hello_world.py
```

#### macOS/Linux

```
cd python_projects
python3 hello_world.py
```

<!-- TODO: Add screenshots of running Python programs in terminal -->

## Passing Arguments to Python Programs

You can pass information to your Python programs when you run them:

```
python my_program.py argument1 argument2
```

Example Python program that uses arguments:

```python
import sys

# sys.argv[0] is the program name
# sys.argv[1], sys.argv[2], etc. are the arguments
print("Program name:", sys.argv[0])
print("First argument:", sys.argv[1])
```

<!-- TODO: Add examples of running programs with arguments -->

## Running Python in Interactive Mode

Python has an interactive mode where you can type and execute code line by line:

1. Open your terminal
2. Type `python` or `python3` and press Enter
3. You should see the Python prompt (`>>>`)
4. Type Python code and press Enter to execute it
5. To exit, type `exit()` or press `Ctrl+Z` on Windows or `Ctrl+D` on macOS/Linux

<!-- TODO: Add screenshot of interactive Python session -->

## Installing Python Packages with pip

You can install additional Python libraries using pip:

```
pip install package_name
```

or

```
python -m pip install package_name
```

Example:

```
pip install requests
```

<!-- TODO: Add examples of installing packages -->

## Terminal Tips and Tricks

- Press the up arrow to recall previous commands
- Press `Tab` for auto-completion
- Use `Ctrl+C` to stop a running program
- Type `help` (Windows) or `man command_name` (macOS/Linux) to get help on commands

<!-- TODO: Add more tips and tricks -->

## Troubleshooting

Common issues when using the terminal:

<!-- TODO: Add common terminal issues and solutions -->

## Next Steps

Now that you know how to use the terminal to run your Python programs, you're ready to start building more complex projects with the AI Tutor platform!
