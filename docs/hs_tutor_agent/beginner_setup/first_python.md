# Creating Your First Python Program

This guide will walk you through creating and running your first Python program, which is an exciting milestone in your coding journey!

## Setting Up Your Workspace

Before you start coding, it's good to set up a dedicated folder for your Python projects:

1. Create a folder named "python_projects" on your computer
2. Open VS Code
3. Click on "File" > "Open Folder" and select your new folder

<!-- TODO: Add screenshots of folder creation and opening in VS Code -->

## Creating a New Python File

Now let's create your first Python file:

1. In VS Code, click on the Explorer icon in the left sidebar
2. Click on "New File" icon
3. Name your file `hello_world.py`
   - The `.py` extension tells VS Code this is a Python file

<!-- TODO: Add screenshots of file creation process -->

## Writing Your First Code

In your new file, type the following code:

```python
# This is my first Python program
print("Hello, World!")
```

This code does two things:

- The first line is a comment (starts with `#`). Comments are ignored by Python but help humans understand the code.
- The second line uses the `print()` function to display the text "Hello, World!" on the screen.

<!-- TODO: Add screenshot of code in VS Code -->

## Running Your Program

Now it's time to run your program and see the results:

### Method 1: Using VS Code's Run Button

1. Make sure your `hello_world.py` file is open
2. Look for the "Play" button (▶️) in the top-right corner of VS Code
3. Click the button to run your program
4. Look at the terminal at the bottom of VS Code to see your output

### Method 2: Using the Terminal

1. Open a terminal in VS Code by clicking "Terminal" > "New Terminal"
2. In the terminal, type:
   ```
   python hello_world.py
   ```
   or if that doesn't work, try:
   ```
   python3 hello_world.py
   ```
3. Press Enter to run your program

<!-- TODO: Add screenshots of both methods -->

## Expected Output

After running your program, you should see the following output:

```
Hello, World!
```

Congratulations! You've just written and run your first Python program!

## Making Changes

Try modifying your program to make it more personal:

1. Change your code to:

   ```python
   name = "Your Name"  # Replace with your actual name
   print("Hello, " + name + "!")
   ```

2. Run your program again using one of the methods above

3. You should now see a personalized greeting with your name

<!-- TODO: Add screenshot of modified code and output -->

## Adding More Functionality

Let's expand your program to do a bit more:

```python
name = "Your Name"  # Replace with your actual name
age = 15  # Replace with your actual age

print("Hello, " + name + "!")
print("You are " + str(age) + " years old.")
print("In 10 years, you will be " + str(age + 10) + " years old.")
```

This program introduces several new concepts:

- Storing numbers in variables
- Converting numbers to strings with `str()`
- Basic math operations

<!-- TODO: Add screenshot of expanded code and output -->

## Common Errors and Solutions

When writing Python code, you might encounter some errors:

### Syntax Errors

These happen when your code doesn't follow Python's rules.

Example:

```python
print("Hello, World!"
```

Error: Missing closing parenthesis

<!-- TODO: Add more common syntax errors and solutions -->

### Runtime Errors

These happen when your code runs but encounters a problem during execution.

Example:

```python
name = "Your Name"
age = "fifteen"
print("In 10 years, you will be " + age + 10 + " years old.")
```

Error: Can't add a string and a number

<!-- TODO: Add more common runtime errors and solutions -->

## Next Steps

Now that you've created your first Python program, you're ready to learn how to [run code in the terminal](terminal_basics.md) more effectively!
