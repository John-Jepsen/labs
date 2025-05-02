# Your First Python Program: Hello World!

In this tutorial, you'll write your very first Python program. This simple program will display the message "Hello, World!" on your screen.

## What You'll Learn

- How to create a Python file
- How to write a basic Python command
- How to run your code
- How to modify your program

## Step 1: Create a New Python File

1. Open Visual Studio Code
2. Click on "File" → "New File"
3. Save the file by clicking "File" → "Save" (or pressing Ctrl+S on Windows/Linux, Cmd+S on Mac)
4. Name the file `hello_world.py` (the `.py` extension tells your computer this is a Python file)

## Step 2: Write Your First Line of Code

In your new file, type the following line:

```python
print("Hello, World!")
```

That's it! Let's break down what this line does:

- `print()` is a built-in Python function that displays text on the screen
- The text inside the parentheses is what will be displayed
- The quotation marks `"` tell Python that you want to display the exact text between them
- The parentheses `()` tell Python that you're using a function

## Step 3: Run Your Code

Now it's time to see your program in action!

1. Make sure you've saved your file
2. Open a terminal in VS Code by clicking "Terminal" → "New Terminal"
3. In the terminal, type:
   ```
   python hello_world.py
   ```
4. Press Enter

You should see the following output:

```
Hello, World!
```

Congratulations! You've just written and run your first Python program! 🎉

## Step 4: Make It Your Own

Let's modify the program to make it more personal:

1. Change your code to:

   ```python
   name = "Your Name"  # Replace with your actual name
   print("Hello, " + name + "!")
   ```

2. Save the file and run it again using the same command:
   ```
   python hello_world.py
   ```

You should now see a personalized greeting with your name!

## What's Happening?

Let's break down the new code:

1. `name = "Your Name"` creates a variable called `name` and stores your name in it

   - A variable is like a labeled container that holds data
   - The equals sign `=` assigns the value on the right to the variable on the left

2. `print("Hello, " + name + "!")` combines multiple pieces of text:
   - `"Hello, "` is a fixed piece of text
   - `name` is your variable containing your name
   - `"!"` is another fixed piece of text
   - The `+` signs join these pieces together

## Try It Yourself!

Now that you understand the basics, try these challenges:

1. Change the greeting from "Hello" to something else like "Hi" or "Greetings"
2. Add another line using `print()` to display a second message
3. Create a new variable for your age and display it in a message

Example solution:

```python
name = "Your Name"
age = 15

print("Hi, " + name + "!")
print("You are " + str(age) + " years old.")
```

Note: `str(age)` converts your age number into text so it can be combined with other text.

## What's Next?

Now that you've written your first program, you're ready to learn more Python concepts! Here are some topics to explore next:

- [Getting user input](../user_input/user_input.md)
- [Making decisions with if statements](../conditionals/conditionals.md)
- [Working with numbers](../numbers/numbers.md)

Remember, every professional programmer started exactly where you are now - with their first "Hello, World!" program. Keep practicing, and you'll be coding complex applications before you know it!
