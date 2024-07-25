```

     ,o888888o.    8 888888888o.      8 888888888o
    8888     `88.  8 8888    `^888.   8 8888    `88.
 ,8 8888       `8. 8 8888        `88. 8 8888     `88
 88 8888           8 8888         `88 8 8888     ,88
 88 8888           8 8888          88 8 8888.   ,88'
 88 8888           8 8888          88 8 8888888888
 88 8888   8888888 8 8888         ,88 8 8888    `88.
 `8 8888       .8' 8 8888        ,88' 8 8888      88
    8888     ,88'  8 8888    ,o88P'   8 8888    ,88'
     `8888888P'    8 888888888P'      8 888888888P

```

```
In the tranquil town of Bashlandia, an evil entity known as the Compiler wreaked havoc, causing programs to crash and citizens to despair. Just when hope seemed lost, a hero named Debuggerman emerged, wielding the powerful tools of GDB. With unwavering determination, Debuggerman set breakpoints, stepped through code, and inspected variables with precision. Each bug revealed and squashed brought newfound stability to Bashlandia's digital realm. Through tireless effort and expertise, Debuggerman vanquished the Compiler, restoring peace and functionality to the town, proving that even the toughest code could be conquered with the right tools and a sharp mind.

```

#### Step 1: Install GDB

**On Debian-based Linux (e.g., Ubuntu):**

```sh
sudo apt-get update
sudo apt-get install gdb
```

**On Windows:**

1. Download MinGW, which includes GDB.
2. Install MinGW and select the `mingw32-gdb` package.

#### Step 2: Compile Your C Program with Debug Information

Compile your program with the `-g` option to include debugging information.

```sh
gcc -g -o example example.c
```

#### Step 3: Start GDB

Start GDB by loading your program into it.

```sh
gdb example
```

#### Step 4: Basic GDB Commands

Here are some basic commands to get you started with GDB.

**Run the program:**

```sh
(gdb) run
```

**Set a breakpoint at the start of `main` function:**

```sh
(gdb) break main
```

**Continue running the program until the next breakpoint:**

```sh
(gdb) continue
```

**Print the value of a variable:**

```sh
(gdb) print variable_name
```

**Step to the next line of code (step over functions):**

```sh
(gdb) next
```

**Exit GDB:**

```sh
(gdb) quit
```

#### Step 5: Practical Example with Detailed Steps

1. **Compile with Debug Information:**

   ```sh
   gcc -g -o example example.c
   ```

2. **Start GDB:**

   ```sh
   gdb example
   ```

3. **Set a Breakpoint at `main`:**

   ```sh
   (gdb) break main
   ```

4. **Run the Program:**

   ```sh
   (gdb) run
   ```

   You'll see output like:

   ```
   Starting program: /path/to/example
   welcome to the program with a bug!
   ```

5. **Step into `main`:**

   ```sh
   (gdb) step
   ```

6. **Print the Value of `d`:**

   ```sh
   (gdb) print d
   ```

7. **Step to the Next Line (to the `scanf`):**

   ```sh
   (gdb) next
   ```

8. **Notice the Bug:**
   You'll notice that `scanf` expects a pointer, but we are passing `d` by value. To fix the bug, modify the code as follows:

   ```c
   scanf("%d", &d);
   ```

9. **Recompile the Program:**

   ```sh
   gcc -g -o example example.c
   ```

10. **Restart GDB:**

    ```sh
    (gdb) run
    ```

11. **Set the Breakpoint and Run Again:**

    ```sh
    (gdb) break main
    (gdb) run
    ```

12. **Step Over `scanf`:**

    ```sh
    (gdb) next
    ```

13. **Print the Value of `d` After Input:**

    ```sh
    (gdb) print d
    ```

14. **Continue Execution:**

    ```sh
    (gdb) continue
    ```

15. **Exit GDB:**
    ```sh
    (gdb) quit
    ```

By following these steps, you'll learn how to debug the program, identify the bug (passing `d` by value instead of by reference), and fix it. After fixing the bug, you can recompile and run the program again to ensure it works correctly.
