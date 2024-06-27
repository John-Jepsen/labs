## Adventure Quest: The Bash Scripting Journey

### Introduction

Welcome, brave coder, to the mystical land of Bashlandia! Your quest is to aid the villagers by crafting powerful scripts to solve their everyday problems. Along the way, you'll encounter various challenges that will test your scripting skills. Ready your terminal, for your adventure begins now!

### Chapter 1: The Missing File

Your journey begins in the village of Fileton. The villagers are in distress because an important document has gone missing. Your first task is to write a script that checks if a given file exists and prints a message indicating whether it does or not.

```bash
# File Existence Check Script
echo "Enter the filename to check:"
read filename

if [ -f "$filename" ]; then
    echo "The file '$filename' exists."
else
    echo "The file '$filename' does not exist."
fi
```

### Chapter 2: The Mysterious Directory

Having completed your first task, you move on to explore the mysterious forests of Dirwood. The villagers need to know what lies within the forest. Your task is to write a script that lists all files and directories in the current directory.

```bash
# Directory Listing Script
echo "Listing all files and directories in the current directory:"
ls -la
```

### Chapter 3: The Enchanted Count

In the heart of Dirwood lies a magical grove where the number of files in a directory holds the key to hidden treasures. Write a script that counts the number of files in a directory specified by the user.

```bash
# File Count Script
echo "Enter the directory to count files in:"
read dir

if [ -d "$dir" ]; then
    file_count=$(ls -1q "$dir" | wc -l)
    echo "There are $file_count files in the directory '$dir'."
else
    echo "The directory '$dir' does not exist."
fi
```

### Chapter 4: The Transporting Spell

Your adventure leads you to the village of Moville, where the villagers need help moving important scrolls to a safe location. Write a script that copies a file to a new location specified by the user.

```bash
# File Copy Script
echo "Enter the source file to copy:"
read source

echo "Enter the destination path:"
read destination

if [ -f "$source" ]; then
    cp "$source" "$destination"
    echo "The file '$source' has been copied to '$destination'."
else
    echo "The file '$source' does not exist."
fi
```

### Chapter 5: The Book of Words

In the village of Wordton, the ancient Book of Words holds great power. The villagers need to know the number of words it contains. Write a script that counts the number of words in a file specified by the user.

```bash
# Word Count Script
echo "Enter the filename to count words in:"
read filename

if [ -f "$filename" ]; then
    word_count=$(wc -w < "$filename")
    echo "The file '$filename' contains $word_count words."
else
    echo "The file '$filename' does not exist."
fi
```

### Chapter 6: The Arcane Calculator

Further in your journey, you arrive at the town of Calcville, where the villagers require your skills to perform calculations for their trade. Write a script that takes two numbers and an arithmetic operator (+, -, \*, /) from the user and performs the corresponding calculation.

```bash
# Basic Calculator Script
echo "Enter the first number:"
read num1

echo "Enter the operator (+, -, *, /):"
read operator

echo "Enter the second number:"
read num2

case $operator in
    +) result=$(echo "$num1 + $num2" | bc) ;;
    -) result=$(echo "$num1 - $num2" | bc) ;;
    *) result=$(echo "$num1 * $num2" | bc) ;;
    /) result=$(echo "scale=2; $num1 / $num2" | bc) ;;
    *) echo "Invalid operator"; exit 1 ;;
esac

echo "The result of $num1 $operator $num2 is: $result"
```

### Chapter 7: The Guardian of Backups

Your final challenge leads you to the Guardian of Backups in the village of Backupia. The Guardian needs your help to create a backup of a vital file. Write a script that creates a backup of a file by copying it to the same directory with a ".bak" extension.

```bash
# File Backup Script
echo "Enter the filename to back up:"
read filename

if [ -f "$filename" ]; then
    cp "$filename" "$filename.bak"
    echo "The file '$filename' has been backed up as '$filename.bak'."
else
    echo "The file '$filename' does not exist."
fi
```

### Conclusion

Congratulations, valiant coder! You have successfully navigated the challenges of Bashlandia and helped the villagers with your scripting prowess. Your journey has enhanced your skills, preparing you for even greater adventures ahead. May your terminal always be powerful and your scripts ever efficient!
