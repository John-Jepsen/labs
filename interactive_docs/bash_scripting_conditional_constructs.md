```
________                    ___      ___                         ___
`MMMMMMMb.                  `MM      `MM                         `MM 68b
 MM    `Mb                   MM       MM                          MM Y89
 MM     MM    ___     ____   MM  __   MM    ___   ___  __     ____MM ___    ___
 MM    .M9  6MMMMb   6MMMMb\ MM 6MMb  MM  6MMMMb  `MM 6MMb   6MMMMMM `MM  6MMMMb
 MMMMMMM(  8M'  `Mb MM'    ` MMM9 `Mb MM 8M'  `Mb  MMM9 `Mb 6M'  `MM  MM 8M'  `Mb
 MM    `Mb     ,oMM YM.      MM'   MM MM     ,oMM  MM'   MM MM    MM  MM     ,oMM
 MM     MM ,6MM9'MM  YMMMMb  MM    MM MM ,6MM9'MM  MM    MM MM    MM  MM ,6MM9'MM
 MM     MM MM'   MM      `Mb MM    MM MM MM'   MM  MM    MM MM    MM  MM MM'   MM
 MM    .M9 MM.  ,MM L    ,MM MM    MM MM MM.  ,MM  MM    MM YM.  ,MM  MM MM.  ,MM
_MMMMMMM9' `YMMM9'YbMYMMMM9 _MM_  _MM_MM_`YMMM9'Yb_MM_  _MM_ YMMMMMM__MM_`YMMM9'Yb.
```

## Adventure Quest: The Bash Scripting Journey - Conditional Constructs

### Introduction

Welcome back, brave coder, to the mystical land of Bashlandia! The villagers now face more complex challenges, requiring your mastery of conditional constructs. Ready your terminal, for a new adventure filled with logical decisions and branching paths begins now!

### Chapter 1: The Oracle's Dilemma

In the village of Ifville, the Oracle needs your help to make decisions based on various conditions. Your task is to write a script using the `if` command that checks multiple conditions and prints different messages accordingly.

```bash
# The Oracle's Dilemma Script
echo "Enter the current temperature (in °C):"
read temperature

if [ "$temperature" -lt 0 ]; then
    echo "It's freezing! Stay warm."
elif [ "$temperature" -lt 15 ]; then
    echo "It's cold. Wear a jacket."
elif [ "$temperature" -lt 25 ]; then
    echo "The weather is pleasant. Enjoy your day!"
else
    echo "It's hot! Stay cool and hydrated."
fi
```

### Chapter 2: The Seer's Prediction

The Seer of Caseville has visions that depend on certain patterns. Write a script using the `case` command to interpret these visions based on the user's input.

```bash
# The Seer's Prediction Script
echo "Enter the name of a magical creature:"
read creature

case $creature in
  dragon | phoenix)
    echo "The $creature brings great power and fire."
    ;;
  unicorn | pegasus)
    echo "The $creature brings purity and grace."
    ;;
  griffin | centaur)
    echo "The $creature symbolizes strength and wisdom."
    ;;
  *)
    echo "The nature of the $creature is unknown."
    ;;
esac
```

### Chapter 3: The Council of Choice

In the council chamber of Selectoria, the villagers need to make a choice from a list of options. Write a script using the `select` command to present a menu and handle their choice.

```bash
# The Council of Choice Script
echo "Choose your path to adventure:"
options=("Mountains" "Forest" "Desert" "Sea")
select path in "${options[@]}"; do
    case $path in
        "Mountains")
            echo "You chose the Mountains. Prepare for a rocky journey!"
            break
            ;;
        "Forest")
            echo "You chose the Forest. Beware of the wild creatures!"
            break
            ;;
        "Desert")
            echo "You chose the Desert. Stay hydrated and avoid the heat!"
            break
            ;;
        "Sea")
            echo "You chose the Sea. Ready your ship for the voyage!"
            break
            ;;
        *)
            echo "Invalid choice. Please select a valid path."
            ;;
    esac
done
```

### Chapter 4: The Arithmetic Challenge

In the town of Arithmetica, the villagers need to solve an arithmetic challenge. Write a script using the `((...))` command to evaluate an arithmetic expression and print the result.

```bash
# The Arithmetic Challenge Script
echo "Enter the first number:"
read num1

echo "Enter the second number:"
read num2

echo "Enter the operation (+, -, *, /):"
read operation

case $operation in
    +) ((result = num1 + num2)) ;;
    -) ((result = num1 - num2)) ;;
    *) ((result = num1 * num2)) ;;
    /) ((result = num1 / num2)) ;;
    *) echo "Invalid operation"; exit 1 ;;
esac

echo "The result of $num1 $operation $num2 is: $result"
```

### Chapter 5: The Enigma of Conditions

In the village of Conditia, the villagers face an enigma that requires evaluating complex conditions. Write a script using the `[[...]]` command to evaluate multiple conditions and print appropriate messages.

```bash
# The Enigma of Conditions Script
echo "Enter a number:"
read number

if [[ $number -gt 0 && $number -lt 10 ]]; then
    echo "The number is between 1 and 9."
elif [[ $number -ge 10 && $number -le 20 ]]; then
    echo "The number is between 10 and 20."
else
    echo "The number is outside the range of 1 to 20."
fi
```

### Conclusion

Congratulations, noble coder! You have mastered the art of conditional constructs and helped the villagers of Bashlandia with your scripting prowess. Your journey has deepened your understanding, preparing you for even greater challenges ahead. May your terminal always be powerful and your scripts ever logical!
