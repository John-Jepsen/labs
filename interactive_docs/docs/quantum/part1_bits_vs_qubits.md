# Part 1: Bits vs Qubits — Booleans on Steroids

## Objective

Compare classical bits with quantum qubits to understand the fundamental differences that enable quantum computing's unique capabilities. By the end of this session, your team will understand superposition, visualize quantum states, and implement basic quantum operations that have no classical equivalent.

---

## Core Concepts

### Classical Bits vs Quantum Qubits

| Classical Bit                   | Quantum Qubit                          |
| :------------------------------ | :------------------------------------- |
| Can be 0 OR 1                   | Can be in superposition of 0 AND 1     |
| Deterministic                   | Probabilistic                          |
| Directly observable             | Collapses upon measurement             |
| Binary logic                    | Complex amplitude logic                |
| Can represent 1 state at a time | Can represent 2^n states with n qubits |

### Quantum State Notation

**Dirac (Bra-Ket) Notation**:

- State "0" is represented as: |0⟩
- State "1" is represented as: |1⟩
- Superposition: α|0⟩ + β|1⟩, where |α|² + |β|² = 1

## Visual Explanation

![Bit vs Qubit Visualization](https://placeholder-for-your-diagram.png)

A classical bit is like a coin showing either heads or tails, while a qubit is like a spinning coin—it has some probability of being heads and some probability of being tails until observed.

---

## Environment Setup

Ensure you have the necessary packages installed:

| Command to run                        |
| :------------------------------------ |
| `pip install qiskit matplotlib numpy` |

For Cirq users:
| Command to run |
| :---- |
| `pip install cirq matplotlib numpy` |

## Starter Code

Create a new Python file called `bit_vs_qubit.py` with the following structure:

```python
# TEAM MEMBERS: [List names here]
# DATE: [Today's date]

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Circle

# Choose your framework
USE_QISKIT = True  # Set to False if using Cirq

if USE_QISKIT:
    from qiskit import QuantumCircuit, transpile, Aer, assemble
    from qiskit.visualization import plot_bloch_vector, plot_histogram
else:
    import cirq
    from cirq.sim.density_matrix_simulator import DensityMatrixSimulator

def classical_bit_operations():
    """Demonstrate classical bit operations"""
    # Initialize a classical bit (0 or 1)
    bit = 0
    print(f"Initial bit value: {bit}")

    # Classical NOT operation
    bit = 1 - bit
    print(f"After NOT: {bit}")

    # Classical AND operation
    bit2 = 1
    bit_and = bit & bit2
    print(f"AND operation: {bit} & {bit2} = {bit_and}")

    # Classical OR operation
    bit_or = bit | bit2
    print(f"OR operation: {bit} | {bit2} = {bit_or}")

    # Classical XOR operation
    bit_xor = bit ^ bit2
    print(f"XOR operation: {bit} ^ {bit2} = {bit_xor}")

    # CHALLENGE: Can we represent multiple states simultaneously with classical bits?
    # YOUR ANSWER HERE

def qubit_operations_qiskit():
    """Demonstrate qubit operations using Qiskit"""
    # CREATE QUANTUM CIRCUIT
    # YOUR CODE HERE: Create a circuit with 1 qubit and 1 classical bit

    # Initial state is |0⟩
    # YOUR CODE HERE: Print or visualize the initial state

    # Apply Hadamard gate to create superposition
    # YOUR CODE HERE: Add H gate and visualize superposition

    # Measure the qubit
    # YOUR CODE HERE: Add measurement operation

    # Simulate the circuit
    # YOUR CODE HERE: Run the circuit on a simulator and get counts

    # CHALLENGE: What happens if you measure multiple times?
    # YOUR CODE HERE

def qubit_operations_cirq():
    """Demonstrate qubit operations using Cirq"""
    # Initial qubit
    q = cirq.LineQubit(0)

    # Initial state |0⟩
    circuit = cirq.Circuit()

    # YOUR CODE HERE: Create superposition with Hadamard

    # YOUR CODE HERE: Measure the qubit

    # YOUR CODE HERE: Simulate and print results

    # CHALLENGE: What happens if you measure multiple times?
    # YOUR CODE HERE

def visualize_qubit_vs_bit():
    """Visualize the difference between a bit and qubit"""
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(10, 4))

    # Classical bit visualization (simple binary state)
    ax1.set_title('Classical Bit')
    ax1.set_xlim(-1.5, 1.5)
    ax1.set_ylim(-1.5, 1.5)

    # Draw a simple binary representation
    circle0 = Circle((-0.5, 0), 0.4, color='blue', alpha=0.7)
    circle1 = Circle((0.5, 0), 0.4, color='red', alpha=0.2)  # Dimmed to show it's not active
    ax1.add_patch(circle0)
    ax1.add_patch(circle1)
    ax1.text(-0.5, 0, "0", ha='center', va='center', color='white')
    ax1.text(0.5, 0, "1", ha='center', va='center')
    ax1.text(0, -1, "The bit is definitely in state |0⟩", ha='center')

    # Qubit visualization (probability distribution)
    ax2.set_title('Quantum Qubit (after Hadamard)')
    ax2.set_xlim(-1.5, 1.5)
    ax2.set_ylim(-1.5, 1.5)

    # Draw a superposition representation
    circle0 = Circle((-0.5, 0), 0.4, color='blue', alpha=0.5)
    circle1 = Circle((0.5, 0), 0.4, color='red', alpha=0.5)
    ax2.add_patch(circle0)
    ax2.add_patch(circle1)
    ax2.text(-0.5, 0, "0", ha='center', va='center')
    ax2.text(0.5, 0, "1", ha='center', va='center')
    ax2.text(0, -1, "The qubit has 50% probability of being |0⟩ or |1⟩", ha='center')

    plt.tight_layout()
    plt.savefig("bit_vs_qubit.png")
    plt.show()

    # CHALLENGE: Modify this function to show different probability distributions
    # YOUR CODE HERE

def main():
    print("CLASSICAL BIT OPERATIONS")
    print("========================")
    classical_bit_operations()

    print("\nQUANTUM QUBIT OPERATIONS")
    print("========================")
    if USE_QISKIT:
        qubit_operations_qiskit()
    else:
        qubit_operations_cirq()

    print("\nVISUALIZING DIFFERENCES")
    print("========================")
    visualize_qubit_vs_bit()

    # TEAM DISCUSSION POINT:
    # What operations can you perform with qubits that are impossible with classical bits?
    # YOUR DISCUSSION NOTES HERE

if __name__ == "__main__":
    main()
```

## Collaborative Challenge: Implement the Missing Sections

Working together as a team, your challenge is to:

1. Complete all sections marked with `# YOUR CODE HERE`
2. Implement the `qubit_operations_qiskit()` or `qubit_operations_cirq()` function (based on your chosen framework)
3. Enhance the `visualize_qubit_vs_bit()` function to show different quantum states
4. Discuss and document your answers to the challenge questions

## Discussion Checkpoints

### Checkpoint 1: After Classical Bit Operations

- Can we represent multiple states simultaneously with classical bits?
- What makes quantum superposition fundamentally different from classical probability?

### Checkpoint 2: After Implementing Quantum Operations

- What happens when you measure a qubit in superposition?
- Why does quantum require complex numbers for amplitudes, not just probabilities?

### Checkpoint 3: After Visualization

- How does visualizing qubits help understand quantum behavior?
- What information is lost when a quantum state is measured?

## Debugging Walkthrough

Common issues and their solutions:

1. **Problem**: Quantum circuit doesn't show expected probabilities
   **Solution**: Check if the Hadamard gate was properly applied and that you're using the correct simulator backend

2. **Problem**: Visualization doesn't display properly
   **Solution**: Ensure matplotlib is working by testing with a simple plot first; check for correct array dimensions

3. **Problem**: Getting errors with complex numbers
   **Solution**: Remember that quantum amplitudes are complex numbers; use numpy's complex number support

## Extension Challenge: Bloch Sphere Representation

For a deeper understanding, implement a function to visualize qubits on the Bloch sphere:

```python
def bloch_sphere_visualization():
    """Visualize qubit states on the Bloch sphere"""
    # YOUR CODE HERE

    # For Qiskit:
    # from qiskit.visualization import plot_bloch_vector
    # plot_bloch_vector([x, y, z])

    # For Cirq:
    # You'll need to use matplotlib to create a 3D plot
```

## What Would Break This?

Discuss with your team:

1. What happens if we try to create a superposition state with α|0⟩ + β|1⟩ where |α|² + |β|² ≠ 1?
2. Can we clone an unknown quantum state? Why or why not?
3. What happens if we try to directly print the state of a qubit without measuring?

## Capstone Group Task

Design and implement a quantum coin flip simulator that:

1. Creates a "fair" quantum coin (50/50 chance)
2. Creates a "biased" quantum coin (with configurable bias)
3. Compares results of multiple flips between classical and quantum coins
4. Visualizes the results with histograms

## How Would This Work on Real Hardware?

If you were to run these experiments on real quantum hardware:

1. How would noise affect your superposition states?
2. How many shots (repetitions) would you need for accurate statistics?
3. Would gate errors make it difficult to distinguish between intended superposition and errors?

## Next Steps

In the next session, we'll explore the essential linear algebra concepts needed to work effectively with quantum states and operations.
