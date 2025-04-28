# Part 6: Algorithms Primer

## Objective

Understand and implement foundational quantum algorithms that demonstrate quantum advantage. By the end of this session, your team will be able to implement the Deutsch-Jozsa algorithm and Grover's search algorithm, analyze their performance compared to classical alternatives, and understand the principles that give quantum algorithms their power.

---

## Core Concepts

### Quantum Algorithm Paradigms

- **Quantum Parallelism**: Exploiting superposition to evaluate a function for multiple inputs simultaneously
- **Quantum Interference**: Using interference to amplify desired results and cancel unwanted ones
- **Quantum Measurement**: Extracting classical information from quantum states

### Key Quantum Algorithms

| Algorithm                 | Problem                                   | Classical Complexity | Quantum Complexity | Speedup     |
| :------------------------ | :---------------------------------------- | :------------------- | :----------------- | :---------- |
| Deutsch-Jozsa             | Determine if f(x) is constant or balanced | O(2^(n-1) + 1)       | O(1)               | Exponential |
| Grover's Search           | Find marked item in unsorted database     | O(N)                 | O(√N)              | Quadratic   |
| Shor's Factoring          | Find prime factors of integer N           | O(e^(log N)^(1/3))   | O((log N)^3)       | Exponential |
| Quantum Fourier Transform | Fourier transform                         | O(N log N)           | O(log^2 N)         | Exponential |

## Visual Explanation

![Quantum Algorithm Comparison](https://placeholder-for-algorithm-diagram.png)

Quantum algorithms gain their advantage by exploring multiple solution paths simultaneously through superposition, then using interference to increase the probability of measuring the correct answer.

---

## Environment Setup

Ensure you have the necessary packages installed:

| Command to run                               |
| :------------------------------------------- |
| `pip install qiskit matplotlib numpy pandas` |

For Cirq users:
| Command to run |
| :---- |
| `pip install cirq matplotlib numpy pandas` |

## Starter Code

Create a new Python file called `quantum_algorithms.py` with the following structure:

```python
# TEAM MEMBERS: [List names here]
# DATE: [Today's date]

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Choose your framework
USE_QISKIT = True  # Set to False if using Cirq

if USE_QISKIT:
    from qiskit import QuantumCircuit, execute, Aer
    from qiskit.visualization import plot_histogram
    from qiskit.quantum_info import Statevector
else:
    import cirq
    from cirq.sim.density_matrix_simulator import DensityMatrixSimulator

# ===================================
# Deutsch-Jozsa Algorithm
# ===================================

def deutsch_jozsa_oracle(circuit, n_qubits, oracle_type, ancilla_idx):
    """
    Implements the oracle for the Deutsch-Jozsa algorithm

    Args:
        circuit: Quantum circuit to add the oracle to
        n_qubits: Number of qubits in the circuit (excluding ancilla)
        oracle_type: 'constant_0', 'constant_1', or 'balanced'
        ancilla_idx: Index of the ancilla qubit
    """
    # YOUR CODE HERE
    # Implement different types of oracles:
    # 1. Constant-0 oracle: f(x) = 0 for all x
    # 2. Constant-1 oracle: f(x) = 1 for all x
    # 3. Balanced oracle: f(x) = 0 for half of inputs, 1 for other half
    pass

def deutsch_jozsa_algorithm_qiskit(n_qubits, oracle_type):
    """
    Implements the Deutsch-Jozsa algorithm using Qiskit

    Args:
        n_qubits: Number of input qubits
        oracle_type: Type of oracle to use

    Returns:
        QuantumCircuit with the algorithm
    """
    # YOUR CODE HERE
    # Create a circuit with n_qubits + 1 qubits (ancilla)
    # Apply Hadamard gates to all qubits
    # Apply the oracle
    # Apply Hadamard gates to input qubits
    # Measure input qubits
    # Return the circuit
    pass

def deutsch_jozsa_algorithm_cirq(n_qubits, oracle_type):
    """
    Implements the Deutsch-Jozsa algorithm using Cirq

    Args:
        n_qubits: Number of input qubits
        oracle_type: Type of oracle to use

    Returns:
        Cirq circuit with the algorithm
    """
    # YOUR CODE HERE
    # Similar to the Qiskit implementation but using Cirq
    pass

def analyze_deutsch_jozsa_results(result_counts):
    """
    Analyzes the results from the Deutsch-Jozsa algorithm

    Args:
        result_counts: Measurement counts from circuit execution

    Returns:
        String indicating if function is constant or balanced
    """
    # YOUR CODE HERE
    # Determine if the function is constant or balanced from results
    # If all qubits are 0, the function is constant
    # Otherwise, the function is balanced
    pass

# ===================================
# Grover's Search Algorithm
# ===================================

def grover_oracle_qiskit(circuit, n_qubits, marked_state):
    """
    Implements Grover's oracle for a specific marked state

    Args:
        circuit: Quantum circuit to add the oracle to
        n_qubits: Number of qubits
        marked_state: Binary string representing the marked state
    """
    # YOUR CODE HERE
    # Implement the phase oracle that flips the phase of the marked state
    pass

def diffusion_operator_qiskit(circuit, n_qubits):
    """
    Implements the diffusion operator for Grover's algorithm

    Args:
        circuit: Quantum circuit to add the operator to
        n_qubits: Number of qubits
    """
    # YOUR CODE HERE
    # Implement the diffusion operator: H^⊗n (2|0⟩⟨0| - I) H^⊗n
    pass

def grover_algorithm_qiskit(n_qubits, marked_state, num_iterations=None):
    """
    Implements Grover's search algorithm using Qiskit

    Args:
        n_qubits: Number of qubits (log2 of search space size)
        marked_state: Binary string of the state to find
        num_iterations: Number of Grover iterations (optional)

    Returns:
        QuantumCircuit with Grover's algorithm
    """
    # YOUR CODE HERE
    # If num_iterations is None, calculate optimal number: π/4 * sqrt(N)
    # Create circuit with n_qubits
    # Apply H gates to create superposition
    # For each iteration:
    #   Apply oracle
    #   Apply diffusion operator
    # Measure all qubits
    # Return the circuit
    pass

def grover_oracle_cirq(qubits, marked_state):
    """
    Implements Grover's oracle for Cirq

    Args:
        qubits: List of qubits
        marked_state: Binary string representing the marked state

    Returns:
        Cirq operations for the oracle
    """
    # YOUR CODE HERE
    # Implement the phase oracle for Cirq
    pass

def diffusion_operator_cirq(qubits):
    """
    Implements the diffusion operator for Cirq

    Args:
        qubits: List of qubits

    Returns:
        Cirq operations for the diffusion operator
    """
    # YOUR CODE HERE
    # Implement the diffusion operator for Cirq
    pass

def grover_algorithm_cirq(n_qubits, marked_state, num_iterations=None):
    """
    Implements Grover's search algorithm using Cirq

    Args:
        n_qubits: Number of qubits
        marked_state: Binary string of the state to find
        num_iterations: Number of Grover iterations (optional)

    Returns:
        Cirq circuit with Grover's algorithm
    """
    # YOUR CODE HERE
    # Similar to Qiskit implementation but using Cirq
    pass

def analyze_grover_results(result_counts, marked_state):
    """
    Analyzes the results from Grover's algorithm

    Args:
        result_counts: Measurement counts from circuit execution
        marked_state: The marked state we're searching for

    Returns:
        Success probability and analysis
    """
    # YOUR CODE HERE
    # Calculate the probability of measuring the marked state
    # Compare with random guessing (1/N)
    # Calculate speedup
    pass

def classical_search_simulation(n_items, rng_seed=None):
    """
    Simulates a classical search for comparison

    Args:
        n_items: Number of items in the search space
        rng_seed: Random number generator seed (optional)

    Returns:
        Number of tries needed to find the marked item
    """
    # YOUR CODE HERE
    # Simulate a classical search by generating random guesses
    # Count how many tries are needed to find the marked item
    # Return statistics
    pass

def compare_quantum_vs_classical(n_qubits_list):
    """
    Compares quantum vs classical search performance

    Args:
        n_qubits_list: List of qubit numbers to test

    Returns:
        DataFrame with comparison results
    """
    # YOUR CODE HERE
    # For each n_qubits:
    #   Run Grover's algorithm
    #   Simulate classical search
    #   Compare performance
    # Return and visualize results
    pass

def main():
    print("QUANTUM ALGORITHM DEMONSTRATIONS")
    print("==============================")

    print("\nDEUTSCH-JOZSA ALGORITHM")
    print("======================")
    n_qubits = 3  # Number of input qubits

    for oracle_type in ['constant_0', 'constant_1', 'balanced']:
        print(f"\nTesting oracle type: {oracle_type}")

        if USE_QISKIT:
            circuit = deutsch_jozsa_algorithm_qiskit(n_qubits, oracle_type)
            print(circuit.draw())

            # Execute the circuit
            simulator = Aer.get_backend('qasm_simulator')
            result = execute(circuit, simulator, shots=1024).result()
            counts = result.get_counts()

            print("Results:", counts)
            conclusion = analyze_deutsch_jozsa_results(counts)
            print(f"Conclusion: Function is {conclusion}")
        else:
            circuit = deutsch_jozsa_algorithm_cirq(n_qubits, oracle_type)
            print(circuit)

            # Execute the circuit with Cirq
            # YOUR CODE HERE

    print("\nGROVER'S SEARCH ALGORITHM")
    print("========================")
    n_qubits = 3  # 2^3 = 8 items in the search space
    marked_state = '101'  # The item we're searching for

    if USE_QISKIT:
        circuit = grover_algorithm_qiskit(n_qubits, marked_state)
        print(circuit.draw())

        # Execute the circuit
        simulator = Aer.get_backend('qasm_simulator')
        result = execute(circuit, simulator, shots=1024).result()
        counts = result.get_counts()

        print("Results:", counts)
        analysis = analyze_grover_results(counts, marked_state)
        print(analysis)
    else:
        circuit = grover_algorithm_cirq(n_qubits, marked_state)
        print(circuit)

        # Execute the circuit with Cirq
        # YOUR CODE HERE

    print("\nQUANTUM VS CLASSICAL COMPARISON")
    print("==============================")
    comparison = compare_quantum_vs_classical([2, 3, 4, 5, 6])
    print(comparison)

    # TEAM DISCUSSION POINT:
    # What gives quantum algorithms their advantage over classical algorithms?
    # YOUR DISCUSSION NOTES HERE

if __name__ == "__main__":
    main()
```

## Collaborative Challenge: Implement Quantum Algorithms

Working together as a team, your challenge is to:

1. Complete all functions marked with `# YOUR CODE HERE`
2. Implement and test the Deutsch-Jozsa algorithm
3. Implement and test Grover's search algorithm
4. Compare the performance of quantum vs. classical approaches

## Team Roles for This Exercise

For this lab, consider the following role assignments:

1. **Oracle Designer**: Focuses on implementing the quantum oracles for both algorithms
2. **Algorithm Implementer**: Works on the main algorithm structure
3. **Performance Analyst**: Compares quantum vs. classical performance
4. **Visualization Expert**: Creates clear visualizations of results and speedups
5. **Documentation Lead**: Explains the algorithms and their applications

## Discussion Checkpoints

### Checkpoint 1: After Implementing Deutsch-Jozsa

- What is the key insight that allows the Deutsch-Jozsa algorithm to work?
- Why can quantum computing determine if a function is constant or balanced in one query?
- What are the limitations of this algorithm in practical applications?

### Checkpoint 2: After Implementing Grover's Search

- How does the diffusion operator amplify the amplitude of the marked state?
- Why does Grover's algorithm achieve quadratic speedup but not exponential?
- What happens if you run too many iterations of Grover's algorithm?

### Checkpoint 3: After Performance Comparison

- Which types of problems are well-suited for quantum algorithms?
- What patterns do you see in problems where quantum computing excels?
- How do the resource requirements scale for quantum vs. classical approaches?

## Debugging Walkthrough

Common issues and their solutions:

1. **Problem**: Deutsch-Jozsa algorithm always returns "constant"
   **Solution**: Check that your balanced oracle is correctly implemented; it should mark exactly half of the inputs

2. **Problem**: Grover's search doesn't find the marked item
   **Solution**: Verify the number of iterations; too many or too few can reduce success probability

3. **Problem**: Quantum circuit is too complex to visualize
   **Solution**: For larger qubit numbers, focus on visualizing results rather than the full circuit

## Extension Challenge: Implement Quantum Counting

Implement a quantum counting algorithm that combines Grover's algorithm with the Quantum Fourier Transform:

```python
def quantum_counting_algorithm(n_qubits, marked_state, counting_qubits=4):
    """
    Implements the quantum counting algorithm

    Args:
        n_qubits: Number of qubits for the search space
        marked_state: The marked state to count
        counting_qubits: Number of qubits for counting

    Returns:
        Circuit that estimates the number of solutions
    """
    # YOUR CODE HERE
    # Implement quantum counting algorithm
    # Use phase estimation on Grover's operator
    # Return circuit that estimates number of solutions
```

## What Would Break This?

Discuss with your team:

1. What assumptions do these algorithms make about quantum computers?
2. How would noise and decoherence affect algorithm performance?
3. What are the current limitations in implementing these algorithms on real hardware?
4. Why can't we just read out all the information in a superposition?

## Capstone Group Task

Design and implement a "Quantum Function Identifier" that:

1. Takes a mystery oracle function
2. Uses the Deutsch-Jozsa and related algorithms to determine its properties
3. Classifies the function as constant, balanced, OR-type, AND-type, etc.
4. Compares the quantum approach with classical simulation

## How Would This Work on Real Hardware?

On real quantum hardware:

1. How do we implement oracles for real problems?
2. What are the current record sizes for implementing these algorithms?
3. How do we measure algorithm success on noisy hardware?
4. What is the quantum volume needed to demonstrate quantum advantage?

## Next Steps

In the next session, we'll explore quantum noise and error correction, focusing on the real-world challenges of implementing quantum algorithms on imperfect hardware.
