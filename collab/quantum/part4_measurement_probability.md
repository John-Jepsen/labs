# Part 4: Measurement & Probability

## Objective

Explore the probabilistic nature of quantum measurements and understand how quantum states collapse upon observation. By the end of this session, your team will be able to predict measurement outcomes, analyze repeated experiment results, and understand the fundamental differences between quantum probability and classical uncertainty.

---

## Core Concepts

### Quantum Measurement

Quantum measurement is the process of extracting classical information from a quantum system. Key principles:

- Measurement collapses superposition states into basis states
- Measurement outcomes are probabilistic
- Probabilities are determined by the squared magnitudes of amplitudes
- Post-measurement, the quantum state "resets" to the measured state

### Measurement Probability Calculation

For a qubit state |ψ⟩ = α|0⟩ + β|1⟩:

- Probability of measuring |0⟩: P(0) = |α|²
- Probability of measuring |1⟩: P(1) = |β|²
- The sum of all probabilities must equal 1: |α|² + |β|² = 1

### Projective Measurements

A measurement in quantum mechanics is described by a set of projection operators {P₀, P₁, ...}:

- For standard basis measurement: P₀ = |0⟩⟨0| and P₁ = |1⟩⟨1|
- The probability of outcome 'k' is p(k) = ⟨ψ|Pₖ|ψ⟩

## Visual Explanation

![Quantum Measurement Visualization](https://placeholder-for-your-diagram.png)

Quantum measurement can be visualized as projecting a state vector onto the measurement basis. The probability of a particular outcome is related to the "length" of this projection.

---

## Environment Setup

Ensure you have the necessary packages installed:

| Command to run                                       |
| :--------------------------------------------------- |
| `pip install qiskit matplotlib numpy pandas seaborn` |

For Cirq users:
| Command to run |
| :---- |
| `pip install cirq matplotlib numpy pandas seaborn` |

## Starter Code

Create a new Python file called `quantum_measurement.py` with the following structure:

```python
# TEAM MEMBERS: [List names here]
# DATE: [Today's date]

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

# Choose your framework
USE_QISKIT = True  # Set to False if using Cirq

if USE_QISKIT:
    from qiskit import QuantumCircuit, execute, Aer
    from qiskit.visualization import plot_histogram
    from qiskit.quantum_info import Statevector
else:
    import cirq
    import sympy

def theoretical_probabilities(state_vector):
    """
    Calculate theoretical measurement probabilities from a state vector

    Args:
        state_vector: Quantum state as a NumPy array

    Returns:
        Dictionary of basis states and their probabilities
    """
    # YOUR CODE HERE
    # For a state |ψ⟩ = α|0⟩ + β|1⟩:
    # P(0) = |α|², P(1) = |β|²
    pass

def circuit_with_measurement_qiskit():
    """Create and measure a quantum circuit with Qiskit"""
    # Create a simple circuit with superposition
    circuit = QuantumCircuit(1, 1)

    # YOUR CODE HERE
    # Apply gates to create an interesting state
    # Add measurement
    # Draw the circuit
    # Run on simulator
    # Plot histogram of results
    pass

def circuit_with_measurement_cirq():
    """Create and measure a quantum circuit with Cirq"""
    # YOUR CODE HERE
    # Create a qubit and circuit
    # Apply gates to create an interesting state
    # Add measurement
    # Print the circuit
    # Run on simulator
    # Plot results
    pass

def measure_multiple_times(n_shots=1024):
    """
    Perform the same measurement multiple times and analyze statistics

    Args:
        n_shots: Number of repetitions
    """
    # YOUR CODE HERE
    # Create a circuit with a known state
    # Run multiple shots on simulator
    # Compare results with theoretical predictions
    pass

def state_tomography_demo():
    """Demonstrate simple state tomography"""
    # YOUR CODE HERE
    # Prepare a state
    # Measure in different bases (X, Y, Z)
    # Reconstruct the state from measurements
    pass

def analyze_measurement_results(counts, shots, theoretical_probs=None):
    """
    Analyze measurement results and compare with theory

    Args:
        counts: Dictionary of results
        shots: Number of experiment repetitions
        theoretical_probs: Expected probabilities (optional)
    """
    # YOUR CODE HERE
    # Calculate empirical probabilities
    # Calculate statistical metrics (variance, std error)
    # Compare with theoretical expectations if provided
    pass

def visualize_measurement_process():
    """Visualize the measurement process"""
    # YOUR CODE HERE
    # Create a sequence of images showing:
    # 1. Initial superposition state
    # 2. Measurement operation
    # 3. Collapsed state after measurement
    # 4. Multiple measurements showing distribution
    pass

def measure_in_different_bases():
    """Measure the same state in different bases"""
    # YOUR CODE HERE
    # Create a state
    # Measure in computational (Z) basis
    # Measure in X basis (apply H before measurement)
    # Measure in Y basis (apply S†H before measurement)
    # Compare results
    pass

def main():
    print("QUANTUM MEASUREMENT & PROBABILITY")
    print("================================")

    print("\nTHEORETICAL PROBABILITIES")
    print("========================")
    # Example: |ψ⟩ = (√0.3)|0⟩ + (√0.7)|1⟩
    example_state = np.array([[np.sqrt(0.3)], [np.sqrt(0.7)]], dtype=complex)
    probs = theoretical_probabilities(example_state)
    print(f"State probabilities: {probs}")

    print("\nQUANTUM CIRCUIT WITH MEASUREMENT")
    print("===============================")
    if USE_QISKIT:
        circuit_with_measurement_qiskit()
    else:
        circuit_with_measurement_cirq()

    print("\nMULTIPLE MEASUREMENTS")
    print("====================")
    # Try different numbers of shots
    for shots in [10, 100, 1000, 10000]:
        print(f"\nRunning with {shots} shots:")
        measure_multiple_times(shots)

    print("\nMEASUREMENT IN DIFFERENT BASES")
    print("=============================")
    measure_in_different_bases()

    print("\nMEASUREMENT VISUALIZATION")
    print("========================")
    visualize_measurement_process()

    print("\nSTATE TOMOGRAPHY DEMO")
    print("====================")
    state_tomography_demo()

    # TEAM DISCUSSION POINT:
    # How does quantum probability differ from classical probability?
    # YOUR DISCUSSION NOTES HERE

if __name__ == "__main__":
    main()
```

## Collaborative Challenge: Implement Measurement Analysis

Working together as a team, your challenge is to:

1. Complete the functions marked with `# YOUR CODE HERE`
2. Implement measurement in computational and non-computational bases
3. Analyze how the number of shots affects measurement accuracy
4. Visualize the measurement process and results effectively

## Team Roles for This Exercise

For this lab, consider the following role assignments:

1. **Circuit Designer**: Creates quantum circuits with interesting states
2. **Measurement Analyst**: Implements the probability calculations and analysis
3. **Statistics Expert**: Analyzes how shot count affects result accuracy
4. **Visualization Specialist**: Creates clear visualizations of the measurement process
5. **Documentation Lead**: Explains the conceptual meaning of results

## Discussion Checkpoints

### Checkpoint 1: After Implementing Basic Measurements

- Why is quantum measurement probabilistic rather than deterministic?
- What is the role of the Born rule in quantum measurement?
- How does superposition "collapse" during measurement?

### Checkpoint 2: After Multiple Measurement Analysis

- How many measurements (shots) are needed for reliable statistics?
- What statistical tools help us analyze measurement uncertainty?
- How can we distinguish quantum randomness from classical noise?

### Checkpoint 3: After Different Basis Measurements

- Why might we want to measure in different bases?
- How is measuring in the X-basis different from the Z-basis?
- What information can we extract from each type of measurement?

## Debugging Walkthrough

Common issues and their solutions:

1. **Problem**: Getting unexpected measurement probabilities
   **Solution**: Check state normalization and verify correct squaring of amplitudes (not just the amplitude values)

2. **Problem**: Statistics don't match theoretical expectations
   **Solution**: Increase the number of shots for more accurate results; statistical fluctuations are normal

3. **Problem**: Basis measurements are confusing
   **Solution**: Remember that measuring in a different basis means transforming the state first, then measuring in the computational basis

## Extension Challenge: Partial Measurements

Implement a function to demonstrate partial measurement of a two-qubit system:

```python
def partial_measurement_demo():
    """
    Demonstrate partial measurement of a multi-qubit system
    """
    # YOUR CODE HERE
    # Create a two-qubit entangled state
    # Measure only one qubit
    # Analyze the effect on the unmeasured qubit
    # Visualize the conditional probabilities
```

## What Would Break This?

Discuss with your team:

1. What would happen if measurements didn't collapse quantum states?
2. Can we design a measurement that doesn't disturb the quantum state?
3. What would it mean if measurement results weren't truly random?
4. How would imperfect detectors affect measurement results?

## Capstone Group Task

Design and implement a "Quantum State Guessing Game" that:

1. Prepares a secret quantum state
2. Allows players a limited number of measurements in different bases
3. Challenges players to guess the prepared state
4. Scores based on how close the guessed state is to the actual state
5. Includes different difficulty levels

## How Would This Work on Real Hardware?

On real quantum hardware:

1. How do readout errors affect measurement results?
2. What techniques are used to mitigate measurement errors?
3. How are measurement results physically obtained from qubits?
4. What's the difference between destructive and non-destructive measurements?

## Next Steps

In the next session, we'll explore multi-qubit systems and entanglement, building on our understanding of single qubit operations and measurements.
