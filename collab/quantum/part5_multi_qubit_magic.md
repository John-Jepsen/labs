# Part 5: Multi-Qubit Magic

## Objective

Explore the fascinating world of multi-qubit systems and quantum entanglement. By the end of this session, your team will be able to create and manipulate entangled states, understand Bell states and their properties, and appreciate how entanglement enables quantum computing's exponential advantage over classical computers.

---

## Core Concepts

### Multi-Qubit Systems

- **Tensor Product**: The mathematical operation that combines quantum systems
- **State Space Growth**: With n qubits, state space grows as 2^n
- **Separable States**: States that can be written as tensor products of individual qubit states
- **Entangled States**: States that cannot be written as tensor products of individual qubit states

### Common Two-Qubit Gates

| Gate | Matrix/Circuit                                         | Description                            |
| :--- | :----------------------------------------------------- | :------------------------------------- | --- |
| CNOT | ![CNOT Gate](https://placeholder-for-cnot-diagram.png) | Flips target qubit if control qubit is | 1⟩  |
| CZ   | ![CZ Gate](https://placeholder-for-cz-diagram.png)     | Applies Z gate to target if control is | 1⟩  |
| SWAP | ![SWAP Gate](https://placeholder-for-swap-diagram.png) | Exchanges the states of two qubits     |

### Bell States

The four maximally entangled two-qubit states:

- |Φ⁺⟩ = (|00⟩ + |11⟩)/√2
- |Φ⁻⟩ = (|00⟩ - |11⟩)/√2
- |Ψ⁺⟩ = (|01⟩ + |10⟩)/√2
- |Ψ⁻⟩ = (|01⟩ - |10⟩)/√2

## Visual Explanation

![Quantum Entanglement](https://placeholder-for-entanglement-diagram.png)

In entangled states, measuring one qubit instantly determines the state of the other, regardless of distance. This "spooky action at a distance" (as Einstein called it) is a fundamental feature of quantum mechanics with no classical analog.

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

Create a new Python file called `multi_qubit_systems.py` with the following structure:

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
    from qiskit.visualization import plot_histogram, plot_bloch_multivector
    from qiskit.quantum_info import Statevector
else:
    import cirq
    from cirq.sim.density_matrix_simulator import DensityMatrixSimulator

def tensor_product_demo():
    """Demonstrate tensor product of quantum states"""
    # Define single-qubit states
    q0_state = np.array([1, 0])  # |0⟩
    q1_state = np.array([0, 1])  # |1⟩

    # YOUR CODE HERE
    # Compute tensor product of states
    # Show resulting state vector
    # Interpret the results
    pass

def create_bell_state_qiskit(bell_type='phi_plus'):
    """
    Create the specified Bell state using Qiskit

    Args:
        bell_type: One of 'phi_plus', 'phi_minus', 'psi_plus', 'psi_minus'

    Returns:
        Quantum circuit with the Bell state
    """
    # YOUR CODE HERE
    # Create a circuit with 2 qubits
    # Apply appropriate gates to create the Bell state
    # Return the circuit
    pass

def create_bell_state_cirq(bell_type='phi_plus'):
    """
    Create the specified Bell state using Cirq

    Args:
        bell_type: One of 'phi_plus', 'phi_minus', 'psi_plus', 'psi_minus'

    Returns:
        Cirq circuit with the Bell state
    """
    # YOUR CODE HERE
    # Create a circuit with 2 qubits
    # Apply appropriate gates to create the Bell state
    # Return the circuit
    pass

def visualize_entangled_state(state_vector):
    """
    Visualize an entangled state

    Args:
        state_vector: 4-element state vector for a 2-qubit system
    """
    # YOUR CODE HERE
    # Create a visual representation of the entangled state
    # Show correlations between qubits
    pass

def measure_entangled_state(n_shots=1024):
    """
    Create and measure an entangled state

    Args:
        n_shots: Number of measurements to perform
    """
    # YOUR CODE HERE
    # Create a Bell state
    # Measure both qubits
    # Analyze the correlations between measurements
    pass

def demonstrate_ghz_state():
    """Create and analyze a 3-qubit GHZ state |000⟩ + |111⟩"""
    # YOUR CODE HERE
    # Create a 3-qubit circuit
    # Apply gates to create the GHZ state
    # Visualize and measure the state
    pass

def demonstrate_w_state():
    """Create and analyze a 3-qubit W state |001⟩ + |010⟩ + |100⟩"""
    # YOUR CODE HERE
    # Create a 3-qubit circuit
    # Apply gates to create the W state
    # Visualize and measure the state
    pass

def test_bell_inequality():
    """Demonstrate violation of Bell's inequality"""
    # YOUR CODE HERE
    # Create an entangled state
    # Perform measurements at different angles
    # Calculate correlation values
    # Check if Bell's inequality is violated
    pass

def quantum_teleportation_demo():
    """Implement the quantum teleportation protocol"""
    # YOUR CODE HERE
    # Create a 3-qubit circuit
    # Prepare arbitrary state to teleport
    # Create entangled pair between qubits 1 and 2
    # Perform Bell measurement on qubits 0 and 1
    # Apply corrections to qubit 2 based on measurement results
    # Verify teleportation success
    pass

def compare_independent_vs_entangled():
    """Compare independent qubit behavior with entangled qubits"""
    # YOUR CODE HERE
    # Create two circuits: one with independent qubits, one with entangled qubits
    # Apply the same operations to both
    # Measure and compare results
    # Highlight the differences
    pass

def main():
    print("MULTI-QUBIT SYSTEMS AND ENTANGLEMENT")
    print("===================================")

    print("\nTENSOR PRODUCT DEMONSTRATION")
    print("===========================")
    tensor_product_demo()

    print("\nCREATING BELL STATES")
    print("===================")
    if USE_QISKIT:
        for bell_type in ['phi_plus', 'phi_minus', 'psi_plus', 'psi_minus']:
            circuit = create_bell_state_qiskit(bell_type)
            print(f"\nBell state: {bell_type}")
            print(circuit.draw())
    else:
        for bell_type in ['phi_plus', 'phi_minus', 'psi_plus', 'psi_minus']:
            circuit = create_bell_state_cirq(bell_type)
            print(f"\nBell state: {bell_type}")
            print(circuit)

    print("\nMEASURING ENTANGLED STATES")
    print("=========================")
    for shots in [10, 100, 1000]:
        print(f"\nMeasuring with {shots} shots:")
        measure_entangled_state(shots)

    print("\nMULTI-QUBIT ENTANGLED STATES")
    print("===========================")
    print("\nGHZ State:")
    demonstrate_ghz_state()
    print("\nW State:")
    demonstrate_w_state()

    print("\nBELL'S INEQUALITY TEST")
    print("====================")
    test_bell_inequality()

    print("\nQUANTUM TELEPORTATION")
    print("====================")
    quantum_teleportation_demo()

    print("\nINDEPENDENT VS ENTANGLED QUBITS")
    print("=============================")
    compare_independent_vs_entangled()

    # TEAM DISCUSSION POINT:
    # What makes entanglement different from classical correlation?
    # YOUR DISCUSSION NOTES HERE

if __name__ == "__main__":
    main()
```

## Collaborative Challenge: Implement Multi-Qubit Operations

Working together as a team, your challenge is to:

1. Complete the functions marked with `# YOUR CODE HERE`
2. Create and visualize all four Bell states
3. Demonstrate the correlations in measurement outcomes for entangled qubits
4. Implement and test the quantum teleportation protocol

## Team Roles for This Exercise

For this lab, consider the following role assignments:

1. **Entanglement Engineer**: Focuses on creating entangled states
2. **Measurement Specialist**: Analyzes the statistics of entangled measurements
3. **Teleportation Developer**: Implements the quantum teleportation protocol
4. **Visualization Expert**: Creates visuals of multi-qubit states
5. **Bell Test Analyzer**: Implements and analyzes Bell inequality tests

## Discussion Checkpoints

### Checkpoint 1: After Creating Bell States

- How does the creation of Bell states demonstrate quantum entanglement?
- Why can't we describe Bell states as separate qubit states?
- What's the significance of having four different Bell states?

### Checkpoint 2: After Measuring Entangled States

- What patterns do you observe in the measurement results?
- How do the correlations between entangled qubits differ from classical correlations?
- What happens when you measure just one qubit of an entangled pair?

### Checkpoint 3: After Quantum Teleportation

- How does quantum teleportation work without violating the no-cloning theorem?
- Why are classical communication channels necessary for teleportation?
- How is the information transferred in teleportation?

## Debugging Walkthrough

Common issues and their solutions:

1. **Problem**: Bell states aren't showing expected correlations
   **Solution**: Ensure the Hadamard and CNOT gates are applied in the correct order

2. **Problem**: Teleportation protocol isn't working
   **Solution**: Verify all three steps: entanglement creation, Bell measurement, and conditional corrections

3. **Problem**: Tensor product calculations are incorrect
   **Solution**: Remember that tensor product increases dimensionality; check matrix dimensions

## Extension Challenge: Quantum Superdense Coding

Implement the quantum superdense coding protocol:

```python
def superdense_coding_demo():
    """
    Implement the quantum superdense coding protocol

    Shows how to transmit 2 classical bits using 1 qubit transfer
    """
    # YOUR CODE HERE
    # Create an entangled Bell pair
    # Encode 2 classical bits by applying operations to 1 qubit
    # Send the qubit
    # Decode the 2 bits with a Bell measurement
    # Verify all 4 possible messages can be transmitted
```

## What Would Break This?

Discuss with your team:

1. What happens to entanglement when one qubit interacts with the environment (decoherence)?
2. Can entanglement be used to transmit information faster than light? Why or why not?
3. What would happen if quantum mechanics allowed cloning of quantum states?
4. How would errors in two-qubit gates affect entanglement quality?

## Capstone Group Task

Design and implement an "Entanglement-Based Quantum Game" that:

1. Creates different types of entangled states
2. Allows players to make strategic measurement choices
3. Demonstrates how entanglement can provide an advantage
4. Visualizes the quantum correlations

For example, create a simplified version of the "CHSH game" where entanglement helps two players coordinate without communication.

## How Would This Work on Real Hardware?

On real quantum hardware:

1. How is entanglement created physically?
2. What limits the fidelity of two-qubit gates?
3. How can we verify entanglement was actually created?
4. What's the current record for the number of qubits entangled together?

## Next Steps

In the next session, we'll explore quantum algorithms that leverage the properties of superposition and entanglement to solve problems more efficiently than classical computers.
