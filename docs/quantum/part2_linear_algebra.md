# Part 2: Linear Algebra Survival Kit

## Objective

Master the essential linear algebra operations that form the mathematical foundation of quantum computing. By the end of this session, your team will be able to represent quantum states as vectors, understand how quantum operations work as matrices, and apply these concepts to predict the behavior of simple quantum systems.

---

## Core Concepts

### Vectors, Matrices and Quantum States

In quantum computing, we represent:

- **Quantum states** as vectors (using ket notation $\ket{\psi}$)
- **Quantum operations** as matrices (unitary matrices)
- **Measurements** as projections of vectors

### Key Linear Algebra Operations

| Operation                    | Mathematical Form | Quantum Computing Application         |
| :--------------------------- | :---------------- | :------------------------------------ |
| Vector addition              | $\vec{v} + \vec{w}$ | Superposition of states               |
| Matrix-vector multiplication | $\mat{M}\vec{v}$  | Applying quantum gates                |
| Matrix multiplication        | $\mat{M}\mat{N}$  | Combining quantum operations          |
| Inner product                | $\inner{\vec{v}}{\vec{w}}$ | Calculating measurement probabilities |
| Tensor product               | $\vec{v} \otimes \vec{w}$ | Combining quantum systems             |

## Visual Explanation

![Quantum Linear Algebra](https://placeholder-for-your-diagram.png)

Quantum states live in a complex vector space called Hilbert space. A qubit's state can be visualized as a point on the Bloch sphere, with quantum operations rotating this point.

---

## Environment Setup

Ensure you have the necessary packages installed:

| Command to run                       |
| :----------------------------------- |
| `pip install numpy scipy matplotlib` |

## Starter Code

Create a new Python file called `quantum_linear_algebra.py` with the following structure:

```python
# TEAM MEMBERS: [List names here]
# DATE: [Today's date]

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Quantum states in computational basis
# |0⟩ and |1⟩ as column vectors
ket_0 = np.array([[1], [0]], dtype=complex)
ket_1 = np.array([[0], [1]], dtype=complex)

# Common quantum gates as matrices
I = np.array([[1, 0], [0, 1]], dtype=complex)  # Identity
X = np.array([[0, 1], [1, 0]], dtype=complex)  # Pauli-X (NOT)
Z = np.array([[1, 0], [0, -1]], dtype=complex)  # Pauli-Z
H = (1/np.sqrt(2)) * np.array([[1, 1], [1, -1]], dtype=complex)  # Hadamard

def print_state(state, label="State"):
    """Pretty print a quantum state vector"""
    print(f"{label}:")
    # Complex numbers are displayed as (real, imag)
    # YOUR CODE HERE: Format and display the state vector

def apply_gate(gate, state):
    """Apply a quantum gate to a state vector"""
    # YOUR CODE HERE: Implement matrix-vector multiplication
    pass

def vector_inner_product(state1, state2):
    """Calculate the inner product between two state vectors"""
    # YOUR CODE HERE: Implement inner product
    # Hint: For complex vectors, we need the complex conjugate
    pass

def state_to_bloch(state):
    """Convert a qubit state vector to Bloch sphere coordinates"""
    # A pure state |ψ⟩ = a|0⟩ + b|1⟩ maps to:
    # x = 2*Re(a*b*)
    # y = 2*Im(a*b*)
    # z = |a|^2 - |b|^2

    # YOUR CODE HERE: Calculate Bloch coordinates
    pass

def plot_bloch_vector(bloch_coords, title="Bloch Sphere Representation"):
    """Plot a state on the Bloch sphere"""
    fig = plt.figure(figsize=(8, 8))
    ax = fig.add_subplot(111, projection='3d')

    # Draw the Bloch sphere
    u, v = np.mgrid[0:2*np.pi:20j, 0:np.pi:10j]
    x = np.sin(v) * np.cos(u)
    y = np.sin(v) * np.sin(u)
    z = np.cos(v)
    ax.plot_wireframe(x, y, z, color="gray", alpha=0.2)

    # Draw the axes
    ax.plot([-1, 1], [0, 0], [0, 0], 'k-', alpha=0.5, lw=1)  # x-axis
    ax.plot([0, 0], [-1, 1], [0, 0], 'k-', alpha=0.5, lw=1)  # y-axis
    ax.plot([0, 0], [0, 0], [-1, 1], 'k-', alpha=0.5, lw=1)  # z-axis

    # Add basis states
    ax.text(0, 0, 1.1, r'$\ket{0}$')
    ax.text(0, 0, -1.1, r'$\ket{1}$')

    # Plot the input Bloch vector
    x, y, z = bloch_coords
    ax.plot([0, x], [0, y], [0, z], 'r-', lw=2)
    ax.plot([x], [y], [z], 'ro', markersize=10)

    # Set labels and title
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Z')
    ax.set_title(title)

    # Set the limits
    ax.set_xlim([-1.2, 1.2])
    ax.set_ylim([-1.2, 1.2])
    ax.set_zlim([-1.2, 1.2])

    plt.tight_layout()
    return fig

def measure_probability(state):
    """Calculate the measurement probabilities for a quantum state"""
    # YOUR CODE HERE: Calculate probability of measuring |0⟩ and |1⟩
    pass

def tensor_product_demo():
    """Demonstrate tensor product to combine quantum systems"""
    # YOUR CODE HERE: Implement and explain tensor products
    pass

def main():
    print("QUANTUM STATE VECTORS")
    print("====================")
    print_state(ket_0, "Basis state |0⟩")
    print_state(ket_1, "Basis state |1⟩")

    # Create a superposition state
    print("\nCREATING SUPERPOSITION")
    print("=====================")

    # YOUR CODE HERE: Create and print a superposition state
    # Hint: apply_gate(H, ket_0) creates |+⟩ state

    # Team Exercise: Create and visualize different states
    # 1. Create the |+⟩ state
    # YOUR CODE HERE

    # 2. Create the |-⟩ state
    # YOUR CODE HERE

    # 3. Create the |i⟩ state (hint: involves complex numbers)
    # YOUR CODE HERE

    print("\nQUANTUM GATE OPERATIONS")
    print("======================")

    # YOUR CODE HERE: Apply different gates and observe results

    print("\nMEASUREMENT PROBABILITIES")
    print("========================")

    # YOUR CODE HERE: Calculate measurement probabilities

    print("\nCOMBINING QUANTUM SYSTEMS")
    print("========================")
    tensor_product_demo()

    # TEAM DISCUSSION POINT:
    # How does quantum state representation differ from classical state?
    # YOUR DISCUSSION NOTES HERE

if __name__ == "__main__":
    main()
```

## Collaborative Challenge: Implement the Linear Algebra Functions

Working together as a team, your challenge is to:

1. Implement all functions marked with `# YOUR CODE HERE`
2. Create and visualize various quantum states on the Bloch sphere
3. Verify your understanding by calculating expected probabilities

## Team Roles for this Exercise

For this lab, consider the following role assignments:

1. **Matrix Operations Developer**: Implements core matrix functions
2. **Visualization Specialist**: Focuses on Bloch sphere visualization
3. **Verification Analyst**: Tests functions with known examples
4. **Documentation Lead**: Explains the mathematics in comments
5. **Extension Developer**: Works on additional features or optimizations

## Discussion Checkpoints

### Checkpoint 1: After State Vectors

- How do quantum state vectors differ from classical states?
- What is the significance of complex numbers in quantum computing?

### Checkpoint 2: After Gate Operations

- Why must quantum gates be unitary matrices?
- How does the Hadamard gate create superposition?

### Checkpoint 3: After Measurement

- How do we calculate the probability of measuring a specific outcome?
- Why is quantum measurement probabilistic rather than deterministic?

## Debugging Walkthrough

Common issues and their solutions:

1. **Problem**: Matrices or vectors have incorrect dimensions
   **Solution**: Double-check that you're using column vectors (shape n×1) not row vectors

2. **Problem**: Getting complex numbers when expecting real results
   **Solution**: For some values like probabilities, you may need to take the absolute square: `np.abs(value)**2`

3. **Problem**: Bloch sphere visualization appears distorted
   **Solution**: Ensure your state vector is normalized, and Bloch coordinates are calculated correctly

## Extension Challenge: Quantum Circuits with Linear Algebra

Implement a simple quantum circuit simulator using only linear algebra operations:

```python
def simulate_circuit(gates, initial_state=None):
    """
    Simulate a quantum circuit using linear algebra

    Args:
        gates: List of tuples (gate, qubit_indices) for multi-qubit systems
        initial_state: Starting state vector, defaults to |0...0⟩

    Returns:
        Final state vector after applying all gates
    """
    # YOUR CODE HERE
    pass
```

## What Would Break This?

Discuss with your team:

1. What happens if your quantum state vector is not normalized?
2. Is every 2×2 matrix a valid quantum operation? Why or why not?
3. How would errors in matrix multiplication affect quantum simulations?

## Capstone Group Task

Develop a quantum "memory" game that challenges players to:

1. Observe a quantum state's Bloch sphere representation
2. Identify which quantum gates (H, X, Z) were applied to the |0⟩ state
3. Calculate the correct measurement probabilities

Each team member should contribute a different puzzle for others to solve.

## How Would This Work on Real Hardware?

In real quantum computers:

1. How does the mathematical formalism of state vectors map to physical qubits?
2. How do sources of noise and decoherence affect the linear algebra description?
3. Why can't we directly measure the quantum state vector of a physical qubit?

## Next Steps

In the next session, we'll explore single-qubit operations in more detail, focusing on implementing and visualizing various quantum gates.
