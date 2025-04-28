# Part 3: Single-Qubit Operations

## Objective

Master the fundamental quantum gates that operate on single qubits. By the end of this session, your team will understand the geometric and algebraic interpretation of common quantum gates, implement gate sequences in code, and visualize their effects on the Bloch sphere.

---

## Core Concepts

### Single-Qubit Gates

| Gate      | Matrix Representation                                             | Effect on Qubit       | Classical Analog |
| :-------- | :---------------------------------------------------------------- | :-------------------- | :--------------- |
| Pauli-X   | $\begin{pmatrix} 0 & 1 \\ 1 & 0 \end{pmatrix}$                    | Bit flip (0↔1)        | NOT gate         |
| Pauli-Y   | $\begin{pmatrix} 0 & -i \\ i & 0 \end{pmatrix}$                   | Bit + phase flip      | None             |
| Pauli-Z   | $\begin{pmatrix} 1 & 0 \\ 0 & -1 \end{pmatrix}$                   | Phase flip            | None             |
| Hadamard  | $\frac{1}{\sqrt{2}}\begin{pmatrix} 1 & 1 \\ 1 & -1 \end{pmatrix}$ | Creates superposition | None             |
| S (Phase) | $\begin{pmatrix} 1 & 0 \\ 0 & i \end{pmatrix}$                    | π/2 phase rotation    | None             |
| T         | $\begin{pmatrix} 1 & 0 \\ 0 & e^{i\pi/4} \end{pmatrix}$           | π/4 phase rotation    | None             |

### Gate Sequences and Composition

Quantum gates can be applied in sequence to create more complex operations:

- Gates are applied from right to left in matrix notation
- Matrix multiplication represents sequential application
- Identity property: IX = XI = X

## Visual Explanation

![Bloch Sphere Gate Visualization](https://placeholder-for-your-diagram.png)

Single-qubit gates can be understood as rotations on the Bloch sphere:

- X gate: 180° rotation around the x-axis
- Y gate: 180° rotation around the y-axis
- Z gate: 180° rotation around the z-axis
- Hadamard: 180° rotation around the x+z axis

---

## Environment Setup

Ensure you have the necessary packages installed:

| Command to run                                        |
| :---------------------------------------------------- |
| `pip install qiskit matplotlib numpy jupyter seaborn` |

For Cirq users:
| Command to run |
| :---- |
| `pip install cirq matplotlib numpy jupyter seaborn` |

## Starter Code

Create a new Python file called `single_qubit_gates.py` with the following structure:

```python
# TEAM MEMBERS: [List names here]
# DATE: [Today's date]

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch
from mpl_toolkits.mplot3d import proj3d
import matplotlib.animation as animation

# Choose your framework
USE_QISKIT = True  # Set to False if using Cirq

if USE_QISKIT:
    from qiskit import QuantumCircuit, execute, Aer
    from qiskit.visualization import plot_bloch_vector, plot_histogram, plot_bloch_multivector
    from qiskit.quantum_info import Statevector
else:
    import cirq
    from cirq.sim.density_matrix_simulator import DensityMatrixSimulator

class Arrow3D(FancyArrowPatch):
    """Custom 3D arrow for visualization"""
    def __init__(self, xs, ys, zs, *args, **kwargs):
        super().__init__((0,0), (0,0), *args, **kwargs)
        self._verts3d = xs, ys, zs

    def do_3d_projection(self, renderer=None):
        xs3d, ys3d, zs3d = self._verts3d
        xs, ys, zs = proj3d.proj_transform(xs3d, ys3d, zs3d, self.axes.M)
        self.set_positions((xs[0], ys[0]), (xs[1], ys[1]))
        return np.min(zs)

def setup_bloch_sphere(ax, title="Bloch Sphere"):
    """Set up Bloch sphere visualization"""
    # YOUR CODE HERE: Draw Bloch sphere wireframe
    # Hint: Use sphere equations x^2 + y^2 + z^2 = 1

    # Add basis state labels
    ax.text(0, 0, 1.1, r'$|0\rangle$')
    ax.text(0, 0, -1.1, r'$|1\rangle$')
    ax.text(1.1, 0, 0, r'$|+\rangle$')
    ax.text(-1.1, 0, 0, r'$|-\rangle$')
    ax.text(0, 1.1, 0, r'$|i\rangle$')
    ax.text(0, -1.1, 0, r'$|-i\rangle$')

    # Set labels and title
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Z')
    ax.set_title(title)

    # Set the limits
    ax.set_xlim([-1.2, 1.2])
    ax.set_ylim([-1.2, 1.2])
    ax.set_zlim([-1.2, 1.2])

    return ax

def state_to_bloch(state_vector):
    """Convert state vector to Bloch sphere coordinates"""
    # For a state |ψ⟩ = a|0⟩ + b|1⟩:
    # x = 2*Re(a*b*)
    # y = 2*Im(a*b*)
    # z = |a|^2 - |b|^2

    # YOUR CODE HERE: Calculate Bloch coordinates
    # Return [x, y, z]
    pass

def apply_x_gate_qiskit():
    """Demonstrate X gate with Qiskit"""
    # YOUR CODE HERE: Create circuit with X gate
    # Visualize before and after states
    pass

def apply_z_gate_qiskit():
    """Demonstrate Z gate with Qiskit"""
    # YOUR CODE HERE: Create circuit with Z gate
    # Visualize before and after states
    pass

def apply_h_gate_qiskit():
    """Demonstrate Hadamard gate with Qiskit"""
    # YOUR CODE HERE: Create circuit with H gate
    # Visualize before and after states
    pass

def apply_x_gate_cirq():
    """Demonstrate X gate with Cirq"""
    # YOUR CODE HERE: Create circuit with X gate
    # Visualize before and after states
    pass

def apply_z_gate_cirq():
    """Demonstrate Z gate with Cirq"""
    # YOUR CODE HERE: Create circuit with Z gate
    # Visualize before and after states
    pass

def apply_h_gate_cirq():
    """Demonstrate Hadamard gate with Cirq"""
    # YOUR CODE HERE: Create circuit with H gate
    # Visualize before and after states
    pass

def gate_sequence_visualization():
    """Visualize a sequence of gates applied to a qubit"""
    # Animation of gate sequence: H → X → Z
    # YOUR CODE HERE: Create an animation of gate sequence
    pass

def implement_arbitrary_rotation():
    """Implement an arbitrary rotation on Bloch sphere"""
    # YOUR CODE HERE: Implement Rx, Ry, Rz rotations
    # Show how to achieve any rotation
    pass

def main():
    print("SINGLE-QUBIT QUANTUM GATES")
    print("=========================")

    print("\nX GATE (QUANTUM NOT)")
    print("===================")
    if USE_QISKIT:
        apply_x_gate_qiskit()
    else:
        apply_x_gate_cirq()

    print("\nZ GATE (PHASE FLIP)")
    print("===================")
    if USE_QISKIT:
        apply_z_gate_qiskit()
    else:
        apply_z_gate_cirq()

    print("\nHADAMARD GATE (SUPERPOSITION)")
    print("=============================")
    if USE_QISKIT:
        apply_h_gate_qiskit()
    else:
        apply_h_gate_cirq()

    print("\nGATE SEQUENCE VISUALIZATION")
    print("===========================")
    gate_sequence_visualization()

    print("\nARBITRARY ROTATIONS")
    print("===================")
    implement_arbitrary_rotation()

    # TEAM DISCUSSION POINT:
    # How do quantum gates differ from classical gates?
    # YOUR DISCUSSION NOTES HERE

if __name__ == "__main__":
    main()
```

## Collaborative Challenge: Implement Gate Visualizations

Working together as a team, your challenge is to:

1. Complete the functions marked with `# YOUR CODE HERE`
2. Implement and visualize the effect of X, Z, and H gates
3. Create a visualization of a gate sequence (H → X → Z)
4. Implement arbitrary rotations (Rx, Ry, Rz gates)

## Team Roles for This Exercise

For this lab, consider the following role assignments:

1. **Quantum Gate Implementer**: Focuses on implementing gate operations
2. **Visualization Developer**: Works on Bloch sphere visualizations
3. **Animation Specialist**: Creates gate sequence animation
4. **Verification Tester**: Tests gate implementations against expected outcomes
5. **Documentation Lead**: Explains the gate effects and their implications

## Discussion Checkpoints

### Checkpoint 1: After Implementing Basic Gates

- How does the X gate compare to a classical NOT gate?
- What special property does the Hadamard gate have?
- Why are there no classical equivalents to Z, S and T gates?

### Checkpoint 2: After Gate Sequence Visualization

- Is the order of gate application important? Why?
- Can all single-qubit gates be composed from a smaller set?
- What does it mean that gates are unitary?

### Checkpoint 3: After Arbitrary Rotations

- How can we reach any point on the Bloch sphere?
- How many parameters are needed to specify an arbitrary single-qubit state?
- Why are rotational gates important for quantum algorithms?

## Debugging Walkthrough

Common issues and their solutions:

1. **Problem**: State vector doesn't change after applying gate
   **Solution**: Check if you're creating a new circuit for each gate or correctly updating the state vector

2. **Problem**: Bloch sphere visualization is incorrect
   **Solution**: Ensure that the state vector is normalized and the Bloch coordinate conversion is correct

3. **Problem**: Animation not displaying properly
   **Solution**: Test with a simple animation first, and verify matplotlib's animation capabilities in your environment

## Extension Challenge: Quantum Tomography

Implement a simplified version of quantum state tomography:

```python
def quantum_tomography(state_vector):
    """
    Perform simplified quantum state tomography

    Args:
        state_vector: The quantum state to analyze

    Returns:
        Reconstructed Bloch sphere coordinates based on "measurements"
    """
    # YOUR CODE HERE
    pass
```

This should simulate:

1. Measuring the state in X, Y, and Z bases
2. Using the measurement statistics to reconstruct the Bloch vector
3. Comparing the reconstructed state with the original

## What Would Break This?

Discuss with your team:

1. What happens if you apply a non-unitary matrix to a qubit?
2. Can you create an operation that extracts both α and β from a qubit state α|0⟩ + β|1⟩ in a single measurement?
3. What limitations would noise introduce to single-qubit operations?

## Capstone Group Task

Create an interactive "Quantum Gate Explorer" tool that:

1. Allows users to select from common gates (X, Y, Z, H, S, T)
2. Shows the gate's matrix representation
3. Visualizes the gate's effect on different input states
4. Displays the resulting measurement probabilities
5. Demonstrates the gate's effect as a rotation on the Bloch sphere

## How Would This Work on Real Hardware?

On real quantum hardware:

1. How accurately can single-qubit gates be implemented?
2. What causes gate errors, and how are they measured?
3. How could you determine the fidelity of your gate operations?
4. What's the difference between coherent and incoherent errors?

## Next Steps

In the next session, we'll explore measurement and probability in quantum computing, focusing on how to extract classical information from quantum states.
