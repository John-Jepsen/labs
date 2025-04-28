# Part 0: Orientation — Road-map & Dev Setup

## Objective

Set up a collaborative quantum computing development environment and understand the roadmap for your quantum computing journey. By the end of this session, your team will have a functioning quantum computing environment and a clear understanding of the curriculum ahead.

---

## Environment Setup

### Requirements

- Python 3.10+
- Git for version control
- VS Code with Python extension or Jupyter Notebook
- Command line access

### Installation Instructions

| Command to run                                |
| :-------------------------------------------- |
| `pip install qiskit matplotlib numpy jupyter` |

For Cirq users:
| Command to run |
| :---- |
| `pip install cirq matplotlib numpy jupyter` |

### Environment Verification

Test your quantum environment with this simple script:

```python
# For Qiskit
try:
    import qiskit
    from qiskit import QuantumCircuit
    print(f"Qiskit version: {qiskit.__version__}")

    # Create a simple quantum circuit
    qc = QuantumCircuit(1, 1)
    qc.h(0)  # Apply Hadamard gate
    qc.measure(0, 0)  # Measure qubit 0 into classical bit 0

    print("✅ Qiskit is correctly installed!")
    print("Circuit created:")
    print(qc.draw())
except ImportError:
    print("❌ Qiskit not found. Please install with: pip install qiskit")
```

Alternative verification for Cirq:

```python
# For Cirq
try:
    import cirq
    print(f"Cirq version: {cirq.__version__}")

    # Create a simple quantum circuit
    q0 = cirq.LineQubit(0)
    circuit = cirq.Circuit(
        cirq.H(q0),  # Apply Hadamard gate
        cirq.measure(q0, key='m')  # Measure qubit
    )

    print("✅ Cirq is correctly installed!")
    print("Circuit created:")
    print(circuit)
except ImportError:
    print("❌ Cirq not found. Please install with: pip install cirq")
```

## The Quantum Playground Roadmap

![Quantum Computing Learning Path](https://placeholder-for-your-diagram.png)

### Course Structure Overview

1. **Orientation & Setup** (You are here)

   - Environment setup
   - Team formation
   - Understanding the learning journey

2. **Bits vs Qubits**

   - Classical computing review
   - Introduction to quantum states
   - Superposition concept

3. **Linear Algebra Foundations**

   - Vectors and matrices in quantum computing
   - Hands-on matrix manipulation
   - Quantum state representation

4. **Single-Qubit Operations**

   - Quantum gates visualization
   - Bloch sphere representation
   - Circuit building basics

5. **Measurement & Probability**

   - Quantum measurement theory
   - Randomness and probability
   - Repeated execution analysis

6. **Multi-Qubit Systems**

   - Entanglement phenomena
   - Bell states creation
   - Multi-qubit circuits

7. **Quantum Algorithms**

   - Deutschh-Jozsa algorithm
   - Grover's search algorithm
   - Algorithm analysis techniques

8. **Error Correction & Noise**

   - Real-world quantum limitations
   - Noise models
   - Error mitigation strategies

9. **Quantum Toolchain Exploration**

   - Vendor platforms comparison
   - Cloud quantum access
   - Framework differences

10. **Capstone Project**
    - End-to-end quantum application
    - Team collaboration
    - Results presentation

## Team Roles & Collaboration

For each lab session, team members should rotate through the following roles:

1. **Quantum Developer**: Writes the quantum circuit code
2. **Classical Developer**: Implements supporting classical code
3. **Debugger**: Tests code and identifies issues
4. **Analyst**: Interprets results and leads discussions
5. **Presenter**: Documents findings and prepares explanations

### Collaboration Process

1. **Start**: Read the lab document together, clarify team roles
2. **Implement**: Work together on the coding challenges
3. **Checkpoint**: Discuss insights at designated checkpoints
4. **Debug**: Troubleshoot issues as a team
5. **Extend**: Work on extension challenges if time permits
6. **Review**: Present results and reflect on learnings

## Discussion Topics

Take time to discuss these questions with your team:

1. What previous programming experience do team members have?
2. What are your expectations for this quantum computing curriculum?
3. What real-world applications of quantum computing interest your team most?
4. How will you organize your collaborative coding sessions?

## Collaboration Challenge

### Team Setup Exercise

1. Create a shared repository for your team's quantum projects
2. Each team member should clone the repository locally
3. Create a hello-quantum.py file with the following structure:

```python
# TEAM MEMBERS: [List names here]
# DATE: [Today's date]

# Import quantum library (Qiskit or Cirq)
# YOUR CODE HERE

def create_bell_pair():
    """
    Creates a simple Bell pair (entangled qubits)
    Returns the quantum circuit
    """
    # YOUR CODE HERE
    pass

def main():
    # Initialize team member roles
    team_roles = {
        "Quantum Developer": "[Name]",
        "Classical Developer": "[Name]",
        "Debugger": "[Name]",
        "Analyst": "[Name]",
        "Presenter": "[Name]"
    }

    print("Quantum Playground - Team Setup")
    print("================================")
    print("Team Roles:")
    for role, name in team_roles.items():
        print(f"- {role}: {name}")

    # Create and display a simple quantum circuit
    # YOUR CODE HERE

if __name__ == "__main__":
    main()
```

4. Complete the code as a team
5. Commit and push your changes
6. Verify everyone can run the same code

## What's Next?

In the next session, we'll dive into comparing classical bits and quantum qubits, exploring the fundamental differences that make quantum computing unique.

## Extension: IBM Quantum Experience

For those interested in accessing real quantum hardware:

1. Create an IBM Quantum account at [quantum-computing.ibm.com](https://quantum-computing.ibm.com/)
2. Setup your IBM Quantum API token with Qiskit
3. Explore the available quantum processors

## Troubleshooting Tips

- **ImportError**: Ensure your Python environment has the correct packages installed
- **Version conflicts**: Create a dedicated virtual environment for quantum computing
- **Visualization issues**: Make sure matplotlib is correctly installed
- **Jupyter notebook problems**: Try running with `jupyter notebook --NotebookApp.iopub_data_rate_limit=1e10`
