# Functions and Graphs in Machine Learning

## Introduction

This document provides a comprehensive exploration of functions and their graphical representations, focusing on their fundamental role in machine learning. We'll examine different types of functions, their properties, and how they form the mathematical backbone of various machine learning algorithms and models.

## Environment Setup

To run the code examples in this document, you'll need the following Python environment:

```bash
# Create and activate a virtual environment (recommended)
python -m venv ml-math-env
source ml-math-env/bin/activate  # On Windows: ml-math-env\Scripts\activate

# Install required packages
pip install numpy sympy matplotlib jupyter sklearn
```

## Core Concepts

### Functions and Mappings

A function $f: X \rightarrow Y$ is a rule that assigns to each element $x \in X$ exactly one element $y \in Y$. In machine learning, functions are used to represent models that map from input features to output predictions.

**Formal Definition:**

- Domain: Set $X$ of all possible input values
- Codomain: Set $Y$ containing all possible output values
- Range: Subset of $Y$ consisting of the values $f(x)$ actually taken by the function
- Function notation: $f(x) = y$, where $x \in X$ and $y \in Y$

**Python Implementation:**

```python
import numpy as np
import matplotlib.pyplot as plt
from sympy import symbols, Function, lambdify

# Define a function using NumPy
def square_function(x):
    """A simple function that squares its input."""
    return x**2

# Map this function over a domain
domain = np.linspace(-5, 5, 100)  # 100 points between -5 and 5
range_values = square_function(domain)

# Visualize the function
plt.figure(figsize=(10, 6))
plt.plot(domain, range_values, 'b-', linewidth=2)
plt.title('Graph of f(x) = x²')
plt.xlabel('x (Domain)')
plt.ylabel('f(x) (Range)')
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)
plt.show()

# Exploring the function properties
print(f"Domain example: {domain[:5]}...")
print(f"Range example: {range_values[:5]}...")
print(f"Minimum value in range: {np.min(range_values)}")
print(f"Maximum value in range: {np.max(range_values)}")
```

### Types of Functions

Functions in machine learning exhibit various important properties that affect model behavior, training dynamics, and generalization capabilities.

**Injective, Surjective, and Bijective Functions:**

- **Injective (One-to-One)**: If distinct inputs map to distinct outputs: $x_1 \neq x_2 \implies f(x_1) \neq f(x_2)$
- **Surjective (Onto)**: If every element in the codomain $Y$ is mapped to by at least one element from the domain $X$: $\forall y \in Y, \exists x \in X: f(x) = y$
- **Bijective (One-to-One and Onto)**: If a function is both injective and surjective, it establishes a perfect one-to-one correspondence between elements of the domain and codomain.

**Example and Visualization:**

```python
import numpy as np
import matplotlib.pyplot as plt

# Example functions
def function_a(x):
    """f(x) = x² for x ≥ 0 (not injective, not surjective)"""
    return x**2

def function_b(x):
    """f(x) = x³ (injective, surjective)"""
    return x**3

def function_c(x):
    """f(x) = e^x (injective, not surjective)"""
    return np.exp(x)

def function_d(x):
    """f(x) = sin(x) (not injective, not surjective)"""
    return np.sin(x)

# Domain for visualization
x = np.linspace(-2, 2, 1000)

# Create subplots for each function
fig, axs = plt.subplots(2, 2, figsize=(14, 10))
fig.suptitle('Types of Functions', fontsize=16)

# Plot each function
axs[0, 0].plot(x, function_a(x), 'r-')
axs[0, 0].set_title('f(x) = x² for x ≥ 0\n(Not Injective, Not Surjective)')
axs[0, 0].grid(True)
axs[0, 0].axhline(y=0, color='k', linestyle='-', alpha=0.3)
axs[0, 0].axvline(x=0, color='k', linestyle='-', alpha=0.3)

axs[0, 1].plot(x, function_b(x), 'g-')
axs[0, 1].set_title('f(x) = x³\n(Bijective: Injective and Surjective)')
axs[0, 1].grid(True)
axs[0, 1].axhline(y=0, color='k', linestyle='-', alpha=0.3)
axs[0, 1].axvline(x=0, color='k', linestyle='-', alpha=0.3)

axs[1, 0].plot(x, function_c(x), 'b-')
axs[1, 0].set_title('f(x) = e^x\n(Injective, Not Surjective)')
axs[1, 0].grid(True)
axs[1, 0].axhline(y=0, color='k', linestyle='-', alpha=0.3)
axs[1, 0].axvline(x=0, color='k', linestyle='-', alpha=0.3)

axs[1, 1].plot(x, function_d(x), 'm-')
axs[1, 1].set_title('f(x) = sin(x)\n(Not Injective, Not Surjective)')
axs[1, 1].grid(True)
axs[1, 1].axhline(y=0, color='k', linestyle='-', alpha=0.3)
axs[1, 1].axvline(x=0, color='k', linestyle='-', alpha=0.3)

plt.tight_layout(rect=[0, 0, 1, 0.95])
plt.show()
```

### Function Composition

Function composition is a fundamental operation that combines two functions to create a new function. This is particularly important in neural networks, where multiple layers of functions are composed together.

**Formal Definition:**

- Given functions $f: X \rightarrow Y$ and $g: Y \rightarrow Z$, their composition $(g \circ f): X \rightarrow Z$ is defined as $(g \circ f)(x) = g(f(x))$ for all $x \in X$.

**Example:**

```python
import numpy as np
import matplotlib.pyplot as plt

# Define two functions
def f(x):
    """f(x) = x² + 1"""
    return x**2 + 1

def g(x):
    """g(x) = sin(x)"""
    return np.sin(x)

# Function compositions
def g_of_f(x):
    """g(f(x)) = sin(x² + 1)"""
    return g(f(x))

def f_of_g(x):
    """f(g(x)) = (sin(x))² + 1"""
    return f(g(x))

# Domain for visualization
x = np.linspace(-3, 3, 1000)

# Visualization
plt.figure(figsize=(14, 8))

plt.subplot(2, 2, 1)
plt.plot(x, f(x), 'r-')
plt.title('f(x) = x² + 1')
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

plt.subplot(2, 2, 2)
plt.plot(x, g(x), 'b-')
plt.title('g(x) = sin(x)')
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

plt.subplot(2, 2, 3)
plt.plot(x, g_of_f(x), 'g-')
plt.title('(g ∘ f)(x) = g(f(x)) = sin(x² + 1)')
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

plt.subplot(2, 2, 4)
plt.plot(x, f_of_g(x), 'm-')
plt.title('(f ∘ g)(x) = f(g(x)) = sin²(x) + 1')
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)

plt.tight_layout()
plt.show()

# Notice that g ∘ f ≠ f ∘ g (function composition is not commutative)
```

### Inverse Functions

The inverse of a function $f$ is a function $f^{-1}$ that "undoes" the operation of $f$. Not all functions have inverses, but those that do are particularly useful in machine learning for tasks like normalization, dimensionality reduction, and generative models.

**Formal Definition:**

- A function $f: X \rightarrow Y$ has an inverse $f^{-1}: Y \rightarrow X$ if and only if $f$ is bijective.
- The inverse satisfies: $f^{-1}(f(x)) = x$ for all $x \in X$ and $f(f^{-1}(y)) = y$ for all $y \in Y$.

**Example:**

```python
import numpy as np
import matplotlib.pyplot as plt

# A bijective function and its inverse
def cube(x):
    """f(x) = x³ (bijective)"""
    return x**3

def cube_root(y):
    """f⁻¹(y) = ∛y"""
    return np.cbrt(y)  # Or y**(1/3)

# Verify inverse relationship
x_values = np.linspace(-2, 2, 10)
y_values = cube(x_values)
x_recovered = cube_root(y_values)

print("Original x    | f(x) = x³    | f⁻¹(f(x)) = ∛(x³)")
print("-" * 45)
for x, y, x_rec in zip(x_values, y_values, x_recovered):
    print(f"{x:+.6f}    | {y:+.6f}    | {x_rec:+.6f}")

# Visualization
x = np.linspace(-2, 2, 1000)
y = cube(x)

plt.figure(figsize=(12, 6))

# Plot function and inverse
plt.subplot(1, 2, 1)
plt.plot(x, y, 'b-', linewidth=2, label='f(x) = x³')
plt.plot(y, x, 'r-', linewidth=2, label='f⁻¹(y) = ∛y')
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)
plt.title('Function and Its Inverse')
plt.legend()

# Plot identity function to demonstrate f⁻¹(f(x)) = x
plt.subplot(1, 2, 2)
plt.plot(x, cube_root(cube(x)), 'g-', linewidth=2, label='f⁻¹(f(x)) = x')
plt.plot(x, x, 'k--', linewidth=1, label='y = x')
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)
plt.title('Verification: f⁻¹(f(x)) = x')
plt.legend()

plt.tight_layout()
plt.show()
```

## Application in Machine Learning: Activation Functions

Activation functions in neural networks are a perfect example of how functions are used in machine learning. They introduce non-linearity to the model, allowing it to learn complex patterns.

```python
import numpy as np
import matplotlib.pyplot as plt

# Common activation functions
def sigmoid(x):
    """Sigmoid activation: σ(x) = 1/(1+e^(-x))"""
    return 1 / (1 + np.exp(-x))

def relu(x):
    """ReLU activation: f(x) = max(0, x)"""
    return np.maximum(0, x)

def tanh(x):
    """Tanh activation: tanh(x)"""
    return np.tanh(x)

def leaky_relu(x, alpha=0.1):
    """Leaky ReLU: f(x) = max(αx, x)"""
    return np.maximum(alpha * x, x)

def softmax(x):
    """Softmax function (for vector input)"""
    exp_x = np.exp(x - np.max(x))  # Subtract max for numerical stability
    return exp_x / exp_x.sum()

# Domain for visualization
x = np.linspace(-5, 5, 1000)

# Visualization
plt.figure(figsize=(15, 10))

# Sigmoid
plt.subplot(2, 3, 1)
plt.plot(x, sigmoid(x), 'r-', linewidth=2)
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)
plt.title('Sigmoid: σ(x) = 1/(1+e^(-x))')
plt.ylim(-0.1, 1.1)

# ReLU
plt.subplot(2, 3, 2)
plt.plot(x, relu(x), 'g-', linewidth=2)
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)
plt.title('ReLU: f(x) = max(0, x)')
plt.ylim(-1, 5)

# Tanh
plt.subplot(2, 3, 3)
plt.plot(x, tanh(x), 'b-', linewidth=2)
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)
plt.title('Tanh: tanh(x)')
plt.ylim(-1.1, 1.1)

# Leaky ReLU
plt.subplot(2, 3, 4)
plt.plot(x, leaky_relu(x), 'm-', linewidth=2)
plt.grid(True)
plt.axhline(y=0, color='k', linestyle='-', alpha=0.3)
plt.axvline(x=0, color='k', linestyle='-', alpha=0.3)
plt.title('Leaky ReLU: f(x) = max(0.1x, x)')
plt.ylim(-1, 5)

# Softmax (example with vector input)
plt.subplot(2, 3, 5)
z = np.linspace(-2, 2, 5)  # 5 logits
softmax_values = softmax(z)
plt.bar(range(len(z)), softmax_values, color='orange')
plt.xticks(range(len(z)), [f'z{i+1}' for i in range(len(z))])
plt.title('Softmax: ezᵢ/Σezⱼ')
plt.ylim(0, 0.5)
plt.text(0, 0.45, f'Input z: {z.round(2)}', fontsize=10)
plt.text(0, 0.42, f'Output: {softmax_values.round(2)}', fontsize=10)

# Properties comparison
plt.subplot(2, 3, 6)
plt.text(0.5, 0.9, 'Activation Function Properties', fontsize=12, ha='center')
props = [
    'Range',
    'Differentiable',
    'Monotonic',
    'Computationally Efficient',
    'Vanishing Gradient Issue',
    'Main Usage'
]
sigmoid_props = ['(0, 1)', 'Yes', 'Yes', 'No', 'Yes', 'Binary classification']
relu_props = ['[0, ∞)', 'No at x=0', 'Yes', 'Yes', 'No', 'Hidden layers']
tanh_props = ['(-1, 1)', 'Yes', 'Yes', 'No', 'Yes', 'Hidden layers']

for i, prop in enumerate(props):
    plt.text(0.1, 0.8 - i*0.13, prop, fontsize=10)
    plt.text(0.5, 0.8 - i*0.13, sigmoid_props[i], fontsize=10)
    plt.text(0.7, 0.8 - i*0.13, relu_props[i], fontsize=10)
    plt.text(0.9, 0.8 - i*0.13, tanh_props[i], fontsize=10)
plt.text(0.5, 0.85, 'Sigmoid', fontsize=10, ha='center')
plt.text(0.7, 0.85, 'ReLU', fontsize=10, ha='center')
plt.text(0.9, 0.85, 'Tanh', fontsize=10, ha='center')
plt.axis('off')

plt.tight_layout()
plt.show()

# Implementation in neural networks (pseudocode)
"""
def forward_pass(x, weights, biases, activation_fn):
    # Compute linear transformation
    z = weights @ x + biases

    # Apply activation function
    a = activation_fn(z)

    return a
"""
```

## Theorem: Inverse Function Theorem

The Inverse Function Theorem provides conditions under which a function has an inverse in a neighborhood of a point.

**Theorem Statement:**
Let $f: \mathbb{R}^n \rightarrow \mathbb{R}^n$ be a continuously differentiable function on an open set $U$. If at a point $\mathbf{a} \in U$, the Jacobian determinant of $f$ is non-zero ($\det J_f(\mathbf{a}) \neq 0$), then there exists an open set $V$ containing $\mathbf{a}$ such that:

1. $f$ is one-to-one on $V$
2. The image $f(V)$ is open
3. The inverse function $f^{-1}: f(V) \rightarrow V$ is continuously differentiable

**Implications for Machine Learning:**
This theorem is particularly important for understanding invertible neural networks (like normalizing flows) and certain dimensionality reduction techniques. It provides the theoretical foundation for transformations that can be reliably inverted.

## Collaborative Peer Task: Function Analysis in Neural Networks

In groups of 2-3, work through the following problem:

Consider a simple neural network with one hidden layer defined by:

- Input layer: $\mathbf{x} \in \mathbb{R}^2$
- Hidden layer: $\mathbf{h} = \sigma(W_1 \mathbf{x} + \mathbf{b}_1)$ where $W_1 \in \mathbb{R}^{3 \times 2}$, $\mathbf{b}_1 \in \mathbb{R}^3$, and $\sigma$ is the sigmoid function
- Output layer: $\mathbf{y} = \text{softmax}(W_2 \mathbf{h} + \mathbf{b}_2)$ where $W_2 \in \mathbb{R}^{2 \times 3}$, $\mathbf{b}_2 \in \mathbb{R}^2$

**Tasks:**

1. Implement this neural network in Python.
2. Generate a grid of points in $\mathbb{R}^2$ and visualize the decision boundary.
3. Analyze the injectivity/surjectivity of each layer and the entire network.
4. Experiment with different activation functions and discuss their impact on the function properties of the network.

**Checkpoint Questions:**

- Is the composition of injective functions always injective?
- Can a neural network with ReLU activations represent a bijective function?
- How does the choice of activation function affect the invertibility of a neural network?

```python
# Starter code for the peer task
import numpy as np
import matplotlib.pyplot as plt

def sigmoid(x):
    return 1 / (1 + np.exp(-np.clip(x, -100, 100)))  # Clip to avoid overflow

def softmax(x):
    exp_x = np.exp(x - np.max(x, axis=1, keepdims=True))
    return exp_x / exp_x.sum(axis=1, keepdims=True)

class SimpleNN:
    def __init__(self, activation=sigmoid):
        # Initialize with random weights
        self.W1 = np.random.randn(3, 2) * 0.1
        self.b1 = np.zeros(3)
        self.W2 = np.random.randn(2, 3) * 0.1
        self.b2 = np.zeros(2)
        self.activation = activation

    def forward(self, X):
        # First layer
        self.z1 = X @ self.W1.T + self.b1
        self.h = self.activation(self.z1)

        # Output layer
        self.z2 = self.h @ self.W2.T + self.b2
        self.y_hat = softmax(self.z2)

        return self.y_hat

    def predict(self, X):
        probs = self.forward(X)
        return np.argmax(probs, axis=1)

# TODO: Complete the implementation and analysis
# 1. Generate data grid
# 2. Visualize decision boundary
# 3. Analyze function properties
# 4. Experiment with different activations
```

## Edge Case Analysis

When working with functions in machine learning, several edge cases can lead to unexpected behavior:

1. **Vanishing and Exploding Gradients**: Some activation functions like sigmoid can lead to vanishing gradients when inputs are far from zero, impeding training.

2. **Non-differentiable Points**: Functions like ReLU have points where derivatives are not defined (at x=0), which can affect gradient-based optimization.

3. **Domain Restrictions**: Some functions like logarithm have restricted domains, which can cause numerical issues if inputs go outside the valid range.

```python
import numpy as np
import matplotlib.pyplot as plt

# Demonstration of vanishing gradients in sigmoid
x = np.linspace(-10, 10, 1000)
sigmoid_vals = 1 / (1 + np.exp(-x))
sigmoid_grad = sigmoid_vals * (1 - sigmoid_vals)

plt.figure(figsize=(12, 5))

plt.subplot(1, 2, 1)
plt.plot(x, sigmoid_vals, 'b-', label='Sigmoid')
plt.plot(x, sigmoid_grad, 'r-', label='Gradient')
plt.title('Sigmoid and its Gradient')
plt.grid(True)
plt.legend()
plt.xlabel('x')
plt.ylabel('Value')

# Safe division function to handle division by zero
def safe_divide(x, y, default=0):
    """Safely divide x by y, returning default where y is zero."""
    with np.errstate(divide='ignore', invalid='ignore'):
        result = np.divide(x, y)
        if isinstance(result, np.ndarray):
            result[~np.isfinite(result)] = default
        elif not np.isfinite(result):
            result = default
    return result

# Function with potential numerical issues
def problematic_function(x):
    """A function with potential numerical issues"""
    return np.log(x) / (1 - np.exp(-x))

x_safe = np.linspace(0.01, 5, 1000)  # Avoid x=0 for log
try:
    y_risky = problematic_function(x_safe)
except RuntimeWarning:
    y_risky = np.array([problematic_function(xi) if xi > 0 and xi != 1 else np.nan for xi in x_safe])

plt.subplot(1, 2, 2)
plt.plot(x_safe, y_risky, 'g-')
plt.title('Function with Numerical Instabilities')
plt.grid(True)
plt.xlabel('x')
plt.ylabel('Value')
plt.ylim(-10, 10)  # Limit y-range to see the main behavior

plt.tight_layout()
plt.show()

# Best practices for handling edge cases
print("Best Practices for Handling Function Edge Cases in ML:")
print("1. Use numerically stable implementations (e.g., log-sum-exp trick)")
print("2. Clipping values to safe ranges before applying sensitive functions")
print("3. Monitoring for NaN/Inf values during training")
print("4. Using activation functions appropriate for your problem domain")
print("5. Proper weight initialization to avoid extreme input values")
```

## Challenge Activity: Function Approximation

**Advanced Challenge:**
Implement a neural network to approximate a complex function $f(x) = \sin(x) + 0.1x^2$ over the interval $[-5, 5]$.

**Steps:**

1. Generate training data by sampling the true function.
2. Design a neural network with appropriate activation functions.
3. Train the network to approximate the function.
4. Analyze the approximation error as a function of network complexity.
5. Investigate how well the network extrapolates outside the training domain.

**Doctoral-level Extension:**
Investigate the theoretical guarantees of function approximation using neural networks. Implement the Universal Approximation Theorem in practice by studying how the approximation error changes with the width and depth of the network. Explore recent advances in residual networks and their connection to numerical solutions of differential equations.

## Conclusion

Functions and their graphical representations form the mathematical foundation of machine learning models. By understanding different types of functions, their properties, and how they can be composed and inverted, we gain insights into the behavior and capabilities of machine learning algorithms. From activation functions that introduce non-linearity to inverse functions that enable generative modeling, these mathematical concepts translate directly into practical implementation considerations.

## References

1. Goodfellow, I., Bengio, Y., & Courville, A. (2016). Deep Learning. MIT Press.
2. Strang, G. (2006). Linear Algebra and Its Applications (4th ed.). Brooks Cole.
3. Apostol, T. M. (1974). Mathematical Analysis (2nd ed.). Addison-Wesley.
4. He, K., Zhang, X., Ren, S., & Sun, J. (2016). Deep residual learning for image recognition. In Proceedings of the IEEE conference on computer vision and pattern recognition (pp. 770-778).
