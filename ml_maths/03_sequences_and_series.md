# Sequences and Series in Machine Learning

## Introduction

This document explores sequences and series - fundamental mathematical structures that underpin many machine learning concepts, from iterative optimization algorithms to model convergence properties. Through theoretical foundations and practical implementations, we'll discover how these mathematical tools enable us to analyze and improve machine learning systems.

## Environment Setup

To run the code examples in this document, you'll need the following Python environment:

```bash
# Create and activate a virtual environment (recommended)
python -m venv ml-math-env
source ml-math-env/bin/activate  # On Windows: ml-math-env\Scripts\activate

# Install required packages
pip install numpy sympy matplotlib jupyter tensorflow
```

## Core Concepts

### Sequences

A sequence is an ordered list of numbers, denoted as $(a_n)_{n=1}^{\infty}$ or simply $(a_n)$. In machine learning, sequences appear in many contexts, such as:

- Iterations of optimization algorithms
- Training losses across epochs
- Accuracy measurements during validation
- Weight updates in neural networks

**Formal Definition:**

- A sequence is a function $a: \mathbb{N} \rightarrow \mathbb{R}$ where $a(n) = a_n$ is the $n$-th term.
- Sequence notation: $(a_1, a_2, a_3, \ldots)$ or $(a_n)_{n=1}^{\infty}$

**Common Sequences in Machine Learning:**

1. Learning rate schedules: $\alpha_n = \alpha_0 \cdot \gamma^n$ (exponential decay)
2. Gradient descent iterations: $\theta_{n+1} = \theta_n - \alpha_n \nabla f(\theta_n)$
3. Error sequences: $e_n = |y - \hat{y}_n|$

**Python Implementation:**

```python
import numpy as np
import matplotlib.pyplot as plt

# Example: Learning rate decay sequence
def learning_rate_sequence(initial_rate=0.1, decay_factor=0.9, steps=100):
    """Generate a sequence of learning rates with exponential decay."""
    return [initial_rate * (decay_factor ** n) for n in range(steps)]

# Example: Gradient descent sequence for a simple function
def gradient_descent_sequence(f, df, initial_point, learning_rate, steps=100):
    """Generate the sequence of points from gradient descent."""
    points = [initial_point]
    for i in range(steps):
        current = points[-1]
        next_point = current - learning_rate * df(current)
        points.append(next_point)
    return np.array(points)

# Visualize learning rate decay
lr_seq = learning_rate_sequence(initial_rate=0.1, decay_factor=0.95, steps=50)
plt.figure(figsize=(10, 6))
plt.plot(range(len(lr_seq)), lr_seq, 'bo-')
plt.title('Learning Rate Decay Sequence')
plt.xlabel('Step (n)')
plt.ylabel('Learning Rate')
plt.grid(True)
plt.show()

# Example: Gradient descent for f(x) = x² + 2x + 1
def f(x):
    return x**2 + 2*x + 1

def df(x):
    return 2*x + 2

# Generate the sequence of points
gd_seq = gradient_descent_sequence(f, df, initial_point=2.0, learning_rate=0.1, steps=20)
x_values = np.linspace(-3, 3, 100)
y_values = f(x_values)

# Visualize the gradient descent process
plt.figure(figsize=(10, 6))
plt.plot(x_values, y_values, 'b-', label='f(x) = x² + 2x + 1')
plt.plot(gd_seq, f(gd_seq), 'ro-', label='Gradient Descent Path')
plt.title('Gradient Descent Sequence')
plt.xlabel('x')
plt.ylabel('f(x)')
plt.grid(True)
plt.legend()
plt.show()

# Print the sequence of points and function values
print("Gradient Descent Sequence:")
for i, point in enumerate(gd_seq[:10]):  # Show first 10 points
    print(f"Step {i}: x = {point:.6f}, f(x) = {f(point):.6f}")
```

### Sequence Convergence

Convergence is a critical concept in machine learning, determining whether algorithms will find optimal solutions and how quickly they approach them.

**Formal Definition:**
A sequence $(a_n)$ converges to a limit $L$ if for every $\epsilon > 0$, there exists an $N \in \mathbb{N}$ such that for all $n > N$, $|a_n - L| < \epsilon$.

We write: $\lim_{n \to \infty} a_n = L$ or $a_n \to L$ as $n \to \infty$.

**Types of Convergence in ML:**

1. **Convergence of model parameters**: Do the weights stabilize during training?
2. **Convergence of loss functions**: Does the training error approach a minimum?
3. **Convergence of predictions**: Do the model's outputs stabilize?

**Example:**

```python
import numpy as np
import matplotlib.pyplot as plt
from sympy import symbols, limit, oo, sqrt

# Sequence examples
def sequence_1(n):
    """Sequence a_n = 1/n (converges to 0)"""
    return 1/n

def sequence_2(n):
    """Sequence a_n = (n+1)/n (converges to 1)"""
    return (n+1)/n

def sequence_3(n):
    """Sequence a_n = (-1)^n (does not converge)"""
    return (-1)**n

def sequence_4(n):
    """Sequence a_n = n/(n+1) (converges to 1)"""
    return n/(n+1)

# Calculate sequence terms
n_values = np.arange(1, 101)
seq1_values = [sequence_1(n) for n in n_values]
seq2_values = [sequence_2(n) for n in n_values]
seq3_values = [sequence_3(n) for n in n_values]
seq4_values = [sequence_4(n) for n in n_values]

# Visualization
plt.figure(figsize=(15, 10))

plt.subplot(2, 2, 1)
plt.plot(n_values, seq1_values, 'ro-')
plt.axhline(y=0, color='k', linestyle='--')
plt.title('Sequence: $a_n = 1/n$')
plt.xlabel('n')
plt.ylabel('Value')
plt.grid(True)

plt.subplot(2, 2, 2)
plt.plot(n_values, seq2_values, 'bo-')
plt.axhline(y=1, color='k', linestyle='--')
plt.title('Sequence: $a_n = (n+1)/n$')
plt.xlabel('n')
plt.ylabel('Value')
plt.grid(True)

plt.subplot(2, 2, 3)
plt.plot(n_values, seq3_values, 'go-')
plt.title('Sequence: $a_n = (-1)^n$')
plt.xlabel('n')
plt.ylabel('Value')
plt.grid(True)

plt.subplot(2, 2, 4)
plt.plot(n_values, seq4_values, 'mo-')
plt.axhline(y=1, color='k', linestyle='--')
plt.title('Sequence: $a_n = n/(n+1)$')
plt.xlabel('n')
plt.ylabel('Value')
plt.grid(True)

plt.tight_layout()
plt.show()

# Using SymPy to compute limits symbolically
n = symbols('n')
print("Symbolic limits of sequences:")
print(f"lim (1/n) as n→∞ = {limit(1/n, n, oo)}")
print(f"lim (n+1)/n as n→∞ = {limit((n+1)/n, n, oo)}")
print(f"lim n/(n+1) as n→∞ = {limit(n/(n+1), n, oo)}")
```

### Series

A series is the sum of the terms of a sequence. In machine learning, series appear in various contexts:

- Approximating complex functions
- Computing gradients over batches
- Taylor expansions of loss functions
- Analyzing convergence rates

**Formal Definition:**
Given a sequence $(a_n)$, the corresponding series is denoted $\sum_{n=1}^{\infty} a_n = a_1 + a_2 + a_3 + \ldots$

The partial sums of the series form a new sequence $(S_N)$, where $S_N = \sum_{n=1}^{N} a_n$.

**Common Series in Machine Learning:**

1. Geometric series: $\sum_{n=0}^{\infty} \alpha^n = 1 + \alpha + \alpha^2 + \ldots$ (for $|\alpha| < 1$)
2. Exponential series: $e^x = \sum_{n=0}^{\infty} \frac{x^n}{n!}$
3. Taylor series of activation functions and loss functions

**Python Implementation:**

```python
import numpy as np
import matplotlib.pyplot as plt
from sympy import symbols, Sum, oo, exp, factorial

# Geometric series: sum of α^n from n=0 to N
def geometric_series(alpha, N):
    """Calculate the partial sum of the geometric series."""
    if abs(alpha) >= 1:
        print("Warning: Geometric series only converges for |α| < 1")
    return sum(alpha**n for n in range(N+1))

# Exponential series: approximation of e^x
def exp_series_approx(x, N):
    """Approximate e^x using N terms of its Taylor series."""
    return sum(x**n / np.math.factorial(n) for n in range(N+1))

# Visualize geometric series convergence
alphas = [0.5, 0.8, 0.9, 0.95]
N_values = range(1, 51)

plt.figure(figsize=(12, 6))
for alpha in alphas:
    partial_sums = [geometric_series(alpha, N) for N in N_values]
    true_sum = 1 / (1 - alpha) if abs(alpha) < 1 else float('inf')
    plt.plot(N_values, partial_sums, 'o-', label=f'α = {alpha}, True sum = {true_sum:.2f}')
    plt.axhline(y=true_sum, linestyle='--', alpha=0.5)

plt.title('Convergence of Geometric Series: $\\sum_{n=0}^{N} \\alpha^n$')
plt.xlabel('Number of Terms (N)')
plt.ylabel('Partial Sum')
plt.legend()
plt.grid(True)
plt.show()

# Visualize exponential series approximation
x_values = np.linspace(-2, 2, 100)
terms = [1, 2, 3, 5, 10]

plt.figure(figsize=(12, 6))
for N in terms:
    approx_values = [exp_series_approx(x, N) for x in x_values]
    plt.plot(x_values, approx_values, label=f'N = {N} terms')

# Plot the true exponential function
plt.plot(x_values, np.exp(x_values), 'k--', label='True exp(x)')

plt.title('Approximation of $e^x$ by Partial Sums of its Taylor Series')
plt.xlabel('x')
plt.ylabel('$e^x$')
plt.legend()
plt.grid(True)
plt.ylim(-1, 10)
plt.show()

# Using SymPy for symbolic series
x, n = symbols('x n')
print("Symbolic series examples:")

# Geometric series sum formula
a = symbols('a')
geometric_sum = Sum(a**n, (n, 0, oo))
print(f"∑ a^n from n=0 to ∞ = {geometric_sum.doit()}, for |a| < 1")

# Exponential series formula
exp_sum = Sum(x**n/factorial(n), (n, 0, oo))
print(f"∑ x^n/n! from n=0 to ∞ = {exp_sum.doit()}")
```

### Convergence of Series

The convergence of series is crucial for understanding whether algorithms will converge to optimal solutions and how approximations work in machine learning.

**Formal Definition:**
A series $\sum_{n=1}^{\infty} a_n$ converges if the sequence of partial sums $(S_N)$ converges to a finite limit $S$. In that case, we write $\sum_{n=1}^{\infty} a_n = S$.

**Tests for Convergence:**

1. **Geometric Series Test**: $\sum_{n=0}^{\infty} r^n$ converges to $\frac{1}{1-r}$ if $|r| < 1$
2. **Ratio Test**: If $\lim_{n \to \infty} \left|\frac{a_{n+1}}{a_n}\right| = L < 1$, then the series converges absolutely
3. **Comparison Test**: If $0 \leq a_n \leq b_n$ and $\sum b_n$ converges, then $\sum a_n$ converges

**Common Convergent Series in ML:**

1. $\sum_{n=1}^{\infty} \frac{1}{n^2} = \frac{\pi^2}{6}$ (converges)
2. $\sum_{n=1}^{\infty} \frac{1}{n}$ (diverges - harmonic series)
3. $\sum_{n=0}^{\infty} \frac{x^n}{n!} = e^x$ (converges for all $x$)

## Application in Machine Learning: Learning Rate Schedules

Learning rate schedules are practical applications of sequences in machine learning, controlling how quickly model parameters are updated during training.

```python
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf

# Common learning rate schedules
def constant_schedule(initial_lr=0.01, steps=100):
    """Constant learning rate schedule."""
    return np.ones(steps) * initial_lr

def step_decay_schedule(initial_lr=0.01, drop_factor=0.5, epochs_drop=10, steps=100):
    """Step decay learning rate schedule."""
    return initial_lr * np.power(drop_factor, np.floor((1+np.arange(steps))/epochs_drop))

def exp_decay_schedule(initial_lr=0.01, decay_rate=0.9, steps=100):
    """Exponential decay learning rate schedule."""
    return initial_lr * np.power(decay_rate, np.arange(steps))

def time_based_decay(initial_lr=0.01, decay_rate=0.01, steps=100):
    """Time-based decay learning rate schedule."""
    return initial_lr / (1 + decay_rate * np.arange(steps))

def cosine_decay_schedule(initial_lr=0.01, steps=100):
    """Cosine decay learning rate schedule."""
    return initial_lr * 0.5 * (1 + np.cos(np.pi * np.arange(steps) / steps))

# Visualize learning rate schedules
steps = 100
schedules = {
    'Constant': constant_schedule(steps=steps),
    'Step Decay': step_decay_schedule(steps=steps),
    'Exponential Decay': exp_decay_schedule(steps=steps),
    'Time-based Decay': time_based_decay(steps=steps),
    'Cosine Decay': cosine_decay_schedule(steps=steps)
}

plt.figure(figsize=(12, 6))
for name, schedule in schedules.items():
    plt.plot(range(steps), schedule, label=name)

plt.title('Learning Rate Schedules')
plt.xlabel('Training Step')
plt.ylabel('Learning Rate')
plt.legend()
plt.grid(True)
plt.yscale('log')  # Log scale to better visualize differences
plt.show()

# Simple demonstration of learning rate impact on training
def train_with_different_lr_schedules(schedules, epochs=100):
    """Compare training with different learning rate schedules using a simple model."""
    # Generate synthetic data
    np.random.seed(42)
    X = np.random.rand(1000, 10)
    y = np.random.randint(0, 2, 1000)

    results = {}

    for name, schedule in schedules.items():
        # Build a simple model
        model = tf.keras.Sequential([
            tf.keras.layers.Dense(16, activation='relu', input_shape=(10,)),
            tf.keras.layers.Dense(8, activation='relu'),
            tf.keras.layers.Dense(1, activation='sigmoid')
        ])

        # Use the custom learning rate schedule
        lr_schedule = tf.keras.optimizers.schedules.LearningRateSchedule()
        optimizer = tf.keras.optimizers.SGD(learning_rate=0.01)

        model.compile(optimizer=optimizer, loss='binary_crossentropy', metrics=['accuracy'])

        # Track learning rate and loss history
        lr_history = []
        loss_history = []

        # Custom callback to update learning rate manually
        class LRSchedulerCallback(tf.keras.callbacks.Callback):
            def on_epoch_begin(self, epoch, logs=None):
                if epoch < len(schedule):
                    tf.keras.backend.set_value(self.model.optimizer.learning_rate, schedule[epoch])
                    lr_history.append(schedule[epoch])

            def on_epoch_end(self, epoch, logs=None):
                loss_history.append(logs['loss'])

        # Train the model
        history = model.fit(
            X, y,
            epochs=min(epochs, len(schedule)),
            batch_size=32,
            verbose=0,
            callbacks=[LRSchedulerCallback()]
        )

        results[name] = {
            'lr_history': lr_history,
            'loss_history': loss_history
        }

    return results

# Uncomment to run training comparison (may take some time)
"""
training_results = train_with_different_lr_schedules(schedules, epochs=100)

# Plot training results
plt.figure(figsize=(12, 10))

plt.subplot(2, 1, 1)
for name, result in training_results.items():
    plt.plot(result['lr_history'], label=name)
plt.title('Learning Rate During Training')
plt.xlabel('Epoch')
plt.ylabel('Learning Rate')
plt.legend()
plt.grid(True)
plt.yscale('log')

plt.subplot(2, 1, 2)
for name, result in training_results.items():
    plt.plot(result['loss_history'], label=name)
plt.title('Loss During Training')
plt.xlabel('Epoch')
plt.ylabel('Loss')
plt.legend()
plt.grid(True)

plt.tight_layout()
plt.show()
"""
```

## Theorem: Geometric Series Convergence

Geometric series are particularly important in machine learning, appearing in discount factors in reinforcement learning, convergence analysis of iterative methods, and more.

**Theorem Statement:**
The geometric series $\sum_{n=0}^{\infty} r^n = 1 + r + r^2 + r^3 + \cdots$ converges if and only if $|r| < 1$. In that case, the sum is $\frac{1}{1-r}$.

**Proof (Sketch):**
For $|r| < 1$, the partial sum $S_N = \sum_{n=0}^{N} r^n = \frac{1-r^{N+1}}{1-r}$. As $N \to \infty$, $r^{N+1} \to 0$, so $S_N \to \frac{1}{1-r}$.

For $|r| \geq 1$, the terms $r^n$ do not approach zero, so the series diverges.

**Application in Machine Learning:**
In reinforcement learning, the discounted future rewards follow a geometric series with discount factor $\gamma < 1$:
$\sum_{t=0}^{\infty} \gamma^t r_{t+1} = r_1 + \gamma r_2 + \gamma^2 r_3 + \cdots$

The expected return converges when $\gamma < 1$, which is why discount factors are typically set between 0 and 1.

## Collaborative Peer Task: Convergence Analysis in Gradient Descent

In groups of 2-3, work through the following problem:

Consider the gradient descent algorithm for minimizing a function $f(x) = x^2$:

- Starting point: $x_0$
- Update rule: $x_{n+1} = x_n - \alpha \cdot f'(x_n) = x_n - 2\alpha \cdot x_n = (1 - 2\alpha) \cdot x_n$

**Tasks:**

1. Express $x_n$ in terms of $x_0$ and $n$.
2. For what values of the learning rate $\alpha$ does the sequence $(x_n)$ converge to the minimum (0)?
3. Implement the algorithm in Python and verify your theoretical results.
4. Analyze the convergence rate for different values of $\alpha$.

**Checkpoint Questions:**

- How does the sequence $(x_n)$ relate to a geometric series?
- What happens when $\alpha$ is too large? Can you identify the threshold?
- What is the optimal value of $\alpha$ for fastest convergence?

```python
# Starter code for the peer task
import numpy as np
import matplotlib.pyplot as plt

def gradient_descent_quadratic(x0, alpha, steps=20):
    """Perform gradient descent on f(x) = x^2."""
    sequence = [x0]
    for _ in range(steps):
        x_next = (1 - 2*alpha) * sequence[-1]
        sequence.append(x_next)
    return np.array(sequence)

# TODO: Complete the analysis
# 1. Derive the formula for x_n
# 2. Determine convergence conditions
# 3. Implement and verify
# 4. Analyze convergence rates

# Hint: Start by testing different alphas
alphas = [0.1, 0.4, 0.5, 0.6]
x0 = 10.0
steps = 20

plt.figure(figsize=(10, 6))
for alpha in alphas:
    sequence = gradient_descent_quadratic(x0, alpha, steps)
    plt.plot(range(steps+1), sequence, 'o-', label=f'α = {alpha}')

plt.title('Gradient Descent Sequences for Different Learning Rates')
plt.xlabel('Step (n)')
plt.ylabel('x_n')
plt.grid(True)
plt.legend()
plt.axhline(y=0, color='k', linestyle='--', alpha=0.3)
plt.show()
```

## Edge Case Analysis

When working with sequences and series in machine learning, several edge cases require careful attention:

1. **Divergent Sequences**: Gradient descent with too large a learning rate can lead to divergent parameter sequences.
2. **Slow Convergence**: Some sequences converge too slowly to be practical (e.g., $\frac{1}{n}$ approaches 0 very slowly).
3. **Oscillatory Behavior**: Sequences that oscillate may not have a clear limit, complicating convergence analysis.
4. **Numerical Precision**: Limited floating-point precision can affect the calculation of series with very small terms.

```python
import numpy as np
import matplotlib.pyplot as plt

# Edge case 1: Divergent gradient descent
def divergent_gd(x0=1.0, alpha=0.6, steps=20):
    """Demonstrate divergent gradient descent for f(x) = x^2 with large alpha."""
    sequence = [x0]
    for _ in range(steps):
        x_next = (1 - 2*alpha) * sequence[-1]
        sequence.append(x_next)
    return np.array(sequence)

# Edge case 2: Very slow convergence
def slow_convergence_sequence(steps=100):
    """Sequence 1/n that converges very slowly to 0."""
    return np.array([1/n for n in range(1, steps+1)])

# Edge case 3: Oscillatory sequence
def oscillatory_sequence(steps=100):
    """Sequence (-1)^n that oscillates between -1 and 1."""
    return np.array([(-1)**n for n in range(steps)])

# Edge case 4: Numerical precision issue
def precision_issue_sequence(steps=30):
    """Sequence where floating-point precision becomes an issue."""
    return np.array([1/(2**n) for n in range(steps)])

# Visualize edge cases
plt.figure(figsize=(15, 10))

plt.subplot(2, 2, 1)
div_seq = divergent_gd(alpha=0.6)
plt.plot(range(len(div_seq)), div_seq, 'ro-')
plt.title('Divergent Gradient Descent (α=0.6)')
plt.xlabel('Step (n)')
plt.ylabel('x_n')
plt.grid(True)

plt.subplot(2, 2, 2)
slow_seq = slow_convergence_sequence()
plt.plot(range(1, len(slow_seq)+1), slow_seq, 'go-')
plt.title('Slow Convergence: 1/n')
plt.xlabel('n')
plt.ylabel('1/n')
plt.grid(True)

plt.subplot(2, 2, 3)
osc_seq = oscillatory_sequence()
plt.plot(range(len(osc_seq)), osc_seq, 'bo-')
plt.title('Oscillatory Sequence: (-1)^n')
plt.xlabel('n')
plt.ylabel('(-1)^n')
plt.grid(True)

plt.subplot(2, 2, 4)
prec_seq = precision_issue_sequence()
plt.plot(range(len(prec_seq)), prec_seq, 'mo-')
plt.title('Numerical Precision Issue: 1/2^n')
plt.xlabel('n')
plt.ylabel('1/2^n')
plt.yscale('log')  # Log scale to see the small values
plt.grid(True)

plt.tight_layout()
plt.show()

# Strategies for handling edge cases
print("Strategies for Handling Edge Cases in Sequences and Series:")
print("1. Divergent sequences: Use adaptive learning rates or learning rate schedules")
print("2. Slow convergence: Use acceleration techniques like momentum or Adam")
print("3. Oscillatory behavior: Add damping terms or use averaging techniques")
print("4. Numerical precision: Use stable summation algorithms or log-domain calculations")
```

## Challenge Activity: Sequence-based Optimization

**Advanced Challenge:**
Implement and compare the convergence properties of different optimization algorithms (Gradient Descent, Momentum, RMSProp, Adam) on a non-convex function.

**Steps:**

1. Define a non-convex function (e.g., $f(x) = x^4 - 4x^2 + x$).
2. Implement the four optimization algorithms.
3. Track the sequence of points and function values for each algorithm.
4. Analyze and visualize the convergence behavior.
5. Determine which algorithm converges fastest and most reliably.

**Doctoral-level Extension:**
Analyze the convergence guarantees of these optimization algorithms using the theory of dynamical systems. Investigate the relationship between the eigenvalues of the Hessian matrix at critical points and the convergence behavior of different optimizers. Develop a new adaptive step size algorithm that automatically adjusts based on the local geometry of the function.

## Conclusion

Sequences and series provide powerful mathematical tools for understanding and improving machine learning algorithms. By analyzing convergence properties, we can determine whether algorithms will reach optimal solutions and how quickly they will do so. From learning rate schedules to function approximations, these concepts appear throughout machine learning, making them essential knowledge for anyone working in the field.

## References

1. Rudin, W. (1976). Principles of Mathematical Analysis (3rd ed.). McGraw-Hill.
2. Kingma, D. P., & Ba, J. (2014). Adam: A Method for Stochastic Optimization. arXiv:1412.6980.
3. Bottou, L., Curtis, F. E., & Nocedal, J. (2018). Optimization Methods for Large-Scale Machine Learning. SIAM Review, 60(2), 223–311.
4. Smith, L. N. (2017). Cyclical Learning Rates for Training Neural Networks. 2017 IEEE Winter Conference on Applications of Computer Vision (WACV), 464–472.
