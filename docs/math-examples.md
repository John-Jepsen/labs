# Mathematical Notation Examples

This page demonstrates the advanced mathematical notation capabilities after the improvements.

## Quantum Computing Examples

### Quantum State Representation
The quantum state of a qubit can be written as:
$$\ket{\psi} = \alpha\ket{0} + \beta\ket{1}$$

Where $\alpha$ and $\beta$ are complex amplitudes satisfying $|\alpha|^2 + |\beta|^2 = 1$.

### Quantum Operations
The Hadamard gate creates superposition:
$$\op{H}\ket{0} = \frac{1}{\sqrt{2}}(\ket{0} + \ket{1})$$

Inner products can be calculated as:
$$\braket{\psi}{\phi} = \sum_i \alpha_i^* \beta_i$$

## Neural Network Mathematics

### Forward Pass
The forward pass computation:
$$\hat{y} = \vec{x}^{\transpose}\vec{w} + b$$

### Loss Function with Custom Macro
Using our custom `\loss` macro:
$$\loss(\theta) = \frac{1}{m}\sum_{i=1}^{m} \left(y^{(i)} - h_\theta(x^{(i)})\right)^2$$

### Gradients with Enhanced Notation
The gradient with respect to weights:
$$\pdv{\loss}{\vec{w}} = \frac{1}{m}\mat{X}^{\transpose}(\mat{X}\vec{w} - \vec{y})$$

## Advanced Calculus

### Multivariate Chain Rule
$$\pdv{f}{x} = \pdv{f}{u}\pdv{u}{x} + \pdv{f}{v}\pdv{v}{x}$$

### Vector Calculus
The gradient operator:
$$\gradient f = \begin{pmatrix} \pdv{f}{x_1} \\ \pdv{f}{x_2} \\ \vdots \\ \pdv{f}{x_n} \end{pmatrix}$$

## Probability and Statistics

### Expectation with Custom Notation
$$\E[X] = \sum_{x} x \cdot \Pr(X = x)$$

### Bayes' Theorem
$$\Pr(A \given B) = \frac{\Pr(B \given A) \Pr(A)}{\Pr(B)}$$

## Set Theory Examples

### Set Operations
Let $A, B \subset \R$. Then:
- Union: $A \cup B$
- Intersection: $A \cap B$  
- Complement: $A^c$
- Empty set: $\emptyset$

### Function Spaces
The space of continuous functions: $C([0,1], \R)$

## Complexity Theory

### Algorithm Analysis
- Time complexity: $\bigO{n \log n}$
- Space complexity: $\bigTheta{n}$
- Lower bound: $\bigOmega{n}$

## Mathematical Environments

<div class="theorem">
**Fundamental Theorem of Calculus**: If $f$ is continuous on $[a,b]$ and $F$ is an antiderivative of $f$, then:
$$\int_a^b f(x) \dd{x} = F(b) - F(a)$$
</div>

<div class="definition">
**Hilbert Space**: A complete inner product space $\mathcal{H}$ with inner product $\inner{\cdot}{\cdot}$ and induced norm $\norm{\cdot}$.
</div>

<div class="example">
**Example**: The space $L^2([0,1])$ of square-integrable functions is a Hilbert space with inner product:
$$\inner{f}{g} = \int_0^1 f(x)g(x) \dd{x}$$
</div>

## Interactive Features

Right-click any equation to copy its LaTeX source to clipboard! The system includes advanced features like:

- High-quality SVG rendering
- Responsive design for mobile devices
- Dark mode support  
- Copy-to-clipboard functionality
- Mathematical accessibility features
- Custom macros for common notation

## Advanced Typography

The mathematical notation now features:

1. **Enhanced spacing** around equations
2. **Consistent styling** with the site theme
3. **Improved readability** on all devices
4. **Professional typography** optimized for mathematical content
5. **Performance optimizations** for fast loading

Try interacting with the equations - the improvements make mathematical content more accessible and user-friendly than ever before!