# Advanced Mathematical Notation Guide

## Overview

This documentation site now features state-of-the-art mathematical notation rendering with enhanced typography, performance, and user experience. The system uses advanced MathJax 3.x configuration with custom macros, improved styling, and accessibility features.

## Key Features

### 🚀 **Performance Optimized**
- SVG output for crisp rendering at all zoom levels
- Optimized loading and caching
- Minimal reflow for smooth scrolling

### 🎨 **Enhanced Typography**
- Custom mathematical styling with Material Design integration
- Responsive design for all screen sizes
- Dark mode support
- Print-optimized rendering

### ♿ **Accessibility**
- Screen reader support
- Keyboard navigation
- High contrast mode compatibility
- Semantic mathematical markup

### 🛠 **Advanced Features**
- Copy-to-clipboard functionality (right-click equations)
- Custom mathematical macros for common notation
- Equation numbering and referencing
- Interactive context menus

## Mathematical Macros

The system includes over 50 predefined macros for common mathematical notation:

### Quantum Computing
```latex
\ket{0}           → |0⟩ (quantum state)
\bra{0}           → ⟨0| (bra notation)
\braket{0}{1}     → ⟨0|1⟩ (inner product)
\op{H}            → Ĥ (quantum operator)
\norm{v}          → ‖v‖ (vector norm)
```

### Linear Algebra
```latex
\mat{A}           → **A** (matrix)
\vec{v}           → **v** (vector)
\transpose        → ᵀ (transpose)
\hermitian        → † (Hermitian conjugate)
\trace            → tr (trace)
```

### Calculus
```latex
\dd{x}            → dx (differential)
\dv{f}{x}         → df/dx (derivative)
\pdv{f}{x}        → ∂f/∂x (partial derivative)
\gradient         → ∇ (gradient)
\laplacian        → ∇² (Laplacian)
```

### Machine Learning
```latex
\argmin           → argmin
\argmax           → argmax
\loss             → ℒ (loss function)
\sigmoid          → σ (sigmoid)
\relu             → ReLU
```

### Set Theory
```latex
\R                → ℝ (real numbers)
\C                → ℂ (complex numbers)
\N                → ℕ (natural numbers)
\emptyset         → ∅ (empty set)
```

### Complexity Theory
```latex
\bigO{n}          → O(n) (big O notation)
\bigTheta{n}      → Θ(n) (big theta)
\bigOmega{n}      → Ω(n) (big omega)
```

## Usage Examples

### Basic Equations

**Inline math**: Use single dollar signs for inline equations like $E = mc^2$.

**Display math**: Use double dollar signs for centered equations:

$$
\ket{\psi} = \alpha\ket{0} + \beta\ket{1}
$$

### Advanced Examples

**Quantum State Evolution**:
$$
\ket{\psi(t)} = e^{-i\op{H}t/\hbar}\ket{\psi(0)}
$$

**Machine Learning Loss Function**:
$$
\loss(\theta) = \frac{1}{m}\sum_{i=1}^{m} \left(h_\theta(x^{(i)}) - y^{(i)}\right)^2
$$

**Matrix Operations**:
$$
\mat{A}\vec{x} = \lambda\vec{x}
$$

**Calculus**:
$$
\gradient f = \begin{pmatrix} \pdv{f}{x_1} \\ \pdv{f}{x_2} \\ \vdots \\ \pdv{f}{x_n} \end{pmatrix}
$$

## Mathematical Environments

Use custom CSS classes for theorem-like environments:

```html
<div class="theorem">
The quantum measurement postulate states that...
</div>

<div class="definition">
A Hilbert space is a complete inner product space...
</div>

<div class="proof">
We proceed by mathematical induction...
</div>
```

Available environments:
- `.theorem` - Green border, for theorems
- `.definition` - Green border, for definitions  
- `.lemma` - Green border, for lemmas
- `.proposition` - Green border, for propositions
- `.corollary` - Green border, for corollaries
- `.proof` - Yellow border, for proofs
- `.example` - Blue border, for examples
- `.note` - Purple border, for notes
- `.remark` - Purple border, for remarks

## Equation Numbering

Enable equation numbering by using the `align` environment:

```latex
\begin{align}
x &= a + b \label{eq:simple} \\
y &= c + d \label{eq:another}
\end{align}
```

Reference equations using `\eqref{eq:simple}` or create links like `[Equation 1](#mjx-eqn-1)`.

## Best Practices

### Performance
1. **Use macros** for repetitive notation
2. **Avoid overly complex** single equations
3. **Break up long expressions** across multiple lines
4. **Use appropriate environments** for mathematical content

### Accessibility
1. **Provide alt text** for complex diagrams
2. **Use semantic markup** in mathematical environments
3. **Ensure sufficient contrast** in custom styling
4. **Test with screen readers** when possible

### Consistency
1. **Use consistent notation** throughout documents
2. **Define variables** before using them
3. **Use standard mathematical conventions**
4. **Document custom notation** when necessary

## Interactive Features

### Copy to Clipboard
Right-click any equation to copy its LaTeX source to clipboard.

### Zoom and Pan
Double-click equations to zoom. Use the context menu for additional options.

### Responsive Design
Equations automatically scale and reflow on mobile devices.

## Troubleshooting

### Common Issues

**Equations not rendering**:
- Check that dollar signs are properly escaped
- Verify macro syntax
- Look for unmatched braces

**Slow performance**:
- Reduce complex nested expressions
- Use macros instead of repetitive notation
- Check for memory leaks in custom JavaScript

**Mobile display problems**:
- Test equation width on various screen sizes
- Use line breaks for very long expressions
- Verify touch interaction works properly

### Browser Support

The advanced mathematical notation system supports:
- Chrome 80+
- Firefox 75+
- Safari 13+
- Edge 80+

### Getting Help

For mathematical notation issues:
1. Check the browser console for errors
2. Verify MathJax configuration is loaded
3. Test with simplified expressions
4. Review the mathematical environments guide

## Migration from Basic Setup

If upgrading from basic MathJax:

1. **Update macro usage**: Replace custom notation with provided macros
2. **Review environments**: Convert ad-hoc styling to semantic environments
3. **Test responsiveness**: Verify mobile display quality
4. **Update references**: Use new equation referencing system
5. **Optimize performance**: Replace complex expressions with macros

## Future Enhancements

Planned improvements include:
- Interactive equation editor
- Mathematical search functionality
- Enhanced accessibility features
- Advanced plotting integration
- Collaborative editing support

For the latest updates and features, check the repository documentation.