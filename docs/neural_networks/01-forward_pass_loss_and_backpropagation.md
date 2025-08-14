# Foundations of Neural Network Training: Forward Pass, Loss, and Backpropagation

---

## Learning Objectives

- Understand the core components of a simple neural network: **inputs, weights, outputs**.
- Perform a **forward pass** to compute model predictions.
- Calculate **Mean Squared Error (MSE)** as a loss function.
- Derive and interpret **gradients using backpropagation**.
- Apply the **chain rule** to compute weight updates.
- Understand the role of **gradients in model optimization**.
- Recognize when to apply **early stopping** during training.
- Analyze the impact of **gradient size (steep vs. flat)** on learning speed and stability.

---

## 1. Forward Pass

- Formula:
  $$
  \hat{y} = x \times w
  $$
- Purpose:
  - Happens during both training and inference.
  - Calculates the predicted output ($\hat{y}$) using input $x$ and weight $w$.
  - Often uses Euclidean distance for error calculation.

---

## 2. Loss Calculation

- Loss Function (Mean Squared Error - MSE):
  $$
  \loss_{\text{MSE}} = \frac{1}{n} \sum_{i=1}^{n} (y_i - \hat{y}_i)^2 \label{eq:mse}
  $$
- Purpose:
  - Measures the average squared difference between the true output $y$ and predicted output $\hat{y}$.

---

## 3. Backward Pass (Gradient Calculation)

- Objective: Calculate $\pdv{\loss}{w}$ to update weights.

### Step-by-Step:

1. Compute derivative of loss with respect to prediction:

   $$
   \pdv{\loss}{\hat{y}} = -2(y - \hat{y}) \label{eq:loss-grad}
   $$

2. Compute derivative of prediction with respect to weight:

   $$
   \pdv{\hat{y}}{w} = x \label{eq:pred-grad}
   $$

3. Chain Rule Application:
   $$
   \pdv{\loss}{w} = \pdv{\loss}{\hat{y}} \times \pdv{\hat{y}}{w} = -2(y - \hat{y}) \cdot x \label{eq:chain-rule}
   $$

- Interpretation:
  - $\pdv{\loss}{w}$ tells us how to adjust $w$ to minimize the loss.
  - This is used in gradient descent updates.

---

## 4. Optimization Insight

- **Learning Rate** affects how fast weights change.
- **Early Stopping** is applied when answers are "good enough" to avoid overfitting.
- Steep vs. Flat Gradients:
  - Sharp gradients lead to fast convergence but risk instability.
  - Flat gradients slow learning but improve stability.

---

# Worked Example (By Hand)

---

### Initial Parameters

- Input ($x$): 2
- Weight ($w$): 3
- True Output ($y$): 10
- Learning Rate ($\eta$): 0.1

---

## First Iteration

1. **Forward Pass**

   $$
   \hat{y} = x \times w = 2 \times 3 = 6
   $$

   Predicted Output: $\hat{y} = 6$

2. **Loss Calculation**

   $$
   \text{MSE} = (y - \hat{y})^2 = (10 - 6)^2 = 16
   $$

   Loss: **16**

3. **Backward Pass**

   $$
   \frac{\partial L}{\partial \hat{y}} = -2(10 - 6) = -8
   $$

   $$
   \frac{\partial \hat{y}}{\partial w} = 2
   $$

   $$
   \frac{\partial L}{\partial w} = -8 \times 2 = -16
   $$

   Gradient: **-16**

4. **Weight Update**
   $$
   w_{\text{new}} = 3 - 0.1 \times (-16) = 3 + 1.6 = 4.6
   $$
   New Weight: **4.6**

---

## Second Iteration

1. **Forward Pass**

   $$
   \hat{y} = 2 \times 4.6 = 9.2
   $$

   Predicted Output: $\hat{y} = 9.2$

2. **Loss Calculation**

   $$
   \text{MSE} = (10 - 9.2)^2 = 0.8^2 = 0.64
   $$

   New Loss: **0.64**

3. **Backward Pass**

   $$
   \frac{\partial L}{\partial \hat{y}} = -2(10 - 9.2) = -1.6
   $$

   $$
   \frac{\partial \hat{y}}{\partial w} = 2
   $$

   $$
   \frac{\partial L}{\partial w} = -1.6 \times 2 = -3.2
   $$

   Gradient: **-3.2**

4. **Weight Update**
   $$
   w_{\text{new}} = 4.6 - 0.1 \times (-3.2) = 4.6 + 0.32 = 4.92
   $$
   New Weight: **4.92**

---

### Observation

- Loss reduced from **16** to **0.64** in just two iterations.
- Weight moved from **3** to **4.92** quickly toward the optimal value.

---
