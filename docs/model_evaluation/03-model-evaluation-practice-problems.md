# Model Evaluation Practice Problems

Test your understanding of model evaluation concepts with these practice problems.

## Classification Problems

### Problem 1: Confusion Matrix Analysis

Given the following confusion matrix for a binary classification model:

|                   | Predicted Positive | Predicted Negative |
| ----------------- | ------------------ | ------------------ |
| Actually Positive | 85                 | 15                 |
| Actually Negative | 20                 | 80                 |

Calculate:

1. Accuracy
2. Precision
3. Recall
4. F1 Score

### Problem 2: ROC Curve Interpretation

You have trained two models (A and B) on the same dataset. The ROC curves yield the following AUC values:

- Model A: AUC = 0.75
- Model B: AUC = 0.92

Which model performs better and why? In what scenarios might you still choose the apparently worse-performing model?

## Regression Problems

### Problem 3: Regression Metrics

A regression model for house price prediction gives the following metrics:

- MAE: $45,000
- RMSE: $65,000
- R²: 0.82

Interpret these metrics and explain what they tell you about the model's performance.

### Problem 4: Validation Strategy

You are building a time series model to predict stock prices. Explain why a simple random train-test split might be problematic. Design an appropriate validation strategy for this scenario.

## Python Implementation

### Problem 5: Cross-Validation Code

Write Python code using scikit-learn to perform stratified 5-fold cross-validation on a dataset with imbalanced classes. Include the calculation of precision, recall, and F1 score for each fold.
