# Model Evaluation Study Guide

This study guide provides an overview of key concepts and techniques in model evaluation.

## Metrics for Classification

- **Accuracy**: Proportion of correct predictions among the total number of predictions
- **Precision**: Proportion of true positives among all positive predictions
- **Recall**: Proportion of true positives among all actual positives
- **F1 Score**: Harmonic mean of precision and recall
- **ROC Curve**: Plot of true positive rate vs. false positive rate
- **AUC**: Area under the ROC curve

## Metrics for Regression

- **Mean Absolute Error (MAE)**: Average absolute difference between predicted and actual values
- **Mean Squared Error (MSE)**: Average squared difference between predicted and actual values
- **Root Mean Squared Error (RMSE)**: Square root of MSE
- **R-squared**: Proportion of variance explained by the model
- **Adjusted R-squared**: R-squared adjusted for the number of predictors

## Validation Techniques

- **Train-Test Split**: Splitting data into training and testing sets
- **Cross-Validation**: K-fold cross-validation, leave-one-out cross-validation
- **Stratified Sampling**: Maintaining class distribution in splits
- **Time Series Validation**: For time-dependent data
