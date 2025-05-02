# Model Evaluation Visual Guide

Visual explanations of key model evaluation concepts.

## Confusion Matrix Visualization

A confusion matrix is a table that visualizes the performance of a classification model:

```
                  Predicted
                  | Positive | Negative |
Actual  Positive  |    TP    |    FN    |
        Negative  |    FP    |    TN    |
```

Where:

- **TP (True Positive)**: Correctly predicted positive cases
- **TN (True Negative)**: Correctly predicted negative cases
- **FP (False Positive)**: Incorrectly predicted positive cases (Type I error)
- **FN (False Negative)**: Incorrectly predicted negative cases (Type II error)

## ROC Curve Visualization

The Receiver Operating Characteristic (ROC) curve plots the True Positive Rate against the False Positive Rate at various threshold settings:

```
    1 |       ._____
      |      /
      |     /
TPR   |    /
      |   /
      |  /
      | /
    0 +------------
      0    FPR     1
```

A perfect classifier would have a curve that goes straight up the y-axis and then across the top, with an AUC of 1.0.

## Regression Error Visualization

Visualizing regression errors helps understand model performance:

```
    y |    o    o
      |   /
      |  o    o
      | /   o
      |/o    o
      +----------
            x

      o: Actual data points
      /: Perfect prediction line
      Distance from point to line: Error
```

## Cross-Validation Visualization

K-fold cross-validation divides the data into k subsets:

```
Fold 1: [Test] [Train] [Train] [Train] [Train]
Fold 2: [Train] [Test] [Train] [Train] [Train]
Fold 3: [Train] [Train] [Test] [Train] [Train]
Fold 4: [Train] [Train] [Train] [Test] [Train]
Fold 5: [Train] [Train] [Train] [Train] [Test]
```

Each subset serves as a test set once while the remaining subsets form the training set.
