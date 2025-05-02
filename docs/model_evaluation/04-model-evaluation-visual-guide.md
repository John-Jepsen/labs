# Model Evaluation Visual Guide and Checklist

## Visual Decision Tree for Choosing the Right Metric

```
Start
  |
  ├── Is your target variable continuous? (e.g., price, temperature)
  |   |
  |   ├── YES --> Regression Metrics
  |   |          |
  |   |          ├── Do you want a normalized score (0-1 scale)?
  |   |          |   |
  |   |          |   ├── YES --> R² Score
  |   |          |   |
  |   |          |   └── NO --> Continue
  |   |          |
  |   |          ├── Do you want to penalize large errors more?
  |   |          |   |
  |   |          |   ├── YES --> Mean Squared Error (MSE)
  |   |          |   |
  |   |          |   └── NO --> Continue
  |   |          |
  |   |          └── Do you want errors in same units as data?
  |   |              |
  |   |              └── YES --> Mean Absolute Error (MAE)
  |   |
  |   └── NO --> Classification Metrics
  |              |
  |              ├── Do you need a detailed breakdown of all prediction types?
  |              |   |
  |              |   └── YES --> Confusion Matrix
  |              |
  |              ├── Are false positives and false negatives equally important?
  |              |   |
  |              |   ├── YES --> Is the class distribution balanced?
  |              |   |   |
  |              |   |   ├── YES --> Accuracy Score
  |              |   |   |
  |              |   |   └── NO --> F1 Score
  |              |   |
  |              |   └── NO --> Continue
  |              |
  |              ├── Are false positives more costly?
  |              |   |
  |              |   └── YES --> Precision Score
  |              |
  |              └── Are false negatives more costly?
  |                  |
  |                  └── YES --> Recall Score
  |
End
```

## Quick Model Evaluation Checklist

### Before Implementing

- [ ] **Understand the problem type**:

  - [ ] Regression (continuous output)
  - [ ] Classification (categorical output)
  - [ ] Multi-output or single-output

- [ ] **Analyze input data**:

  - [ ] Format (CSV, arrays, DataFrames)
  - [ ] Shape and dimensions
  - [ ] Presence of non-numeric columns
  - [ ] Missing values

- [ ] **Choose appropriate metrics**:
  - [ ] For regression: R², MSE, MAE
  - [ ] For classification: Confusion Matrix, Accuracy, Precision, Recall, F1

### During Implementation

- [ ] **Input processing**:

  - [ ] Parse input data correctly
  - [ ] Remove non-numeric identifier columns
  - [ ] Handle missing values
  - [ ] Convert to appropriate types

- [ ] **Data alignment**:

  - [ ] Ensure true and predicted data have same shape
  - [ ] Handle index alignment (for DataFrames)
  - [ ] Check for column name consistency

- [ ] **Library and function usage**:

  - [ ] Import correct libraries (pandas, numpy, sklearn.metrics)
  - [ ] Use appropriate metric functions
  - [ ] Check parameter order (typically y_true, y_pred)

- [ ] **Edge case handling**:
  - [ ] Empty input
  - [ ] Different shapes
  - [ ] Constant true values
  - [ ] Perfect predictions
  - [ ] Missing values

### After Implementation

- [ ] **Verification**:

  - [ ] Test with provided examples
  - [ ] Manually calculate for simple cases
  - [ ] Verify metric ranges make sense

- [ ] **Interpretation**:
  - [ ] Understand what the metric values mean
  - [ ] Know which direction is better (higher or lower)
  - [ ] Consider how to communicate results

## ASCII Visualizations of Key Concepts

### Regression Metrics

**R² Score (1.0 = perfect, 0.0 = predicting mean, < 0 = worse than mean)**

```
  Perfect Fit (R² = 1.0)       Poor Fit (R² close to 0)      Bad Fit (R² < 0)

      y|                            y|                           y|
       |  *  *                       |     *                      |     *
       | *    *                      |  *     *                   |        *
       |*      *                     | *       *                  |*  *
       |        *                    |          *                 |     *
       |         *                   |*           *               |           *
  -----+-----------> x          -----+-------------> x       -----+-------------> x
       |                            |                           |
  Predicted line follows         Predicted line captures     Predicted line is worse
  actual points perfectly        overall trend but with      than just predicting
                                 error                       the mean value
```

**MSE vs MAE**

```
       MSE (penalizes large errors)         MAE (treats all errors equally)

       Error^2|                             |Error|
              |                   *         |                   *
              |                             |
              |                             |
              |                             |
              |                             |
              |         *                   |         *
              |                             |
        ------+-----------> Error     ------+-----------> Error
              |                             |

       Large errors are                All errors contribute
       amplified quadratically         proportionally to their magnitude
```

### Classification Metrics

**Confusion Matrix**

```
                   PREDICTED
            |    Positive  |  Negative  |
      ------+-------------+------------+
      Pos   |     TP      |     FN     |
  A   |     |  (Correct)  |  (Missed)  |
  C   +-----+-------------+------------+
  T   |     |     FP      |     TN     |
  U   Neg   |  (False     |  (Correct  |
  A   |     |   Alarm)    |  Rejection)|
  L   ------+-------------+------------+
```

**Precision, Recall, and F1 Score**

```
Precision = TP / (TP + FP)          Recall = TP / (TP + FN)
"Of all predicted positives,        "Of all actual positives,
 how many were correct?"             how many did we find?"

    PREDICTED                         PREDICTED
  P       N                         P       N
P|TP|    |FN|                     P|TP|    |FN|
 |--|----|----|                    |--|----|----|
A|  |    |  |                     A|  |    |  |
 |--|----|----|                    |--|----|----|
N|FP|    |TN|                     N|FP|    |TN|
 |--|----|----|                    |--|----|----|
  \________/                              \____/
 Focus area                              Focus area


F1 Score = 2 * (Precision * Recall) / (Precision + Recall)
"Harmonic mean of precision and recall"

            PREDICTED
          P       N
        P|TP|    |FN|
         |--|----|----|
        A|  |    |  |
         |--|----|----|
        N|FP|    |TN|
         |--|----|----|
          \________/
         Both areas are
         important
```

## Common Pitfalls Visualized

**Training vs. Testing Data Split**

```
        WRONG                           RIGHT
  +---------------+             +---------------+
  |               |             |               |
  |   Train and   |             |    Train      |  70%
  |   evaluate    |             |               |
  |   on same     |             +---------------+
  |   data        |             |               |
  |               |             |    Test       |  30%
  +---------------+             |               |
                                +---------------+

  Leads to overfitting           Proper evaluation
  and unrealistic                of model performance
  estimates
```

**Class Imbalance Effect on Accuracy**

```
Imbalanced Dataset (90% Class 0, 10% Class 1):

Model predicting always Class 0:
  +-------------------+
  |        90%        | Class 0 (correct predictions)
  +-------------------+
  | 10% | Class 1 (incorrect predictions)
  +-----+

→ 90% accuracy despite not learning anything!

For imbalanced datasets, prefer:
- Precision/Recall/F1
- Balanced accuracy
- Area under ROC curve
```

**Correlation vs. Causation**

```
High R² doesn't imply causation!

     Ice Cream Sales       vs.        Drowning Incidents
          /\                                /\
         /  \                              /  \
        /    \                            /    \
       /      \                          /      \
  ----+--------+--------           ----+--------+--------
      Jan      Jul     Dec             Jan      Jul     Dec

Both correlate with temperature, but one doesn't cause the other.
```

## One-Page Summary Table

| Metric               | Formula                      | Range           | Use When                                | Watch Out For                  |
| -------------------- | ---------------------------- | --------------- | --------------------------------------- | ------------------------------ | ----------------------------- | -------------------------- |
| **R² Score**         | 1 - (SS_residual / SS_total) | (-∞, 1]         | Need normalized metric for regression   | Constant true values           |
| **MSE**              | Mean((y_true - y_pred)²)     | [0, ∞)          | Want to penalize large errors           | Scale dependence               |
| **MAE**              | Mean(                        | y_true - y_pred | )                                       | [0, ∞)                         | Want errors in original units | Not differentiable at zero |
| **Confusion Matrix** | Count of TP, FP, TN, FN      | N/A             | Need detailed prediction breakdown      | Needs interpretation           |
| **Accuracy**         | (TP + TN) / Total            | [0, 1]          | Classes are balanced                    | Misleading for imbalanced data |
| **Precision**        | TP / (TP + FP)               | [0, 1]          | False positives are costly              | Ignores false negatives        |
| **Recall**           | TP / (TP + FN)               | [0, 1]          | False negatives are costly              | Ignores false positives        |
| **F1 Score**         | 2 _ (P _ R) / (P + R)        | [0, 1]          | Need balance between precision & recall | Ignores true negatives         |
