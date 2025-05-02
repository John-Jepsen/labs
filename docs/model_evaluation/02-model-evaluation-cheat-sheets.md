# Model Evaluation Metrics: Quick Reference Cheat Sheets

## Regression Metrics Cheat Sheet

### R² Score (Coefficient of Determination)

**What it measures**: Proportion of the variance in the dependent variable that is predictable from the independent variable(s).

**Formula**: R² = 1 - (SS_residual / SS_total)

- SS_residual: Sum of squares of residuals = Σ(y_true - y_pred)²
- SS_total: Total sum of squares = Σ(y_true - mean(y_true))²

**Range**:

- 1.0: Perfect prediction
- 0.0: Model predicts the mean of the data
- < 0.0: Worse than predicting the mean

**Code**:

```python
from sklearn.metrics import r2_score

# For single output
score = r2_score(y_true, y_pred)

# For multi-output
score = r2_score(y_true, y_pred, multioutput='variance_weighted')
```

**Use when**:

- You want to know how well your model explains the variance in your data
- You want a normalized metric that's comparable across different models

**Limitations**:

- Can be negative if model is worse than predicting the mean
- Not reliable if data violates regression assumptions
- Can be misleadingly high with many features

---

### Mean Squared Error (MSE)

**What it measures**: Average of squared differences between predicted and actual values.

**Formula**: MSE = (1/n) \* Σ(y_true - y_pred)²

**Range**:

- 0: Perfect prediction
- > 0: Higher values indicate worse predictions

**Code**:

```python
from sklearn.metrics import mean_squared_error

# Basic usage
mse = mean_squared_error(y_true, y_pred)

# With square root to get RMSE
rmse = mean_squared_error(y_true, y_pred, squared=False)
```

**Use when**:

- You want to penalize larger errors more severely
- Your data is on a similar scale or has been normalized
- Outliers are particularly important to detect

**Limitations**:

- Scale dependent (not normalized)
- Gives disproportionate weight to outliers
- Not easily interpretable in terms of original units

---

### Mean Absolute Error (MAE)

**What it measures**: Average of absolute differences between predicted and actual values.

**Formula**: MAE = (1/n) \* Σ|y_true - y_pred|

**Range**:

- 0: Perfect prediction
- > 0: Higher values indicate worse predictions

**Code**:

```python
from sklearn.metrics import mean_absolute_error

mae = mean_absolute_error(y_true, y_pred)
```

**Use when**:

- You want errors to be measured in the same units as the data
- You want all error magnitudes to be treated equally
- Data contains outliers that shouldn't be given extra weight

**Limitations**:

- Scale dependent (not normalized)
- Doesn't penalize large errors as severely as MSE
- Not differentiable at zero (can cause issues in some optimization algorithms)

---

## Classification Metrics Cheat Sheet

### Confusion Matrix

**What it measures**: Counts of true positives (TP), false positives (FP), true negatives (TN), and false negatives (FN).

**Visualization**:

```
                 Predicted
              Pos       Neg
Actual  Pos | TP    |  FN  |
        Neg | FP    |  TN  |
```

**Code**:

```python
from sklearn.metrics import confusion_matrix

cm = confusion_matrix(y_true, y_pred)
```

**Use when**:

- You want to see a detailed breakdown of prediction results
- You need a foundation for calculating other metrics
- Different types of errors have different costs

**Interpretation**:

- TP: Correctly identified positive cases
- TN: Correctly identified negative cases
- FP: Negative cases incorrectly identified as positive
- FN: Positive cases incorrectly identified as negative

---

### Accuracy Score

**What it measures**: Proportion of correct predictions (both positive and negative).

**Formula**: Accuracy = (TP + TN) / (TP + TN + FP + FN)

**Range**:

- 1.0: Perfect prediction
- 0.0: All predictions wrong

**Code**:

```python
from sklearn.metrics import accuracy_score

accuracy = accuracy_score(y_true, y_pred)
```

**Use when**:

- Classes are balanced
- False positives and false negatives have similar costs
- You need a simple, interpretable metric

**Limitations**:

- Misleading for imbalanced classes
- Doesn't distinguish between different types of errors

---

### Precision Score

**What it measures**: Proportion of true positives among positive predictions.

**Formula**: Precision = TP / (TP + FP)

**Range**:

- 1.0: No false positives
- 0.0: All positive predictions are wrong

**Code**:

```python
from sklearn.metrics import precision_score

# Binary classification
precision = precision_score(y_true, y_pred)

# Multi-class
precision = precision_score(y_true, y_pred, average='weighted')
```

**Use when**:

- False positives are costly
- You care more about the quality of positive predictions
- You want to answer "Of the items labeled as positive, how many are actually positive?"

**Limitations**:

- Doesn't account for true negatives
- Can be high even with many false negatives

---

### Recall Score

**What it measures**: Proportion of true positives among actual positives.

**Formula**: Recall = TP / (TP + FN)

**Range**:

- 1.0: All positives correctly identified
- 0.0: No positives correctly identified

**Code**:

```python
from sklearn.metrics import recall_score

# Binary classification
recall = recall_score(y_true, y_pred)

# Multi-class
recall = recall_score(y_true, y_pred, average='weighted')
```

**Use when**:

- False negatives are costly
- You need to identify as many positives as possible
- You want to answer "Of all the positive items, how many were correctly identified?"

**Limitations**:

- Doesn't account for false positives
- Can be high even with many false positives

---

### F1 Score

**What it measures**: Harmonic mean of precision and recall.

**Formula**: F1 = 2 _ (precision _ recall) / (precision + recall)

**Range**:

- 1.0: Perfect precision and recall
- 0.0: Either precision or recall is zero

**Code**:

```python
from sklearn.metrics import f1_score

# Binary classification
f1 = f1_score(y_true, y_pred)

# Multi-class
f1 = f1_score(y_true, y_pred, average='weighted')
```

**Use when**:

- You need a balance between precision and recall
- Both false positives and false negatives are important
- Classes are imbalanced

**Limitations**:

- Doesn't account for true negatives
- Gives equal weight to precision and recall

---

## Data Processing Cheat Sheet

### CSV String to DataFrame

```python
import pandas as pd
from io import StringIO

# Parse CSV string into DataFrame
df = pd.read_csv(StringIO(csv_string))

# Remove non-numeric identifier column
if 'id_column' in df.columns:
    df = df.drop('id_column', axis='columns')

# Check and handle missing values
missing_count = df.isnull().sum().sum()
if missing_count > 0:
    df = df.fillna(0)  # or other strategy

# Ensure numeric data types
for col in df.columns:
    df[col] = pd.to_numeric(df[col], errors='coerce')
```

### Data Alignment

```python
# Ensure same shape for comparison
min_rows = min(df_true.shape[0], df_pred.shape[0])
df_true = df_true.iloc[:min_rows]
df_pred = df_pred.iloc[:min_rows]

# For column alignment (if needed)
common_cols = list(set(df_true.columns) & set(df_pred.columns))
df_true = df_true[common_cols]
df_pred = df_pred[common_cols]
```

### Converting Between Types

```python
# DataFrame to numpy array
array = df.values

# List to numpy array
array = np.array(my_list)

# Flatten multi-dimensional array
flat_array = array.flatten()

# Convert string labels to integers
from sklearn.preprocessing import LabelEncoder
encoder = LabelEncoder()
int_labels = encoder.fit_transform(string_labels)
```

---

## Problem-Specific Cheat Sheet Templates

### Template for R² Score Problems

```python
import pandas as pd
from io import StringIO
from sklearn.metrics import r2_score

def evaluate_r2(true_csv, pred_csv):
    # Parse input
    df_true = pd.read_csv(StringIO(true_csv))
    df_pred = pd.read_csv(StringIO(pred_csv))

    # Remove non-numeric columns
    if 'id_column' in df_true.columns:
        df_true = df_true.drop('id_column', axis='columns')
    if 'id_column' in df_pred.columns:
        df_pred = df_pred.drop('id_column', axis='columns')

    # Ensure same shape
    min_rows = min(df_true.shape[0], df_pred.shape[0])
    df_true = df_true.iloc[:min_rows]
    df_pred = df_pred.iloc[:min_rows]

    # Calculate R² score
    return r2_score(df_true, df_pred)
```

### Template for Mean Squared Error Problems

```python
import pandas as pd
from io import StringIO
from sklearn.metrics import mean_squared_error

def evaluate_mse(true_csv, pred_csv):
    # Parse input
    df_true = pd.read_csv(StringIO(true_csv))
    df_pred = pd.read_csv(StringIO(pred_csv))

    # Remove non-numeric columns
    if 'id_column' in df_true.columns:
        df_true = df_true.drop('id_column', axis='columns')
    if 'id_column' in df_pred.columns:
        df_pred = df_pred.drop('id_column', axis='columns')

    # Ensure same shape
    min_rows = min(df_true.shape[0], df_pred.shape[0])
    df_true = df_true.iloc[:min_rows]
    df_pred = df_pred.iloc[:min_rows]

    # Calculate MSE
    return mean_squared_error(df_true, df_pred)
```

### Template for Confusion Matrix Problems

```python
import numpy as np
from sklearn.metrics import confusion_matrix

def generate_cm(y_true, y_pred):
    # Convert inputs to numpy arrays if needed
    y_true = np.array(y_true)
    y_pred = np.array(y_pred)

    # Generate confusion matrix
    return confusion_matrix(y_true, y_pred)
```
