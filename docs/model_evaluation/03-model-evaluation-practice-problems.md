# Model Evaluation Practice Problems

This document contains practice problems to test your understanding of model evaluation metrics and data processing techniques. Each problem includes a detailed explanation of the solution.

## Practice Problem 1: R² Score Calculation

**Problem**:

You are given the following actual and predicted values:

```
Actual: [3, -0.5, 2, 7, 1.2]
Predicted: [2.5, 0.0, 2, 8, 1.5]
```

Calculate the R² score manually and verify your result using scikit-learn's implementation.

**Solution**:

Step 1: Calculate the mean of the actual values.

```python
import numpy as np
y_true = np.array([3, -0.5, 2, 7, 1.2])
y_pred = np.array([2.5, 0.0, 2, 8, 1.5])

mean_true = np.mean(y_true)  # = 2.54
```

Step 2: Calculate the total sum of squares (SS_total).

```python
ss_total = sum((y_true - mean_true) ** 2)  # = 36.052
```

Step 3: Calculate the sum of squares of residuals (SS_residual).

```python
ss_residual = sum((y_true - y_pred) ** 2)  # = 1.61
```

Step 4: Calculate the R² score.

```python
r2_manual = 1 - (ss_residual / ss_total)  # = 0.9553
```

Step 5: Verify with scikit-learn.

```python
from sklearn.metrics import r2_score
r2_sklearn = r2_score(y_true, y_pred)  # Should match our manual calculation
print(f"Manual R²: {r2_manual}, Sklearn R²: {r2_sklearn}")
```

**Explanation**:

The R² score measures how well the predictions match the actual values. A score of 0.9553 means that the model explains about 95.53% of the variance in the data. This is a very good fit. The score can range from 1.0 (perfect prediction) to negative values (worse than predicting the mean).

## Practice Problem 2: Processing CSV Input Data

**Problem**:

You are given the following CSV string representing true values:

```
id,feature1,feature2,feature3
A,10,20,30
B,15,25,35
C,12,22,32
D,18,28,38
```

And another CSV string representing predicted values:

```
id,feature1,feature2,feature3
A,11,19,31
B,16,26,34
C,13,21,33
E,19,29,39
```

Write a function to process these inputs and prepare them for calculating Mean Squared Error.

**Solution**:

```python
import pandas as pd
from io import StringIO
from sklearn.metrics import mean_squared_error

def prepare_data_for_mse(true_csv, pred_csv):
    # Parse CSV strings into DataFrames
    df_true = pd.read_csv(StringIO(true_csv))
    df_pred = pd.read_csv(StringIO(pred_csv))

    # Store original shapes
    print(f"Original shapes - True: {df_true.shape}, Pred: {df_pred.shape}")

    # Set id column as index for easier alignment
    df_true.set_index('id', inplace=True)
    df_pred.set_index('id', inplace=True)

    # Find common indices
    common_indices = df_true.index.intersection(df_pred.index)

    # Filter both DataFrames to only include common indices
    df_true = df_true.loc[common_indices]
    df_pred = df_pred.loc[common_indices]

    print(f"Aligned shapes - True: {df_true.shape}, Pred: {df_pred.shape}")
    print(f"Common indices: {common_indices.tolist()}")

    return df_true, df_pred

# Test the function
true_csv = """id,feature1,feature2,feature3
A,10,20,30
B,15,25,35
C,12,22,32
D,18,28,38"""

pred_csv = """id,feature1,feature2,feature3
A,11,19,31
B,16,26,34
C,13,21,33
E,19,29,39"""

df_true, df_pred = prepare_data_for_mse(true_csv, pred_csv)

# Calculate MSE
mse = mean_squared_error(df_true, df_pred)
print(f"Mean Squared Error: {mse}")
```

**Explanation**:

This function does the following:

1. Parses the CSV strings into pandas DataFrames.
2. Sets the 'id' column as the index for both DataFrames.
3. Finds the common indices between the true and predicted data.
4. Filters both DataFrames to only include rows with common indices.
5. Returns the aligned DataFrames ready for MSE calculation.

The result shows that the common indices are A, B, and C. Row D is only in the true data, and row E is only in the predicted data, so both are excluded from the final calculation.

## Practice Problem 3: Handling Multi-Dimensional Data for MAE

**Problem**:

You are given two DataFrames with multiple columns representing true and predicted values. Calculate the Mean Absolute Error:

```python
# True values
true_df = pd.DataFrame({
    'sensor1': [100, 102, 98, 103, 99],
    'sensor2': [540, 535, 545, 530, 550],
    'sensor3': [350, 348, 352, 351, 349]
})

# Predicted values
pred_df = pd.DataFrame({
    'sensor1': [101, 103, 97, 104, 100],
    'sensor2': [538, 533, 547, 529, 552],
    'sensor3': [349, 350, 351, 352, 348]
})
```

**Solution**:

```python
import pandas as pd
import numpy as np
from sklearn.metrics import mean_absolute_error

# True values
true_df = pd.DataFrame({
    'sensor1': [100, 102, 98, 103, 99],
    'sensor2': [540, 535, 545, 530, 550],
    'sensor3': [350, 348, 352, 351, 349]
})

# Predicted values
pred_df = pd.DataFrame({
    'sensor1': [101, 103, 97, 104, 100],
    'sensor2': [538, 533, 547, 529, 552],
    'sensor3': [349, 350, 351, 352, 348]
})

# Method 1: Calculate MAE for each column separately
mae_by_column = {}
for column in true_df.columns:
    mae_by_column[column] = mean_absolute_error(
        true_df[column], pred_df[column]
    )

print("MAE by column:")
for col, mae in mae_by_column.items():
    print(f"{col}: {mae}")

# Method 2: Calculate overall MAE
overall_mae = mean_absolute_error(true_df.values, pred_df.values)
print(f"Overall MAE: {overall_mae}")

# Method 3: Manual calculation for verification
def manual_mae(y_true, y_pred):
    return np.mean(np.abs(y_true - y_pred))

manual_overall_mae = manual_mae(true_df.values, pred_df.values)
print(f"Manual overall MAE: {manual_overall_mae}")
```

**Explanation**:

This solution demonstrates three approaches:

1. Calculating MAE for each sensor column separately, which is useful when different sensors have different scales or importance.
2. Calculating an overall MAE across all values, treating each entry equally regardless of which sensor it came from.
3. Implementing a manual calculation to verify the scikit-learn result.

The MAE values by column would show which sensors have the largest prediction errors. The overall MAE gives a single metric for the entire prediction set.

## Practice Problem 4: Confusion Matrix and Derived Metrics

**Problem**:

You have the following actual and predicted class labels for a binary classification problem:

```python
y_true = [1, 0, 1, 1, 0, 1, 0, 0, 1, 1]
y_pred = [1, 0, 0, 1, 0, 1, 1, 0, 1, 0]
```

1. Generate the confusion matrix.
2. Calculate accuracy, precision, recall, and F1 score.
3. Explain what each metric tells you about the model's performance.

**Solution**:

```python
import numpy as np
from sklearn.metrics import confusion_matrix, accuracy_score, precision_score, recall_score, f1_score

y_true = np.array([1, 0, 1, 1, 0, 1, 0, 0, 1, 1])
y_pred = np.array([1, 0, 0, 1, 0, 1, 1, 0, 1, 0])

# Generate confusion matrix
cm = confusion_matrix(y_true, y_pred)
print("Confusion Matrix:")
print(cm)

# Extract values from confusion matrix
tn, fp, fn, tp = cm.ravel()
print(f"True Negatives: {tn}")
print(f"False Positives: {fp}")
print(f"False Negatives: {fn}")
print(f"True Positives: {tp}")

# Calculate metrics
accuracy = accuracy_score(y_true, y_pred)
precision = precision_score(y_true, y_pred)
recall = recall_score(y_true, y_pred)
f1 = f1_score(y_true, y_pred)

print(f"Accuracy: {accuracy:.4f}")
print(f"Precision: {precision:.4f}")
print(f"Recall: {recall:.4f}")
print(f"F1 Score: {f1:.4f}")

# Manual calculations for verification
manual_accuracy = (tp + tn) / (tp + tn + fp + fn)
manual_precision = tp / (tp + fp)
manual_recall = tp / (tp + fn)
manual_f1 = 2 * (manual_precision * manual_recall) / (manual_precision + manual_recall)

print("\nManual Calculations:")
print(f"Accuracy: {manual_accuracy:.4f}")
print(f"Precision: {manual_precision:.4f}")
print(f"Recall: {manual_recall:.4f}")
print(f"F1 Score: {manual_f1:.4f}")
```

**Explanation**:

The confusion matrix shows:

- True Negatives (TN): Number of class 0 instances correctly predicted as class 0
- False Positives (FP): Number of class 0 instances incorrectly predicted as class 1
- False Negatives (FN): Number of class 1 instances incorrectly predicted as class 0
- True Positives (TP): Number of class 1 instances correctly predicted as class 1

From these values:

- Accuracy measures overall correctness: (TP + TN) / Total
- Precision measures quality of positive predictions: TP / (TP + FP)
- Recall measures ability to find all positives: TP / (TP + FN)
- F1 Score is the harmonic mean of precision and recall, balancing both concerns

In this example:

- Accuracy tells us what fraction of predictions were correct.
- Precision tells us what fraction of predicted positives were actually positive.
- Recall tells us what fraction of actual positives were correctly predicted.
- F1 Score provides a balance between precision and recall.

## Practice Problem 5: Handling Edge Cases

**Problem**:

How would you handle the following edge cases when calculating model evaluation metrics?

1. Empty input data
2. Inputs with different shapes
3. All true values are the same (constant)
4. Perfect predictions (true = predicted)
5. Missing values in the data

**Solution**:

```python
import numpy as np
import pandas as pd
from io import StringIO
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error

def robust_model_evaluation(true_data, pred_data):
    """
    Robustly evaluate model performance handling various edge cases.

    Parameters:
    true_data: DataFrame or array-like of true values
    pred_data: DataFrame or array-like of predicted values

    Returns:
    dict: Dictionary of evaluation metrics
    """
    results = {}

    # Case 1: Check for empty input
    if len(true_data) == 0 or len(pred_data) == 0:
        print("Warning: Empty input data detected")
        return {
            'r2_score': None,
            'mse': None,
            'mae': None,
            'error': 'Empty input data'
        }

    # Case 2: Handle different shapes
    if hasattr(true_data, 'shape') and hasattr(pred_data, 'shape'):
        if true_data.shape != pred_data.shape:
            print(f"Warning: Shape mismatch - True: {true_data.shape}, Pred: {pred_data.shape}")

            # Find minimum dimensions
            if isinstance(true_data, pd.DataFrame) and isinstance(pred_data, pd.DataFrame):
                # For DataFrames
                common_cols = list(set(true_data.columns) & set(pred_data.columns))
                if not common_cols:
                    return {
                        'r2_score': None,
                        'mse': None,
                        'mae': None,
                        'error': 'No common columns'
                    }

                true_data = true_data[common_cols]
                pred_data = pred_data[common_cols]

                min_rows = min(len(true_data), len(pred_data))
                true_data = true_data.iloc[:min_rows]
                pred_data = pred_data.iloc[:min_rows]
            else:
                # For arrays
                min_shape = [min(d1, d2) for d1, d2 in zip(true_data.shape, pred_data.shape)]
                true_data = true_data[tuple(slice(0, d) for d in min_shape)]
                pred_data = pred_data[tuple(slice(0, d) for d in min_shape)]

            print(f"Adjusted shapes - True: {true_data.shape}, Pred: {pred_data.shape}")

    # Case 3: Handle constant true values
    if isinstance(true_data, pd.DataFrame):
        constant_check = (true_data.std() == 0).all()
    else:
        constant_check = np.std(true_data) == 0

    if constant_check:
        print("Warning: All true values are constant")
        # R² is undefined when all true values are the same
        results['r2_score'] = None
    else:
        try:
            results['r2_score'] = r2_score(true_data, pred_data)
        except Exception as e:
            print(f"Error calculating R²: {e}")
            results['r2_score'] = None

    # Case 4: Perfect predictions
    if isinstance(true_data, pd.DataFrame):
        perfect_check = (true_data == pred_data).all().all()
    else:
        perfect_check = np.array_equal(true_data, pred_data)

    if perfect_check:
        print("Note: Perfect predictions detected (true = predicted)")
        # R² should be 1.0, MSE and MAE should be 0.0

    # Case 5: Handle missing values
    if isinstance(true_data, pd.DataFrame) and isinstance(pred_data, pd.DataFrame):
        true_missing = true_data.isnull().sum().sum()
        pred_missing = pred_data.isnull().sum().sum()

        if true_missing > 0 or pred_missing > 0:
            print(f"Warning: Missing values detected - True: {true_missing}, Pred: {pred_missing}")
            # Option 1: Drop rows with missing values
            true_data = true_data.dropna()
            pred_data = pred_data.dropna()

            # Option 2: Fill missing values (alternative approach)
            # true_data = true_data.fillna(true_data.mean())
            # pred_data = pred_data.fillna(pred_data.mean())

    # Calculate remaining metrics
    try:
        results['mse'] = mean_squared_error(true_data, pred_data)
    except Exception as e:
        print(f"Error calculating MSE: {e}")
        results['mse'] = None

    try:
        results['mae'] = mean_absolute_error(true_data, pred_data)
    except Exception as e:
        print(f"Error calculating MAE: {e}")
        results['mae'] = None

    return results

# Test the function with various edge cases
# Example 1: Empty input
true_empty = np.array([])
pred_empty = np.array([])
print("Test Case 1: Empty input")
print(robust_model_evaluation(true_empty, pred_empty))

# Example 2: Different shapes
true_diff_shape = np.array([[1, 2, 3], [4, 5, 6]])
pred_diff_shape = np.array([[1, 2], [3, 4], [5, 6]])
print("\nTest Case 2: Different shapes")
print(robust_model_evaluation(true_diff_shape, pred_diff_shape))

# Example 3: Constant true values
true_constant = np.array([5, 5, 5, 5, 5])
pred_constant = np.array([4, 5, 6, 4, 6])
print("\nTest Case 3: Constant true values")
print(robust_model_evaluation(true_constant, pred_constant))

# Example 4: Perfect predictions
true_perfect = np.array([1, 2, 3, 4, 5])
pred_perfect = np.array([1, 2, 3, 4, 5])
print("\nTest Case 4: Perfect predictions")
print(robust_model_evaluation(true_perfect, pred_perfect))

# Example 5: Missing values
true_missing = pd.DataFrame({'A': [1, 2, np.nan, 4], 'B': [5, 6, 7, 8]})
pred_missing = pd.DataFrame({'A': [1.1, 2.2, 3.3, np.nan], 'B': [5.5, np.nan, 7.7, 8.8]})
print("\nTest Case 5: Missing values")
print(robust_model_evaluation(true_missing, pred_missing))
```

**Explanation**:

This comprehensive function handles multiple edge cases:

1. **Empty input data**: Returns None for all metrics with an error message.
2. **Different shapes**: Adjusts the arrays or DataFrames to match shapes by finding common columns and limiting to the minimum number of rows.
3. **Constant true values**: R² score is undefined when all true values are the same (would cause division by zero), so we return None for this metric.
4. **Perfect predictions**: Notes that the predictions match the true values exactly, which should result in R² = 1.0, MSE = 0, MAE = 0.
5. **Missing values**: Detects missing values and handles them by either dropping rows with missing values or filling them (two strategies shown).

The function provides informative warnings for each edge case and attempts to calculate metrics where possible, making it robust for real-world data.
