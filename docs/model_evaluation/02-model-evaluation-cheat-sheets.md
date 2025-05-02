# Model Evaluation Cheat Sheets

Quick reference guides for model evaluation techniques and metrics.

## Classification Metrics Cheat Sheet

| Metric    | Formula                                         | Use Case                        |
| --------- | ----------------------------------------------- | ------------------------------- |
| Accuracy  | (TP + TN) / (TP + TN + FP + FN)                 | Balanced datasets               |
| Precision | TP / (TP + FP)                                  | When false positives are costly |
| Recall    | TP / (TP + FN)                                  | When false negatives are costly |
| F1 Score  | 2 _ (Precision _ Recall) / (Precision + Recall) | Balancing precision and recall  |
| AUC       | Area under ROC curve                            | Model comparison                |

## Regression Metrics Cheat Sheet

| Metric | Formula                   | Use Case                |
| ------ | ------------------------- | ----------------------- |
| MAE    | (1/n) \* Σ\|y_i - ŷ_i\|   | Linear error importance |
| MSE    | (1/n) \* Σ(y_i - ŷ_i)²    | Penalize large errors   |
| RMSE   | √((1/n) \* Σ(y_i - ŷ_i)²) | Same units as target    |
| R²     | 1 - (SS_res / SS_tot)     | Explanatory power       |

## Common Python Code Snippets

```python
# Classification metrics
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score

accuracy = accuracy_score(y_true, y_pred)
precision = precision_score(y_true, y_pred)
recall = recall_score(y_true, y_pred)
f1 = f1_score(y_true, y_pred)
auc = roc_auc_score(y_true, y_pred_proba)

# Regression metrics
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
import numpy as np

mae = mean_absolute_error(y_true, y_pred)
mse = mean_squared_error(y_true, y_pred)
rmse = np.sqrt(mse)
r2 = r2_score(y_true, y_pred)

# Cross-validation
from sklearn.model_selection import cross_val_score, KFold

cv = KFold(n_splits=5, shuffle=True, random_state=42)
scores = cross_val_score(model, X, y, cv=cv, scoring='accuracy')
```
