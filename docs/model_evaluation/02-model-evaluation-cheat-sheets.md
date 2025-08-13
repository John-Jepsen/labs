# Model Evaluation Cheat Sheets

Quick reference for formulas, when-to-use, and scikit-learn scorers.

## Classification metrics

| Metric | Formula | Notes |
| --- | --- | --- |
| Accuracy | (TP+TN)/(TP+TN+FP+FN) | Only for balanced classes/costs |
| Precision | TP/(TP+FP) | How pure positive predictions are |
| Recall (TPR) | TP/(TP+FN) | How complete positive retrieval is |
| Specificity (TNR) | TN/(TN+FP) | True negative rate |
| F1 | 2·PR/(P+R) | Harmonic mean of P and R |
| Fβ | (1+β²)·P·R/(β²·P+R) | β>1 emphasizes recall |
| ROC-AUC | ∫ TPR dFPR | Threshold-free ranking quality |
| PR-AUC | ∫ Precision dRecall | Prefer under imbalance |
| MCC | (TP·TN−FP·FN)/√((TP+FP)(TP+FN)(TN+FP)(TN+FN)) | Balanced single summary |

Common sklearn scorers:

- binary/multiclass: 'accuracy', 'precision', 'recall', 'f1', 'f1_macro', 'f1_weighted', 'roc_auc', 'roc_auc_ovr', 'average_precision'

## Regression metrics

| Metric | Formula | Notes |
| --- | --- | --- |
| MAE | mean(|y−ŷ|) | Robust to outliers |
| MSE | mean((y−ŷ)²) | Penalizes large errors |
| RMSE | √MSE | Same units as y |
| R² | 1 − SS_res/SS_tot | Descriptive fit |
| MAPE | mean(|(y−ŷ)/y|) | Beware y≈0 |
| sMAPE | mean(2·|y−ŷ|/(|y|+|ŷ|)) | Scale-free |
| MASE | mean(|y−ŷ|) / mean(|y_t − y_{t−1}|) | Baseline-relative |

Sklearn scorers:

- 'neg_mean_absolute_error', 'neg_mean_squared_error', 'neg_root_mean_squared_error', 'r2'

## Splitting and CV

```python
from sklearn.model_selection import (
    train_test_split, KFold, StratifiedKFold, GroupKFold, TimeSeriesSplit,
    cross_val_score
)
```

- Classification: use StratifiedKFold; set shuffle=True, random_state for reproducibility.
- Time series: use TimeSeriesSplit; never shuffle.
- Grouped data: use GroupKFold/StratifiedGroupKFold.

## Threshold selection

Given costs C_FP, C_FN and prevalence π, calibrated probabilities ->

t* = (C_FP · (1−π)) / (C_FN · π + C_FP · (1−π))

Grid search a threshold on validation to maximize expected utility or Fβ.

## Quick code snippets

```python
# Cross-validated ROC-AUC
from sklearn.model_selection import StratifiedKFold, cross_val_score
from sklearn.metrics import make_scorer, roc_auc_score
cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)
auc = cross_val_score(model, X, y, cv=cv, scoring='roc_auc').mean()

# Precision-Recall curve and AP
from sklearn.metrics import precision_recall_curve, average_precision_score
p, r, th = precision_recall_curve(y_true, y_prob)
ap = average_precision_score(y_true, y_prob)

# Regression CV with RMSE
import numpy as np
from sklearn.model_selection import KFold
cv = KFold(n_splits=5, shuffle=True, random_state=42)
rmse = (-cross_val_score(model, X, y, cv=cv, scoring='neg_root_mean_squared_error')).mean()
```
