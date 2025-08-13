# Model Evaluation Practice Problems

Test your understanding with realistic scenarios. Solutions are provided in collapsed sections.

## Classification

### 1) Confusion Matrix Analysis

A binary classifier produced:

|                   | Predicted Positive | Predicted Negative |
| ----------------- | ------------------ | ------------------ |
| Actually Positive | 85                 | 15                 |
| Actually Negative | 20                 | 80                 |

Compute Accuracy, Precision, Recall, F1, Specificity, MCC.

<details><summary>Solution</summary>

TP=85, FN=15, FP=20, TN=80.

- Acc = (85+80)/(85+15+20+80) = 165/200 = 0.825
- Prec = 85/(85+20) ≈ 0.8095
- Rec = 85/(85+15) = 0.85
- F1 ≈ 2·(0.8095·0.85)/(0.8095+0.85) ≈ 0.829
- Spec = 80/(80+20) = 0.80
- MCC = (85·80 − 20·15)/sqrt((105)(100)(100)(95)) ≈ 0.65

</details>

### 2) PR vs ROC under imbalance

Dataset has 1% positives. Model A: ROC-AUC=0.95, PR-AUC=0.25. Model B: ROC-AUC=0.92, PR-AUC=0.35. Which is preferable and why? How would you choose an operating threshold?

<details><summary>Solution</summary>
Under heavy imbalance, PR-AUC is more indicative of early precision; Model B likely better. Choose threshold by maximizing Fβ (β>1) or expected utility on validation, or by optimizing Precision@k/Recall@k at business-relevant k.
</details>

## Regression

### 3) Metrics interpretation

House price model metrics: MAE=$45k, RMSE=$65k, R²=0.82. What does this indicate? Which metric would you report to stakeholders and why?

<details><summary>Solution</summary>
Average absolute error is $45k; RMSE>$MAE indicates a tail of larger errors; R² shows strong explanatory power but not business impact. Report MAE (intuitive), and optionally RMSE; include prediction intervals.
</details>

### 4) Time series validation

Why is random train/test split problematic for stock prediction? Propose a correct validation.

<details><summary>Solution</summary>
Temporal leakage breaks causality. Use rolling-origin or expanding window CV, ensuring train times precede test times; consider embargo after events.
</details>

## Coding

### 5) Stratified CV on imbalanced data

Write sklearn code to run stratified 5-fold CV computing precision, recall, and F1 per fold.

<details><summary>Solution</summary>

```python
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import precision_score, recall_score, f1_score
import numpy as np

cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)
precisions, recalls, f1s = [], [], []

for tr, te in cv.split(X, y):
    m = model.fit(X[tr], y[tr])
    yhat = m.predict(X[te])
    precisions.append(precision_score(y[te], yhat))
    recalls.append(recall_score(y[te], yhat))
    f1s.append(f1_score(y[te], yhat))

print({'precision': float(np.mean(precisions)), 'recall': float(np.mean(recalls)), 'f1': float(np.mean(f1s))})
```

</details>
