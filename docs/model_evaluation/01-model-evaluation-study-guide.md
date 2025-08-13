# Model Evaluation Study Guide

A practical deep dive into classification, regression, ranking, and probabilistic evaluation. Includes formulas, pitfalls, and runnable code.

## Classification metrics

Confusion matrix (binary) with threshold t:

- TP: y=1 and p>=t
- FP: y=0 and p>=t
- TN: y=0 and p<t
- FN: y=1 and p<t

Core metrics:

- Accuracy = (TP + TN) / (TP + TN + FP + FN)
- Precision = TP / (TP + FP)
- Recall (TPR) = TP / (TP + FN)
- Specificity (TNR) = TN / (TN + FP)
- F1 = 2 · (Precision · Recall) / (Precision + Recall)
- Balanced Accuracy = (TPR + TNR)/2
- Matthews Correlation Coefficient (MCC) = (TP·TN − FP·FN) / sqrt((TP+FP)(TP+FN)(TN+FP)(TN+FN))

Curves and areas:

- ROC curve: plot TPR vs FPR as threshold varies; ROC-AUC reflects ranking quality, relatively insensitive to class skew.
- PR curve: plot Precision vs Recall; PR-AUC is more informative under heavy imbalance.

Averaging (multi-class):

- Micro: aggregate TP/FP/FN globally (good with class-size differences).
- Macro: average metric per class equally (highlights rare-class performance).
- Weighted: macro weighted by class support.

Thresholding and costs:

If probabilities are calibrated and positive class prevalence is π, the Bayes-optimal threshold is:

t* = (C_FP · (1 − π)) / (C_FN · π + C_FP · (1 − π))

Report curves (ROC/PR) or cost curves instead of a single threshold. When reporting a single operating point, explain how t was chosen.

Calibration:

- Diagnose with reliability curves and Brier score; fix with Platt scaling (sigmoid) or isotonic regression fitted on validation data.

Imbalanced learning tips:

- Prefer PR-AUC, Recall@k, Precision@k, MCC; use stratified CV; consider class weights or focal loss; inspect per-class metrics.

## Regression metrics

- MAE = mean(|y − ŷ|)
- MSE = mean((y − ŷ)^2), RMSE = sqrt(MSE)
- R^2 = 1 − SS_res/SS_tot; adjusted R^2 penalizes extra predictors.
- MAPE = mean(|(y − ŷ)/y|) (careful when y≈0); sMAPE avoids division by zero.
- MASE compares to a naive baseline; useful across time series of different scales.

When to use which:

- MAE for robustness to outliers; RMSE when large errors are costly; use scale-free metrics (sMAPE/MASE) for cross-series comparability.

## Validation strategies

- IID data: K-fold; stratified K-fold for classification. K in [5,10] is common.
- Hyperparameter tuning: nested CV to avoid optimistic selection bias.
- Grouped data: GroupKFold or StratifiedGroupKFold to prevent group leakage.
- Time series: rolling-origin (expanding) or sliding-window splits; no shuffling; optionally apply an embargo to avoid leakage.

Sklearn splitters:

```python
from sklearn.model_selection import StratifiedKFold, GroupKFold, TimeSeriesSplit

skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)
gkf = GroupKFold(n_splits=5)
tss = TimeSeriesSplit(n_splits=5)
```

## Model comparison and uncertainty

- Compare models with paired resampling (CV) or bootstrap on the same samples.
- McNemar’s test for paired classification errors (same test set); DeLong test for AUC differences.
- Always report confidence intervals (CIs) alongside point estimates.

Bootstrap CIs (generic):

```python
import numpy as np

def bootstrap_ci(metric_fn, y_true, y_pred, B=1000, alpha=0.05, rng=42):
    rng = np.random.default_rng(rng)
    n = len(y_true)
    stats = []
    for _ in range(B):
        idx = rng.integers(0, n, n)
        stats.append(metric_fn(y_true[idx], y_pred[idx]))
    lo, hi = np.quantile(stats, [alpha/2, 1-alpha/2])
    return float(lo), float(hi)
```

## Calibration and reliability

```python
from sklearn.calibration import calibration_curve

prob_true, prob_pred = calibration_curve(y_true, y_prob, n_bins=10, strategy='uniform')
```

Fit a calibrator on validation data (Platt or isotonic), then re-evaluate metrics and thresholds.

## End-to-end example (imbalanced classification)

```python
from sklearn.datasets import make_classification
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import roc_auc_score, average_precision_score, f1_score
import numpy as np

X, y = make_classification(n_samples=5000, n_features=20, weights=[0.9, 0.1], random_state=0)
cv = StratifiedKFold(n_splits=5, shuffle=True, random_state=0)
aucs, aps, f1s = [], [], []

for tr, te in cv.split(X, y):
    clf = LogisticRegression(max_iter=500, class_weight='balanced').fit(X[tr], y[tr])
    p = clf.predict_proba(X[te])[:, 1]
    yhat = (p >= 0.5).astype(int)
    aucs.append(roc_auc_score(y[te], p))
    aps.append(average_precision_score(y[te], p))
    f1s.append(f1_score(y[te], yhat))

print({'roc_auc_mean': float(np.mean(aucs)), 'pr_auc_mean': float(np.mean(aps)), 'f1_mean': float(np.mean(f1s))})
```

## End-to-end example (time series regression)

```python
import numpy as np
from sklearn.linear_model import Ridge
from sklearn.model_selection import TimeSeriesSplit
from sklearn.metrics import mean_absolute_error

n = 1000
rng = np.random.default_rng(0)
X = rng.normal(size=(n, 5)).cumsum(axis=0)
y = X[:, 0] * 0.1 + rng.normal(0, 0.5, size=n)

cv = TimeSeriesSplit(n_splits=5)
maes = []
for tr, te in cv.split(X):
    model = Ridge().fit(X[tr], y[tr])
    yhat = model.predict(X[te])
    maes.append(mean_absolute_error(y[te], yhat))
print({'mae_mean': float(np.mean(maes))})
```

## Evaluation checklist

- Metric matches objective and class/series characteristics
- Split strategy prevents leakage and mirrors production
- Threshold chosen on validation with cost model (if applicable)
- Report uncertainty (CIs) and compare fairly (paired tests)
- Validate and, if needed, calibrate probabilities
