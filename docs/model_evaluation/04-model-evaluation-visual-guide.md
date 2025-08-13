# Model Evaluation Visual Guide

Visual explanations of key model evaluation concepts with interpretation notes.

## Confusion Matrix

```
                  Predicted
                  | Positive | Negative |
Actual  Positive  |    TP    |    FN    |
        Negative  |    FP    |    TN    |
```

Tips:
- High FN → low recall; high FP → low precision.
- Normalize by true label to compare across classes.

## ROC Curve

The ROC curve plots TPR vs FPR as the threshold varies.

```
    1 |       ._____
      |      /
TPR   |     /
      |    /
      |   /
      |  /
      | /
    0 +------------
      0    FPR     1
```

- Random guess: diagonal line (AUC≈0.5)
- Better model: bows toward top-left
- Imbalanced data: ROC can look optimistic; inspect PR curve too

## Precision-Recall (PR) Curve

- Shows trade-off between precision and recall.
- Baseline precision equals positive class prevalence.
- Early precision at low recall often matters for triage/search.

## Calibration Plot (Reliability Curve)

- Plot predicted probability vs observed frequency.
- Perfect calibration: y=x line; overconfident models lie above/below.
- Use Brier score and log loss to quantify.

## Learning Curves

- Train/test score vs training set size.
- Gap narrows with more data; persistent gap → variance-limited; both low → bias-limited.

## Validation Schemes

K-fold and Time Series splits:

```
KFold:      [Tst][Trn][Trn][Trn][Trn]
            [Trn][Tst][Trn][Trn][Trn]
            ...

TimeSeries: [Trn]→[Tst]
            [Trn   ]→[ Tst]
            [Trn     ]→[  Tst]
```

- For time series, never shuffle across time.
- For grouped data, use GroupKFold to avoid leakage.
