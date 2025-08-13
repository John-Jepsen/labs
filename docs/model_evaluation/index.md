# Model Evaluation

Evaluate the right thing, the right way. This section gives you practical, production-grade guidance to choose metrics, validate models correctly, compare models rigorously, and avoid common traps like data leakage and misleading metrics.

## Quick start: choose the right metric

- Binary classification (balanced): ROC-AUC for ranking; F1 if you care about both precision/recall; Accuracy only if classes and costs are balanced.
- Binary classification (imbalanced): PR-AUC, Recall@k, Precision@k, F-beta for recall-heavy applications, Matthews Correlation Coefficient (MCC) for balanced summary.
- Multi-class: Macro-averaged F1 for class balance, Weighted F1 for class imbalance, Top-k accuracy for retrieval-like tasks.
- Regression: MAE when outliers matter less; RMSE when large errors must be penalized; MAPE/sMAPE when relative error matters; R^2 is descriptive, not a business metric.
- Ranking/recommendation: MAP, NDCG@k, HitRate@k, coverage/diversity.
- Probabilistic forecasts: Log loss (cross-entropy), Brier score, calibration curves, sharpness vs calibration trade-off.
- Time series: MAE/RMSE on rolling-origin splits; MASE and sMAPE for scale-free comparisons.

## Avoid the top pitfalls

- Train/test contamination: leakage via target, time, or group identity. Use time-aware or group-aware splits.
- Overfitting on the test set: keep a holdout; use nested CV for fair model selection.
- Misleading accuracy on imbalanced data: prefer PR-AUC or cost-sensitive metrics.
- Threshold choice ignored: optimize threshold on validation with business costs; report sensitivity to threshold.
- No uncertainty: add confidence intervals via bootstrap or analytical tests (e.g., DeLong for AUC).

## What you’ll find here

- [Study Guide](01-model-evaluation-study-guide.md): Deep dive with formulas, code, and best practices
- [Cheat Sheets](02-model-evaluation-cheat-sheets.md): One-page references for metrics and scikit-learn scorers
- [Practice Problems](03-model-evaluation-practice-problems.md): Hands-on exercises with solutions
- [Visual Guide](04-model-evaluation-visual-guide.md): Intuitive diagrams and plot interpretations

Tip: All pages support math (MathJax), admonitions, and tabs. Copy/paste the provided code to reproduce results.
