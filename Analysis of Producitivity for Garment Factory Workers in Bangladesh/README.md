# Garment Worker Productivity Classification  
## Statistical Analysis — LBK

---

## Objective
Predict whether garment workers in Bangladesh meet daily production targets based on behavioral, environmental, and management-related variables.

---

## Methodology
- **Dataset:** Garment Worker Productivity dataset (UCI Machine Learning Repository)  
- **Target Variable:** achieved_target (binary)  
- **Models Evaluated:** Logistic Regression, Linear Discriminant Analysis (LDA), Quadratic Discriminant Analysis (QDA), Regularized Discriminant Analysis (RDA), and k-Nearest Neighbors (k-NN)  
- **Validation Approach:** 10-fold cross-validation and bootstrap AUC comparison  
- **Evaluation Metrics:** Accuracy, AUC, F1-score, Precision, Recall  

---

## Key Results
- **Best Model:** Regularized Discriminant Analysis (RDA)  
- **Average AUC:** 0.87 (RDA) vs 0.83 (LDA) vs 0.79 (k-NN)  
- Logistic Regression performed best in interpretability.  
- **Top Predictors:** Workload, incentive type, and department.  

---

## Tools
- **Language:** R  
- **Packages:** MASS, caret, ggplot2, boot  
- **Platform:** Windows 11  
- **Version Control:** Git + GitHub  

---

## References
- Bisgaard & Travis (1991). *Analysis of Productivity in Garment Manufacturing.*  
- UCI Machine Learning Repository — Garment Worker Productivity Dataset.  
- Bruce, Bruce & Gedeck (2020). *Practical Statistics for Data Scientists.*  

---

## Author
**Luka Bojovic**  
[lukabojovic@gmail.com](mailto:lukabojovic@gmail.com)  
[linkedin.com/in/lukabojovic](https://linkedin.com/in/lukabojovic)  
[github.com/lukabojovic](https://github.com/lukabojovic)
