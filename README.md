## Analysis-of-Factors-that-Impact-Test-Scores-performance-of-Students
# Goal:

Identify and quantify the most influential factors affecting standardized test scores in English and Mathematics at two critical educational stages.

# Dataset:

China Education Panel Survey (CEPS) - longitudinal data on 390 students, with over 70 variables per student, spanning academic, psychological, social, and institutional domains.

# Key Steps:

Data Preprocessing: Scaling, splitting, and organizing data for analysis.

Imputation:

Diagnosed missingness as MCAR, MAR, or MNAR using correlation heatmaps and percentage missingness.

Applied k-Nearest Neighbors (KNN) and Multiple Imputation by Chained Equations (MICE) with Predictive Mean Matching (PMM) or CART, depending on missingness mechanism.

Validated imputation with density plots and cross-validation RMSE.

Modelling:

Compared Elastic Net, Random Forest, Gradient Boosting Machine (GBM), and Support Vector Machine (SVM).

Used Recursive Feature Elimination (RFE) and cross-validation to optimize feature subsets and prevent overfitting.

Visualized and compared model performance and feature importance.

Key Results
Elastic Net provided the best generalization, balancing predictive power and interpretability, with R² up to 0.27 for English in 5th grade.

GBM achieved high training accuracy but suffered from overfitting on small samples.

Feature Importance:

Year 5: Quality of math homework corrections and academic stress were dominant predictors, with surprising cross-subject effects (e.g., attitude toward English predicting math performance).

Year 9: Subject satisfaction and fundamental skills (e.g., geometry sense) became more important, with peer relationships gaining influence.

Imputation:

Advanced imputation reduced RMSE by over 85%, preserving data integrity for modelling.

MNAR variables required special attention; PMM and CART in MICE were most effective.
