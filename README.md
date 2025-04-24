# Multivariate Analysis of Rental Dataset: FA, PCA, and CA

# Author: Moka Kaleji                                                              
# Affiliation: Statistics for high-dimensional dataset, University of Bologna 

This repository contains a complete and commented R implementation of three key multivariate analysis methods — **Factor Analysis (FA)**, **Principal Component Analysis (PCA)**, and **Cluster Analysis (CA)** — applied to a rental dataset. The project is designed for academic and practical use, providing detailed statistical insights through dimensionality reduction, latent structure extraction, and unsupervised classification.

---

## Dataset

- **Name:** `rental.txt`
- **Description:** Contains rental-related variables across different cities. The first column includes city names (removed for analysis), and the rest are numeric variables relevant for multivariate analysis.
- **Preprocessing:** Standardization, correlation inspection, and missing value handling (if any) are performed before applying the analysis.

---

## Methods Overview

### 1. Factor Analysis (FA)

Factor Analysis is used to uncover latent variables (factors) that explain correlations among observed variables. The implementation includes:

- Kaiser-Meyer-Olkin (KMO) Test
- Bartlett's Test of Sphericity
- Parallel Analysis to determine number of factors
- Maximum Likelihood Estimation
- Varimax, Promax, and Quartimax Rotations
- Communality values and residual diagnostics
- Factor score plots (2D and 3D using `plotly` and `scatterplot3d`)
- Automated export of loadings and communalities to Word

📄 Output files:
- `KMO_result.docx`
- `Factor_Loadings.docx`
- `Communality_Table.docx`

---

### 2. Principal Component Analysis (PCA)

Principal Component Analysis is used to reduce the dimensionality of the dataset while retaining the maximum amount of variance. This implementation includes:

- Eigenvalue analysis
- Scree plot and explained variance visualization
- Projection of cities into principal component space
- Biplots for interpretation
- Comparison of PCA with Factor Analysis for interpretation purposes

---

### 3. Cluster Analysis (CA)

Cluster Analysis is applied to group similar cities based on the principal components. The steps include:

- Standardization of PCA scores
- Hierarchical clustering with dendrogram
- K-means clustering and Elbow method
- Cluster visualization in 2D and 3D
- City labeling and profiling by cluster

---

## Requirements

- R 4.x or higher
- R packages:
  - `psych`, `GPArotation`, `nFactors`, `corrplot`, `ggplot2`, `plotly`, `scatterplot3d`
  - `knitr`, `kableExtra`, `officer`, `flextable`, `factoextra`, `cluster`, `NbClust`

Install packages using:

```r
install.packages(c("psych", "GPArotation", "nFactors", "corrplot", "ggplot2", 
                   "plotly", "scatterplot3d", "knitr", "kableExtra", "officer", 
                   "flextable", "factoextra", "cluster", "NbClust"))
