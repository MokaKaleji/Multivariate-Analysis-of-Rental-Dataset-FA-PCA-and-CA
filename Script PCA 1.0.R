################################################################################
#                        Rental Dataset: PCA & Exploratory Analysis               #
################################################################################
# Author: Moka Kaleji                                                               
# Affiliation: Statistics for high-dimensional dataset, University of Bologna            
#                                                                                 
# Description:
#   This script performs Principal Component Analysis (PCA) on the rental dataset.
#   It standardizes variables, computes and interprets principal components, and
#   exports loadings and explained variance tables for reporting.
################################################################################

########################## 1. Load Required Libraries ###########################
library(psych)       # Psychometric functions for factor analysis & PCA
library(ggplot2)     # General-purpose plotting
library(corrplot)    # Visualization of correlation matrices
library(factoextra)  # PCA and clustering visualization
library(flextable)   # Create Word tables from data frames
library(officer)     # Export Word documents
################################################################################

########################## 2. Data Import & Standardization ####################
# Load dataset; expects first column as categorical identifier (e.g., city)
data <- read.table("rental.txt", header = TRUE)
# Remove non-numeric city column for PCA
data_pca <- data[, -1]
# Check for missing values and report
na_count <- sum(is.na(data_pca))
cat("Missing values detected:", na_count, "\n")
# Standardize variables to zero mean and unit variance
data_scaled <- scale(data_pca)
################################################################################

########################## 3. Compute PCA #######################################
# Perform PCA on standardized data
# center=TRUE, scale.=TRUE ensures consistency even if data_scaled already scaled
pca_model <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Display summary: standard deviations, proportions, cumulative
cat("PCA Summary:\n")
print(summary(pca_model))
################################################################################

########################## 4. Scree Plot & Variance Explained ##################
# Scree plot with eigenvalues to help determine number of components
fviz_eig(pca_model, addlabels = TRUE, main = "Scree Plot of PCA")

# Calculate explained and cumulative variance
explained_variance <- pca_model$sdev^2 / sum(pca_model$sdev^2)
cumulative_variance <- cumsum(explained_variance)
# Prepare table for Word export
table_variance <- data.frame(
  Component = paste0("PC", seq_along(explained_variance)),
  Explained_Variance    = round(explained_variance, 4),
  Cumulative_Variance   = round(cumulative_variance, 4)
)
# Export variance table
tab_var <- flextable(table_variance) %>%
  set_caption("Explained and Cumulative Variance by Principal Components") %>%
  autofit()
doc_var <- read_docx() %>% body_add_flextable(tab_var)
print(doc_var, target = "PCA_Variance.docx")
################################################################################

########################## 5. PCA Loadings ######################################
# Extract loadings (rotation matrix)
pca_loadings <- as.data.frame(pca_model$rotation)
# Prepare loadings table for first three components
loadings_table <- data.frame(
  Variable = rownames(pca_loadings),
  PC1 = round(pca_loadings[,1], 4),
  PC2 = round(pca_loadings[,2], 4),
  PC3 = round(pca_loadings[,3], 4)
)
# Export loadings table
tab_load <- flextable(loadings_table) %>%
  set_caption("PCA Loadings for First Three Components") %>%
  autofit()
doc_load <- read_docx() %>% body_add_flextable(tab_load)
print(doc_load, target = "PCA_Loadings.docx")
################################################################################

########################## 6. Visualize PCA Results ############################
# Biplot of variables and observations on first two PCs
fviz_pca_biplot(pca_model,
                label = "var",            # show variable labels
                habillage = NULL,          # no grouping variable
                addEllipses = FALSE,
                repel = TRUE,
                title = "PCA Biplot: Variables on PC1 vs PC2"
)
################################################################################

# End of PCA analysis script
################################################################################
