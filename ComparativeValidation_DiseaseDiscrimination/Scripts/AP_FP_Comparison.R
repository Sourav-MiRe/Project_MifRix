
load("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\GitHub_ScriptsData\\ComparativeValidation_DiseaseDiscrimination\\DATA\\AP_FP_Tetsing_DS.RData")

###################################################################
## Data Processing 
library(dplyr)
library(tidyr)

## AM denotes the taxa abundance profile and MM denotes Microbiome Functional Profile here

AM_test$sample_id <- rownames(AM_test)
MM_test$sample_id <- rownames(MM_test)

AM_long <- AM_test %>%
  pivot_longer(
    cols = -c(sample_id, diseaseCat),
    names_to = "Disease",
    values_to = "AM"
  )

MM_long <- MM_test %>%
  pivot_longer(
    cols = -c(sample_id, diseaseCat),
    names_to = "Disease",
    values_to = "MM")

true_labels <- AM_test %>%
  select(sample_id, diseaseCat) %>%
  separate_rows(diseaseCat, sep = ";")

AM_filtered <- AM_long %>%
  inner_join(true_labels,
             by = c("sample_id", "Disease" = "diseaseCat"))

MM_filtered <- MM_long %>%
  inner_join(true_labels,
             by = c("sample_id", "Disease" = "diseaseCat"))

AM_MM_testing <- AM_filtered %>%
  select(sample_id, Disease, AM) %>%
  inner_join(
    MM_filtered %>% select(sample_id, Disease, MM),
    by = c("sample_id", "Disease")
  ) %>%
  select(sample_id, AM, MM, Disease)




####################################################################

diseases <- unique(AM_MM_testing$Disease)
box_data <- list()
labels <- c()

for (d in diseases) {
  am_vals <- AM_MM_testing$AM[AM_MM_testing$Disease == d]
  mm_vals <- AM_MM_testing$MM[AM_MM_testing$Disease == d]
  
  box_data[[length(box_data)+1]] <- am_vals
  box_data[[length(box_data)+1]] <- mm_vals
  
  labels <- c(labels, paste(d, "AM"), paste(d, "MM"))
}


AM_MM_testing <- as.data.frame(AM_MM_testing)

pdf("DS_testing_AD.pdf", height = 3.3, width = 2.3)
boxplot(AM_MM_testing[AM_MM_testing$Disease =='AD','AM'],AM_MM_testing[AM_MM_testing$Disease =='AD','MM'], 
        outline = FALSE, names = c("AM","MM"),
        col = c("skyblue", "salmon"),
        ylab = "Disease Probability")
dev.off()

### Running Wilcoxon rank-sum test

diseases <- unique(AM_MM_testing$Disease)

disease_vec <- c()
pval_vec <- c()

for (d in diseases) {
  
  am_vals <- AM_MM_testing$AM[AM_MM_testing$Disease == d]
  mm_vals <- AM_MM_testing$MM[AM_MM_testing$Disease == d]
  
  test_result <- wilcox.test(am_vals, mm_vals, exact = FALSE)
  
  
  disease_vec <- c(disease_vec, d)
  pval_vec <- c(pval_vec, test_result$p.value)
}

pval_df_AM_MM_testing <- data.frame(Disease = disease_vec, P_value = pval_vec)
write.xlsx(pval_df_AM_MM_testing,"pval_df_AP_FP_testing.xlsx")


#==============================================================
# Performance Comparison between AP, FP and MifRix-final-score
#==============================================================

load("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\AM_MM_Comparison_ComparativeValidation\\CD_performance_composite.RData")

CD_validation <- read.xlsx("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\AM_MM_Comparison_ComparativeValidation\\CD_validation_accuracy_all.xlsx", sheetName = 'Sheet1')

DOD_validation <- read.xlsx("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\AM_MM_Comparison_ComparativeValidation\\DOD_validation_result.xlsx", sheetName = 'Sheet1', check.names = F)

DOD_accuracy_composite$study_name[11:12] <- c("LiJ_2014_IBD","LiJ_2014_T2D")
DOD_accuracy_composite$study_name[23:24] <- c("YuJ_2015_CRC","YuJ_2015_T2D")

rownames(DOD_validation) <- DOD_validation$study_name
DOD_validation <- DOD_validation[DOD_accuracy_composite$study_name,]
DOD_validation$Accuracy_composite <- DOD_accuracy_composite$Recall
rownames(DOD_validation) <- NULL

DOD_validation$Accuracy_AP <- round(DOD_validation$Accuracy_AP,3)
DOD_validation$Accuracy_FP <- round(DOD_validation$Accuracy_FP,3)
DOD_validation$Accuracy_composite <- round(DOD_validation$Accuracy_composite,3)

DOD_validation$performance <- ifelse(DOD_validation$Accuracy_FP > DOD_validation$Accuracy_AP, "FP",ifelse(DOD_validation$Accuracy_AP > DOD_validation$Accuracy_FP, "AP", "equal"))
DOD_validation$performance_new <- ifelse(DOD_validation$Accuracy_composite >= pmax(DOD_validation$Accuracy_AP, DOD_validation$Accuracy_FP),"composite",DOD_validation$performance)


################### HEATMAP ################

library(ComplexHeatmap)
library(circlize)
library(grid)

mat <- CD_validation[, c("Accuracy_AP", "Accuracy_FP", "Accuracy_composite")]
rownames(mat) <- CD_validation$Study.Name
mat <- t(as.matrix(mat))

col_fun <- colorRamp2(
  c(0.3, 0.6, 0.8, 1),
  c("#fee8c8", "#fdbb84", "#fc8d59", "#e34a33"))

ht <- Heatmap(
  mat,
  name = "Accuracy",
  col = col_fun,
  
  cluster_rows = T,
  cluster_columns = T,
  
  show_row_dend = FALSE,      
  show_column_dend = FALSE,   
  
  show_row_names = TRUE,
  show_column_names = TRUE,
  
  row_names_side = "left",
  row_names_gp = gpar(fontsize = 8),
  column_names_gp = gpar(fontsize = 8),
  
  heatmap_legend_param = list(
    at = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0", "0.25", "0.5", "0.75", "1"),
    title = "Accuracy"
  ),
  
  width = unit(ncol(mat) * 6, "mm"),
  height = unit(nrow(mat) * 5, "mm"),
  
  # Add numbers inside cells
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.rect(x, y, width, height,
              gp = gpar(fill = fill, col = "grey40", lwd = 0.3))
    grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 6))
  }
)

draw(ht)


ht_drawn <- draw(ht)
row_order <- row_order(ht_drawn)
col_order <- column_order(ht_drawn)
carpet_df <- mat[row_order, col_order]
carpet_cd_val_accuracy <- as.data.frame(carpet_df)

############# DOD Validation #############

mat <- DOD_validation[, c("Accuracy_AP", "Accuracy_FP", "Accuracy_composite")]
rownames(mat) <- DOD_validation$study_name
mat <- t(as.matrix(mat))

col_fun <- colorRamp2(
  c(0.3, 0.6, 0.8, 1),
  c("#d0f0e0", "#a8ddb5", "#80cdc1", "#74a9cf"))

ht <- Heatmap(
  mat,
  name = "Accuracy",
  col = col_fun,
  
  cluster_rows = T,
  cluster_columns = T,
  
  show_row_dend = FALSE,      
  show_column_dend = FALSE,   
  
  show_row_names = TRUE,
  show_column_names = TRUE,
  
  row_names_side = "left",
  row_names_gp = gpar(fontsize = 8),
  column_names_gp = gpar(fontsize = 8),
  
  heatmap_legend_param = list(
    at = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0", "0.25", "0.5", "0.75", "1"),
    title = "Accuracy"
  ),
  
  width = unit(ncol(mat) * 6, "mm"),
  height = unit(nrow(mat) * 5, "mm"),
  
  # Add numbers inside cells
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.rect(x, y, width, height,
              gp = gpar(fill = fill, col = "grey40", lwd = 0.3))
    grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 6))
  }
)

draw(ht)

ht_drawn <- draw(ht)
row_order <- row_order(ht_drawn)
col_order <- column_order(ht_drawn)
carpet_df <- mat[row_order, col_order]
carpet_dod_val_accuracy <- as.data.frame(carpet_df)


#=========================================
# Adding the weighted accuracy
#=========================================

weighted_AP_CD <- weighted.mean(CD_validation$Accuracy_AP, CD_validation$n_samples)
weighted_FP_CD <- weighted.mean(CD_validation$Accuracy_FP, CD_validation$n_samples)
weighted_comp_CD <- weighted.mean(CD_validation$Accuracy_composite, CD_validation$n_samples)

weighted_AP_DOD <- weighted.mean(DOD_validation$Accuracy_AP, DOD_validation$n_samples)
weighted_FP_DOD <- weighted.mean(DOD_validation$Accuracy_FP, DOD_validation$n_samples)
weighted_comp_DOD <- weighted.mean(DOD_validation$Accuracy_composite, DOD_validation$n_samples)

################### HEATMAP ################

carpet_cd_val_accuracy$weighted_Mean <- c(round(weighted_FP_CD,3),round(weighted_comp_CD,3),round(weighted_AP_CD,3))
carpet_cd_val_accuracy <- as.matrix(carpet_cd_val_accuracy)

col_fun <- colorRamp2(
  c(0.3, 0.6, 0.8, 1),
  c("#fee8c8", "#fdbb84", "#fc8d59", "#e34a33"))

ht <- Heatmap(
  carpet_cd_val_accuracy,
  name = "Accuracy",
  col = col_fun,
  
  cluster_rows = F,
  cluster_columns = F,
  
  show_row_dend = FALSE,      
  show_column_dend = FALSE,   
  
  show_row_names = TRUE,
  show_column_names = TRUE,
  
  row_names_side = "left",
  row_names_gp = gpar(fontsize = 8),
  column_names_gp = gpar(fontsize = 8),
  
  heatmap_legend_param = list(
    at = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0", "0.25", "0.5", "0.75", "1"),
    title = "Accuracy"
  ),
  
  width = unit(ncol(carpet_cd_val_accuracy) * 6, "mm"),
  height = unit(nrow(carpet_cd_val_accuracy) * 5, "mm"),
  
  # Add numbers inside cells
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.rect(x, y, width, height,
              gp = gpar(fill = fill, col = "grey40", lwd = 0.3))
    grid.text(sprintf("%.2f", carpet_cd_val_accuracy[i, j]), x, y, gp = gpar(fontsize = 6))
  }
)

draw(ht)


############# DOD Validation #############

carpet_dod_val_accuracy$weighted_Mean <- c(round(weighted_FP_DOD,3),round(weighted_AP_DOD,3),round(weighted_comp_DOD,3))
carpet_dod_val_accuracy <- as.matrix(carpet_dod_val_accuracy)

col_fun <- colorRamp2(
  c(0.3, 0.6, 0.8, 1),
  c("#d0f0e0", "#a8ddb5", "#80cdc1", "#74a9cf"))

ht <- Heatmap(
  carpet_dod_val_accuracy,
  name = "Accuracy",
  col = col_fun,
  
  cluster_rows = F,
  cluster_columns = F,
  
  show_row_dend = FALSE,      
  show_column_dend = FALSE,   
  
  show_row_names = TRUE,
  show_column_names = TRUE,
  
  row_names_side = "left",
  row_names_gp = gpar(fontsize = 8),
  column_names_gp = gpar(fontsize = 8),
  
  heatmap_legend_param = list(
    at = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0", "0.25", "0.5", "0.75", "1"),
    title = "Accuracy"
  ),
  
  width = unit(ncol(carpet_dod_val_accuracy) * 6, "mm"),
  height = unit(nrow(carpet_dod_val_accuracy) * 5, "mm"),
  
  # Add numbers inside cells
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.rect(x, y, width, height,
              gp = gpar(fill = fill, col = "grey40", lwd = 0.3))
    grid.text(sprintf("%.2f", carpet_dod_val_accuracy[i, j]), x, y, gp = gpar(fontsize = 6))
  }
)

draw(ht)





