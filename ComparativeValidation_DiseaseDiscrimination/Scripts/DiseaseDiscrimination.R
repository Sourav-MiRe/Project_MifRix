setwd("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\GitHub_ScriptsData\\ComparativeValidation_DiseaseDiscrimination\\DATA\\")

########## Data Preparation #############

load("All_Disease_Scores_Test_Validation_AP_FP.RData")

library(dplyr)
library(xlsx)

################   TESTING DATA ######################
#### Data Preparation ####
library(tidyr)

# Disease columns (exclude diseaseCat)
disease_cols <- setdiff(colnames(AM_test), "diseaseCat")

# Add sample_id
AM_test$sample_id <- rownames(AM_test)

# Expand multi-disease rows
AM_long <- AM_test %>%
  separate_rows(diseaseCat, sep = ";")

# Unique diseases
diseases <- unique(AM_long$diseaseCat)

for (d in diseases) {
  
  # Subset samples belonging to that disease
  df_sub <- AM_long %>% filter(diseaseCat == d)
  
  # Disease risk
  disease_risk <- df_sub[[d]]
  
  # Non-disease risk (mean of other disease columns)
  other_cols <- setdiff(disease_cols, d)
  non_disease_risk <- rowMeans(df_sub[, other_cols], na.rm = TRUE)
  
  # Create dataframe
  out_df <- data.frame(
    disease_risk = disease_risk,
    non_disease_risk = non_disease_risk
  )
  
  rownames(out_df) <- df_sub$sample_id
  
  # Name object
  obj_name <- paste0(d, "_RiskScores_AM")
  
  assign(obj_name, out_df, envir = .GlobalEnv)
}


objects_to_save <- ls(pattern = "_RiskScores_AM$")


save(list = objects_to_save, file = "Disease_NonDisease_Testing_AP_RiskScore.RData")


#### Similarly Data preparation for FP #####

# Disease columns (exclude diseaseCat)
disease_cols <- setdiff(colnames(MM_test), "diseaseCat")

# Add sample_id
MM_test$sample_id <- rownames(MM_test)

# Expand multi-disease rows
MM_long <- MM_test %>%
  separate_rows(diseaseCat, sep = ";")

# Unique diseases
diseases <- unique(MM_long$diseaseCat)

for (d in diseases) {
  
  # Subset samples belonging to that disease
  df_sub <- MM_long %>% filter(diseaseCat == d)
  
  # Disease risk
  disease_risk <- df_sub[[d]]
  
  # Non-disease risk (mean of other disease columns)
  other_cols <- setdiff(disease_cols, d)
  non_disease_risk <- rowMeans(df_sub[, other_cols], na.rm = TRUE)
  
  # Create dataframe
  out_df <- data.frame(
    disease_risk = disease_risk,
    non_disease_risk = non_disease_risk
  )
  
  rownames(out_df) <- df_sub$sample_id
  
  # Name object
  obj_name <- paste0(d, "_RiskScores_MM")
  
  assign(obj_name, out_df, envir = .GlobalEnv)
}


objects_to_save <- ls(pattern = "_RiskScores_MM$")


save(list = objects_to_save, file = "Disease_NonDisease_Testing_FP_RiskScore.RData")


#### Plotting ####


plot_risk_scores_combined <- function(data_list, disease_names, filename) {
  
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  
  base_path <- "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\GitHub_ScriptsData\\ComparativeValidation_DiseaseDiscrimination\\DATA\\"
  file_path <- paste0(base_path, filename)
  
  #box_colors <- c("#B2DF8A", "#CAB2D6")
  box_colors <- c("#FF62BC", "#00BF7D")
  box_fill <- "white" 
  
  combined_data <- do.call(rbind, Map(function(df, disease) {
    df$Disease <- disease
    return(df)
  }, data_list, disease_names))
  
  table_long <- combined_data %>%
    pivot_longer(cols = c("disease_risk", "non_disease_risk"), 
                 names_to = "Group", 
                 values_to = "Value")
  
  table_long$Group <- factor(table_long$Group, 
                             levels = c("disease_risk", "non_disease_risk"),
                             labels = c("Disease", "Others"))
  
  table_long$CombinedLabel <- paste(table_long$Disease, table_long$Group, sep = " - ")
  
  p <- ggplot(table_long, aes(x = CombinedLabel, y = Value, fill = Group)) +
    geom_boxplot(outlier.shape = NA, fill = box_fill, color = "black") +  
    geom_jitter(width = 0.15, size = 0.5, alpha = 1, aes(color = Group)) +  
    scale_fill_manual(values = c("Disease" = box_fill, "Others" = box_fill)) +  
    scale_color_manual(values = box_colors) +  
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme(
      legend.position = "none", 
      panel.grid = element_blank(),  
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),  
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  
      axis.text.y = element_text(size = 8),  
      axis.title.y = element_text(size = 12),  
      plot.title = element_blank()
    ) +
    scale_y_continuous(limits = c(0, 1))  
  
  ggsave(filename = file_path, plot = p, height = 3, width = 8, dpi = 1200)
}

data_list_AM <- mget(ls(pattern = "_RiskScores_AM$"))

data_list_MM <- mget(ls(pattern = "_RiskScores_MM$"))

disease_names <- sub("_RiskScores_AM$", "", names(data_list_AM))

plot_risk_scores_combined(data_list_AM, disease_names, "combined_risk_scores_AP.pdf")
plot_risk_scores_combined(data_list_MM, disease_names, "combined_risk_scores_FP.pdf")


############### Paired Wilcoxon Test ##############

get_pvalues <- function(data_list) {
  
  pvals <- sapply(data_list, function(df) {
    
    wilcox.test(df$disease_risk,
                df$non_disease_risk,
                paired = TRUE)$p.value
  })
  
  data.frame(
    Disease = names(pvals),
    p_value = pvals,
    row.names = NULL
  )
}

pvalues_AP <- get_pvalues(data_list_AM)
pvalues_FP <- get_pvalues(data_list_MM)

write.xlsx(pvalues_AP, "pvalues_AP_testing.xlsx")
write.xlsx(pvalues_FP, "pvalues_FP_testing.xlsx")


#-------------- Testing Boxplots Composite ------------

load("Disease_NonDisease_Testing_FP_RiskScore.RData")
load("Disease_NonDisease_Testing_AP_RiskScore.RData")

plot_composite_scores_combined <- function(data_list_AP, data_list_FP, disease_names, filename) {
  
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(DescTools)
  
  base_path <- "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\GitHub_ScriptsData\\ComparativeValidation_DiseaseDiscrimination\\DATA\\"
  file_path <- paste0(base_path, filename)
  
  # Colors
  box_colors <- c("#FF62BC", "#00BF7D")
  box_fill <- "white"
  
  #---------------------------------------------------------
  # Calculate Composite Scores
  #---------------------------------------------------------
  
  composite_list <- Map(function(ap_df, fp_df, disease) {
    
    common_samples <- intersect(rownames(ap_df), rownames(fp_df))
    
    ap_df <- ap_df[common_samples, ]
    fp_df <- fp_df[common_samples, ]
    
    composite_df <- data.frame(
      row.names = common_samples,
      
      disease_composite =
        apply(cbind(ap_df$disease_risk,
                    fp_df$disease_risk), 1,
              function(x) {
                mean(x) * (1 - Gini(x))
              }),
      
      non_disease_composite =
        apply(cbind(ap_df$non_disease_risk,
                    fp_df$non_disease_risk), 1,
              function(x) {
                mean(x) * (1 - Gini(x))
              })
    )
    
    composite_df$Disease <- disease
    
    return(composite_df)
    
  }, data_list_AP, data_list_FP, disease_names)
  
  combined_data <- do.call(rbind, composite_list)
  
  #---------------------------------------------------------
  # Convert to long format
  #---------------------------------------------------------
  
  table_long <- combined_data %>%
    pivot_longer(
      cols = c("disease_composite", "non_disease_composite"),
      names_to = "Group",
      values_to = "Value"
    )
  
  table_long$Group <- factor(
    table_long$Group,
    levels = c("disease_composite", "non_disease_composite"),
    labels = c("Disease", "Others")
  )
  
  table_long$CombinedLabel <- paste(
    table_long$Disease,
    table_long$Group,
    sep = " - "
  )
  
  #---------------------------------------------------------
  # Plot
  #---------------------------------------------------------
  
  p <- ggplot(table_long,
              aes(x = CombinedLabel,
                  y = Value,
                  fill = Group)) +
    
    geom_boxplot(
      outlier.shape = NA,
      fill = box_fill,
      color = "black"
    ) +
    
    geom_jitter(
      width = 0.15,
      size = 0.5,
      alpha = 1,
      aes(color = Group)
    ) +
    
    scale_fill_manual(
      values = c("Disease" = box_fill,
                 "Others" = box_fill)
    ) +
    
    scale_color_manual(values = box_colors) +
    
    theme_minimal() +
    
    labs(
      x = NULL,
      y = "Composite Score"
    ) +
    
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.7
      ),
      axis.text.x = element_text(
        size = 8,
        angle = 45,
        hjust = 1
      ),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 12),
      plot.title = element_blank()
    ) +
    
    scale_y_continuous(limits = c(0, 1))
  
  ggsave(
    filename = file_path,
    plot = p,
    height = 3,
    width = 8,
    dpi = 1200
  )
}

#---------------------------------------------------------
# Get AP and FP dataframes
#---------------------------------------------------------

data_list_AP <- mget(ls(pattern = "_RiskScores_AM$"))
data_list_FP <- mget(ls(pattern = "_RiskScores_MM$"))

# Disease names
disease_names <- sub("_RiskScores_AM$", "", names(data_list_AP))

#---------------------------------------------------------
# Generate Composite Score Plot
#---------------------------------------------------------

plot_composite_scores_combined(
  data_list_AP = data_list_AP,
  data_list_FP = data_list_FP,
  disease_names = disease_names,
  filename = "combined_composite_scores.pdf")

############ COMPOSITE SCORE TESTING P VALUES ######################

get_composite_pvalues <- function(
    data_list_AP,
    data_list_FP,
    disease_names
) {
  
  pval_df <- Map(
    
    function(ap_df, fp_df, disease) {
      
      common_samples <- intersect(
        rownames(ap_df),
        rownames(fp_df)
      )
      
      ap_df <- ap_df[common_samples, ]
      fp_df <- fp_df[common_samples, ]
      
      ######################################################
      # Composite scores
      ######################################################
      
      disease_composite <- apply(
        cbind(
          ap_df$disease_risk,
          fp_df$disease_risk
        ),
        1,
        function(x) {
          mean(x) * (1 - Gini(x))
        }
      )
      
      non_disease_composite <- apply(
        cbind(
          ap_df$non_disease_risk,
          fp_df$non_disease_risk
        ),
        1,
        function(x) {
          mean(x) * (1 - Gini(x))
        }
      )
      
      ######################################################
      # Paired Wilcoxon test
      ######################################################
      
      pval <- wilcox.test(
        disease_composite,
        non_disease_composite,
        paired = TRUE
      )$p.value
      
      data.frame(
        Disease = disease,
        p_value = pval
      )
      
    },
    
    data_list_AP,
    data_list_FP,
    disease_names
  )
  
  pval_df <- do.call(rbind, pval_df)
  
  return(pval_df)
}


pvalues_composite <- get_composite_pvalues(data_list_AP,data_list_FP,disease_names)

write.xlsx(pvalues_composite,"pvalues_composite_scores_testing.xlsx")

#==============================================================
rm(list = ls())

load("Disease_NonDisease_Validation_FP_RiskScore.RData")

df_names <- ls()

p_values_FP <- data.frame(
  DataFrame = df_names,
  P_Value = NA,
  Median_Diff = NA
)

for (i in seq_along(df_names)) {
  df <- get(df_names[i])
  
  disease_med <- median(df$disease_risk, na.rm = TRUE)
  nondisease_med <- median(df$non_disease_risk, na.rm = TRUE)
  
  test_result <- wilcox.test(df$disease_risk, df$non_disease_risk)
  
  p_values_FP$P_Value[i] <- test_result$p.value
  p_values_FP$Median_Diff[i] <- disease_med - nondisease_med
}

p_values_FP_ordered <- p_values_FP[order(p_values_FP$P_Value, -p_values_FP$Median_Diff),]
write.xlsx(p_values_FP_ordered,"p_values_FP_ordered.xlsx")

df_names_FP_overall <- p_values_FP_ordered$DataFrame

risk_data <- lapply(df_names_FP_overall, function(df) {
  get(df)[, c("disease_risk", "non_disease_risk")]
})

risk_data_flat <- do.call(c, risk_data)

plot_names <- gsub("_MM", "", df_names_FP_overall)

pdf("validation_FP_new.pdf",height = 3, width = 10)
par(mar = c(7, 4, 1, 2)) 
beanplot(risk_data_flat, 
         side = "both", 
         what = c(1, 1, 1, 0), 
         overallline = "median", 
         col = list("#9590FF", "gold"),
         #col = list("cyan","burlywood1"),
         names = plot_names, 
         las = 2, cex.axis = 0.8)  
legend("topright", 
       legend = c("Disease", "Non-disease"), 
       fill = c("#9590FF", "gold"), 
       border = "black", 
       bty = "n", 
       cex = 0.8)
dev.off()

rm(risk_data,risk_data_flat,df_names,plot_names,df)


#================================================================

rm(list = ls())

library(beanplot)

load("Disease_NonDisease_Validation_AP_RiskScore.RData")

df_names <- ls()

p_values_AP <- data.frame(
  DataFrame = df_names,
  P_Value = NA,
  Median_Diff = NA
)

for (i in seq_along(df_names)) {
  df <- get(df_names[i])
  
  disease_med <- median(df$disease_risk, na.rm = TRUE)
  nondisease_med <- median(df$non_disease_risk, na.rm = TRUE)
  
  test_result <- wilcox.test(df$disease_risk, df$non_disease_risk)
  
  p_values_AP$P_Value[i] <- test_result$p.value
  p_values_AP$Median_Diff[i] <- disease_med - nondisease_med
}

p_values_AP_ordered <- p_values_AP[order(p_values_AP$P_Value, -p_values_AP$Median_Diff),]
write.xlsx(p_values_AP_ordered,"p_values_AP_ordered.xlsx")

df_names_AP_overall <- p_values_AP_ordered$DataFrame


risk_data <- lapply(df_names_AP_overall, function(df) {
  get(df)[, c("disease_risk", "non_disease_risk")]
})

risk_data_flat <- do.call(c, risk_data)

plot_names <- gsub("_AM", "", df_names_AP_overall)

pdf("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\DiseaseOtherDisease_CrossDiseaseAssociation\\selected_validation_AP_new.pdf",height = 3, width = 10)
par(mar = c(7, 4, 1, 2)) 
beanplot(risk_data_flat, 
         side = "both", 
         what = c(1, 1, 1, 0), 
         overallline = "median", 
         col = list("cyan", "gold"),
         #col = list("#9590FF","burlywood1"),
         names = plot_names, 
         las = 2, cex.axis = 0.8)  
legend("topright", 
       legend = c("Disease", "Non-disease"), 
       fill = c("cyan", "gold"), 
       border = "black", 
       bty = "n", 
       cex = 0.8)
dev.off()

rm(risk_data,risk_data_flat,df_names,plot_names,df)

#===============================================================

rm(list = ls())

load("Disease_NonDisease_Validation_Composite_RiskScore.RData")

df_names <- ls()

p_values_Composite <- data.frame(
  DataFrame = df_names,
  P_Value = NA,
  Median_Diff = NA
)

for (i in seq_along(df_names)) {
  df <- get(df_names[i])
  
  disease_med <- median(df$disease_risk, na.rm = TRUE)
  nondisease_med <- median(df$non_disease_risk, na.rm = TRUE)
  
  test_result <- wilcox.test(df$disease_risk, df$non_disease_risk)
  
  p_values_Composite$P_Value[i] <- test_result$p.value
  p_values_Composite$Median_Diff[i] <- disease_med - nondisease_med
}

p_values_Composite_ordered <- p_values_Composite[order(p_values_Composite$P_Value, -p_values_Composite$Median_Diff),]
write.xlsx(p_values_Composite_ordered,"p_values_Composite_ordered.xlsx")

df_names_Composite_main <- p_values_Composite_ordered$DataFrame

risk_data <- lapply(df_names_Composite_main, function(df) {
  get(df)[, c("disease_risk", "non_disease_risk")]
})

risk_data_flat <- do.call(c, risk_data)

plot_names <- gsub("_Composite", "", df_names_Composite_main)

pdf("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\DiseaseOtherDisease_CrossDiseaseAssociation\\selected_validation_Composite.pdf",height = 3, width = 10)
par(mar = c(7, 4, 1, 2)) 
beanplot(risk_data_flat, 
         side = "both", 
         what = c(1, 1, 1, 0), 
         overallline = "median",
         col = list("coral", "royalblue3"),
         #col = list("#9590FF", "gold"),
         #col = list("cyan","burlywood1"),
         names = plot_names, 
         las = 2, cex.axis = 0.8)  
legend("topright", 
       legend = c("Disease", "Non-disease"), 
       fill = c("coral", "royalblue3"), 
       border = "black", 
       bty = "n", 
       cex = 0.8)
dev.off()

rm(risk_data,risk_data_flat,df_names,plot_names,df)


p_values_Composite_ordered$Num_Samples <- sapply(
  p_values_Composite_ordered$DataFrame,
  function(x) nrow(get(x)))






