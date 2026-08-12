setwd("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\GitHub_ScriptsData\\InterDiseaseCrosstalk\\DATA\\")

##################################################################
      ########### DATA Preparation ################

load("All_Disease_Scores_Test_Validation_AP_FP.RData")

library(dplyr)
library(tidyr)

disease_cols <- setdiff(colnames(AM_test), "diseaseCat")

AM_test$sample_id <- rownames(AM_test)

# split multi-disease labels
AM_long <- AM_test %>%
  separate_rows(diseaseCat, sep = ";")

# unique diseases
diseases <- unique(AM_long$diseaseCat)

for (d in diseases) {
  
  df_sub <- AM_long %>%
    filter(diseaseCat == d)
  
  out_df <- df_sub[, disease_cols]
  out_df <- as.data.frame(out_df)
  
  rownames(out_df) <- df_sub$sample_id
  
  obj_name <- paste0(d, "_CrossDisease_RiskScores_AM")
  
  assign(obj_name, out_df, envir = .GlobalEnv)
}

objects_to_save <- ls(pattern = "_CrossDisease_RiskScores_AM$")

save(list = objects_to_save, file = "CrossDisease_Testing_AP.RData")


########## For FP #################

disease_cols <- setdiff(colnames(MM_test), "diseaseCat")

MM_test$sample_id <- rownames(MM_test)

# split multi-disease labels
MM_long <- MM_test %>%
  separate_rows(diseaseCat, sep = ";")

# unique diseases
diseases <- unique(MM_long$diseaseCat)

for (d in diseases) {
  
  df_sub <- MM_long %>%
    filter(diseaseCat == d)
  
  out_df <- df_sub[, disease_cols]
  out_df <- as.data.frame(out_df)
  
  rownames(out_df) <- df_sub$sample_id
  
  obj_name <- paste0(d, "_CrossDisease_RiskScores_MM")
  
  assign(obj_name, out_df, envir = .GlobalEnv)
}

objects_to_save <- ls(pattern = "_CrossDisease_RiskScores_MM$")

save(list = objects_to_save, file = "CrossDisease_Testing_FP.RData")


################################################################
############## Validation Data Preparation ###################

# risk score columns
risk_cols <- setdiff(colnames(AM_Validation_Risk_Scores),
                     c("study_name","source_input_disease","sample_id"))

# add sample_id column
AM_Validation_Risk_Scores$sample_id <- rownames(AM_Validation_Risk_Scores)

# unique study-disease pairs
pairs <- unique(AM_Validation_Risk_Scores[,c("study_name","source_input_disease")])

for(i in seq_len(nrow(pairs))){
  
  study_i   <- pairs$study_name[i]
  disease_i <- pairs$source_input_disease[i]
  
  df_sub <- AM_Validation_Risk_Scores %>%
    filter(study_name == study_i,
           source_input_disease == disease_i)
  
  out_df <- df_sub[, risk_cols]
  
  # restore original sample ids
  rownames(out_df) <- df_sub$sample_id
  
  obj_name <- paste0(study_i,"_",disease_i,
                     "_CrossDisease_RiskScores_AM")
  
  assign(obj_name, out_df, envir = .GlobalEnv)
}

objects_to_save <- ls(pattern = "_CrossDisease_RiskScores_AM$")

save(list = objects_to_save, file = "CrossDisease_Validation_AP.RData")

#################### For FP in Validation #######################
# risk score columns
risk_cols <- setdiff(colnames(MM_Validation_Risk_Scores),
                     c("study_name","source_input_disease","sample_id"))

# add sample_id column
MM_Validation_Risk_Scores$sample_id <- rownames(MM_Validation_Risk_Scores)

# unique study-disease pairs
pairs <- unique(MM_Validation_Risk_Scores[,c("study_name","source_input_disease")])

for(i in seq_len(nrow(pairs))){
  
  study_i   <- pairs$study_name[i]
  disease_i <- pairs$source_input_disease[i]
  
  df_sub <- MM_Validation_Risk_Scores %>%
    filter(study_name == study_i,
           source_input_disease == disease_i)
  
  out_df <- df_sub[, risk_cols]
  
  # restore original sample ids
  rownames(out_df) <- df_sub$sample_id
  
  obj_name <- paste0(study_i,"_",disease_i,
                     "_CrossDisease_RiskScores_MM")
  
  assign(obj_name, out_df, envir = .GlobalEnv)
}

objects_to_save <- ls(pattern = "_CrossDisease_RiskScores_MM$")

save(list = objects_to_save, file = "CrossDisease_Validation_FP.RData")


################################################################
library(xlsx)
library(ggplot2)
library(tidyr)
library(scales)
library(stringr)
library(dplyr)

load("CrossDisease_Testing_AP.RData")

df_list_CrossDisease <- ls()

for (df_name in df_list_CrossDisease) {
  
  df <- get(df_name)
  
  table_long <- df %>%
    pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value")
  
  box_colors <- hue_pal()(10)  
  
  pdf_name <- paste0(tolower(str_extract(df_name, "^[^_]+")), "_CrossDisease_AP.pdf")
  
  p <- ggplot(table_long, aes(x = Feature, y = Value)) +
    geom_boxplot(
      outlier.shape = NA,  
      color = "black",  
      size = 0.3,  
      fill = "white"
    ) +
    geom_jitter(
      width = 0.18, 
      size = 0.001, 
      alpha = 0.8, 
      aes(color = Feature)
    ) +
    scale_color_manual(values = box_colors) +  
    theme_minimal() +
    labs(
      x = "Diseases",  
      y = "Values"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),  
      legend.position = "none"
    )
  
  ggsave(filename = paste0("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\DiseaseOtherDisease_CrossDiseaseAssociation\\CrossDisease_Plots_Testing_AP\\", pdf_name), 
         plot = p, height = 2.7, width = 2.6, dpi = 1200)
}

############## For Validation Studies ###########

load("CrossDisease_Validation_FP.RData")

df_list_CrossDisease <- ls(pattern = "_CrossDisease_RiskScores_MM$")

base_path <- "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\DiseaseOtherDisease_CrossDiseaseAssociation\\CrossDisease_Plots_Validation_FP\\"

for (df_name in df_list_CrossDisease) {
  
  df <- get(df_name)
  
  # reshape dataframe
  table_long <- df %>%
    pivot_longer(cols = everything(),
                 names_to = "Feature",
                 values_to = "Value")
  
  # color palette
  box_colors <- scales::hue_pal()(length(unique(table_long$Feature)))
  
  # create pdf name using study + disease
  pdf_name <- paste0(
    sub("_CrossDisease_RiskScores_MM$", "", df_name),
    "_CrossDisease_FP.pdf"
  )
  
  # plot
  p <- ggplot(table_long, aes(x = Feature, y = Value)) +
    geom_boxplot(
      outlier.shape = NA,
      color = "black",
      size = 0.3,
      fill = "white"
    ) +
    geom_jitter(
      width = 0.18,
      size = 0.1,
      alpha = 0.8,
      aes(color = Feature)
    ) +
    scale_color_manual(values = box_colors) +
    theme_minimal() +
    labs(
      x = "Diseases",
      y = "Risk score"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
      legend.position = "none"
    )
  
  # save figure
  ggsave(
    filename = paste0(base_path, pdf_name),
    plot = p,
    height = 2.7,
    width = 2.6,
    dpi = 1200
  )
}





#================================================
# Network Building 
#================================================

load("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\GitHub_ScriptsData\\InterDiseaseCrosstalk\\DATA\\All_testing_val_AP_FP_CS_RiskScores.RData")


library(dplyr)

# ---------------------------------------------------------
# Function for pairwise enrichment Fisher's exact test
# ---------------------------------------------------------

run_pairwise_enrichment <- function(df,
                                    threshold = 0.5,
                                    edge_type = "AP") {
  
  disease_cols <- colnames(df)[1:10]
  
  pairs <- combn(disease_cols, 2, simplify = FALSE)
  
  fisher_results <- data.frame()
  
  contingency_list <- list()
  
  for(pair in pairs) {
    
    target_disease <- pair[1]
    query_disease  <- pair[2]
    
    cat(target_disease, "vs", query_disease, "\n")
    
    # ---------------------------------------------------------
    # Samples with target disease > threshold
    # ---------------------------------------------------------
    
    target_samples <- df[df[[target_disease]] > threshold, ]
    
    X <- nrow(target_samples)
    
    if(X == 0) next
    
    # ---------------------------------------------------------
    # Query disease counts
    # ---------------------------------------------------------
    
    query_positive <- sum(
      target_samples[[query_disease]] > threshold,
      na.rm = TRUE
    )
    
    query_negative <- sum(
      target_samples[[query_disease]] <= threshold,
      na.rm = TRUE
    )
    
    # ---------------------------------------------------------
    # Other diseases
    # Excluding target + query disease
    # ---------------------------------------------------------
    
    other_diseases <- setdiff(
      disease_cols,
      c(target_disease, query_disease)
    )
    
    # Positive counts across all other diseases
    other_positive <- sum(
      sapply(other_diseases, function(d) {
        sum(target_samples[[d]] > threshold,
            na.rm = TRUE)
      })
    )
    
    # Negative counts across all other diseases
    other_negative <- sum(
      sapply(other_diseases, function(d) {
        sum(target_samples[[d]] <= threshold,
            na.rm = TRUE)
      })
    )
    
    # ---------------------------------------------------------
    # Contingency matrix
    # ---------------------------------------------------------
    
    cont_mat <- matrix(
      c(
        query_positive,
        query_negative,
        other_positive,
        other_negative
      ),
      nrow = 2,
      byrow = TRUE
    )
    
    rownames(cont_mat) <- c(
      query_disease,
      "Other_Diseases"
    )
    
    colnames(cont_mat) <- c(
      "Greater_0.5",
      "LessEqual_0.5"
    )
    
    # ---------------------------------------------------------
    # Fisher's exact test
    # ---------------------------------------------------------
    
    fisher_res <- fisher.test(cont_mat)
    
    # ---------------------------------------------------------
    # Store contingency matrix
    # ---------------------------------------------------------
    
    pair_name <- paste(
      target_disease,
      query_disease,
      sep = "_vs_"
    )
    
    contingency_list[[pair_name]] <- cont_mat
    
    # ---------------------------------------------------------
    # Direction
    # ---------------------------------------------------------
    
    direction <- ifelse(
      fisher_res$estimate > 1,
      "Positive",
      "Negative"
    )
    
    # ---------------------------------------------------------
    # Store results
    # ---------------------------------------------------------
    
    fisher_results <- rbind(
      fisher_results,
      data.frame(
        Source = target_disease,
        Target = query_disease,
        Edge_Type = edge_type,
        Odds_Ratio = fisher_res$estimate,
        Pvalue = fisher_res$p.value,
        Direction = direction,
        Query_Positive = query_positive,
        Query_Negative = query_negative,
        Other_Positive = other_positive,
        Other_Negative = other_negative
      )
    )
  }
  
  return(list(
    fisher_results = fisher_results,
    contingency_matrices = contingency_list
  ))
}


all_fisher_outputs <- list()

# AP
all_fisher_outputs[["AP_test"]] <- run_pairwise_enrichment(AP_test_overall,edge_type = "AP")

all_fisher_outputs[["AP_val"]] <- run_pairwise_enrichment(AP_val_overall,edge_type = "AP")

# FP
all_fisher_outputs[["FP_test"]] <- run_pairwise_enrichment(FP_test_overall,edge_type = "FP")

all_fisher_outputs[["FP_val"]] <- run_pairwise_enrichment(FP_val_overall,edge_type = "FP")

# Composite
all_fisher_outputs[["Composite_test"]] <- run_pairwise_enrichment(Composite_test_overall,edge_type = "Composite")

all_fisher_outputs[["Composite_val"]] <- run_pairwise_enrichment(Composite_val_overall,edge_type = "Composite")




create_network_edges <- function(AP_df,FP_df,Composite_df,dataset_label = "Testing") {
  
  
  merged_df <- AP_df %>%
    
    dplyr::select(
      Source,
      Target,
      Direction_AP = Direction,
      Pvalue_AP = Pvalue
    ) %>%
    
    inner_join(
      FP_df %>%
        dplyr::select(
          Source,
          Target,
          Direction_FP = Direction,
          Pvalue_FP = Pvalue
        ),
      by = c("Source", "Target")
    ) %>%
    
    inner_join(
      Composite_df %>%
        dplyr::select(
          Source,
          Target,
          Direction_Composite = Direction,
          Pvalue_Composite = Pvalue
        ),
      by = c("Source", "Target")
    )
  
  # ---------------------------------------------------------
  # Final network df
  # ---------------------------------------------------------
  
  network_df <- data.frame()
  
  for(i in 1:nrow(merged_df)) {
    
    row <- merged_df[i, ]
    
    edge_type <- "None"
    
    value <- 0
    
    # =========================================================
    # POSITIVE CONSISTENCY
    # =========================================================
    
    if(
      row$Direction_AP == "Positive" &
      row$Direction_FP == "Positive" &
      row$Direction_Composite == "Positive"
    ) {
      
      edge_type <- "Positive"
      
      n_sig <- sum(
        c(
          row$Pvalue_AP <= 0.05,
          row$Pvalue_FP <= 0.05,
          row$Pvalue_Composite <= 0.05
        )
      )
      
      if(n_sig == 3) {
        value <- 1
      }
      
      if(n_sig == 2) {
        value <- 0.7
      }
      
      if(n_sig == 1) {
        value <- 0.3
      }
    }
    
    # =========================================================
    # NEGATIVE CONSISTENCY
    # =========================================================
    
    if(
      row$Direction_AP == "Negative" &
      row$Direction_FP == "Negative" &
      row$Direction_Composite == "Negative"
    ) {
      
      edge_type <- "Negative"
      
      n_sig <- sum(
        c(
          row$Pvalue_AP <= 0.05,
          row$Pvalue_FP <= 0.05,
          row$Pvalue_Composite <= 0.05
        )
      )
      
      if(n_sig == 3) {
        value <- -1
      }
      
      if(n_sig == 2) {
        value <- -0.7
      }
      
      if(n_sig == 1) {
        value <- -0.3
      }
    }
    
    # ---------------------------------------------------------
    # Store
    # ---------------------------------------------------------
    
    network_df <- rbind(
      network_df,
      data.frame(
        Source = row$Source,
        Target = row$Target,
        Dataset = dataset_label,
        Edge_Type = edge_type,
        Value = value
      )
    )
  }
  
  return(network_df)
}


Testing_Network <- create_network_edges(
  
  AP_df =
    all_fisher_outputs[["AP_test"]]$fisher_results,
  
  FP_df =
    all_fisher_outputs[["FP_test"]]$fisher_results,
  
  Composite_df =
    all_fisher_outputs[["Composite_test"]]$fisher_results,
  
  dataset_label = "Testing")

Validation_Network <- create_network_edges(
  
  AP_df =
    all_fisher_outputs[["AP_val"]]$fisher_results,
  
  FP_df =
    all_fisher_outputs[["FP_val"]]$fisher_results,
  
  Composite_df =
    all_fisher_outputs[["Composite_val"]]$fisher_results,
  
  dataset_label = "Validation")

Testing_Network_trimmed <- Testing_Network[!Testing_Network$Value == 0,]
Validation_Network_trimmed <- Validation_Network[!Validation_Network$Value == 0,]

Testing_Network_trimmed <- Testing_Network_trimmed[,-3]
Validation_Network_trimmed <- Validation_Network_trimmed[,-3]

Testing_Network_trimmed$Source <- ifelse(Testing_Network_trimmed$Source == 'IBD_GutInflammation',"IBD",Testing_Network_trimmed$Source)
Testing_Network_trimmed$Source <- ifelse(Testing_Network_trimmed$Source == 'Parkinsons',"PD",Testing_Network_trimmed$Source)

Testing_Network_trimmed$Target <- ifelse(Testing_Network_trimmed$Target == 'major_depressive_disorder',"MDD",Testing_Network_trimmed$Target)
Testing_Network_trimmed$Target <- ifelse(Testing_Network_trimmed$Target == 'Parkinsons',"PD",Testing_Network_trimmed$Target)
Testing_Network_trimmed$Target <- ifelse(Testing_Network_trimmed$Target == 'Schizophrenia',"SCZ",Testing_Network_trimmed$Target)


Validation_Network_trimmed$Source <- ifelse(Validation_Network_trimmed$Source == 'IBD_GutInflammation',"IBD",Validation_Network_trimmed$Source)
Validation_Network_trimmed$Source <- ifelse(Validation_Network_trimmed$Source == 'Parkinsons',"PD",Validation_Network_trimmed$Source)
Validation_Network_trimmed$Source <- ifelse(Validation_Network_trimmed$Source == 'Schizophrenia',"SCZ",Validation_Network_trimmed$Source)

Validation_Network_trimmed$Target <- ifelse(Validation_Network_trimmed$Target == 'major_depressive_disorder',"MDD",Validation_Network_trimmed$Target)
Validation_Network_trimmed$Target <- ifelse(Validation_Network_trimmed$Target == 'Parkinsons',"PD",Validation_Network_trimmed$Target)
Validation_Network_trimmed$Target <- ifelse(Validation_Network_trimmed$Target == 'Schizophrenia',"SCZ",Validation_Network_trimmed$Target)


write.csv(Testing_Network_trimmed,"Testing_Network_trimmed.csv",row.names = FALSE, quote = F)
write.csv(Validation_Network_trimmed,"Validation_Network_trimmed.csv",row.names = FALSE, quote = F)





























