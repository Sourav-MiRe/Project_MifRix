
load("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\GitHub_ScriptsData\\DiseaseSharedSignatures\\DATA\\All_Disease_Testing_SHAP_MD_AP.RData")
load("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\GitHub_ScriptsData\\DiseaseSharedSignatures\\DATA\\All_Disease_Val_SHAP_MD_AP.RData")


MS_testing_val <- as.data.frame(rbind(MS_SHAP_AP,MS_SHAP_AP_VAL_DOD))
CRC_testing_val <- as.data.frame(rbind(CRC_SHAP_AP,CRC_SHAP_AP_VAL_DOD))
T2D_testing_val <- as.data.frame(rbind(T2D_SHAP_AP,T2D_SHAP_AP_VAL_DOD))
major_depressive_disorder_testing_val <- as.data.frame(rbind(major_depressive_disorder_SHAP_AP,major_depressive_disorder_SHAP_AP_VAL_DOD))
IBD_GutInflammation_testing_val <- as.data.frame(rbind(IBD_GutInflammation_SHAP_AP,IBD_GutInflammation_SHAP_AP_VAL_DOD))
Schizophrenia_testing_val <- as.data.frame(rbind(Schizophrenia_SHAP_AP,Schizophrenia_SHAP_AP_VAL_DOD))
CVD_testing_val <- as.data.frame(rbind(CVD_SHAP_AP,CVD_SHAP_AP_VAL_DOD))
IBS_testing_val <- as.data.frame(rbind(IBS_SHAP_AP,IBS_SHAP_AP_VAL_DOD))
Parkinsons_testing_val <- as.data.frame(rbind(Parkinsons_SHAP_AP,Parkinsons_SHAP_AP_VAL_DOD))
AD_testing_val <- as.data.frame(rbind(AD_SHAP_AP,AD_SHAP_AP_VAL_DOD))


MS_AP_combined <- rbind(MS_AP[, intersect(colnames(MS_AP),colnames(MS_AP_VAL_DOD)), drop = FALSE],
                        MS_AP_VAL_DOD[, intersect(colnames(MS_AP),colnames(MS_AP_VAL_DOD)), drop = FALSE])
CRC_AP_combined <- rbind(CRC_AP[, intersect(colnames(CRC_AP),colnames(CRC_AP_VAL_DOD)), drop = FALSE],
                        CRC_AP_VAL_DOD[, intersect(colnames(CRC_AP),colnames(CRC_AP_VAL_DOD)), drop = FALSE])
T2D_AP_combined <- rbind(T2D_AP[, intersect(colnames(T2D_AP),colnames(T2D_AP_VAL_DOD)), drop = FALSE],
                        T2D_AP_VAL_DOD[, intersect(colnames(T2D_AP),colnames(T2D_AP_VAL_DOD)), drop = FALSE])
major_depressive_disorder_AP_combined <- rbind(major_depressive_disorder_AP[, intersect(colnames(major_depressive_disorder_AP),colnames(major_depressive_disorder_AP_VAL_DOD)), drop = FALSE],
                         major_depressive_disorder_AP_VAL_DOD[, intersect(colnames(major_depressive_disorder_AP),colnames(major_depressive_disorder_AP_VAL_DOD)), drop = FALSE])
IBD_GutInflammation_AP_combined <- rbind(IBD_GutInflammation_AP[, intersect(colnames(IBD_GutInflammation_AP),colnames(IBD_GutInflammation_AP_VAL_DOD)), drop = FALSE],
                         IBD_GutInflammation_AP_VAL_DOD[, intersect(colnames(IBD_GutInflammation_AP),colnames(IBD_GutInflammation_AP_VAL_DOD)), drop = FALSE])
Schizophrenia_AP_combined <- rbind(Schizophrenia_AP[, intersect(colnames(Schizophrenia_AP),colnames(Schizophrenia_AP_VAL_DOD)), drop = FALSE],
                         Schizophrenia_AP_VAL_DOD[, intersect(colnames(Schizophrenia_AP),colnames(Schizophrenia_AP_VAL_DOD)), drop = FALSE])
CVD_AP_combined <- rbind(CVD_AP[, intersect(colnames(CVD_AP),colnames(CVD_AP_VAL_DOD)), drop = FALSE],
                         CVD_AP_VAL_DOD[, intersect(colnames(CVD_AP),colnames(CVD_AP_VAL_DOD)), drop = FALSE])
IBS_AP_combined <- rbind(IBS_AP[, intersect(colnames(IBS_AP),colnames(IBS_AP_VAL_DOD)), drop = FALSE],
                         IBS_AP_VAL_DOD[, intersect(colnames(IBS_AP),colnames(IBS_AP_VAL_DOD)), drop = FALSE])
Parkinsons_AP_combined <- rbind(Parkinsons_AP[, intersect(colnames(Parkinsons_AP),colnames(Parkinsons_AP_VAL_DOD)), drop = FALSE],
                         Parkinsons_AP_VAL_DOD[, intersect(colnames(Parkinsons_AP),colnames(Parkinsons_AP_VAL_DOD)), drop = FALSE])
AD_AP_combined <- rbind(AD_AP[, intersect(colnames(AD_AP),colnames(AD_AP_VAL_DOD)), drop = FALSE],
                         AD_AP_VAL_DOD[, intersect(colnames(AD_AP),colnames(AD_AP_VAL_DOD)), drop = FALSE])



MS_NON_AP_combined <- rbind(MS_NON_AP[, intersect(colnames(MS_NON_AP),colnames(MS_NON_AP_VAL_DOD)), drop = FALSE],
                        MS_NON_AP_VAL_DOD[, intersect(colnames(MS_NON_AP),colnames(MS_NON_AP_VAL_DOD)), drop = FALSE])
CRC_NON_AP_combined <- rbind(CRC_NON_AP[, intersect(colnames(CRC_NON_AP),colnames(CRC_NON_AP_VAL_DOD)), drop = FALSE],
                         CRC_NON_AP_VAL_DOD[, intersect(colnames(CRC_NON_AP),colnames(CRC_NON_AP_VAL_DOD)), drop = FALSE])
T2D_NON_AP_combined <- rbind(T2D_NON_AP[, intersect(colnames(T2D_NON_AP),colnames(T2D_NON_AP_VAL_DOD)), drop = FALSE],
                         T2D_NON_AP_VAL_DOD[, intersect(colnames(T2D_NON_AP),colnames(T2D_NON_AP_VAL_DOD)), drop = FALSE])
major_depressive_disorder_NON_AP_combined <- rbind(major_depressive_disorder_NON_AP[, intersect(colnames(major_depressive_disorder_NON_AP),colnames(major_depressive_disorder_NON_AP_VAL_DOD)), drop = FALSE],
                                               major_depressive_disorder_NON_AP_VAL_DOD[, intersect(colnames(major_depressive_disorder_NON_AP),colnames(major_depressive_disorder_NON_AP_VAL_DOD)), drop = FALSE])
IBD_GutInflammation_NON_AP_combined <- rbind(IBD_GutInflammation_NON_AP[, intersect(colnames(IBD_GutInflammation_NON_AP),colnames(IBD_GutInflammation_NON_AP_VAL_DOD)), drop = FALSE],
                                         IBD_GutInflammation_NON_AP_VAL_DOD[, intersect(colnames(IBD_GutInflammation_NON_AP),colnames(IBD_GutInflammation_NON_AP_VAL_DOD)), drop = FALSE])
Schizophrenia_NON_AP_combined <- rbind(Schizophrenia_NON_AP[, intersect(colnames(Schizophrenia_NON_AP),colnames(Schizophrenia_NON_AP_VAL_DOD)), drop = FALSE],
                                   Schizophrenia_NON_AP_VAL_DOD[, intersect(colnames(Schizophrenia_NON_AP),colnames(Schizophrenia_NON_AP_VAL_DOD)), drop = FALSE])
CVD_NON_AP_combined <- rbind(CVD_NON_AP[, intersect(colnames(CVD_NON_AP),colnames(CVD_NON_AP_VAL_DOD)), drop = FALSE],
                         CVD_NON_AP_VAL_DOD[, intersect(colnames(CVD_NON_AP),colnames(CVD_NON_AP_VAL_DOD)), drop = FALSE])
IBS_NON_AP_combined <- rbind(IBS_NON_AP[, intersect(colnames(IBS_NON_AP),colnames(IBS_NON_AP_VAL_DOD)), drop = FALSE],
                         IBS_NON_AP_VAL_DOD[, intersect(colnames(IBS_NON_AP),colnames(IBS_NON_AP_VAL_DOD)), drop = FALSE])
Parkinsons_NON_AP_combined <- rbind(Parkinsons_NON_AP[, intersect(colnames(Parkinsons_NON_AP),colnames(Parkinsons_NON_AP_VAL_DOD)), drop = FALSE],
                                Parkinsons_NON_AP_VAL_DOD[, intersect(colnames(Parkinsons_NON_AP),colnames(Parkinsons_NON_AP_VAL_DOD)), drop = FALSE])
AD_NON_AP_combined <- rbind(AD_NON_AP[, intersect(colnames(AD_NON_AP),colnames(AD_NON_AP_VAL_DOD)), drop = FALSE],
                        AD_NON_AP_VAL_DOD[, intersect(colnames(AD_NON_AP),colnames(AD_NON_AP_VAL_DOD)), drop = FALSE])


datasets <- list(
  MS = MS_testing_val,
  CRC = CRC_testing_val,
  T2D = T2D_testing_val,
  major_depressive_disorder = major_depressive_disorder_testing_val,
  IBD_GutInflammation = IBD_GutInflammation_testing_val,
  Schizophrenia = Schizophrenia_testing_val,
  CVD = CVD_testing_val,
  IBS = IBS_testing_val,
  Parkinsons = Parkinsons_testing_val,
  AD = AD_testing_val)

# save(datasets,file = "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\AP_DOD_SHAP_Testing_Validation_combined.RData")
# 
# save(AD_AP_combined,AD_NON_AP_combined,CRC_AP_combined,CRC_NON_AP_combined,CVD_AP_combined,CVD_NON_AP_combined,IBS_AP_combined,IBS_NON_AP_combined,IBD_GutInflammation_AP_combined,IBD_GutInflammation_NON_AP_combined,major_depressive_disorder_AP_combined,major_depressive_disorder_NON_AP_combined,MS_AP_combined,MS_NON_AP_combined,Parkinsons_AP_combined,Parkinsons_NON_AP_combined,Schizophrenia_AP_combined,Schizophrenia_NON_AP_combined,T2D_AP_combined,T2D_NON_AP_combined,
#      file = "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\DOD_ClusterTaxaHeatmaps_TestingValidationCombined_AP\\DOD_AP_Testing_Validation_Combined.RData")

#------ Count number of taxa > 0 median SHAP in each disease ------

positive_median_counts <- sapply(names(datasets), function(disease) {
  
  df <- datasets[[disease]]
  
  species_df <- df[, !(colnames(df) %in% c("IsIndustrialized", "Is16s")),
                   drop = FALSE]
  
  medians <- apply(
    species_df,
    2,
    median,
    na.rm = TRUE
  )
  
  sum(medians > 0, na.rm = TRUE)
})

positive_median_counts

# ==========================================================
# Store top 50 species per disease
# ==========================================================

top50_species_per_disease <- list()

for(disease in names(datasets)) {
  
  cat("Processing top50:", disease, "\n")
  
  df <- datasets[[disease]]
  
  # Remove metadata columns
  species_df <- df[, !(colnames(df) %in% c("IsIndustrialized", "Is16s")),
                   drop = FALSE]
  
  # Median SHAP per species
  medians <- apply(
    species_df,
    2,
    median,
    na.rm = TRUE
  )
  
  # Keep only positive median SHAP species
  medians <- medians[medians > 0]
  
  # Sort decreasing
  medians <- sort(medians, decreasing = TRUE)
  
  # Top 50
  top50_species_per_disease[[disease]] <-
    medians[1:min(50, length(medians))]
}

# ==========================================================
# Union of top-50 species across diseases
# ==========================================================

all_species <- unique(
  unlist(
    lapply(top50_species_per_disease, names)
  )
)

cat("Total unique species in union =", length(all_species), "\n")

# ==========================================================
# Diseases
# ==========================================================

diseases <- names(top50_species_per_disease)

# ==========================================================
# Initialize matrices
# ==========================================================

library(effsize)

HedgesG_df <- matrix(
  NA_real_,
  nrow = length(all_species),
  ncol = length(diseases),
  dimnames = list(all_species, diseases)
)

WilcoxP_df <- matrix(
  NA_real_,
  nrow = length(all_species),
  ncol = length(diseases),
  dimnames = list(all_species, diseases)
)

Direction_df <- matrix(
  NA_real_,
  nrow = length(all_species),
  ncol = length(diseases),
  dimnames = list(all_species, diseases)
)

# ==========================================================
# Main loop
# ==========================================================

for (disease in diseases) {
  
  cat("\nProcessing:", disease, "\n")
  
  # --------------------------------------------------------
  # Abundance matrices
  # --------------------------------------------------------
  
  ap_df <- get(
    paste0(disease, "_AP_combined"),
    envir = .GlobalEnv
  )
  
  non_df <- get(
    paste0(disease, "_NON_AP_combined"),
    envir = .GlobalEnv
  )
  
  # --------------------------------------------------------
  # SHAP matrix
  # --------------------------------------------------------
  
  shap_df <- get(
    paste0(disease, "_testing_val"),
    envir = .GlobalEnv
  )
  
  species_shap <- shap_df[
    ,
    !(colnames(shap_df) %in% c("IsIndustrialized", "Is16s")),
    drop = FALSE
  ]
  
  # --------------------------------------------------------
  # Species from UNION having SHAP > 0
  # in at least 70% samples
  # --------------------------------------------------------
  
  candidate_species <- intersect(
    all_species,
    colnames(species_shap)
  )
  
  positive_fraction <- colMeans(
    species_shap[, candidate_species, drop = FALSE] > 0,
    na.rm = TRUE
  )
  
  species_70pct <- names(
    positive_fraction[
      positive_fraction >= 0.70
    ]
  )
  
  # --------------------------------------------------------
  # Disease-specific top50 species
  # --------------------------------------------------------
  
  top50_species <- names(
    top50_species_per_disease[[disease]]
  )
  
  # --------------------------------------------------------
  # Final species set
  # Top50 + union species with SHAP>0 in >=70%
  # --------------------------------------------------------
  
  species_to_test <- union(
    top50_species,
    species_70pct
  )
  
  cat(
    "Top50 =", length(top50_species),
    "| SHAP70 =", length(species_70pct),
    "| Total tested =", length(species_to_test),
    "\n"
  )
  
  # --------------------------------------------------------
  # Calculate statistics
  # --------------------------------------------------------
  
  for (sp in species_to_test) {
    
    # Species must exist in abundance matrices
    
    if (!(sp %in% colnames(ap_df)) ||
        !(sp %in% colnames(non_df))) {
      next
    }
    
    x1 <- ap_df[[sp]]
    x2 <- non_df[[sp]]
    
    x1 <- x1[!is.na(x1)]
    x2 <- x2[!is.na(x2)]
    
    if (length(x1) < 2 || length(x2) < 2) {
      next
    }
    
    # ------------------------------------------------------
    # Hedges G
    # ------------------------------------------------------
    
    g <- tryCatch({
      
      res <- cohen.d(
        x1,
        x2,
        hedges.correction = TRUE
      )
      
      as.numeric(res$estimate)
      
    }, error = function(e) {
      
      NA_real_
    })
    
    # ------------------------------------------------------
    # Wilcoxon p-value
    # ------------------------------------------------------
    
    p <- tryCatch({
      
      wilcox.test(
        x1,
        x2
      )$p.value
      
    }, error = function(e) {
      
      NA_real_
    })
    
    # ------------------------------------------------------
    # Store
    # ------------------------------------------------------
    
    HedgesG_df[sp, disease] <- g
    WilcoxP_df[sp, disease] <- p
    
    # ------------------------------------------------------
    # Direction score
    # ------------------------------------------------------
    
    direction <- 0
    
    if (!is.na(g) && !is.na(p)) {
      
      if (g > 0 && p <= 0.05) {
        
        direction <- 2
        
      } else if (g > 0 &&
                 p > 0.05 &&
                 p <= 0.10) {
        
        direction <- 1
        
      } else if (g < 0 &&
                 p <= 0.05) {
        
        direction <- -2
        
      } else if (g < 0 &&
                 p > 0.05 &&
                 p <= 0.10) {
        
        direction <- -1
        
      } else {
        
        direction <- 0
      }
    }
    
    Direction_df[sp, disease] <- direction
  }
}

# ==========================================================
# Convert to data frames
# ==========================================================

HedgesG_df  <- as.data.frame(HedgesG_df)
WilcoxP_df  <- as.data.frame(WilcoxP_df)
Direction_df <- as.data.frame(Direction_df)

save(HedgesG_df, WilcoxP_df, Direction_df, file = "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\DiseaseSignature_directionality_Testing_Validation.RData")


#------------------------------------------------------------
# Plotting 
#------------------------------------------------------------

# common_DAMP <- intersect(disease_taxa$species,names(carpet_SharedSignatures_directions))
# df <- Direction_df[rownames(Direction_df) %in% common_DAMP,]
# mat <- as.matrix(t(df))

load("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\DiseaseSignature_directionality_Testing_Validation.RData")

Direction_df[is.na(Direction_df)] <- 0

mat <- as.matrix(t(Direction_df))

mat_filtered <- mat[, colSums(mat != 0, na.rm = TRUE) >= 3]

library(ComplexHeatmap)
library(circlize)
library(grid)

col_fun <- colorRamp2(
  c(-2, -1, 0, 1, 2),
  c("#542788", "#B2ABD2", "#F7F7F7", "#FDB863", "#D95F0E")
)


ht <- Heatmap(
  mat_filtered,
  name = "Direction of Association",
  col = col_fun,
  cluster_rows = T,                   
  cluster_columns = T,               
  show_row_dend = T,                 
  show_column_dend = FALSE,
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_side = "left",
  row_names_gp = gpar(fontsize = 7.7),
  column_names_gp = gpar(fontsize = 7.7),
  heatmap_legend_param = list(
    at = c(-2, -1, 0, 1, 2),
    labels = c("-2", "-1", "0", "1", "2"),
    title = "Associations"
  ),
  width = unit(ncol(mat_filtered) * 3, "mm"),
  height = unit(nrow(mat_filtered) * 3, "mm"),
  
  # Use cell_fun instead of layer_fun
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.rect(x, y, width = width, height = height, 
              gp = gpar(fill = fill, col = "grey60", lwd = 0.4))
  }
)

pdf("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\SharedSignatures_directions_Testing_Val_Combined.pdf", 
    height = 8, width = 16)
draw(ht)
dev.off()

##### Carpet #####

ht_drawn <- draw(ht)
row_order <- row_order(ht_drawn)
col_order <- column_order(ht_drawn)
carpet_df <- mat_filtered[row_order, col_order]

carpet_SharedSignatures_directions <- as.data.frame(carpet_df)

library(xlsx)
write.xlsx(carpet_SharedSignatures_directions, "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\carpet_SharedSignatures_Testing_Validation_Combined.xlsx")


#--------------------------------------------------------------------
# Inter-disease correlation using Risk-Scores in Testing + Validation
#--------------------------------------------------------------------

load("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\DiseaseOtherDisease_CrossDiseaseAssociation\\All_Disease_Scores_Test_Validation_AM_MM.RData")
rm(MM_test,MM_Validation_Risk_Scores)
AM_Validation_Risk_Scores <- AM_Validation_Risk_Scores[,-11]
names(AM_Validation_Risk_Scores)[11] <- "diseaseCat"
all(names(AM_test) == names(AM_Validation_Risk_Scores))

AM_validation_RiskScores <- AM_Validation_Risk_Scores
AM_testing_RiskScores <- AM_test

AP_testing_RiskScores <- AM_testing_RiskScores
AP_validation_RiskScores <- AM_validation_RiskScores

rm(AM_testing_RiskScores,AM_validation_RiskScores)

save(AP_testing_RiskScores,AP_validation_RiskScores,file = "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\Overlaying_Validation_on_TestingData\\AP_testing_validation_RiskScores.RData")

AP_testing_val_combined_risks <- as.data.frame(rbind(AM_test,AM_Validation_Risk_Scores))


save(AP_testing_val_combined_risks, file = "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\AP_DOD_testing_val_combined_RiskScores.RData")



load("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\AP_DOD_testing_val_combined_RiskScores.RData")

combine_and_correlate_probabilities <- function(
    df,
    threshold = 0.5,
    drop_cols = c("study_name", "Source")
) {
  # Packages
  if (!requireNamespace("vegan", quietly = TRUE)) install.packages("vegan")
  if (!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")
  library(vegan)
  library(Hmisc)
  df <- as.data.frame(df)
  if (!is.data.frame(df)) stop("Input must be a data.frame")
  
  # 1) Remove unwanted columns if present
  drop_present <- intersect(drop_cols, colnames(df))
  if (length(drop_present) > 0) {
    df <- df[, setdiff(colnames(df), drop_present), drop = FALSE]
  }
  
  # 2) Keep only numeric columns
  combined_df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
  if (ncol(combined_df) == 0) {
    stop("No numeric columns available after filtering.")
  }
  
  # 3) Spearman correlation
  spearman_result <- rcorr(as.matrix(combined_df), type = "spearman")
  spearman_cor <- spearman_result$r
  spearman_p   <- spearman_result$P
  
  # 4) Jaccard distance (on binarized probabilities, diseases as features)
  binary_df <- (combined_df > threshold) * 1
  binary_df_t <- t(binary_df)
  jaccard_dist <- as.matrix(vegdist(binary_df_t, method = "jaccard", na.rm = TRUE))
  
  # 5) Kendall correlation + scaled distance
  kendall_cor  <- cor(combined_df, method = "kendall", use = "pairwise.complete.obs")
  kendall_dist <- (1 - kendall_cor) / 2
  diag(kendall_dist) <- 0
  
  # 6) Return exactly what you requested
  return(list(
    
    spearman_correlation = spearman_cor,
    spearman_pvalues = spearman_p,
    jaccard_distance = jaccard_dist,
    kendall_correlation = kendall_cor,
    kendall_distance = kendall_dist
  ))
}

AP_testing_validation_combined <- combine_and_correlate_probabilities(AP_testing_val_combined_risks, threshold = 0.5, drop_cols = c("diseaseCat"))


#-----------------------------------------------------------
# Cross-Disease Associations based on the shared signatures
#-----------------------------------------------------------

carpet_SharedSignatures_directions <- read.xlsx("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\carpet_SharedSignatures_Testing_Validation_Combined.xlsx", sheetName = 'Sheet1')
rownames(carpet_SharedSignatures_directions) <- carpet_SharedSignatures_directions$NA.
carpet_SharedSignatures_directions$NA. <- NULL

CrossDisease_signatures_df <- as.data.frame(carpet_SharedSignatures_directions)

dist_mat <- as.matrix(dist(CrossDisease_signatures_df,method = "manhattan"))

similarity_mat <- 1 - (dist_mat / max(dist_mat))

diag(similarity_mat) <- 1

similarity_mat <- as.data.frame(similarity_mat)


#----------------------------------------------------------------------------------
# Correlating overlapping features based and risk-score-based disease co-occurance
#----------------------------------------------------------------------------------

AP_DiseaseAssociation_Corr <- as.data.frame(round(AP_testing_validation_combined[["spearman_correlation"]],2))
AP_SharedSignature_df <- similarity_mat

AP_DiseaseAssociation_Corr <- AP_DiseaseAssociation_Corr[rownames(AP_SharedSignature_df),names(AP_SharedSignature_df)]

# Ensure same ordering
diseases <- rownames(AP_DiseaseAssociation_Corr)

result_list <- list()

k <- 1

for(i in 1:(length(diseases)-1)) {
  
  for(j in (i+1):length(diseases)) {
    
    pair_name <- paste(diseases[i], diseases[j], sep = "_")
    
    result_list[[k]] <- data.frame(
      disease_pair = pair_name,
      shared_signature = AP_SharedSignature_df[i, j],
      shared_risk = AP_DiseaseAssociation_Corr[i, j],
      stringsAsFactors = FALSE
    )
    
    k <- k + 1
  }
}

DiseaseSignatureRisk_df <- do.call(rbind, result_list)

rownames(DiseaseSignatureRisk_df) <- DiseaseSignatureRisk_df$disease_pair
DiseaseSignatureRisk_df$disease_pair <- NULL

save(DiseaseSignatureRisk_df, file = "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\DiseaseSignatureRisk_df.RData")

load("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\DiseaseSignatureRisk_df.RData")

library(ggplot2)
library(ggrepel)

ggplot(DiseaseSignatureRisk_df,
       aes(x = shared_signature, y = shared_risk)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()

# ggplot(DiseaseSignatureRisk_df,
#        aes(
#          x = shared_signature,
#          y = shared_risk,
#          label = rownames(DiseaseSignatureRisk_df))) +
#   geom_point(size = 3) +
#   geom_text_repel(size = 3) +
#   geom_smooth(method = "lm", se = TRUE) +
#   theme_minimal()


cor.test(DiseaseSignatureRisk_df$shared_signature,DiseaseSignatureRisk_df$shared_risk, method = 'spearman')


#------------------------------------------------------------
# Retaining Consistent Associations 
#------------------------------------------------------------

library(xlsx)

consistent_associations <- read.xlsx("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\DiseaseOtherDisease_CrossDiseaseAssociation\\ConsistencyNetwork\\Consistent_DiseaseAssociations.xlsx", sheetName = 'Sheet1')

consistent_pairs <- paste0(consistent_associations$Disease1,"_",consistent_associations$Disease2)

rownames(DiseaseSignatureRisk_df) <- gsub("IBD_GutInflammation","IBD",rownames(DiseaseSignatureRisk_df))
rownames(DiseaseSignatureRisk_df) <- gsub("Schizophrenia","SCZ",rownames(DiseaseSignatureRisk_df))
rownames(DiseaseSignatureRisk_df) <- gsub("Parkinsons","PD",rownames(DiseaseSignatureRisk_df))
rownames(DiseaseSignatureRisk_df) <- gsub("major_depressive_disorder","MDD",rownames(DiseaseSignatureRisk_df))

intersect(rownames(DiseaseSignatureRisk_df), consistent_pairs)

DiseaseSignatureRisk_consistent <- DiseaseSignatureRisk_df[rownames(DiseaseSignatureRisk_df) %in% c("AD_SCZ","MS_CRC","PD_CRC","IBS_CRC","CRC_T2D","CVD_SCZ","CVD_MDD","AD_CVD","MS_CVD","CVD_T2D","IBS_T2D","MS_MDD","IBS_MDD","PD_MDD","MS_IBD","MS_T2D","PD_IBS","PD_SCZ","PD_MS","IBD_T2D"),]
DiseaseSignatureRisk_consistent_literature <- DiseaseSignatureRisk_df[rownames(DiseaseSignatureRisk_df) %in% c("AD_SCZ","PD_CRC","IBS_CRC","CRC_T2D","CVD_SCZ","AD_CVD","CVD_T2D","PD_IBS","PD_SCZ","PD_MS","IBD_T2D"),]


library(ggplot2)
library(ggrepel)

p <- ggplot(DiseaseSignatureRisk_consistent,
            aes(
              x = shared_signature,
              y = shared_risk,
              label = rownames(DiseaseSignatureRisk_consistent))) +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()

ggsave(filename = "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\AP\\Scatterplot_DiseaseRisk_SharedSignatures_TestingValidation.pdf",plot = p,width = 5,height = 4)

cor.test(DiseaseSignatureRisk_consistent$shared_signature,DiseaseSignatureRisk_consistent$shared_risk, method = 'spearman')

#----------------------------------------------------------
#               PROCUSTES Analysis
#----------------------------------------------------------

sig_dist  <- 1 - AP_SharedSignature_df
risk_dist <- (1 - AP_DiseaseAssociation_Corr)/2

library(ade4)

sig_pcoa <- dudi.pco(as.dist(sig_dist),scannf = FALSE,nf = 2)

risk_pcoa <- dudi.pco(as.dist(risk_dist),scannf = FALSE,nf = 2)

sig_coord <- sig_pcoa$li
risk_coord <- risk_pcoa$li

proc_test <- procuste.randtest(risk_coord,sig_coord,nrepet = 999)

#--------------------------------------------------
# Plot PCoA scatter plots for both
#--------------------------------------------------
# ---------------------------------------
# Shared signature PCoA
# ---------------------------------------

sig_plot_df <- sig_coord
sig_plot_df$Disease <- rownames(sig_plot_df)

p1 <- ggplot(
  sig_plot_df,
  aes(
    x = A1,
    y = A2,
    label = Disease
  )
) +
  geom_point(size = 4) +
  geom_text_repel(size = 4) +
  labs(
    title = "PCoA of Shared Signature Similarity",
    x = paste0(
      "PCoA1 (",
      round(100 * sig_pcoa$eig[1] / sum(sig_pcoa$eig), 1),
      "%)"
    ),
    y = paste0(
      "PCoA2 (",
      round(100 * sig_pcoa$eig[2] / sum(sig_pcoa$eig), 1),
      "%)"
    )
  ) +
  theme_minimal(base_size = 14)


# ---------------------------------------
# Disease-risk PCoA
# ---------------------------------------

risk_plot_df <- risk_coord
risk_plot_df$Disease <- rownames(risk_plot_df)

p2 <- ggplot(
  risk_plot_df,
  aes(
    x = A1,
    y = A2,
    label = Disease
  )
) +
  geom_point(size = 4) +
  geom_text_repel(size = 4) +
  labs(
    title = "PCoA of Disease-Risk Associations",
    x = paste0(
      "PCoA1 (",
      round(100 * risk_pcoa$eig[1] / sum(risk_pcoa$eig), 1),
      "%)"
    ),
    y = paste0(
      "PCoA2 (",
      round(100 * risk_pcoa$eig[2] / sum(risk_pcoa$eig), 1),
      "%)"
    )
  ) +
  theme_minimal(base_size = 14)


ggsave(filename = "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\PCoA_SharedSignature_TestingValidation.pdf",plot = p1,width = 5,height = 4)

ggsave(filename = "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\SharedSignatureAnalysis\\PCoA_DiseaseRisk_TestingValidation.pdf",plot = p2,width = 5,height = 4)









