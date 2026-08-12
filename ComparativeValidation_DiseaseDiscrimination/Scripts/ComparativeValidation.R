#Man-whitney heatmaps
setwd("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\AM_MM_Comparison_ComparativeValidation\\Figure3_heatmaps_MifRix\\")

library(openxlsx)
library(reshape2)
library(ggplot2)
library(dplyr)

alpha <- 0.05

############################################################
# 1 Load Mann–Whitney results
############################################################

MW_medianDir <- read.xlsx("mann_whitney_results_gini_composite_score_with_MifRix.xlsx", sheet = 1, rowNames = TRUE)
MW_meanDir   <- read.xlsx("mann_whitney_results_gini_composite_score_with_MifRix.xlsx", sheet = 2, rowNames = TRUE)
MW_pvalue    <- read.xlsx("mann_whitney_results_gini_composite_score_with_MifRix.xlsx", sheet = 3, rowNames = TRUE)

############################################################
# 2 Clean p-values
############################################################

MW_pvalue_num <- MW_pvalue
MW_pvalue_num[MW_pvalue_num == "#NA"] <- NA

MW_pvalue_num <- as.data.frame(
  lapply(MW_pvalue_num, as.numeric),
  row.names = rownames(MW_pvalue)
)

############################################################
# 3 Combine direction + significance
############################################################

combined_meandir <- ifelse(
  is.na(as.matrix(MW_meanDir)),
  NA,
  ifelse(
    as.matrix(MW_pvalue_num) <= alpha,
    as.matrix(MW_meanDir),
    0
  )
)

combined_meandir <- as.data.frame(combined_meandir)
rownames(combined_meandir) <- rownames(MW_meanDir)

combined_mediandir <- ifelse(
  is.na(as.matrix(MW_medianDir)),
  NA,
  ifelse(
    as.matrix(MW_pvalue_num) <= alpha,
    as.matrix(MW_medianDir),
    0
  )
)

combined_mediandir <- as.data.frame(combined_mediandir)
rownames(combined_mediandir) <- rownames(MW_medianDir)

############################################################
# 4 Column order
############################################################

new_order <- c(
  "MiFRix-AP",
  "MiFRix-FP",
  "Composite_Gini",
  "DysbiosisScore",
  "HACKS",
  "GMWI",
  "GMWI2"
)

combined_meandir   <- combined_meandir[, new_order]
combined_mediandir <- combined_mediandir[, new_order]

############################################################
# 5 Load study metadata
############################################################

study_meta <- read.xlsx("study_list_SHAP_MW_heatmap.xlsx")

colnames(study_meta) <- c("Study","seq_type")

############################################################
# 6 Filter only required studies
############################################################

combined_meandir   <- combined_meandir[study_meta$Study, ]
combined_mediandir <- combined_mediandir[study_meta$Study, ]

############################################################
# sanity check
############################################################

setdiff(study_meta$Study, rownames(combined_meandir))

############################################################
# 7 Split study groups
############################################################

studies_16s_train <- study_meta %>%
  filter(seq_type %in% c("16s","WGS_training"))

studies_wgs <- study_meta %>%
  filter(seq_type == "WGS")

############################################################
# 8 Color palettes
############################################################

heatmap_colors <- c(
  "1"  = "skyblue1",
  "0"  = "white",
  "-1" = "gold1"
)

seq_colors <- c(
  "16s" = "#F4A6A6",
  "WGS" = "#CFA0E9",
  "WGS_training" = "#9AD9D5"
)

############################################################
# 9 Heatmap plotting function
############################################################

plot_mw_heatmap <- function(df, meta_df, metrics, filename){
  
  df <- df[meta_df$Study, metrics]
  
  mw_melt <- melt(as.matrix(df),
                  varnames = c("Study","Metric"),
                  value.name = "Rank")
  
  mw_melt$Label <- ifelse(is.na(mw_melt$Rank),"@",as.character(mw_melt$Rank))
  
  mw_melt <- left_join(mw_melt, meta_df, by=c("Study"))
  
  p <- ggplot(mw_melt, aes(x=Metric, y=Study)) +
    
    geom_tile(aes(fill=Label),
              color="gray10",
              linewidth=0.4) +
    
    geom_tile(aes(x=-0.5, fill=seq_type),
              width=0.5,
              color = "gray10",
              linewidth = 0.3,
              show.legend=TRUE) +
    
    scale_fill_manual(values=c(heatmap_colors,seq_colors),
                      breaks=c(names(heatmap_colors),names(seq_colors))) +
    
    coord_fixed(ratio=0.6) +
    
    theme_minimal(base_size=16) +
    
    theme(
      axis.text.x=element_text(angle=90,vjust=0.5,hjust=1,size=10),
      axis.text.y=element_text(size=10),
      panel.grid=element_blank(),
      legend.position="right"
    ) +
    
    labs(x="",y="")
  
  ggsave(filename,p,height=7,width=5)
}

############################################################
# 10 Define metric groups
############################################################

metrics_4 <- c(
  "MiFRix-AP",
  "MiFRix-FP",
  "Composite_Gini",
  "DysbiosisScore",
  "HACKS"
)

metrics_6 <- c(
  "MiFRix-AP",
  "MiFRix-FP",
  "Composite_Gini",
  "DysbiosisScore",
  "HACKS",
  "GMWI",
  "GMWI2"
)

############################################################
# 11 Generate heatmaps
############################################################

plot_mw_heatmap(
  combined_meandir,
  studies_16s_train,
  metrics_4,
  "MW_meandir_16s.pdf"
)

plot_mw_heatmap(
  combined_meandir,
  studies_wgs,
  metrics_6,
  "MW_meandir_WGS.pdf"
)


##########################################################################################
#SHAP heatmaps


library(openxlsx)
library(reshape2)
library(ggplot2)
library(dplyr)

############################################################
# Load study metadata
############################################################

study_meta <- read.xlsx("study_list_SHAP_MW_heatmap.xlsx")

colnames(study_meta) <- c("Study","seq_type")

############################################################
# Load SHAP rank sheets
############################################################

shap_without <- read.xlsx(
  "SHAP_Ranks_By_Study.xlsx",
  sheet="No_GMWI_Ranks"
)

shap_with <- read.xlsx(
  "SHAP_Ranks_By_Study.xlsx",
  sheet="Complete_Ranks"
)

############################################################
# Set study_name as rownames
############################################################

rownames(shap_without) <- shap_without$study_name
rownames(shap_with) <- shap_with$study_name

shap_without$study_name <- NULL
shap_with$study_name <- NULL

############################################################
# Function to convert Rank columns -> metric matrix
############################################################

convert_rank_matrix <- function(df, metrics){
  
  result <- matrix(
    NA,
    nrow=nrow(df),
    ncol=length(metrics),
    dimnames=list(rownames(df),metrics)
  )
  
  for(i in 1:nrow(df)){
    
    row_vals <- as.character(df[i,])
    
    for(j in 1:length(row_vals)){
      
      metric <- row_vals[j]
      
      if(metric %in% metrics){
        result[i,metric] <- j
      }
    }
  }
  
  result <- as.data.frame(result)
  
  return(result)
}

############################################################
# Define metric sets
############################################################

metrics4 <- c(
  "MiFRix-AP",
  "MiFRix-FP",
  "Composite_Gini",
  "DysbiosisScore",
  "HACKS"
)

metrics6 <- c(
  "MiFRix-AP",
  "MiFRix-FP",
  "Composite_Gini",
  "DysbiosisScore",
  "HACKS",
  "GMWI",
  "GMWI2"
)

############################################################
# Convert matrices
############################################################

shap_matrix_without <- convert_rank_matrix(
  shap_without,
  metrics4
)

shap_matrix_with <- convert_rank_matrix(
  shap_with,
  metrics6
)

############################################################
# Order studies using study list
############################################################

# find common studies
common_without <- intersect(study_meta$MW_SHAP_study_name, rownames(shap_matrix_without))
common_with <- intersect(study_meta$MW_SHAP_study_name, rownames(shap_matrix_with))

# reorder according to study_meta order
ordered_without <- study_meta$MW_SHAP_study_name[study_meta$MW_SHAP_study_name %in% common_without]
ordered_with <- study_meta$MW_SHAP_study_name[study_meta$MW_SHAP_study_name %in% common_with]

# subset matrices
shap_matrix_without <- shap_matrix_without[ordered_without, ]
shap_matrix_with <- shap_matrix_with[ordered_with, ]

#sanity
setdiff(study_meta$MW_SHAP_study_name, rownames(shap_matrix_without))
setdiff(study_meta$MW_SHAP_study_name, rownames(shap_matrix_with))
any(is.na(shap_matrix_without))

############################################################
# Color palettes
############################################################

rank_colors <- c(
  "1" = "steelblue2",  # light blue
  "2" = "#90EE90",  # light green
  "3" = "#FFFACD",  # peach puff
  "4" = "#E6E6FA",  # lavender
  "5" = "#FFB6C1",  # light pink
  "6" = "tomato3"
)


seq_colors <- c(
  "16s"="#F4A6A6",
  "WGS"="#CFA0E9",
  "WGS_training"="#9AD9D5"
)

############################################################
# Heatmap plotting function
############################################################

plot_shap_heatmap <- function(df, meta_df, filename){
  
  melt_df <- melt(
    as.matrix(df),
    varnames = c("Study", "Metric"),
    value.name = "Rank"
  )
  
  melt_df$Label <- as.character(melt_df$Rank)
  
  melt_df <- left_join(
    melt_df,
    meta_df,
    by = c("Study" = "MW_SHAP_study_name")
  )
  
  p <- ggplot(
    melt_df,
    aes(x = Metric, y = Study)
  ) +
    
    geom_tile(
      aes(fill = Label),
      color = "gray10",
      linewidth = 0.4
    ) +
    
    geom_text(
      aes(label = Label),
      size = 2.7
    ) +
    
    geom_tile(
      aes(x = -0.55, fill = seq_type),
      width = 0.45,
      color = "gray10",
      linewidth = 0.3
    ) +
    
    scale_fill_manual(
      values = c(rank_colors, seq_colors)
    ) +
    
    coord_fixed(ratio = 0.6) +
    
    theme_minimal(base_size = 16) +
    
    theme(
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 10
      ),
      axis.text.y = element_text(size = 10),
      panel.grid = element_blank(),
      legend.position = "right"
    ) +
    
    labs(
      x = "",
      y = ""
    )
  
  ggsave(
    filename,
    p,
    height = 7,
    width = 5
  )
}

############################################################
# Generate SHAP heatmaps
############################################################

plot_shap_heatmap(
  shap_matrix_without,
  study_meta,
  "SHAP_heatmap_16S.pdf")

plot_shap_heatmap(
  shap_matrix_with,
  study_meta,
  "SHAP_heatmap_WGS.pdf")
























