setwd("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\GitHub_ScriptsData\\InterventionAnalysis\\DATA\\")
load("Intervention_studies18_MifRix.RData")

library(dplyr)
library(stringr)
library(ggplot2)
library(xlsx)
library(psych)
library(DescTools)
library(tidyr)
library(scales)
library(ggrepel)
library(tidyverse)

ComputeCompositeScore <- function(AP, FP){
  
  if(class(AP)[1] != class(FP)[1]){
    stop("AP and FP must both be vectors or both be data frames.")
  }
  
  if(is.vector(AP) && is.numeric(AP)){
    
    if(length(AP) != length(FP)){
      stop("AP and FP vectors must have the same length.")
    }
    
    CS <- mapply(function(ap, fp){
      vals <- c(ap, fp)
      mean(vals) * (1 - Gini(vals))
    }, AP, FP)
    
    return(data.frame(AP = AP,FP = FP,CS = CS,stringsAsFactors = FALSE))
  }
  
  if(is.data.frame(AP)){
    
    if(!identical(colnames(AP), colnames(FP))){
      stop("AP and FP data frames must have identical columns.")
    }
    
    composite_df <- AP
    
    disease_cols <- names(AP)[sapply(AP, is.numeric)]
    
    for(disease in disease_cols){
      
      composite_df[[disease]] <-
        mapply(function(ap, fp){
          vals <- c(ap, fp)
          mean(vals) * (1 - Gini(vals))
        },
        AP[[disease]],
        FP[[disease]])
    }
    
    return(composite_df)
  }
  
  stop("Unsupported input type.")
}

#========================================
# DwiyantoJ_2023
#========================================

DwiyantoJ_2023_AP <- DwiyantoJ_2023_AM_MM_RS_metadata[,1:11]
names(DwiyantoJ_2023_AP) <- gsub("_AM","",names(DwiyantoJ_2023_AP))

DwiyantoJ_2023_FP <- DwiyantoJ_2023_AM_MM_RS_metadata[,14:24]
names(DwiyantoJ_2023_FP) <- gsub("_MM","",names(DwiyantoJ_2023_FP))

DwiyantoJ_2023_RS_CS <-  ComputeCompositeScore(DwiyantoJ_2023_AP,DwiyantoJ_2023_FP)
DwiyantoJ_2023_RS_CS$ethnicity <- DwiyantoJ_2023_AM_MM_RS_metadata$ethnicity

### For T2D ####

pdf("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\InterventionAnalysis\\DwiyantoJ_T2D_CS_ethnicity.pdf", height = 5, width = 4.5)
boxplot(DwiyantoJ_2023_RS_CS[DwiyantoJ_2023_RS_CS$ethnicity == 'Chinese','T2D'],
        DwiyantoJ_2023_RS_CS[DwiyantoJ_2023_RS_CS$ethnicity == 'Indian','T2D'],
        DwiyantoJ_2023_RS_CS[DwiyantoJ_2023_RS_CS$ethnicity == 'Jakun','T2D'],
        DwiyantoJ_2023_RS_CS[DwiyantoJ_2023_RS_CS$ethnicity == 'Malay','T2D'],
        names = c("Chinese","Indian","Jakun","Malay"), col = c("burlywood2","cadetblue2","coral","darkolivegreen3"), outline = F, ylab = "T2D Risk using CS")
dev.off()

dunn.test(DwiyantoJ_2023_RS_CS$T2D,DwiyantoJ_2023_RS_CS$ethnicity)

#=============================================
# CRC risk in Polyps samples #
#=============================================

polyps_RS_metadata <- Polyps_AM_MM_Metadata_RS[Polyps_AM_MM_Metadata_RS$diseaseCat == 'Polyps',]

polyps_RS_metadata <- polyps_RS_metadata[,-c(1:5,15,26)]

polyps_RS_metadata_AP <- polyps_RS_metadata[,1:10]
names(polyps_RS_metadata_AP) <- gsub("_AM","", names(polyps_RS_metadata_AP))
polyps_RS_metadata_AP_selected <- polyps_RS_metadata_AP[polyps_RS_metadata_AP$CRC > 0.5,]

table_long <- polyps_RS_metadata_AP_selected %>%
  pivot_longer(
    cols = everything(),
    names_to = "Feature",
    values_to = "Value"
  )

box_colors <- rainbow(length(unique(table_long$Feature)))

p <- ggplot(table_long, aes(x = Feature, y = Value)) +
  geom_boxplot(
    outlier.shape = NA,
    color = "black",
    size = 0.3,
    fill = "white"
  ) +
  geom_jitter(
    width = 0.18,
    size = 0.6,
    alpha = 0.8,
    aes(color = Feature)
  ) +
  scale_color_manual(values = box_colors) +
  theme_minimal() +
  labs(
    x = "Diseases",
    y = "Risk scores"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    legend.position = "none"
  )

ggsave(filename = "polyps_CrossDisease_FP.pdf",plot = p,height = 3,width = 4,dpi = 1200)

ProjectIntervention <- function(
    disease_name,
    PCA_results,
    intervention_SHAP,
    intervention_metadata,
    output_dir = "."
){ 
  
  library(ggplot2)
  library(sp)
  library(dplyr)
  
  ###############################################################
  ## 1. Check feature consistency
  ###############################################################
  
  required_features <- rownames(PCA_results$pca_model$rotation)
  
  missing_features <- setdiff(required_features,
                              colnames(intervention_SHAP))
  
  if(length(missing_features) > 0){
    
    stop(
      paste(
        "Missing",
        length(missing_features),
        "features in intervention SHAP matrix."
      )
    )
  }
  
  intervention_SHAP <-
    intervention_SHAP[, required_features]
  
  
  
  ###############################################################
  ## 2. Scale using testing PCA parameters
  ###############################################################
  
  intervention_scaled <-
    scale(
      intervention_SHAP,
      center = PCA_results$pca_model$center,
      scale  = PCA_results$pca_model$scale
    )
  
  
  
  ###############################################################
  ## 3. Project into PCA space
  ###############################################################
  
  intervention_scores <-
    as.matrix(intervention_scaled) %*%
    PCA_results$pca_model$rotation
  
  
  
  intervention_pca <-
    data.frame(
      Sample = rownames(intervention_SHAP),
      PC1 = intervention_scores[,1],
      PC2 = intervention_scores[,2],
      stringsAsFactors = FALSE
    )
  
  
  
  ###############################################################
  ## 4. Match metadata
  ###############################################################
  
  if(!all(intervention_pca$Sample %in% rownames(intervention_metadata))){
    rownames(intervention_metadata) <- intervention_metadata[,1]
  }
  
  intervention_metadata <- intervention_metadata[intervention_pca$Sample,,drop=FALSE]
  
  
  
  intervention_pca$type <- intervention_metadata$type
  
  
  ###############################################################
  ## 5. Assign clusters using saved ellipses + nearest centroid
  ###############################################################
  
  intervention_cluster <- rep(0, nrow(intervention_pca))
  
  ellipse_list <- PCA_results$ellipse_list
  centroids <- PCA_results$centroids
  
  for(i in seq_len(nrow(intervention_pca))){
    
    ptx <- intervention_pca$PC1[i]
    pty <- intervention_pca$PC2[i]
    
    inside_clusters <- c()
    
    ## Find every ellipse containing the point
    for(cl in names(ellipse_list)){
      
      poly <- ellipse_list[[cl]]
      
      inside <- point.in.polygon(
        point.x = ptx,
        point.y = pty,
        pol.x = poly$x,
        pol.y = poly$y
      )
      
      if(inside > 0){
        inside_clusters <- c(inside_clusters, as.numeric(cl))
      }
      
    }
    
    ## Case 1: Outside every ellipse
    if(length(inside_clusters) == 0){
      
      intervention_cluster[i] <- 0
      
      ## Case 2: Inside exactly one ellipse
    } else if(length(inside_clusters) == 1){
      
      intervention_cluster[i] <- inside_clusters
      
      ## Case 3: Inside multiple ellipses
    } else {
      
      candidate_centroids <-
        centroids %>%
        dplyr::filter(Cluster %in% inside_clusters)
      
      dists <- sqrt(
        (candidate_centroids$PC1.centroid - ptx)^2 +
          (candidate_centroids$PC2.centroid - pty)^2
      )
      
      intervention_cluster[i] <-
        candidate_centroids$Cluster[which.min(dists)]
      
    }
    
  }
  
  intervention_pca$Cluster <- intervention_cluster
  
  # intervention_pca <- intervention_pca %>%
  #   filter(Cluster != 0)
  
  intervention_pca_plot <- intervention_pca %>%
    filter(Cluster != 0)
  
  ###############################################################
  ## 6. Build intervention cluster dataframe
  ###############################################################
  
  intervention_cluster_df <- intervention_pca_plot %>%
    transmute(
      Sample,
      Cluster,
      PC1,
      PC2,
      DataType = "intervention",
      Type = type)
  
  #intervention_cluster_df <- intervention_cluster_df[intervention_cluster_df$Cluster != 0,]
  
  
  
  ###############################################################
  ## 7. Merge with original cluster dataframe
  ###############################################################
  
  cluster_df_all <- bind_rows(PCA_results$cluster_df,intervention_cluster_df)
  
  
  ###############################################################
  ## 8. Overlay intervention samples on existing PCA plot
  ###############################################################
  
  n_groups <- length(unique(intervention_pca$type))
  
  cols <- colorRampPalette(c(
    "#FFD700",  # gold
    "goldenrod1",  # golden yellow
    "yellow",  # yellow-green
    "lightgreen",  # medium green
    "green"   # dark green
  ))(n_groups)
  
  p <- PCA_results$pca_plot +
    geom_point(
      data = intervention_pca_plot,
      aes(PC1, PC2, colour = type),
      shape = 16,
      size = 4.5,
      alpha = 1
    ) +
    scale_colour_manual(
      values = cols,
      name = "Intervention"
    )
  
  
  
  ###############################################################
  ## 9. Save PCA plot
  ###############################################################
  
  pdf(
    file.path(
      output_dir,
      paste0(
        disease_name,
        "_PCA_with_Intervention.pdf"
      )
    ),
    width = 9,
    height = 8
  )
  
  print(p)
  
  dev.off()
  
  
  
  ###############################################################
  ## 10. Print summary
  ###############################################################
  
  cat("\n")
  
  cat("---------------------------------------\n")
  
  cat("Intervention projection completed.\n")
  
  cat("---------------------------------------\n")
  
  cat("\n")
  
  cat("Total intervention samples : ",
      nrow(intervention_pca), "\n")
  
  cat("Projected inside clusters : ",
      sum(intervention_cluster != 0), "\n")
  
  cat("Outside all ellipses       : ",
      sum(intervention_cluster == 0), "\n")
  
  cat("\n")
  
  print(table(intervention_cluster))
  
  cat("\n")
  
  
  
  ###############################################################
  ## 11. Return object
  ###############################################################
  
  return(
    
    list(
      
      intervention_pca = intervention_pca,
      
      intervention_cluster_df = intervention_cluster_df,
      
      cluster_df_all = cluster_df_all,
      
      pca_plot = p
      
    )
    
  )
  
}

load("Polyps_projection_input.RData")

polyps_projection_results <- ProjectIntervention("CRC",CRC_PCA_results,polyps_SHAP_selected,polyps_metadata,"PCA_Intervention")

#===========================================================
# Analysis on NUAGE Med-Diet Data
#==========================================================

load("NUAGE_Overall_data.RData")

NUAGE_CS_common$cumulative_risk <- apply(NUAGE_CS_common[,c(1:9,11)],1,function(x)(length(x[x>=0.5])))

corr_food_score_disease_risk <- corr.test(NUAGE_CS_common[c(T0S0,T0S1,T1S0,T1S1),],NUAGE_metadata[c(T0S0,T0S1,T1S0,T1S1),"food_scores"],adjust="fdr")

df_corr_foodscore_DS <- data.frame("r"=corr_food_score_disease_risk$r,"p"=corr_food_score_disease_risk$p.adj)

df_corr_foodscore_DS$names <- c("AD","CRC","CVD","IBD_plus","IBS","MS","PD","SCZ","T2D","GD","MDD","Cumulative")

ggplot(df_corr_foodscore_DS,aes(x=r,y=-log(p,10)))+geom_point()+geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_hline(yintercept=-log(0.06,10),color="blue")+geom_text_repel(label=df_corr_foodscore_DS$names,size=5)+theme_bw()+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))

corr_metadata <- corr.test(NUAGE_metadata[c(T1S1,T1S0),c(1:31,34:42)]-NUAGE_metadata[c(T0S1,T0S0),c(1:31,34:42)],NUAGE_CS_common[c(T1S1,T1S0),]-NUAGE_CS_common[c(T0S1,T0S0),],use="pairwise.complete",adjust="fdr")

corr_delta_scores <- corr.test((NUAGE_CS_common[c(T1S1,T1S0),]-NUAGE_CS_common[c(T0S1,T0S0),]),(NUAGE_metadata[c(T1S1,T1S0),c("food_scores","cspraxis","IL17")]-NUAGE_metadata[c(T0S1,T0S0),c("food_scores","cspraxis","IL17")]),use="pairwise.complete",adjust="fdr")

df_corr_delta_scores <- data.frame("delta_food_score_r"=corr_delta_scores$r[,1],"delta_food_score_p"=corr_delta_scores$p[,1],"delta_cspraxis_r"=corr_delta_scores$r[,2],"delta_cspraxis_p"=corr_delta_scores$p[,2],"delta_IL17_r"=corr_delta_scores$r[,3],"delta_IL17_p"=corr_delta_scores$p[,3])

df_corr_delta_scores$names <- c("AD","CRC","CVD","IBD_plus","IBS","MS","PD","SCZ","T2D","GD","MDD","Cumulative")

ggplot(df_corr_delta_scores,aes(x=delta_cspraxis_r,y=-log(delta_cspraxis_p,10)))+geom_point()+geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_hline(yintercept=-log(0.06,10),color="blue")+geom_text_repel(label=df_corr_delta_scores$names,size=5)+theme_bw()+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))

ggplot(df_corr_delta_scores,aes(x=delta_IL17_r,y=-log(delta_IL17_p,10)))+geom_point()+geom_hline(yintercept=0)+geom_vline(xintercept=0)+geom_hline(yintercept=-log(0.06,10),color="blue")+geom_text_repel(label=df_corr_delta_scores$names,size=5)+theme_bw()+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15))


#===================================================
# Baseline Risks of the FMT recipients
#===================================================

load("FMT_baseline_Risks.RData")

plot_df <- combined_RS_CS_baseline %>%
  rownames_to_column("Sample") %>%
  pivot_longer(
    cols = AD:major_depressive_disorder,
    names_to = "Disease",
    values_to = "Score")

p <- ggplot(plot_df, aes(Disease, Score)) +
  geom_boxplot(fill = "cornflowerblue", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 1.2, color = "darkblue") +
  geom_hline(
    yintercept = 0.5,
    colour = "red",
    linetype = "dashed",
    linewidth = 0.75
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14)
  ) +
  labs(x = NULL, y = " Risk Score")

load("DavarD_2021_delta_GD_response.RData")

wilcox.test(result_df[result_df$Clin_Response_to_FMT == 'Responder','delta_GD'],result_df[result_df$Clin_Response_to_FMT == 'Non_Responder','delta_GD'])

pdf("DavarD_2021_Delta_GD_CS.pdf", height = 4, width = 3)
boxplot(result_df[result_df$Clin_Response_to_FMT == 'Responder','delta_GD'],
        result_df[result_df$Clin_Response_to_FMT == 'Non_Responder','delta_GD'],
        ylab = "GD Risk using MifRix-CS", names = c("Res","NR"), col = c("royalblue1","gold"))
dev.off()


#=======================================
# KumpP_2017
#=======================================

KumpP_2017_RS_AP <- KumpP_2017_AM_MM_RS[,1:11]
names(KumpP_2017_RS_AP) <- gsub("_AM","",names(KumpP_2017_RS_AP))

KumpP_2017_RS_FP <- KumpP_2017_AM_MM_RS[,14:24]
names(KumpP_2017_RS_FP) <- gsub("_MM","",names(KumpP_2017_RS_FP))

KumpP_2017_RS_CS <-  ComputeCompositeScore(KumpP_2017_RS_AP,KumpP_2017_RS_FP)

KumpP_2017_RS_CS_selected <- KumpP_2017_RS_CS[rownames(KumpP_2017_RS_CS) %in% rownames(KumpP_2017_metadata),]

## Keep only samples that have risk scores
metadata <- KumpP_2017_metadata %>%
  filter(rownames(.) %in% rownames(KumpP_2017_RS_CS_selected))

baseline_ids <- c()
followup_ids <- c()

for(id in unique(metadata$sample_id)){
  
  patient <- metadata %>%
    filter(sample_id == id) %>%
    arrange(time_point)
  
  ## Baseline
  baseline <- patient %>%
    filter(time_point == 0)
  
  if(nrow(baseline) == 0)
    next
  
  ## Immediate next follow-up
  followup <- patient %>%
    filter(time_point > 0) %>%
    slice(1)
  
  if(nrow(followup) == 0)
    next
  
  baseline_ids <- c(baseline_ids, rownames(baseline))
  followup_ids <- c(followup_ids, rownames(followup))
}

## Baseline risk scores
KumpP_2017_RS_CS_baseline <-
  KumpP_2017_RS_CS_selected[baseline_ids, ]

## Immediate follow-up risk scores
KumpP_2017_RS_CS_followup <-
  KumpP_2017_RS_CS_selected[followup_ids, ]

## Add Therapeutic.effect
KumpP_2017_RS_CS_baseline$Therapeutic.effect <-
  metadata[baseline_ids, "Therapeutic.effect"]

KumpP_2017_RS_CS_followup$Therapeutic.effect <-
  metadata[followup_ids, "Therapeutic.effect"]

KumpP_2017_RS_CS_baseline$days_since_fmt <-
  metadata[baseline_ids, "days_since_fmt"]

KumpP_2017_RS_CS_followup$days_since_fmt <-
  metadata[followup_ids, "days_since_fmt"]

delta_KumpP_2017_RS_CS <- KumpP_2017_RS_CS_followup[,1:11] - KumpP_2017_RS_CS_baseline[,1:11]
delta_KumpP_2017_RS_CS$response <- KumpP_2017_RS_CS_followup$Therapeutic.effect


pdf("KumpP_Delta_GD_CS_Response.pdf", height = 4, width = 3)
boxplot(delta_KumpP_2017_RS_CS[delta_KumpP_2017_RS_CS$response == 'Response','CD_disease_Probability'],
        delta_KumpP_2017_RS_CS[delta_KumpP_2017_RS_CS$response == 'No Response','CD_disease_Probability'], ylab = "Delta GD Composite", col = c("royalblue1","gold2"), names = c("Res","NR"), main = "KumpP_2017")
dev.off()

wilcox.test(delta_KumpP_2017_RS_CS[delta_KumpP_2017_RS_CS$response == 'Response','CD_disease_Probability'],
            delta_KumpP_2017_RS_CS[delta_KumpP_2017_RS_CS$response == 'No Response','CD_disease_Probability'])



load("delta_KongL_2020_RS_CS.RData")

pdf("KongL_Delta_GD_CS_Response.pdf", height = 4, width = 3)
boxplot(delta_KongL_2020_RS_CS[delta_KongL_2020_RS_CS$response == 'Response' | delta_KongL_2020_RS_CS$response == 'Partial response','CD_disease_Probability'],
        delta_KongL_2020_RS_CS[delta_KongL_2020_RS_CS$response == 'No response','CD_disease_Probability'], ylab = "Delta GD Composite", col = c("royalblue1","gold2"), names = c("Res","NR"), main = "KongL_2020")
dev.off()

wilcox.test(delta_KongL_2020_RS_CS[delta_KongL_2020_RS_CS$response == 'Response' | delta_KongL_2020_RS_CS$response == 'Partial response','CD_disease_Probability'],
            delta_KongL_2020_RS_CS[delta_KongL_2020_RS_CS$response == 'No response','CD_disease_Probability'])

#==============================================
# FMT cohort from India
#==============================================

donors_AP <- read.csv("Donors_CDFMT_AM.csv", check.names = F, row.names = 1)
donors_FP <- read.csv("Donors_CDFMT_MM.csv", check.names = F, row.names = 1)

donors_AP <- donors_AP[,1:11]
donors_FP <- donors_FP[,1:11]
donors_CS <- ComputeCompositeScore(donors_AP,donors_FP)


pdf("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\InterventionAnalysis\\CDFMT_Patient_Donor_GD_CS.pdf", height = 4, width = 3)
boxplot(CDFMT_RS_CS$CD_disease_Probability,donors_CS$CD_disease_Probability, names = c("Patients","Donors"), col = c("goldenrod","royalblue1"), ylab = "Disease Risk", outline = F)
dev.off()

wilcox.test(CDFMT_RS_CS$CD_disease_Probability,donors_CS$CD_disease_Probability)

load("CDFMT_delta_GD.RData")

pdf("CDFMT_CDrisk_CDAI_CS.pdf", height = 4, width = 3.5)

boxplot(delta_CDAI ~ cut(delta_GD,breaks = c(-Inf, -0.10, 0.10, Inf),
                         labels = c("< -0.10", "[-0.10, 0.10]", "> 0.10"),
                         include.lowest = TRUE),data = CDFMT_delta_GD,col = c("#a6cee3", "#b2df8a", "#fb9a99"),xlab = "Delta GD",ylab = "Delta CDAI",cex.axis = 0.7)
dev.off()

library(dunn.test)
dunn.test(CDFMT_delta_GD$delta_CDAI,cut(CDFMT_delta_GD$delta_GD, breaks = c(-Inf, -0.10, 0.10, Inf),include.lowest = T))

## Remission ##

load("CDFMT_relapse_status.RData")

temp_df_remission <- remission_status[rownames(remission_status) %in% rownames(CDFMT_delta_GD),,drop = F]

all(rownames(temp_df_remission) == rownames(CDFMT_delta_GD))

temp_df_remission <- temp_df_remission[rownames(CDFMT_delta_GD),,drop = F]

CDFMT_delta_GD$remission <- temp_df_remission$relapse_status

pdf("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\InterventionAnalysis\\CDFMT_CDrisk_Remission_CS.pdf", height = 4, width = 3.5)
boxplot(CDFMT_delta_GD[CDFMT_delta_GD$remission == 'remission','delta_GD'],CDFMT_delta_GD[CDFMT_delta_GD$remission == 'relapse','delta_GD'], outline = F,  col = c("lightpink","#b2df8a"),names = c("Remission","Non-Remission"))
dev.off()

wilcox.test(CDFMT_delta_GD[CDFMT_delta_GD$remission == 'remission','delta_GD'],CDFMT_delta_GD[CDFMT_delta_GD$remission == 'relapse','delta_GD'])








