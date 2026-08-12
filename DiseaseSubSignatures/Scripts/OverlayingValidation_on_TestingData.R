setwd("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\GitHub_ScriptsData\\DiseaseSubSignatures\\DATA\\")

library(dplyr)
library(xlsx)
library(ggplot2)
library(effsize)
library(ComplexHeatmap)
library(circlize)
library(grid)

load("All_Disease_Val_SHAP_MD_AP.RData")
load("All_Disease_Testing_SHAP_MD_AP.RData")
load("AP_testing_validation_RiskScores.RData")

#------------------------------------------------------------

load("CD_Testing_Val_Controls.RData")

common_species <- intersect(names(CD_Testing_Control_AP),names(CD_Val_Control))
testing_controls_AP <- CD_Testing_Control_AP[,common_species]
validation_controls_AP <- CD_Val_Control[,common_species]
rm(CD_Testing_Control_AP,CD_Val_Control,CD_Testing_Control_MD)

Parkinsons_AP <- Parkinsons_AP[!grepl("^MBD", rownames(Parkinsons_AP)),]
Parkinsons_MD <- Parkinsons_MD[!grepl("^MBD", rownames(Parkinsons_MD)),]
Parkinsons_SHAP_AP <- Parkinsons_SHAP_AP[!grepl("^MBD", rownames(Parkinsons_SHAP_AP)),]


# ============================================================
# PCA/K-means Training–Validation Pipeline (Template)
# ============================================================
#
# NOTE:
# This script is a structured template based on the requested
# workflow. Replace the dataset lists at the bottom with your
# own objects.
#
# Required packages:
# tidyverse
# cluster
# ggplot2
# sp
# scales
#
# ============================================================

library(dplyr)
library(ggplot2)
library(cluster)
library(scales)
library(sp)

process_DOD_disease <- function(
    disease_name,
    test_shap,
    val_shap,
    test_risk,
    val_risk,
    output_dir="AP_PCA_Validation") {
  
  dir.create(output_dir,showWarnings=FALSE,recursive=TRUE)
  
  ## ----------------------------------------------------------
  ## Filter samples using risk scores
  ## ----------------------------------------------------------
  
  keep_test <- rownames(test_risk)[test_risk[,disease_name] > 0.5]
  
  keep_val <- rownames(val_risk)[val_risk[,disease_name] > 0.5]
  keep_val_clean <- sub(paste0("_", disease_name, "$"),"",keep_val,fixed = FALSE)
  
  keep_test <- intersect(keep_test,rownames(test_shap))
  keep_val_clean <- intersect(keep_val_clean,rownames(val_shap))
  
  test_df <- test_shap[keep_test,,drop=FALSE]
  val_df  <- val_shap[keep_val_clean,,drop=FALSE]
  
  ## ----------------------------------------------------------
  ## Common species
  ## ----------------------------------------------------------
  
  common_species <- intersect(colnames(test_df),colnames(val_df))
  
  species_cols <- setdiff(
    common_species,
    c("IsIndustrialized","Is16s"))
  
  test_species <- test_df[,species_cols,drop=FALSE]
  val_species  <- val_df[,species_cols,drop=FALSE]
  
  keep_cols <- apply(test_species,2,function(x) sd(x)>0)
  
  test_species <- test_species[,keep_cols,drop=FALSE]
  val_species  <- val_species[,keep_cols,drop=FALSE]
  
  ## ----------------------------------------------------------
  ## Scale using testing
  ## ----------------------------------------------------------
  
  test_scaled <- scale(test_species)
  
  ctr <- attr(test_scaled,"scaled:center")
  scl <- attr(test_scaled,"scaled:scale")
  
  val_scaled <- scale(val_species,center=ctr,scale=scl)
  
  ## ----------------------------------------------------------
  ## Determine K
  ## ----------------------------------------------------------
  
  ks <- 3:4
  sil <- numeric(length(ks))
  
  set.seed(123)
  
  for(i in seq_along(ks)){
    
    km <- kmeans(test_scaled,centers=ks[i],nstart=100)
    
    sil[i] <- mean(
      silhouette(km$cluster,
                 dist(test_scaled))[,3]
    )
  }
  
  optimal_k <- ks[which.max(sil)]
  
  km <- kmeans(test_scaled,centers=optimal_k,nstart=200)
  
  train_cluster <- factor(km$cluster)
  
  ## ----------------------------------------------------------
  ## Save cluster centroids
  ## ----------------------------------------------------------
  
  cluster_centers <- data.frame(
    Cluster = factor(seq_len(optimal_k)),
    km$centers,
    check.names = FALSE)
  
  
  ## ----------------------------------------------------------
  ## PCA
  ## ----------------------------------------------------------
  
  ## Fit PCA using only the testing data
  
  pca <- prcomp(test_scaled,center = FALSE,scale. = FALSE)
  
  ## ----------------------------------------------------------
  ## Project testing samples
  ## ----------------------------------------------------------
  
  train_scores <- as.matrix(test_scaled) %*% pca$rotation
  
  train_pca <- data.frame(
    PC1 = train_scores[, 1],
    PC2 = train_scores[, 2],
    Sample = rownames(test_scaled),
    Cluster = train_cluster,
    DataType = "testing",
    stringsAsFactors = FALSE)
  
  ## ----------------------------------------------------------
  ## Project validation samples into the SAME PCA space
  ## ----------------------------------------------------------
  
  val_scores <- as.matrix(val_scaled) %*% pca$rotation
  
  val_pca <- data.frame(
    PC1 = val_scores[, 1],
    PC2 = val_scores[, 2],
    Sample = rownames(val_scaled),
    DataType = "validation",
    stringsAsFactors = FALSE)
  
  ## ----------------------------------------------------------
  ## Save PCA model
  ## ----------------------------------------------------------
  
  pca_model <- list(
    
    ## PCA loadings
    rotation = pca$rotation,
    
    ## Standard deviation of each principal component
    sdev = pca$sdev,
    
    ## Training scaling parameters
    center = ctr,
    scale = scl,
    
    ## Feature names used in PCA
    features = colnames(test_scaled),
    
    ## Variance explained
    variance_explained = (pca$sdev^2) / sum(pca$sdev^2))
  
  ## ----------------------------------------------------------
  ## Build ellipse polygons
  ## ----------------------------------------------------------
  
  ellipse_list <- list()
  
  for(cl in levels(train_cluster)){
    
    d <- train_pca[train_pca$Cluster==cl,]
    
    if(nrow(d)<5) next
    
    stat <- ggplot_build(
      ggplot(
        d,
        aes(PC1,PC2)
      )+
        stat_ellipse(type="norm")
    )
    
    poly <- stat$data[[1]]
    
    ellipse_list[[cl]] <- poly
  }
  
  ## ----------------------------------------------------------
  ## Validation cluster assignment
  ## ----------------------------------------------------------
  
  val_cluster <- rep(0,nrow(val_pca))
  
  for(i in seq_len(nrow(val_pca))){
    
    ptx <- val_pca$PC1[i]
    pty <- val_pca$PC2[i]
    
    assigned <- 0
    
    for(cl in names(ellipse_list)){
      
      poly <- ellipse_list[[cl]]
      
      inside <- point.in.polygon(
        point.x=ptx,
        point.y=pty,
        pol.x=poly$x,
        pol.y=poly$y
      )
      
      if(inside>0){
        
        assigned <- as.numeric(cl)
        break
      }
    }
    
    val_cluster[i] <- assigned
  }
  
  val_pca$Cluster <- factor(val_cluster)
  
  ## ----------------------------------------------------------
  ## Combined cluster df
  ## ----------------------------------------------------------
  
  cluster_df <- bind_rows(
    
    train_pca %>%
      transmute(
        Sample,
        Cluster=as.numeric(as.character(Cluster)),
        DataType,
        PC1,
        PC2
      ),
    
    val_pca %>%
      transmute(
        Sample,
        Cluster=as.numeric(as.character(Cluster)),
        DataType,
        PC1,
        PC2
      )
  )
  
  ## ----------------------------------------------------------
  ## Top species (testing only)
  ## ----------------------------------------------------------
  
  tmp <- test_species
  tmp$Cluster <- train_cluster
  
  cluster_species <- lapply(
    
    split(tmp,tmp$Cluster),
    
    function(x){
      
      x$Cluster <- NULL
      
      med <- apply(
        x,
        2,
        median
      )
      
      med <- med[med>0]
      
      med <- sort(
        med,
        decreasing=TRUE
      )
      
      med[1:min(20,length(med))]
    }
  )
  
  ## ----------------------------------------------------------
  ## Plot
  ## ----------------------------------------------------------
  
  library(dplyr)
  
  pca_centroids <- train_pca %>%
    group_by(Cluster) %>%
    summarise(
      PC1.centroid = mean(PC1),
      PC2.centroid = mean(PC2),
      .groups = "drop")
  
  val_pca$Cluster <- factor(val_cluster)
  
  plot_df <- bind_rows(train_pca,val_pca)
  
  plot_df <- left_join(plot_df,pca_centroids,by = "Cluster")
  
  # val_pca <- subset(val_pca, PC2 < 25) #for IBD
  # plot_df <- subset(plot_df, PC2 < 25) #for IBD
  # val_pca <- subset(val_pca, PC2 > -10) #for PD & MDD
  # plot_df <- subset(plot_df, PC2 > -10) #for PD & MDD
  
  p <- ggplot() +
    
    ## Cluster ellipses (testing only)
    stat_ellipse(
      data = train_pca,
      aes(
        PC1,
        PC2,
        group = Cluster
      ),
      colour = "black",
      linewidth = 0.5
    ) +
    
    ## Lines from centroid to every point
    geom_segment(
      data = subset(plot_df, Cluster != 0),
      aes(
        x = PC1.centroid,
        y = PC2.centroid,
        xend = PC1,
        yend = PC2
      ),
      colour = "grey50",
      linewidth = 0.3,
      alpha = 0.6
    ) +
    
    ## Testing samples
    geom_point(
      data = train_pca,
      aes(PC1, PC2),
      colour = "cornflowerblue",
      size = 2.5,
      alpha = 0.8
    ) +
    
    ## Validation samples
    geom_point(
      data = val_pca,
      aes(PC1, PC2),
      colour = "red",
      size = 2.5,
      alpha = 0.8
    ) +
    
    labs(
      title = disease_name,
      x = "PC1",
      y = "PC2"
    ) +
    
    theme_bw(base_size = 16) +
    
    theme(
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 1
      ),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 18),
      plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave(
    file.path(
      output_dir,
      paste0(disease_name,"_PCA.pdf")),p,width=6,height=6)
  
  return(
    list(
      cluster_df=cluster_df,
      cluster_species=cluster_species,
      cluster_sizes=table(train_cluster),
      cluster_centers = cluster_centers,
      pca_plot=p,
      train_pca = train_pca,
      val_pca = val_pca,
      ellipse_list = ellipse_list,
      centroids = pca_centroids,
      pca_model=pca_model,
      kmeans_model=km
    )
  )
}

AD_PCA_results <- process_DOD_disease("AD",AD_SHAP_AP,AD_SHAP_AP_VAL_DOD,AP_testing_RiskScores[AP_testing_RiskScores$diseaseCat == 'AD',],AP_validation_RiskScores[AP_validation_RiskScores$diseaseCat == 'AD',])
IBD_GutInflammation_PCA_results <- process_DOD_disease("IBD_GutInflammation",IBD_GutInflammation_SHAP_AP,IBD_GutInflammation_SHAP_AP_VAL_DOD,AP_testing_RiskScores[AP_testing_RiskScores$diseaseCat == 'IBD_GutInflammation',],AP_validation_RiskScores[AP_validation_RiskScores$diseaseCat == 'IBD_GutInflammation',])
MS_PCA_results <- process_DOD_disease("MS",MS_SHAP_AP,MS_SHAP_AP_VAL_DOD,AP_testing_RiskScores[AP_testing_RiskScores$diseaseCat == 'MS',],AP_validation_RiskScores[AP_validation_RiskScores$diseaseCat == 'MS',])
CRC_PCA_results <- process_DOD_disease("CRC",CRC_SHAP_AP,CRC_SHAP_AP_VAL_DOD,AP_testing_RiskScores[AP_testing_RiskScores$diseaseCat == 'CRC',],AP_validation_RiskScores[AP_validation_RiskScores$diseaseCat == 'CRC',])
T2D_PCA_results <- process_DOD_disease("T2D",T2D_SHAP_AP,T2D_SHAP_AP_VAL_DOD,AP_testing_RiskScores[AP_testing_RiskScores$diseaseCat == 'T2D',],AP_validation_RiskScores[AP_validation_RiskScores$diseaseCat == 'T2D',])
CVD_PCA_results <- process_DOD_disease("CVD",CVD_SHAP_AP,CVD_SHAP_AP_VAL_DOD,AP_testing_RiskScores[AP_testing_RiskScores$diseaseCat == 'CVD',],AP_validation_RiskScores[AP_validation_RiskScores$diseaseCat == 'CVD',])
IBS_PCA_results <- process_DOD_disease("IBS",IBS_SHAP_AP,IBS_SHAP_AP_VAL_DOD,AP_testing_RiskScores[AP_testing_RiskScores$diseaseCat == 'IBS',],AP_validation_RiskScores[AP_validation_RiskScores$diseaseCat == 'IBS',])
Parkinsons_PCA_results <- process_DOD_disease("Parkinsons",Parkinsons_SHAP_AP,Parkinsons_SHAP_AP_VAL_DOD,AP_testing_RiskScores[AP_testing_RiskScores$diseaseCat == 'Parkinsons',],AP_validation_RiskScores[AP_validation_RiskScores$diseaseCat == 'Parkinsons',])
Schizophrenia_PCA_results <- process_DOD_disease("Schizophrenia",Schizophrenia_SHAP_AP,Schizophrenia_SHAP_AP_VAL_DOD,AP_testing_RiskScores[AP_testing_RiskScores$diseaseCat == 'Schizophrenia',],AP_validation_RiskScores[AP_validation_RiskScores$diseaseCat == 'Schizophrenia',])
major_depressive_disorder_PCA_results <- process_DOD_disease("major_depressive_disorder",major_depressive_disorder_SHAP_AP,major_depressive_disorder_SHAP_AP_VAL_DOD,AP_testing_RiskScores[AP_testing_RiskScores$diseaseCat == 'major_depressive_disorder',],AP_validation_RiskScores[AP_validation_RiskScores$diseaseCat == 'major_depressive_disorder',])



save(AD_PCA_results, IBD_GutInflammation_PCA_results, CRC_PCA_results, MS_PCA_results,
     T2D_PCA_results, CVD_PCA_results, IBS_PCA_results, Parkinsons_PCA_results, Schizophrenia_PCA_results, major_depressive_disorder_PCA_results,
     file = "Projection_PCA_results.RData")



#==============================================================
# Cluster-wise taxa Heat maps for testing and validation
#==============================================================

load("Projection_PCA_results.RData")

library(dplyr)
library(xlsx)
library(ggplot2)
library(effsize)
library(ComplexHeatmap)
library(circlize)
library(grid)

# ============================================================
# Cluster-wise Top20 Species Comparison (Single Disease)
# ============================================================


ClusterSpeciesHeatmap <- function(
    disease,
    PCA_results,
    disease_testing,
    disease_validation,
    testing_control,
    testing_nondisease,
    min_cluster_size=5){
  
  cluster_df <- subset(PCA_results$cluster_df, Cluster!=0)
  cluster_species <- PCA_results$cluster_species
  
  common_species <- Reduce(intersect,list(
    colnames(disease_testing),
    colnames(disease_validation),
    colnames(testing_control),
    colnames(testing_nondisease)))
  
  disease_testing <- disease_testing[,common_species,drop=FALSE]
  disease_validation <- disease_validation[,common_species,drop=FALSE]
  testing_control <- testing_control[,common_species,drop=FALSE]
  testing_nondisease <- testing_nondisease[,common_species,drop=FALSE]
  
  score_fun <- function(x1,x2){
    x1 <- as.numeric(x1); x2 <- as.numeric(x2)
    if(length(unique(c(x1,x2)))<=1) return(0)
    g <- tryCatch(cohen.d(x1,x2,hedges.correction=TRUE)$estimate,error=function(e) NA)
    p <- tryCatch(wilcox.test(x1,x2)$p.value,error=function(e) NA)
    s <- 0
    if(!is.na(g) && !is.na(p)){
      if(g>0 && p<=0.05) s<-2
      else if(g>0 && p<=0.10) s<-1
      else if(g<0 && p<=0.05) s<--2
      else if(g<0 && p<=0.10) s<--1
    }
    s
  }
  
  valid_test <- c(); valid_val <- c(); top_species <- list()
  
  for(cl in sort(unique(cluster_df$Cluster))){
    sp <- cluster_species[[as.character(cl)]]
    if(is.null(sp)) sp <- cluster_species[[cl]]
    if(is.null(sp)) next
    sp <- names(sp)
    sp <- sp[sp %in% common_species]
    
    ts <- intersect(cluster_df$Sample[cluster_df$Cluster==cl & cluster_df$DataType=="testing"],rownames(disease_testing))
    vs <- intersect(cluster_df$Sample[cluster_df$Cluster==cl & cluster_df$DataType=="validation"],rownames(disease_validation))
    
    if(length(ts)>=min_cluster_size){
      valid_test <- c(valid_test,cl)
      top_species[[paste0("GD_testing_Cluster",cl)]] <- sp
      top_species[[paste0("DS_testing_Cluster",cl)]] <- sp
    }
    if(length(vs)>=min_cluster_size){
      valid_val <- c(valid_val,cl)
      top_species[[paste0("GD_val_Cluster",cl)]] <- sp
      top_species[[paste0("DS_val_Cluster",cl)]] <- sp
    }
  }
  
  union_species <- unique(unlist(top_species))
  cols <- c(paste0("GD_testing_Cluster",valid_test),
            paste0("DS_testing_Cluster",valid_test),
            paste0("GD_val_Cluster",valid_val),
            paste0("DS_val_Cluster",valid_val))
  mat <- matrix(0,nrow=length(union_species),ncol=length(cols),
                dimnames=list(union_species,cols))
  
  fill_block <- function(prefix,data_mat,samps,ref_mat){
    for(sp in top_species[[prefix]]){
      if(!(sp %in% colnames(data_mat)) || !(sp %in% colnames(ref_mat))) next
      mat[sp,prefix] <<- score_fun(data_mat[samps,sp],ref_mat[,sp])
    }
  }
  
  for(cl in valid_test){
    samps <- intersect(cluster_df$Sample[cluster_df$Cluster==cl & cluster_df$DataType=="testing"],rownames(disease_testing))
    fill_block(paste0("GD_testing_Cluster",cl),disease_testing,samps,testing_control)
    fill_block(paste0("DS_testing_Cluster",cl),disease_testing,samps,testing_nondisease)
  }
  
  for(cl in valid_val){
    samps <- intersect(cluster_df$Sample[cluster_df$Cluster==cl & cluster_df$DataType=="validation"],rownames(disease_validation))
    fill_block(paste0("GD_val_Cluster",cl),disease_validation,samps,testing_control)
    fill_block(paste0("DS_val_Cluster",cl),disease_validation,samps,testing_nondisease)
  }
  
  ## Rearrange columns cluster-wise
  all_clusters <- sort(unique(c(valid_test, valid_val)))
  
  new_order <- c()
  
  for(cl in all_clusters){
    
    cols <- c(
      paste0("GD_testing_Cluster", cl),
      paste0("DS_testing_Cluster", cl),
      paste0("GD_val_Cluster", cl),
      paste0("DS_val_Cluster", cl)
    )
    
    ## Keep only columns that actually exist
    new_order <- c(new_order, cols[cols %in% colnames(mat)])
  }
  
  mat <- mat[, new_order, drop = FALSE]
  mat <- t(mat)
  
  col_fun <- colorRamp2(c(-2,-1,0,1,2),
                        c("royalblue4","skyblue","gray95","pink","deeppink4"))
  
  ht <- Heatmap(mat,name="Association",col=col_fun,
                cluster_rows=F,cluster_columns=T,
                show_row_dend=FALSE,show_column_dend=FALSE,
                row_names_side="left",
                row_names_gp=gpar(fontsize=10),
                column_names_gp=gpar(fontsize=9),
                heatmap_legend_param=list(at=c(-2,-1,0,1,2)),
                width=unit(max(1,ncol(mat))*4,"mm"),
                height=unit(max(1,nrow(mat))*4,"mm"),
                cell_fun=function(j,i,x,y,width,height,fill){
                  grid.rect(x,y,width,height,gp=gpar(fill=fill,col="grey60",lwd=0.4))
                })
  
  pdf(paste0(disease,"_ClusterSpeciesHeatmap_GD_DS.pdf"),
      width=max(5,ncol(mat)*0.22),
      height=max(5,nrow(mat)*0.7))
  draw(ht)
  dev.off()
  
  ht_drawn <- draw(ht)
  row_order <- row_order(ht_drawn)
  col_order <- column_order(ht_drawn)
  carpet_df <- mat[row_order, col_order]
  
  carpet_df_final <- as.data.frame(carpet_df)
  
  library(xlsx)
  write.xlsx(carpet_df_final,paste0(disease,"_ClusterSpeciesHeatmapScores.xlsx"))
  
  invisible(list(score_matrix=mat,
                 valid_testing_clusters=valid_test,
                 valid_validation_clusters=valid_val,
                 top_species=top_species))
}


ClusterSpeciesHeatmap("AD",AD_PCA_results,AD_AP,AD_AP_VAL_DOD,CD_Testing_Control_AP,AD_NON_AP)
ClusterSpeciesHeatmap("MS",MS_PCA_results,MS_AP,MS_AP_VAL_DOD,CD_Testing_Control_AP,MS_NON_AP)
ClusterSpeciesHeatmap("CRC",CRC_PCA_results,CRC_AP,CRC_AP_VAL_DOD,CD_Testing_Control_AP,CRC_NON_AP)
ClusterSpeciesHeatmap("IBS",IBS_PCA_results,IBS_AP,IBS_AP_VAL_DOD,CD_Testing_Control_AP,IBS_NON_AP)
ClusterSpeciesHeatmap("T2D",T2D_PCA_results,T2D_AP,T2D_AP_VAL_DOD,CD_Testing_Control_AP,T2D_NON_AP)
ClusterSpeciesHeatmap("CVD",CVD_PCA_results,CVD_AP,CVD_AP_VAL_DOD,CD_Testing_Control_AP,CVD_NON_AP)
ClusterSpeciesHeatmap("IBD_GutInflammation",IBD_GutInflammation_PCA_results,IBD_GutInflammation_AP,IBD_GutInflammation_AP_VAL_DOD,CD_Testing_Control_AP,IBD_GutInflammation_NON_AP)
ClusterSpeciesHeatmap("Parkinsons",Parkinsons_PCA_results,Parkinsons_AP,Parkinsons_AP_VAL_DOD,CD_Testing_Control_AP,Parkinsons_NON_AP)
ClusterSpeciesHeatmap("Schizophrenia",Schizophrenia_PCA_results,Schizophrenia_AP,Schizophrenia_AP_VAL_DOD,CD_Testing_Control_AP,Schizophrenia_NON_AP)
ClusterSpeciesHeatmap("major_depressive_disorder",major_depressive_disorder_PCA_results,major_depressive_disorder_AP,major_depressive_disorder_AP_VAL_DOD,CD_Testing_Control_AP,major_depressive_disorder_NON_AP)


#==============================================================
# Cluster-wise Risk Score Analysis
#==============================================================

load("AP_testing_validation_RiskScores.RData")

addRiskScores <- function(PCA_results,
                          disease,
                          testing_scores,
                          validation_scores){
  
  test_df <- testing_scores[testing_scores$diseaseCat == disease,,drop = FALSE]
  
  val_df <- validation_scores[validation_scores$diseaseCat == disease,,drop = FALSE]
  
  rownames(val_df) <- sub(paste0("_", disease, "$"),"",rownames(val_df))
  
  risk_df <- rbind(test_df, val_df)
  
  risk_df$diseaseCat <- NULL
  
  cluster_df <- PCA_results$cluster_df
  
  idx <- match(cluster_df$Sample, rownames(risk_df))
  
  cluster_df <- cbind(cluster_df,risk_df[idx, , drop = FALSE])
  
  PCA_results$cluster_df <- cluster_df
  
  return(as.data.frame(PCA_results$cluster_df))
}

AD_PCA_RS_df <- addRiskScores(AD_PCA_results,"AD",AP_testing_RiskScores,AP_validation_RiskScores)
MS_PCA_RS_df <- addRiskScores(MS_PCA_results,"MS",AP_testing_RiskScores,AP_validation_RiskScores)
CRC_PCA_RS_df <- addRiskScores(CRC_PCA_results,"CRC",AP_testing_RiskScores,AP_validation_RiskScores)
IBS_PCA_RS_df <- addRiskScores(IBS_PCA_results,"IBS",AP_testing_RiskScores,AP_validation_RiskScores)
T2D_PCA_RS_df <- addRiskScores(T2D_PCA_results,"T2D",AP_testing_RiskScores,AP_validation_RiskScores)
CVD_PCA_RS_df <- addRiskScores(CVD_PCA_results,"CVD",AP_testing_RiskScores,AP_validation_RiskScores)
IBD_GutInflammation_PCA_RS_df <- addRiskScores(IBD_GutInflammation_PCA_results,"IBD_GutInflammation",AP_testing_RiskScores,AP_validation_RiskScores)
Parkinsons_PCA_RS_df <- addRiskScores(Parkinsons_PCA_results,"Parkinsons",AP_testing_RiskScores,AP_validation_RiskScores)
Schizophrenia_PCA_RS_df <- addRiskScores(Schizophrenia_PCA_results,"Schizophrenia",AP_testing_RiskScores,AP_validation_RiskScores)
major_depressive_disorder_PCA_RS_df <- addRiskScores(major_depressive_disorder_PCA_results,"major_depressive_disorder",AP_testing_RiskScores,AP_validation_RiskScores)

#--------------------------------------------------------------
# Adding Metadata 
#--------------------------------------------------------------

addMetadata <- function(PCA_RS_df,
                        testing_metadata,
                        validation_metadata){
  
  colnames(testing_metadata) <- gsub(" ", "_", colnames(testing_metadata))
  colnames(validation_metadata) <- gsub(" ", "_", colnames(validation_metadata))
  
  common_cols <- intersect(colnames(testing_metadata),colnames(validation_metadata))
  
  testing_metadata <- testing_metadata[, common_cols, drop = FALSE]
  validation_metadata <- validation_metadata[, common_cols, drop = FALSE]
  
  metadata <- rbind(testing_metadata, validation_metadata)
  
  idx <- match(rownames(PCA_RS_df), rownames(metadata))
  
  metadata <- metadata[idx, , drop = FALSE]
  
  metadata <- metadata[, !colnames(metadata) %in% colnames(PCA_RS_df), drop = FALSE]
  
  PCA_RS_df <- cbind(PCA_RS_df, metadata)
  
  return(PCA_RS_df)
}

AD_PCA_RS_MD <- addMetadata(AD_PCA_RS_df,AD_MD,AD_MD_VAL_DOD)
MS_PCA_RS_MD <- addMetadata(MS_PCA_RS_df,MS_MD,MS_MD_VAL_DOD)
CRC_PCA_RS_MD <- addMetadata(CRC_PCA_RS_df,CRC_MD,CRC_MD_VAL_DOD)
IBS_PCA_RS_MD <- addMetadata(IBS_PCA_RS_df,IBS_MD,IBS_MD_VAL_DOD)
T2D_PCA_RS_MD <- addMetadata(T2D_PCA_RS_df,T2D_MD,T2D_MD_VAL_DOD)
CVD_PCA_RS_MD <- addMetadata(CVD_PCA_RS_df,CVD_MD,CVD_MD_VAL_DOD)
IBD_GutInflammation_PCA_RS_MD <- addMetadata(IBD_GutInflammation_PCA_RS_df,IBD_GutInflammation_MD,IBD_GutInflammation_MD_VAL_DOD)
Parkinsons_PCA_RS_MD <- addMetadata(Parkinsons_PCA_RS_df,Parkinsons_MD,Parkinsons_MD_VAL_DOD)
Schizophrenia_PCA_RS_MD <- addMetadata(Schizophrenia_PCA_RS_df,Schizophrenia_MD,Schizophrenia_MD_VAL_DOD)
major_depressive_disorder_PCA_RS_MD <- addMetadata(major_depressive_disorder_PCA_RS_df,major_depressive_disorder_MD,major_depressive_disorder_MD_VAL_DOD)

save(AD_PCA_RS_MD,MS_PCA_RS_MD,CRC_PCA_RS_MD,IBS_PCA_RS_MD,
     T2D_PCA_RS_MD,CVD_PCA_RS_MD,IBD_GutInflammation_PCA_RS_MD,
     Parkinsons_PCA_RS_MD,Schizophrenia_PCA_RS_MD,major_depressive_disorder_PCA_RS_MD,
     file = "G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\Overlaying_Validation_on_TestingData\\Clusters_PCA_RS_MD.RData")

#=============================================================
# Risk Score Bar Plots
#=============================================================

library(ggplot2)
library(dplyr)
library(reshape2)
library(colorspace)

risk_cols <- c(
  "AD","CRC","CVD","IBD_GutInflammation","IBS","MS",
  "Parkinsons","Schizophrenia","T2D","major_depressive_disorder")

create_risk_boxplot <- function(df, disease_name){
  
  ## Remove Cluster 0
  df <- df %>%
    filter(Cluster != 0)
  
  ## Cluster sizes
  cluster_sizes <- table(df$Cluster)
  
  ## Keep clusters with >=5 samples
  valid_clusters <- names(cluster_sizes[cluster_sizes >= 5])
  
  df <- df %>%
    filter(Cluster %in% valid_clusters)
  
  df$Cluster <- factor(df$Cluster,levels = sort(unique(df$Cluster)))
  
  cluster_palette <- c(
    "1"="#FF4B00",
    "2"="#80B1D3",
    "3"="#BC80BD",
    "4"="#B3DE69",
    "5"="#FB8072")
  
  cluster_palette <- colorspace::lighten(cluster_palette,0.1)
  names(cluster_palette) <- levels(df$Cluster)
  
  plot_long <- melt(
    df,
    id.vars=c(
      "Sample",
      "Cluster",
      "DataType",
      "PC1",
      "PC2"
    ),
    measure.vars=risk_cols,
    variable.name="Disease",
    value.name="RiskScore"
  )
  
  p <- ggplot(
    plot_long,
    aes(
      x=Disease,
      y=RiskScore,
      fill=Cluster
    )
  ) +
    
    geom_boxplot(
      position=position_dodge(0.8),
      outlier.size=0.5
    ) +
    
    geom_hline(
      yintercept=0.5,
      colour="red",
      linetype="dashed",
      linewidth=0.8
    ) +
    
    scale_fill_manual(
      values=cluster_palette) +
    
    labs(
      title=paste0(disease_name," Clusters: Disease Risk Scores"),
      x="Disease",
      y="Risk Score"
    ) +
    
    theme_bw(base_size=14) +
    
    theme(
      plot.title=element_text(
        hjust=0.5,
        face="bold",
        size=16
      ),
      axis.text=element_text(size=16),
      axis.text.x=element_text(
        angle=45,
        hjust=1
      ),
      legend.title=element_text(size=14),
      legend.text=element_text(size=12)
    )
  
  return(p)
}

diseases <- c(
  "AD","CRC","CVD","IBD_GutInflammation","IBS",
  "major_depressive_disorder","MS",
  "Parkinsons","Schizophrenia","T2D")

for(disease in diseases){
  
  cat("Plotting:", disease, "\n")
  
  df <- get(paste0(disease, "_PCA_RS_MD"))
  
  p <- create_risk_boxplot(df,disease)
  
  ggsave(
    paste0(disease, "_RiskScoreBoxplots.pdf"),p,width = 12,height = 5)
}

#-----------------------------------------------------------
# p-values computation between clusters for a disease
#-----------------------------------------------------------

load("G:\\My Drive\\metaQR_FIRST_DRAFT\\MifRix_Manuscript\\AnalysisData\\Explainability\\Overlaying_Validation_on_TestingData\\Clusters_PCA_RS_MD.RData")

calculate_cluster_pvalues <- function(cluster_df){
  
  risk_cols <- c(
    "AD",
    "CRC",
    "CVD",
    "IBD_GutInflammation",
    "IBS",
    "MS",
    "Parkinsons",
    "Schizophrenia",
    "T2D",
    "major_depressive_disorder")
  
  ## Remove Cluster 0
  cluster_df <- subset(cluster_df, Cluster != 0)
  
  ## Cluster sizes
  cluster_sizes <- table(cluster_df$Cluster)
  
  ## Keep clusters having >=5 samples
  valid_clusters <- names(cluster_sizes[cluster_sizes >= 5])
  
  cluster_df <- subset(cluster_df,Cluster %in% valid_clusters)
  
  cluster_df$Cluster <- factor(cluster_df$Cluster,levels = sort(as.numeric(valid_clusters)))
  
  ## Pairwise comparisons
  comparisons <- combn(levels(cluster_df$Cluster),2,simplify = FALSE)
  
  pval_df <- data.frame()
  
  for(d in risk_cols){
    
    for(comp in comparisons){
      
      c1 <- comp[1]
      c2 <- comp[2]
      
      x1 <- cluster_df[cluster_df$Cluster == c1,d]
      
      x2 <- cluster_df[cluster_df$Cluster == c2,d]
      
      pval <- tryCatch(wilcox.test(x1, x2)$p.value,error = function(e) NA)
      
      pval_df <- rbind(pval_df,data.frame(DiseaseRisk = d,Cluster1 = c1,Cluster2 = c2,Pvalue = pval,stringsAsFactors = FALSE))
    }
  }
  
  pval_df
}

diseases <- c(
  "AD",
  "CRC",
  "CVD",
  "IBD_GutInflammation",
  "IBS",
  "major_depressive_disorder",
  "MS",
  "Parkinsons",
  "Schizophrenia",
  "T2D")

for(disease in diseases){
  
  cat("Calculating:", disease, "\n")
  
  cluster_df <- get(paste0(disease, "_PCA_RS_MD"))
  
  pval_df <- calculate_cluster_pvalues(cluster_df)
  
  assign(paste0(disease, "_cluster_pvalues"),pval_df,envir = .GlobalEnv)
  
  ## Significant results
  assign(
    paste0(disease, "_cluster_pvalues_sig"),
    subset(pval_df, Pvalue <= 0.05),envir = .GlobalEnv)
}


#=========================================================
# Bar Plots showing the proportions of Microbiomes
#=========================================================

rownames(AD_PCA_RS_MD) <- gsub("YıldırımS_2022","YildirimS_2022",rownames(AD_PCA_RS_MD))
AD_PCA_RS_MD$study_name <- gsub("YıldırımS_2022","YildirimS_2022",AD_PCA_RS_MD$study_name)
AD_PCA_RS_MD$Sample <- gsub("YıldırımS_2022","YildirimS_2022",AD_PCA_RS_MD$Sample)

load("Clusters_PCA_RS_MD.RData")

plot_cluster_distribution <- function(df,
                                      testing_label = "testing",
                                      validation_label = "validation",
                                      title = "Cluster Distribution",
                                      save_plot = TRUE,
                                      outdir = ".",
                                      width = 6,
                                      height = 3.5,
                                      dpi = 1200) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  df <- df %>% filter(Cluster != 0)
  
  counts <- df %>% count(Cluster, DataType)
  
  counts <- counts %>%
    complete(
      Cluster,
      DataType = c(testing_label, validation_label),
      fill = list(n = 0))
  
  totals <- df %>%
    count(DataType, name = "Total")
  
  counts <- counts %>%
    left_join(totals, by = "DataType") %>%
    mutate(
      Percentage = ifelse(Total > 0, 100 * n / Total, 0),
      Label = ifelse(n == 0, "0", sprintf("%.1f", Percentage)))

  cols <- c("cornflowerblue","brown2")
  names(cols) <- c(testing_label, validation_label)
  
  p <- ggplot(counts,
              aes(x = factor(Cluster),
                  y = Percentage,
                  fill = DataType)) +
    geom_col(position = position_dodge(width = 0.75),
             width = 0.65,
             color = "black") +
    geom_text(aes(label = Label),
              position = position_dodge(width = 0.75),
              vjust = -0.3,
              size = 6) +
    scale_fill_manual(values = cols) +
    labs(
      x = "Cluster",
      y = "Percentage of Samples",
      fill = "",
      title = title
    ) +
    expand_limits(y = max(counts$Percentage) * 1.1) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 17),
      axis.text.y = element_text(size = 17),
      
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right"
    )
  
  ## Save plot
  if (save_plot) {
    
    if (!dir.exists(outdir))
      dir.create(outdir, recursive = TRUE)
    
    ggsave(
      filename = file.path(outdir,
                           paste0(gsub("[^A-Za-z0-9_]", "_", title),
                                  "_Cluster_Distribution.pdf")),
      plot = p,
      width = width,
      height = height,
      dpi = dpi
    )
  }
  
  return(p)
}


plot_cluster_distribution(AD_PCA_RS_MD,title = "AD",outdir = "PercentageSample_Barplots")
plot_cluster_distribution(IBD_GutInflammation_PCA_RS_MD,title = "IBD_GutInflammation",outdir = "PercentageSample_Barplots")
plot_cluster_distribution(CRC_PCA_RS_MD,title = "CRC",outdir = "PercentageSample_Barplots")
plot_cluster_distribution(CVD_PCA_RS_MD,title = "CVD",outdir = "PercentageSample_Barplots")
plot_cluster_distribution(T2D_PCA_RS_MD,title = "T2D",outdir = "PercentageSample_Barplots")
plot_cluster_distribution(Schizophrenia_PCA_RS_MD,title = "Schizophrenia",outdir = "PercentageSample_Barplots")
plot_cluster_distribution(IBS_PCA_RS_MD,title = "IBS",outdir = "PercentageSample_Barplots")
plot_cluster_distribution(Parkinsons_PCA_RS_MD,title = "Parkinsons",outdir = "PercentageSample_Barplots")
plot_cluster_distribution(MS_PCA_RS_MD,title = "MS",outdir = "PercentageSample_Barplots")
plot_cluster_distribution(major_depressive_disorder_PCA_RS_MD,title = "major_depressive_disorder",outdir = "PercentageSample_Barplots")


