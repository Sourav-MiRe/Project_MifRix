args = commandArgs(trailingOnly=TRUE)
folder_path <- args[1]
output_folder_path <- args[3]
folder_files <- list.files(path = folder_path)
profile_matrix_path = ""

if(length(args) == 4){
  profile_matrix_path = args[4]
  if (!file.exists(profile_matrix_path)){
    stop(paste0("Profile matrix path ", profile_matrix_path, " does not exist"))
  } 
}
if (!dir.exists(folder_path)){
  stop(paste0("Folder path ", folder_path, " does not exist"))
}

# if (!dir.exists(output_folder_path)){
#   stop(paste0("Output folder path ", output_folder_path, " does not exist"))
# }

profile_matrix_generator <- function(col_name, profile_matrix_path=""){
  print(col_name)
  if(profile_matrix_path != ""){
    new_df <- read.csv(profile_matrix_path, header = TRUE, stringsAsFactors = FALSE, check.names = F)
  }
  else{
    new_df <-  data.frame(Genome_ID = c(""))
  }
  
  cols_added <- colnames(new_df)
  i <- nrow(new_df)+1
  num_genomes_added <- 0
  for (file_name in folder_files){
    file_path <- paste(folder_path, file_name, sep = "/")
    if(file.info(file_path)$isdir){
      next
    }
    genome_id <- gsub(".annotations.tsv", "", file_name)
    # adding new row with all column values 0
    new_df <- rbind(new_df, c(genome_id, rep(0,length(cols_added)-1)))
    
    # creating a data frame for eggNOG file 
    eggNOG_df <- read.csv(file_path, skip = 4, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    
    # selecting the column to be extracted from df and split each an entry having comma
    eggNOG_col_values <- unlist(lapply(eggNOG_df[[col_name]], function(x) strsplit(x, ",")))
    
    # removing all empty values
    eggNOG_col_values <- eggNOG_col_values[eggNOG_col_values!="-"]
    
    if(col_name == "eggNOG_OGs"){
      eggNOG_col_values <- gsub("@.*", "", eggNOG_col_values[grep("COG|arCOG", eggNOG_col_values)])
    }
    eggNOG_col_values_freq <- table(eggNOG_col_values)
    
    freq_table_colnames <- dimnames(eggNOG_col_values_freq)[[1]]
    
    freq <- c()
    for (value in freq_table_colnames){
      freq <- append(freq, eggNOG_col_values_freq[[value]])
    }
    
    cols_not_pres <- setdiff(freq_table_colnames, cols_added) # (values - cols_added) do not change order 
    new_df[c(cols_not_pres)] <- c(0)
    cols_added <- append(cols_added, cols_not_pres)
    new_df[i, c(freq_table_colnames)] <- freq
    i <- i+1
    num_genomes_added <- num_genomes_added + 1
    print(paste("Added Genome ", num_genomes_added, "for", col_name))
  }
  # deleting empty row
  if(profile_matrix_path == ""){
    new_df <- new_df[-1,]
  }
  print(dim(new_df))
  output_path <- paste0(output_folder_path,col_name,".csv")
  write.csv(new_df, output_path, row.names = F, quote = F)
  print(paste("Created profile matrix at ", output_path))
}

col_name = args[2]


profile_matrix_generator(col_name, profile_matrix_path)

# profile_matrix_generator("CAZy")
# profile_matrix_generator("KEGG_Module")
# profile_matrix_generator("KEGG_reaction")
# profile_matrix_generator("KEGG_ko")
# profile_matrix_generator("EC")
# profile_matrix_generator("eggNOG_OGs")
# profile_matrix_generator("PFAMs")
# profile_matrix_generator("BiGG_Reaction")


