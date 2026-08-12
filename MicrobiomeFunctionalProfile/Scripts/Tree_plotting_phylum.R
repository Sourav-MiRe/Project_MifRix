# ==============================================================================
# Refined Radial Phylogenetic Dendrogram Pipeline
# Integrates multi-ring metadata mapping, vibrant categorical palettes, 
# zero-bounded prevalence scaling, spatial radial expansions, and summary stats.
# ==============================================================================

# ------------------------------------------------------------------------------
# Dependency Verification and Library Initialization
# ------------------------------------------------------------------------------
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ggtreeExtra", quietly = TRUE)) BiocManager::install("ggtreeExtra")
if (!requireNamespace("ggnewscale", quietly = TRUE)) install.packages("ggnewscale")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")

library(ggnewscale)
library(tidyverse)
library(ggtree)
library(ggtreeExtra)
library(ape)
library(viridis)
library(scales)
library(RColorBrewer)
library(openxlsx)

# ------------------------------------------------------------------------------
# 0. Data Ingestion and Matrix Alignment
# ------------------------------------------------------------------------------

# Load intersecting matrices and expanded genomes
load("Train_test_AM_intersecting_fp_final.RData")
load("all_genomes_expanded_updated.RData")
taxa1844_lineage <- read.xlsx("taxa1844_lineage.xlsx")

# Vectorize species names and align dimensions
species <- colnames(Train_test_AM)
Train_test_MD <- Train_test_MD[rownames(Train_test_AM), ,drop = FALSE]

# ------------------------------------------------------------------------------
# 1. Phylogenetic Scaffolding and Tree Construction
# ------------------------------------------------------------------------------
tax_cols <- c("cellular_root","domain","kingdom","phylum","class","order","family","genus","species")

# Clean missing lineages and generate parent-child edge relationships
lineage_clean <- taxa1844_lineage %>%
  select(all_of(tax_cols)) %>%
  filter(!is.na(species)) %>%
  distinct(species,.keep_all = TRUE)

edges <- lineage_clean %>%
  rowwise() %>%
  mutate(lineage_vec = list(na.omit(c_across(all_of(tax_cols))))) %>%
  ungroup() %>%
  select(species, lineage_vec) %>%
  unnest_longer(lineage_vec) %>%
  group_by(species) %>%
  mutate(parent = lag(lineage_vec)) %>%
  ungroup() %>%
  filter(!is.na(parent)) %>%
  distinct(parent, lineage_vec) %>%
  rename(child = lineage_vec)

# Cast edges into standard phylo object
tax_tree <- edges %>%
  select(parent, child) %>%
  distinct() %>%
  as.phylo()

species_final <- tax_tree$tip.label

# ------------------------------------------------------------------------------
# 2. Taxonomic Summary Generation (Fulfills Request 6)
# ------------------------------------------------------------------------------
phylum_map <- lineage_clean %>%
  filter(species %in% species_final) %>%
  select(species, phylum)

phylum_map$phylum <- droplevels(as.factor(phylum_map$phylum))

# Compute percentage representation of each phylum
phylum_summary_df <- phylum_map %>%
  count(phylum, name = "Species_Count") %>%
  mutate(Percentage = (Species_Count / length(species_final)) * 100) %>%
  arrange(desc(Percentage))

cat("\n--- SUMMARY: PHYLUM SPECIES PERCENTAGE ---\n")
print(phylum_summary_df)

# ------------------------------------------------------------------------------
# 3. Prevalence Matrix Computation (Zero-Bounded Continuous Data)
# ------------------------------------------------------------------------------
# Exact same order
stopifnot(identical(rownames(Train_test_MD),rownames(Train_test_AM)))

# Same number of rows
stopifnot(nrow(Train_test_MD) == nrow(Train_test_AM))

AM_use <- Train_test_AM[
  , species_final, drop = FALSE
]

calc_prevalence <- function(species, sample_ids) {
  abund <- AM_use[sample_ids, species, drop = FALSE]
  mean(abund > 0)
}

# ---- Sample groups
samples_ind  <- rownames(Train_test_MD)[
  Train_test_MD$`Cohort Type` == "Industrialized"
]
samples_non  <- rownames(Train_test_MD)[
  Train_test_MD$`Cohort Type` == "Non-Industrialized"
]
samples_wgs  <- rownames(Train_test_MD)[
  Train_test_MD$`Sequence Type` == "WGS"
]
samples_16s  <- rownames(Train_test_MD)[
  Train_test_MD$`Sequence Type` == "16s"
]

# ---- Prevalence matrix
prev_df <- tibble(species = species_final) %>%
  mutate(
    Industrialized     = map_dbl(species, calc_prevalence, samples_ind),
    Non_Industrialized = map_dbl(species, calc_prevalence, samples_non),
    WGS                = map_dbl(species, calc_prevalence, samples_wgs),
    X16S               = map_dbl(species, calc_prevalence, samples_16s)
  )

stopifnot(all(prev_df[-1] >= 0 & prev_df[-1] <= 1))


# ------------------------------------------------------------------------------
# 4. Genome Count Transformation (Fulfills Request 8)
# ------------------------------------------------------------------------------
genome_counts <- all_genomes_expanded_updated %>%
  filter(Species_latest %in% species_final) %>%
  distinct(Genome, Species_latest) %>%
  count(Species_latest, name = "n_genomes") %>%
  rename(species = Species_latest) %>%
  # Apply base-2 exponential scaling to integers
  mutate(log_genomes = log2(n_genomes + 1))

genome_counts <- tibble(species = species_final) %>%
  left_join(genome_counts, by = "species") %>%
  replace_na(list(n_genomes = 0, log_genomes = 0))

# ------------------------------------------------------------------------------
# 5. Genome Source Classification and Summary (Fulfills Request 9)
# ------------------------------------------------------------------------------
genome_source <- all_genomes_expanded_updated %>%
  filter(Species_latest %in% species_final) %>%
  mutate(
    source = case_when(
      database == "RefSeq" ~ "Ref",
      database %in% c("MGnify","UNINA_MAGs","Hadza_Hunter_Gatherer") ~ "MAG"
    )
  ) %>%
  distinct(Genome, Species_latest, source) %>%
  group_by(Species_latest) %>%
  summarise(
    source_class = case_when(
      all(source == "Ref") ~ "Ref",
      all(source == "MAG") ~ "MAG",
      TRUE ~ "Both"
    ),
    .groups = "drop"
  ) %>%
  rename(species = Species_latest)

# Consolidate annotations into singular matrix
anno_df <- prev_df %>%
  left_join(genome_source, by = "species") %>%
  left_join(genome_counts, by = "species") %>%
  replace_na(list(source_class = "MAG")) 

# Compute percentage representation of genome sources
source_summary_df <- anno_df %>%
  count(source_class, name = "Count") %>%
  mutate(Percentage = (Count / length(species_final)) * 100) %>%
  arrange(desc(Percentage))

cat("\n--- SUMMARY: GENOME SOURCE PERCENTAGE ---\n")
print(source_summary_df)

# ------------------------------------------------------------------------------
# 6. Algorithmic Colorimetry Generation 
# ------------------------------------------------------------------------------
# 40 maximally distinct, highly vibrant colors completely free of pale greys/whites
distinct_colors <- c(
  "#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", "#911EB4", "#42D4F4", "#F032E6",
  "#BFEF45", "#FABED4", "#469990", "#DCBEFF", "#9A6324", "#FFFAC8", "#800000", "#AAFFC3",
  "#808000", "#FFD8B1", "#000075", "#A9A9A9", "#FF0000", "#00FF00", "#0000FF", "#FFFF00",
  "#00FFFF", "#FF00FF", "#FF8000", "#FF0080", "#80FF00", "#00FF80", "#0080FF", "#8000FF",
  "#FF8080", "#80FF80", "#8080FF", "#FFFF80", "#FF80FF", "#80FFFF", "#C00000", "#00C000",
  "#0000C0", "#C0C000"
)

phylum_levels <- levels(phylum_map$phylum)
phylum_colors <- rep(distinct_colors, length.out = length(phylum_levels))

# Explicitly anchoring the colors directly to the sorted factor names
names(phylum_colors) <- phylum_levels

# ------------------------------------------------------------------------------
# 7. Base Radial Topology Execution (Branch Coloring for ALL Phylums)
# ------------------------------------------------------------------------------
# 1. Extract all unique phylums from your map

all_phylums <- as.character(unique(phylum_map$phylum))

# 2. Create a list mapping every phylum to its respective species
all_phyla_list <- setNames(lapply(all_phylums, function(p) {
  phylum_map$species[phylum_map$phylum == p]
}), all_phylums)

# 3. Group the entire tree object by these clades
tax_tree_grouped <- groupOTU(tax_tree, all_phyla_list, group_name = "Phylum_Clade")

# 4. Map the colors: "0" covers the deep root/unassigned nodes, the rest map to your exact palette
branch_colors <- c("0" = "grey30", phylum_colors[all_phylums])

# 5. Plot the tree using the new Phylum_Clade groups for branch colors
p <- ggtree(tax_tree_grouped, layout = "fan", open.angle = 30, linewidth = 0.15, mapping = aes(color = Phylum_Clade)) +
  # Apply the branch colors and suppress the branch legend (so it doesn't duplicate the ring legend)
  scale_color_manual(values = branch_colors, guide = "none") +
  # Use a much smaller negative bound to drastically reduce inner space and expand plot
  xlim(-3, NA) +
  theme_void() +
  theme(legend.position = "right") 
# CRITICAL FIX: geom_tippoint is entirely removed to completely eliminate the black inner ring artifact.

# ------------------------------------------------------------------------------
# 8. Phylum Inner Categorical Ring
# ------------------------------------------------------------------------------
p0 <- p + 
  new_scale_fill() +
  geom_fruit(
    data = phylum_map,
    geom = geom_tile,
    mapping = aes(y = species, fill = phylum),
    color = NA, 
    width = 1,
    offset = 0.08,       
    pwidth = 0.05,
    key_glyph = "point"  # Forces tile geometry to output beautiful circular points in legend
  ) +
  scale_fill_manual(
    name = "Phylum", 
    values = phylum_colors, 
    na.value = "#F0F0F0",
    # Overrides mapping to generate large circular dots in the legend with transparent borders
    guide = guide_legend(override.aes = list(shape = 21, size = 6, color = "transparent"))
  )

# ------------------------------------------------------------------------------
# 9. Prevalence Rings (Pivot Longer & NA-Zero Handling)
# ------------------------------------------------------------------------------
# Function to slice continuous values into discrete categories
bin_prevalence <- function(x) {
  cut(x, 
      breaks = c(-Inf, 0, 0.001, 0.01, 0.1, 1), 
      labels = c("0", "0 - 0.001", "0.001 - 0.01", "0.01 - 0.1", "0.1 - 1.0"),
      right = TRUE)
}

# We MUST pivot longer so x maps to a static track instead of sliding the tiles around
prev_long <- anno_df %>%
  pivot_longer(
    cols = c(Industrialized, Non_Industrialized, WGS, X16S),
    names_to = "group",
    values_to = "prevalence"
  ) %>%
  mutate(
    # Keep the columns ordered consistently
    group = factor(group, levels = c("Industrialized", "Non_Industrialized", "WGS", "X16S")),
    # Convert true 0.0 to NA so it drops out of the log scale and renders cleanly as white
    prevalence_plot = bin_prevalence(prevalence)
  )

# Manually assign white to 0, and Viridis hex codes to the rest
prev_colors <- c(
  "0"            = "white",
  "0 - 0.001"    = "#440154FF", # Dark Purple
  "0.001 - 0.01" = "#31688EFF", # Blue
  "0.01 - 0.1"   = "#35B779FF", # Green
  "0.1 - 1.0"    = "#FDE725FF"  # Yellow
)

p1 <- p0 +
  new_scale_fill() + 
  geom_fruit(
    data = prev_long,
    geom = geom_tile,
    mapping = aes(y = species, x = group, fill = prevalence_plot),
    color = "grey50",       # Generates the highly visible, thin borders requested
    linewidth = 0.05,       
    width = 1, 
    offset = 0.1,          # Minimized spacing to keep rings flush against inner elements
    pwidth = 0.25 
  ) +
  scale_fill_manual(
    name = "Prevalence",
    values = prev_colors,
    na.translate = FALSE,
    # CRITICAL FIX: This draws a visible border around the legend boxes so white isn't invisible
    guide = guide_legend(
      override.aes = list(color = "grey30", linewidth = 0.5) 
    )
  )

# ------------------------------------------------------------------------------
# 10. Metagenomic Source Stratification
# ------------------------------------------------------------------------------
p2 <- p1 +
  new_scale_fill() + 
  geom_fruit(
    data = anno_df,
    geom = geom_tile,
    mapping = aes(y = species, x = "Genome Source", fill = source_class),
    color = "grey50",      
    linewidth = 0.05,
    width = 1,
    offset = 0.1,       
    pwidth = 0.06 
  ) +
  scale_fill_manual(
    name = "Genome source",
    values = c(MAG = "#FF1493", Ref = "#00BFFF", Both = "#FFD700")
  )

# ------------------------------------------------------------------------------
# 11. Outer Log2 Bar Projection
# ------------------------------------------------------------------------------
p3 <- p2 +
  new_scale_fill() +
  geom_fruit(
    data = anno_df,
    geom = geom_col,
    mapping = aes(y = species, x = log_genomes),
    orientation = "y",
    offset = 0.04, 
    pwidth = 0.4,       # Maintained at 0.4 to ensure bars stretch visibly outward
    fill = "#C2185B", 
    color = NA 
  )

# ------------------------------------------------------------------------------
# 12. Save High-Resolution Figure
# ------------------------------------------------------------------------------
ggsave(
  "radial_phylogeny_Phylum1.pdf",
  p3,
  width = 16,
  height = 16,
  dpi = 600,
  limitsize = FALSE
)

table(prev_long$prevalence_plot, useNA = "always")
