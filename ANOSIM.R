library(vegan)
library(phyloseq)

#Convert physeq to vegan format
veganotu = function(physeq) {
  require("vegan")
  OTU = otu_table(physeq)
  if (taxa_are_rows(OTU)) {
    OTU = t(OTU)
  }
  return(as(OTU, "matrix"))
}

#Add phyloseq
phylo <- A_phyloseq

#See what variables there are
sample_variables(phylo)

#See what values are for a variable.
unique(get_variable(phylo, "A Variable"))

sample_variables(phylo)

#Remove samples with no metadata
phylo <- subset_samples(phylo, A_Variable !="")
phylo <- subset_samples(phylo, A_Variable !="")

#Normalise your table if needed

#Rarefy (subsample to an equal depth). Rarefy_even_depth (this does to minimum depth - check depth first by doing this) OR Can change sample.size = 'min' to a number such as 1000.

sample_sums(phylo)

min(sample_sums(phylo))

set.seed(123456)

phylo_rare_min <- rarefy_even_depth(phylo, sample.size = min(sample_sums(phylo)), rngseed = FALSE, replace = TRUE, trimOTUs = TRUE, verbose = TRUE)

#Check how many rarifed to
sample_sums(phylo_rare_min)

phylo <- phylo_rare_min

sample_variables(phylo)

unique(get_variable(phylo,"A_variable"))

group = get_variable(phylo, "A_variable")

ano = anosim(phyloseq::distance(phylo, "bray"), group, permutations = 999)
# p value
ano$signif
#R value
ano$statistic



