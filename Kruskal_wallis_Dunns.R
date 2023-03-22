
# Kruskal Wallis and Dunns multiple comparisons test

# Step one - setting up environment. We need a bunch of packages. These are the ones below.
libraries<-c("reshape2","scales","plyr","openxlsx","tidyverse", "readxl", "phyloseq", "viridis", "vegan", "FSA")
lapply(libraries, require, character.only = TRUE)

#############LOAD IN PHYLOSEQ

filt_meth <- prune_taxa(taxa_sums(A_phyloseq) > 2,A_phyloseq)
filt_merg <- transform_sample_counts(filt_meth, function(x) x/sum(x)*100)
taxa_edit <- data.frame(tax_table(filt_merg))
taxa_edit <- taxa_edit %>% mutate_at(vars(Order), ~replace_na(., "Uncultured Bacteria")) %>% as.matrix()
tax_table(filt_merg) <- tax_table(taxa_edit)
order_spring <- tax_glom(filt_merg, taxrank = "Order")
order_df_spring <- psmelt(order_spring)

# Prepping the dataset.
# As there are many uncultured bacteria it will flag the issue later down so if it has an ASV type name it will be uncultured and unique.

uncultured <- filter(order_df_spring, Order == "Uncultured Bacteria")
unc_df_B <- select(uncultured, c("Sample", "A_variable", "A_variable", "A_variable","OTU", "Abundance"))
unc_df_B <- spread(unc_df_B, OTU, Abundance)

cultured <- filter(order_df_spring, Order != "Uncultured Bacteria")
cul_df_B <- select(cultured, c("Sample" ,"A_variable", "A_variable","Order", "Season", "Abundance"))
cul_df_B %>% pivot_wider(names_from = Order, values_from = Abundance, values_fn = list(Abundance = sum))

sig_df_B <- bind_cols(cul_df_B, unc_df_B[3:ncol(unc_df_B)])

krusk_B <- sapply(cul_df_B, function(x) kruskal.test(x ~ cul_df_B$Sample))
krusk_B <- as.data.frame(t(krusk_B))
krusk_B
krusk_B_sig <- filter(krusk_B, p.value < 0.05)
krusk_B_sig

sig_class_B <- rownames(krusk_B_sig)

# Dunn test - using the False discovery rate (bh)
Dunn_class_B <- dunnTest(Abundance ~ Sample, data = cul_df_B, method="bh")
Dunn_class_B

