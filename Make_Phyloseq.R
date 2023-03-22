#Get your OTU table into a phyloseq object

#BendiR package required 

library(bendiR)

#install.packages("phyloseq")
library(phyloseq)

#install.packages("gdata")
library(gdata)


# Save otu table as in 'Test_phyloseq_input_otu_table' but save as a text file.
# Make sure the cell A1 is "# Constructed from biom file" and that cell B1 is "#OTU ID" and the column name for the taxonomy is "taxonomy".
# Make sure there are no empty cells or extra data i.e. column totals in excel sheets before saving as .txt
# OTU names/numbers should not start with a number
#Hyphens or spaces in sample names will become a full stop (.). Therefore it is best to change these in the excel sheet before importing and make sure they match the sample names in the meta file.

#read in otu table.
Phy <- read.csv("/File_path/otu_table.txt",header=T,sep="\t",skip=1)
Phy <- otu_table.txt
#Add meta data as a text file. Sample names as rows in column 1. There should be nothing in cell A1. Make sure sample names match OTU table.
meta <- read.csv ("/File_path/metadata.txt", header=T,sep="\t",row.names=1)
meta <-metadata.txt
#Get tables into structure
#taxonomy is in last column, grab 1st column to one before last (-1)
Phy_otu <- Phy[,2:(ncol(Phy)-1)]

#Change rownames to that of column X.OTU.ID
rownames(Phy_otu) <- Phy$X.OTU.ID

#Convert into phyloseq objects, chnage amplicon to 16S, ITS, 18S. make_tax_table is a function from bendiR so you will either need bendiR installed or run the function first.
taxonomy <- make_tax_table(Phy,"Taxonomy","X.OTU.ID",amplicon="16S",fill_na=TRUE)

#Make phyloseq object by adding otu_table (OTU numbers), tax_table(taxa table),sample_data (meta table)

Phyloseq_object<-phyloseq(otu_table(Phy_otu, taxa_are_rows = TRUE),tax_table(taxonomy),sample_data(meta))

#Remove unwanted objects
rm(taxonomy,Phy_otu,Phy)

Phyloseq_object

#Look at your data
otu_table(Phyloseq_object)
tax_table(Phyloseq_object)
sample_data(Phyloseq_object)

Name_phyloseq <- Phyloseq_object

#Saves the phyloseq. Keep the name the same to save confusion later.

save(Name_phyloseq,file="/File_path/Name_phyloseq.r")


