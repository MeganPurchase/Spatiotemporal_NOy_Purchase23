# Load in Phyloseq

########## Alpha diversity and plot ###################
# Requires BendiR package #

#Run diversity_plot4 to line 69

diversity_plot4<-function(physeq,grouping,diversity=c("fishers","shannon","simpson","observed"),test=c("anova","kruskal"),correction_method=stats::p.adjust.methods){
  if(!phyloseq::taxa_are_rows(physeq)){
    phyloseq::otu_table(physeq)<-t(phyloseq::otu_table(physeq))
  }
  test <- base::match.arg(test,c("anova","kruskal"))
  diversity <- base::match.arg(diversity,c("fishers","shannon","simpson","observed"))
  if(length(correction_method)>1){
    corr_specified<-FALSE
  } else{
    corr_specified<-TRUE
  }
  if(length(test)==2){
    warning("Defaulting to ANOVA")
    test<-"anova"
  }
  if(!length(grep(grouping,colnames(phyloseq::sample_data(physeq))))==0){
    if(diversity=="fishers"){
      div<-vegan::fisher.alpha(t(phyloseq::otu_table(physeq)))
    }  else if(diversity=="observed"){
      div<-colSums(otu_table(physeq) != 0)
    } else {
      div<-vegan::diversity(t(phyloseq::otu_table(physeq)),diversity)
    }
    if(test=="anova"){
      anova<-stats::aov(div ~ as.character(phyloseq::sample_data(physeq)[[grouping]]))
      post_hoc<-stats::TukeyHSD(anova)
      means<-stats::aggregate(anova$model[, 1], list(anova$model[,2]), mean)
      letters<-compact_letters(phyloseq::sample_data(physeq)[[grouping]],post_hoc)
    } else{
      correction_method <- base::match.arg(correction_method)
      if(corr_specified == FALSE){
        warning("Multiple testing correction post-hoc p-values with FDR")
        correction_method<-"fdr"
      }
      kruskal<-stats::kruskal.test(div ~ as.factor(phyloseq::sample_data(physeq)[[grouping]]))
      post_hoc<-DescTools::DunnTest(div~as.factor(phyloseq::sample_data(physeq)[[grouping]]),method=correction_method)
      diversity_table<-cbind.data.frame(div,group=as.character(phyloseq::sample_data(physeq)[[grouping]]))
      means<-stats::aggregate(diversity_table[,"div"], list(diversity_table[,"group"]), mean)
      letters<-compact_letters(phyloseq::sample_data(physeq)[[grouping]],post_hoc)
    }
    
    names(means)<-c("Group","mean")
    
  } else{
    stop(paste("Grouping factor entered does not exist in phyloseq::sample_data. Check phyloseq::sample_data(",substitute(physeq),").",sep=""))
  }
  if(!exists("diversity_table")){
    diversity_table<-cbind.data.frame(div,group=as.character(phyloseq::sample_data(physeq)[[grouping]]))
  }
  
  if(typeof(letters)=="character"){
    letters<-cbind.data.frame(sample=names(letters),letters)
  } else{
    letters<-cbind.data.frame(sample=names(letters$Letters),letters=letters$Letters)
    print(letters)
  }
  plot<-ggplot2::ggplot(data=diversity_table)+ggplot2::geom_boxplot(aes(x=group,y=div,fill=group, colour=group),alpha=0.3,outlier.shape=NA)+ggplot2::geom_jitter(aes(x=group,y=div, colour=group),alpha=0.5)+ggplot2::geom_text(data=letters,aes(x=sample,y=Inf,label=letters,vjust = 1,fontface="bold"),size=4)+theme_journal()+scale_x_discrete("")+scale_y_continuous("Fishers")+ theme(legend.title = element_blank())#+
  #theme(axis.title.x=element_blank(),
  #axis.text.x=element_blank(),
  #axis.ticks.x=element_blank())
  return(list(POSTHOC=post_hoc,PLOT=plot))
}

phyloa <- loaded_phyloseq

phyloa

sample_variables(phyloa)

sample_sums(phyloa)

min(sample_sums(phyloa))


#Rarefy if needed (for all except fishers).

set.seed(123456)

phyloa <- rarefy_even_depth(phyloa, sample.size = min(sample_sums(phyloa)), rngseed = FALSE, replace = FALSE, trimOTUs = TRUE, verbose = TRUE)

# You dont need to rarefy for Fishers alpha diversity, so you can use just phyloa if you are doing fishers ( in ' estimate_richness(phyloa, measures = "Fisher") ')

estimate_richness(phyloa, measures = "Fisher")

#Alpha diversity. phylo, "variable", diversity= can use "observed", "fishers", "shannon", simpsons", test="kruskal" or "anova". Change y axis label on line 72 if the method is changed. Can change multiple comparison correction if lots of groups (>10?) from "none" to fdr or other.

run_alpha <- diversity_plot4(phyloa,"Variable", diversity="fishers",test="kruskal", correction_method = "none")

run_alpha

out <- capture.output(alpha)
cat(out, file="alpha_fisher_p_values.txt", sep="\n")

#Order the variables differently
season_alpha2 <- season_alpha$PLOT+scale_x_discrete(limits=c("Winter","Spring","Summer"), labels=c("Winter","Spring","Summer"))

#Rotate axis labels 90o if needed.
#plot2 <- plot1 + theme(axis.text.x = element_text(angle = 90, vjust=0.55))

#plot2

#Remove key

plot3 <-plot1 + theme(legend.position="none")

plot3

#Customise colours (Change to the same order for scale_fill_manual and scale_color_manual)
plot4 <- plot3+ scale_fill_manual(values = c("#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#BC80BD", "#CCEBC5", "#FFED6F", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494")) + scale_color_manual(values = c("#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#BC80BD", "#CCEBC5", "#FFED6F", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494"))

plot4

#To remove x axis labels and ticks
#alpha$PLOT+theme(axis.title.x=element_blank(),
#axis.text.x=element_blank(),
#axis.ticks.x=element_blank())

#Save pdf

ggsave("alpha_fisher_plot.pdf", width = 20, height = 10, units = "cm") #saves the current plot as a .pdf or .png, the size can be altered.


