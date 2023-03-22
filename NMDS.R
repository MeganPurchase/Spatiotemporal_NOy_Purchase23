library(vegan)
library(ape)
library(dplyr)

# With this command, you`ll perform a NMDS and plot the results
My_data %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

dist <- vegdist(My_data,  method = "bray")

NMDS3 <- metaMDS(My_data, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")

ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)

ef <- envfit(NMDS3, My_data_env, permu = 999, na.rm = TRUE)
ef

plot(NMDS3, type = "t", display = "sites")
plot(ef, p.max = 0.05)


orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)
