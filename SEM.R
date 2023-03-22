#SEM

#packages

packload <- c("piecewiseSEM","semPlot", "readxl", "corrr", "lme4")

#load your data here
# it has to be a table imported from excel or a data frame created in R
read_xls()
df = My_data
###################################################### SEM 
#Piecewise SEM N Cycle Orders

#composite variables: it is not a built in PSEM feature yet so have to compute it by hand
# run multiple regression
orders_model <- lm(Mean_flow_NOZNO2 ~ Cenarchaeales + Opitutales + Bacillales
                   + Gaiellales + Rubrobacterales + Rhodocyclales + Clostridiales
                   + Nostocales + Rhodobacterales + Rhizobiales + Nitrosomonadales + Nitrosphaerales + Nitrospirales, data = df)

summary(orders_model)

# get loadings [2and up,1]

beta_Cenarchaeles<- summary(orders_model)$coefficients[2, 1]
beta_Opitutales <- summary(orders_model)$coefficients[3, 1]
beta_Bacillales <- summary(orders_model)$coefficients[4, 1]
beta_Gaiellales <- summary(orders_model)$coefficients[5, 1]
beta_Rubrobacterales <-  summary(orders_model)$coefficients[6, 1]
beta_Rhodocyclales<- summary(orders_model)$coefficients[7, 1]
beta_Clostridiales <- summary(orders_model)$coefficients[8, 1]
beta_Nostocales <- summary(orders_model)$coefficients[9, 1]
beta_Rhodobacterales <- summary(orders_model)$coefficients[10, 1]
beta_Rhizobiales <-  summary(orders_model)$coefficients[11, 1]
beta_Nitrosomonadales <- summary(orders_model)$coefficients[12, 1]
beta_Nitrosphaerales <- summary(orders_model)$coefficients[13, 1]
beta_Nitrospirales <- summary(orders_model)$coefficients[14, 1]


# compute factor scores
orders <-  beta_Cenarchaeles * df$Cenarchaeales + beta_Opitutales * df$Opitutales
+ beta_Bacillales * df$Bacillales + beta_Gaiellales * df$Gaiellales + beta_Rubrobacterales * df$Rubrobacterales
+ beta_Rhizobiales * df$Rhizobiales + beta_Nitrosomonadales * df$Nitrosomonadales
+ beta_Nitrosphaerales * df$Nitrosphaerales + beta_Nitrospirales * df$Nitrospirales

summary(lm(Mean_flow_NOZNO2  ~ orders, data = data.frame(df, orders)))

#obtain standardised coeficients
coefs(lm(Mean_flow_NOZNO2  ~ orders, data = data.frame(df, orders)))

######################################################################3

#now the same composite calculation for N-cycle rates
rates_model <- lm(Mean_flow_NOZNO2  ~ NetNitrificationRate + NMineralisationRate
                  + AmmonificationRate, data =df)

summary(rates_model)

# get loadings [2and up,1]

beta_NetNitrificationRate <- summary(rates_model)$coefficients[2, 1]
beta_NMineralisationRate <- summary(rates_model)$coefficients[3, 1]
beta_AmmonificationRate<-  summary(rates_model)$coefficients[4, 1]

# compute factor scores
rates <- beta_NetNitrificationRate * df$NetNitrificationRate + beta_NMineralisationRate * df$NMineralisationRate
+ beta_AmmonificationRate * df$AmmonificationRate

summary(lm(Mean_flow_NOZNO2  ~ rates, data = data.frame(df, rates)))

#obtain standardized coefficients
coefs(lm(Mean_flow_NOZNO2  ~ rates, data = data.frame(df, rates)))

###############################################################################
#now the same composite calculation for Soil Characteristics
soil_model <- lm(Mean_flow_NOZNO2  ~ pH + SoilMoistureContent, data = df)

summary(soil_model)

# get loadings [2and up,1]
beta_pH <- summary(soil_model)$coefficients [2,1]
beta_SoilMoistureContent <- summary(soil_model)$coefficients[3, 1]


# compute factor scores
soil <- beta_pH * df$pH + beta_SoilMoistureContent * df$SoilMoistureContent

summary(lm(Mean_flow_NOZNO2  ~ soil, data = data.frame(df, soil)))

#obtain standardized coefficients
coefs(lm(Mean_flow_NOZNO2  ~ soil, data = data.frame(df, soil)))

soiltype_model <- lm(Mean_flow_NOZNO2  ~  SoilType, data = df)

summary(soiltype_model)

######################################################################
#now the same composite calculation for Land use
landuse_model <- lm(Mean_flow_NOZNO2  ~ SoilType, data = df)

summary(landuse_model)

# get loadings [2and up,1]
beta_SoilType <- summary(landuse_model)$coefficients [2,1]


# compute factor scores
landuse <- beta_SoilType * df$SoilType

summary(lm(Mean_flow_NOZNO2  ~ landuse, data = data.frame(df, landuse)))

#obtain standardized coefficients
coefs(lm(Mean_flow_NOZNO2  ~ landuse, data = data.frame(df, landuse)))

landuse_model <- lm(Mean_flow_NOZNO2  ~  SoilType, data = df)

summary(landuse_model)

##############################################################
#composite calculation for Metals

#metal_model <- lm(Peak_flow_NOZNO2 ~  Cd + Cu + Fe + Pb + Ni + Zn, data = df)

#summary(metal_model)

# get loadings [2and up,1]

#beta_Cd <- summary(metal_model)$coefficients[2, 1]
#beta_Cu <- summary(metal_model)$coefficients[3,1]
#beta_Fe <- summary(metal_model)$coefficients[4,1]
#beta_Pb <- summary(metal_model)$coefficients[5,1]
#beta_Ni <- summary(metal_model)$coefficients[6,1]
#beta_Zn <- summary(metal_model)$coefficients[7,1]


# compute factor scores
#metalconcs <- beta_Cd * df$Cd + beta_Cu * df$Cu + beta_Fe * df$Fe + beta_Pb * df$Pb + beta_Ni * df$Ni + beta_Zn * df$Zn
#summary(lm(Peak_flow_NOZNO2 ~ metalconcs, data = data.frame(df, metalconcs)))

#obtain standerdised coeficients
#coefs(lm(Peak_flow_NOZNO2 ~ metalconcs, data = data.frame(df, metalconcs)))

###############

human_model <- lm(Mean_flow_NOZNO2  ~  Human_influence, data = df)

summary(human_model)

# get loadings [2and up,1]

beta_Human_influence <- summary(human_model)$coefficients[2, 1]
#beta_Cu <- summary(metal_model)$coefficients[3,1]
#beta_Fe <- summary(metal_model)$coefficients[4,1]
#beta_Pb <- summary(metal_model)$coefficients[5,1]
#beta_Ni <- summary(metal_model)$coefficients[6,1]
#beta_Zn <- summary(metal_model)$coefficients[7,1]


# compute factor scores
humaninfluence <- beta_Human_influence * df$Human_influence
summary(lm(Mean_flow_NOZNO2  ~ humaninfluence, data = data.frame(df, humaninfluence)))

#obtain standerdised coeficients
coefs(lm(Mean_flow_NOZNO2  ~ humaninfluence, data = data.frame(df, humaninfluence)))

############################################
###comprehensive model for NOy

Model_NOyALL <- psem(
  lmer(Mean_flow_NOZNO2  ~ orders + rates + soil + landuse + humaninfluence + (1|Season), data = data.frame(df, orders, rates, soil, landuse, humaninfluence)))
  lm(orders ~ soil, data = data.frame(orders, soil)),
  lm(orders ~ humaninfluence, data = data.frame(orders, humaninfluence)),
  lm(rates ~ soil, data = data.frame(rates, soil)),
  lm(orders ~ landuse, data = data.frame(orders, landuse)),
  lm(rates ~ humaninfluence, data = data.frame(rates, humaninfluence)),
  lm(humaninfluence ~ landuse, data = data.frame(humaninfluence, landuse)),
  lm(soil ~landuse, data = data.frame(soil, landuse)),
  lm(soil ~ humaninfluence, data = data.frame(soil, humaninfluence)))


SEM_NOyALL_soiltype <- plot(
  Model_NOyALL,
  return = F,
  node_attrs = data.frame(shape = "rectangle", style = "rounded", fixedsize = F, color = "#2F4F4F", fillcolor = "white", fontsize = 8, penwidth = 2),
  edge_attrs = data.frame(style = "solid", color = "black", arrowhead = "none"),
  ns_dashed = T,
  alpha = 0.05,
  show = "std",
  digits = 3,
  add_edge_label_spaces = F,
  title = "NOy PiecewiseSEM final model by SoilType")

SEM_NOyALL_soiltype

summary(
  SEM_NOyALL_soiltype,
  standardize = "scale",
  standardize.type = "latent.linear",
  test.statistic = "F",
  test.type = "II",
  intercepts = FALSE,
  .progressBar = TRUE)

coefs(
  SEM_NOyALL,
  standardize = "scale",
  standardize.type = "latent.linear",
  test.statistic = "F",
  test.type = "II",
  intercepts = FALSE
)