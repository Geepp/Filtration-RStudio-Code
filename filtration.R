library(tidyr)
library(ggplot2)
library(patchwork)
setwd()

data_oyster<-read.table(file="oyster_filtration_data.tsv", sep="\t", header=TRUE)
str(data_oyster)

oyster1 = lm(log_Chl ~ Time, data = data_oyster)
summary(oyster1)
 
ggplot(data_oyster,aes(x=Time,y=log_Chl))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Oyster Chl fluorescence as a function of time", x="Time (min)",
       y="log Chl")+
annotate(geom="text", x=8.1, y=0.69, label="R² = 0.75 ")+
  annotate(geom="text", x=11, y=0.8, label="p-value = 2.2e-16 ")+
  annotate(geom="text", x=9.1, y=0.6, label="y=-0.016787")

p3 <- ggplot(data_oyster,aes(x=Time,y=log_Chl))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Oyster Chl fluorescence as a function of time", x="Time (min)",
       y="log Chl")+
  annotate(geom="text", x=8.1, y=0.69, label="R² = 0.75 ")+
  annotate(geom="text", x=11, y=0.8, label="p-value = 2.2e-16 ")+
  annotate(geom="text", x=9.1, y=0.58, label="y=-0.016787")

data_mussle<-read.table(file="mussles_filtration_data.tsv", sep="\t", header=TRUE)
str(data_mussle)

mussle1 = lm(log_Chl ~ Time, data = data_mussle)
summary(mussle1)

ggplot(data_mussle, aes(x=Time,y=log_Chl))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Mussle Chl fluorescence as a function of time", x="Time (min)",
       y="log Chl")+
  annotate(geom="text", x=7.7, y=1.195, label="R² = 0.54 ")+
  annotate(geom="text", x=11, y=1.25, label="p-value = 3.77e-13 ")+
  annotate(geom="text", x=9.1, y=1.15, label="k=-0.0065844")

p4 <- ggplot(data_mussle, aes(x=Time,y=log_Chl))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Mussle Chl fluorescence as a function of time", x="Time (min)",
       y="")+
  annotate(geom="text", x=7.7, y=1.195, label="R² = 0.54 ")+
  annotate(geom="text", x=11, y=1.25, label="p-value = 3.77e-13 ")+
  annotate(geom="text", x=9.1, y=1.14, label="y=-0.0065844")

#Merge flt graphs

p3 + p4

#linear model slope calculation for individual lenghts 

oystercm<-read.table(file="oyster1cm.tsv", sep="\t", header=TRUE)
str(oystercm)

oystercm = lm(log_Chl ~ Time, data = oystercm)
summary(oystercm)

#lm model for mussels

musselcm<-read.table(file="musselcm.tsv", sep="\t", header=TRUE)
str()

musselcm = lm(log_Chl ~ Time, data = musselcm)
summary(musselcm)

################Graphs for oyster filtration rates across lenghts###############

oyster_fltr <- read.table(file="oyster_fltr.tsv", sep="\t", header=TRUE)
str(oyster_fltr)

ggplot(oyster_fltr, aes(x=Lenght,y=L.min))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Oyster filtration rate across lenghts", x="Lenght (cm)",
       y="Filtration rate (L/min)")

p1 <- ggplot(oyster_fltr, aes(x=Lenght,y=L.min))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Oyster filtration rate across lenghts", x="Lenght (cm)",
       y="Filtration rate (L/min)")+
  annotate(geom = "text", x=8, y=0.075, label ="Intercept:0.0247419")+
  annotate(geom="text", x=8.4, y=0.070, label = "Slope:0.0008853")

oyster_fltr = lm(L.min ~ Lenght, data = oyster_fltr)
summary(oyster_fltr)

###########Graphs for mussel filtration rates across lenghts####################

mussel_fltr <- read.table(file="mussel_fltr.tsv", sep="\t", header=TRUE)
str(mussel_fltr)

ggplot(mussel_fltr, aes(x=Lenght,y=L.min))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Mussel filtration rate across lenghts", x="Lenght (cm)",
       y="Filtration rate (L/min)")

p2 <- ggplot(mussel_fltr, aes(x=Lenght,y=L.min))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Mussel filtration rate across lenghts", x="Lenght (cm)",
       y="")+
  annotate(geom="text", x =4, y=0.02,label = "Intercept:0.0010018")+
  annotate(geom="text", x =4.2, y=0.0185,label = "Slope:0.0012653")

mussel_fltr = lm(L.min ~ Lenght, data = mussel_fltr)
summary(mussel_fltr)

#Combine regression plots

p1 + p2

############################Graphs for dry weight##############################

library(dplyr)
library(broom)

oysterweight<-read.table(file="oyster_weight.tsv", sep="\t", header=TRUE)
str(oysterweight)

ggplot(oysterweight, aes(x=Time,y=logChl))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Oyster filtration rate across weights", x="",
       y="Filtration rate (L/min)")+
  facet_wrap(~ Individual)
  
slopes_df <- oysterweight %>%
  group_by(Individual) %>%
  do(tidy(lm(logChl ~ Time, data = .))) %>%
  filter(term == "Time") %>%
  select(Individual, slope = estimate)

oysterweight_with_slopes <- oysterweight %>%
  left_join(slopes_df, by = "Individual")

###################### Deriving individual slopes for weight ##################


musselweight<-read.table(file="mussel_weight.tsv", sep="\t", header=TRUE)

ggplot(musselweight, aes(x=Time,y=logChl))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Mussel filtration rate across weights", x="",
       y="Filtration rate (L/min)")+
  facet_wrap(~ Individual)

slopes_df <- musselweight %>%
  group_by(Individual) %>%
  do(tidy(lm(logChl ~ Time, data = .))) %>%
  filter(term == "Time") %>%
  select(Individual, slope = estimate)

musselweight_with_slopes <- musselweight %>%
  left_join(slopes_df, by = "Individual")

#Oyster slope graph

oyster_wgt <- read.table(file="oyster_wgt.tsv", sep="\t", header=TRUE)

ggplot(oyster_fltr, aes(x=Weight,y=L.min))+
  geom_point(size=3, shape=18)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Oyster filtration rate across weights", x="Weight (g)",
       y="Filtration rate (L/min)")

############### Patchwork graph for L/min vs. Weight ##########################

oyster_wgt <- read.table(file="oyster_wgt.tsv", sep="\t", header=TRUE)

p5 = ggplot(oyster_wgt, aes(x=Weight,y=L.min))+
  geom_point(size=3, shape=18)+
  ylim(-0.02, 0.1)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Oyster filtration rate across weights", x="Weight (g)",
       y="Filtration rate (L/min)")+
  annotate(geom="text", x =3, y=0.09,label = "Intercept:0.03464")+
  annotate(geom="text", x =3, y=0.083,label = "Slope:0.0004225")

oyster_weight = lm(L.min ~ Weight, data = oyster_wgt)
summary(oyster_weight)

mussel_wgt <- read.table(file="mussel_wgt.tsv", sep="\t", header=TRUE)

p6 = ggplot(mussel_wgt, aes(x=Weight,y=L.min))+
  geom_point(size=3, shape=18)+
  ylim(-0.02, 0.1)+
  geom_smooth(method="lm",se=TRUE, color="grey", alpha=0.2)+
  labs(title="Mussel filtration rate across weights", x="Weight (g)",
       y="")+
  annotate(geom="text", x =0.5, y=0.09,label = "Intercept:0.00813")+
  annotate(geom="text", x =0.5, y=0.083,label = "Slope:0.001271")

mussel_weight = lm(L.min ~ Weight, data = mussel_wgt)
summary(mussel_weight)

#Patchwork
p5 + p6


######################## Abundance map using .shp files #######################

my_sf <- read_sf(
  file.path(getwd(), "water.tjarno.shp")
)
str(my_sf)

#Oyster abundance data
oyster_abundance <- read.table(file="oyster_abundance.tsv", sep="\t", header=TRUE)
str(oyster_abundance)

#Mussel abundance data
mussel_abundance <- read.table(file="mussel_abundance.tsv", sep="\t", header=TRUE)

shp <- st_transform(my_sf, crs = 4326)

minlong <- 11.110289+
  maxlong <- 11.146490+
  minlat  <- 58.873681+
  maxlat  <- 58.892311+
  
  bbox <- st_as_sfc(st_bbox(c(xmin = minlong, xmax = maxlong,
                              ymin = minlat,  ymax = maxlat),
                            crs = st_crs(shp)))

clipped <- st_intersection(my_sf, bbox)


#Oyster abundance map

ggplot() +
  geom_sf(data = clipped, fill = "lightblue", color = "black") +
  theme_minimal()+
  scale_color_gradient(low = "blue", high = "orange")+
  labs(title="Oyster abundance per site", x=" ",
       y=" ", color = "Abundance")+
  scale_size("Abundance")+
  geom_point(data = oyster_abundance, aes(11.14033, 58.87557, color= site1, size = site1))+
  geom_point(data = oyster_abundance, aes(11.14002, 58.87562, color= site2, size = site2))+
  geom_point(data = oyster_abundance, aes(11.13943, 58.87599, color= site3, size = site3))+
  geom_point(data = oyster_abundance, aes(11.13902, 58.87618, color= site4, size = site4))+
  geom_point(data = oyster_abundance, aes(11.12227, 58.87923, color= site5, size = site5))+
  geom_point(data = oyster_abundance, aes(11.12526, 58.87800, color= site6, size = site6))+
  geom_point(data = oyster_abundance, aes(11.12811, 58.87780, color= site7, size = site7))+
  geom_point(data = oyster_abundance, aes(11.12848, 58.87895, color= site8, size = site8))+
  geom_point(data = oyster_abundance, aes(11.12872, 58.87915, color= site9, size = site9))+
  geom_point(data = oyster_abundance, aes(11.12956, 58.87893, color= site10, size = site10))+
  geom_point(data = oyster_abundance, aes(11.14506, 58.87471, color= site11, size = site11))+
  geom_point(data = oyster_abundance, aes(11.14560, 58.87651, color= site12, size = site12))+
  geom_point(data = oyster_abundance, aes(11.14131, 58.87804, color= site13, size = site13))+
  geom_point(data = oyster_abundance, aes(11.14283, 58.88258, color= site14, size = site14))+
  geom_point(data = oyster_abundance, aes(11.12351, 58.88437, color= site15, size = site15))+
  geom_point(data = oyster_abundance, aes(11.12830, 58.88351, color= site16, size = site16))+
  geom_point(data = oyster_abundance, aes(11.13216, 58.88282, color= site17, size = site17))+
  geom_point(data = oyster_abundance, aes(11.13129, 58.88136, color= site18, size = site18))+
  geom_point(data = oyster_abundance, aes(11.12870, 58.88150, color= site19, size = site19))

#Mussel abundance map

ggplot() +
  geom_sf(data = clipped, fill = "lightblue", color = "black") +
  theme_minimal()+
  scale_color_gradient(low = "blue", high = "orange")+
  labs(title="Mussel abundance per site", x=" ",
       y=" ", color = "Abundance")+
  scale_size("Abundance")+
  guides(color = guide_colorbar(order=1))+
  geom_point(data = mussel_abundance, aes(11.14033, 58.87557, color= site1, size = site1))+
  geom_point(data = mussel_abundance, aes(11.14002, 58.87562, color= site2, size = site2))+
  geom_point(data = mussel_abundance, aes(11.13943, 58.87599, color= site3, size = site3))+
  geom_point(data = mussel_abundance, aes(11.13902, 58.87618, color= site4, size = site4))+
  geom_point(data = mussel_abundance, aes(11.12227, 58.87923, color= site5, size = site5))+
  geom_point(data = mussel_abundance, aes(11.12526, 58.87800, color= site6, size = site6))+
  geom_point(data = mussel_abundance, aes(11.12811, 58.87780, color= site7, size = site7))+
  geom_point(data = mussel_abundance, aes(11.12848, 58.87895, color= site8, size = site8))+
  geom_point(data = mussel_abundance, aes(11.12872, 58.87915, color= site9, size = site9))+
  geom_point(data = mussel_abundance, aes(11.12956, 58.87893, color= site10, size = site10))+
  geom_point(data = mussel_abundance, aes(11.14506, 58.87471, color= site11, size = site11))+
  geom_point(data = mussel_abundance, aes(11.14560, 58.87651, color= site12, size = site12))+
  geom_point(data = mussel_abundance, aes(11.14131, 58.87804, color= site13, size = site13))+
  geom_point(data = mussel_abundance, aes(11.14283, 58.88258, color= site14, size = site14))+
  geom_point(data = mussel_abundance, aes(11.12351, 58.88437, color= site15, size = site15))+
  geom_point(data = mussel_abundance, aes(11.12830, 58.88351, color= site16, size = site16))+
  geom_point(data = mussel_abundance, aes(11.13216, 58.88282, color= site17, size = site17))+
  geom_point(data = mussel_abundance, aes(11.13129, 58.88136, color= site18, size = site18))+
  geom_point(data = mussel_abundance, aes(11.12870, 58.88150, color= site19, size = site19))


#Patchwork work..

p1 <- ggplot() +
  geom_sf(data = clipped, fill = "lightblue", color = "black") +
  theme_minimal()+
  scale_color_gradient(low = "blue", high = "orange")+
  labs(title="Oyster abundance per site", x=" ",
       y=" ", color = "Abundance")+
  scale_size("Abundance")+
  geom_point(data = oyster_abundance, aes(11.14033, 58.87557, color= site1, size = site1))+
  geom_point(data = oyster_abundance, aes(11.14002, 58.87562, color= site2, size = site2))+
  geom_point(data = oyster_abundance, aes(11.13943, 58.87599, color= site3, size = site3))+
  geom_point(data = oyster_abundance, aes(11.13902, 58.87618, color= site4, size = site4))+
  geom_point(data = oyster_abundance, aes(11.12227, 58.87923, color= site5, size = site5))+
  geom_point(data = oyster_abundance, aes(11.12526, 58.87800, color= site6, size = site6))+
  geom_point(data = oyster_abundance, aes(11.12811, 58.87780, color= site7, size = site7))+
  geom_point(data = oyster_abundance, aes(11.12848, 58.87895, color= site8, size = site8))+
  geom_point(data = oyster_abundance, aes(11.12872, 58.87915, color= site9, size = site9))+
  geom_point(data = oyster_abundance, aes(11.12956, 58.87893, color= site10, size = site10))+
  geom_point(data = oyster_abundance, aes(11.14506, 58.87471, color= site11, size = site11))+
  geom_point(data = oyster_abundance, aes(11.14560, 58.87651, color= site12, size = site12))+
  geom_point(data = oyster_abundance, aes(11.14131, 58.87804, color= site13, size = site13))+
  geom_point(data = oyster_abundance, aes(11.14283, 58.88258, color= site14, size = site14))+
  geom_point(data = oyster_abundance, aes(11.12351, 58.88437, color= site15, size = site15))+
  geom_point(data = oyster_abundance, aes(11.12830, 58.88351, color= site16, size = site16))+
  geom_point(data = oyster_abundance, aes(11.13216, 58.88282, color= site17, size = site17))+
  geom_point(data = oyster_abundance, aes(11.13129, 58.88136, color= site18, size = site18))+
  geom_point(data = oyster_abundance, aes(11.12870, 58.88150, color= site19, size = site19))


p2 <- ggplot() +
  geom_sf(data = clipped, fill = "lightblue", color = "black") +
  theme_minimal()+
  scale_color_gradient(low = "blue", high = "orange")+
  labs(title="Mussel abundance per site", x=" ",
       y=" ", color = "Abundance")+
  scale_size("Abundance")+
  guides(color = guide_colorbar(order=1))+
  geom_point(data = mussel_abundance, aes(11.14033, 58.87557, color= site1, size = site1))+
  geom_point(data = mussel_abundance, aes(11.14002, 58.87562, color= site2, size = site2))+
  geom_point(data = mussel_abundance, aes(11.13943, 58.87599, color= site3, size = site3))+
  geom_point(data = mussel_abundance, aes(11.13902, 58.87618, color= site4, size = site4))+
  geom_point(data = mussel_abundance, aes(11.12227, 58.87923, color= site5, size = site5))+
  geom_point(data = mussel_abundance, aes(11.12526, 58.87800, color= site6, size = site6))+
  geom_point(data = mussel_abundance, aes(11.12811, 58.87780, color= site7, size = site7))+
  geom_point(data = mussel_abundance, aes(11.12848, 58.87895, color= site8, size = site8))+
  geom_point(data = mussel_abundance, aes(11.12872, 58.87915, color= site9, size = site9))+
  geom_point(data = mussel_abundance, aes(11.12956, 58.87893, color= site10, size = site10))+
  geom_point(data = mussel_abundance, aes(11.14506, 58.87471, color= site11, size = site11))+
  geom_point(data = mussel_abundance, aes(11.14560, 58.87651, color= site12, size = site12))+
  geom_point(data = mussel_abundance, aes(11.14131, 58.87804, color= site13, size = site13))+
  geom_point(data = mussel_abundance, aes(11.14283, 58.88258, color= site14, size = site14))+
  geom_point(data = mussel_abundance, aes(11.12351, 58.88437, color= site15, size = site15))+
  geom_point(data = mussel_abundance, aes(11.12830, 58.88351, color= site16, size = site16))+
  geom_point(data = mussel_abundance, aes(11.13216, 58.88282, color= site17, size = site17))+
  geom_point(data = mussel_abundance, aes(11.13129, 58.88136, color= site18, size = site18))+
  geom_point(data = mussel_abundance, aes(11.12870, 58.88150, color= site19, size = site19))

#Patchwork to get both maps in one

p1 + p2



