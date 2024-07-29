
##########################################################
############## CLSA Full Sample Clustering ###############
##########################################################

# Libraries #####
library(factoextra)
library(fastcluster)
library(cluster)
library(tidyverse)
library(qwraps2)
library(summarytools)
library(NbClust)
library(dendextend)
library(corrplot)
library(gt)
library(paletteer)
library(broom)
library(ggridges)
library(ggpubr)
library(nnet)
library(caret)
library(gtsummary)
library(magick)
library(ggsci)

# Read in data ####

CLSA<-read_csv("CLSA.csv")

CLSA.full<-read_csv("CLSA.csv") %>%
  select(entity_id, Age.group, PA2_WALK_MCQ, PA2_SIT_MCQ, light.m, exer, 
         NUR_FRTVEG_MCQ.rev, smoking, ALC_FREQ.rev,CCT_F2) %>%
  rename(walking = PA2_WALK_MCQ, sitting =PA2_SIT_MCQ, light_PA = light.m,
         exercise = exer, fruit_vegetable = NUR_FRTVEG_MCQ.rev,
         alcohol = ALC_FREQ.rev) %>%
  drop_na()

# standardize ####
CLSA.full.stand<-CLSA.full %>% mutate_each_(list(~scale(.) %>% as.vector), 
                                            vars=c("sitting", "walking", "light_PA", "exercise", "fruit_vegetable",   
                                                   "smoking", "alcohol")) %>%
  as_tibble()


#remove id and age group (only standardized health behaviours remain)
CLSA.full.stand.c<-CLSA.full.stand %>% select(-entity_id, -Age.group)

# cluster analysis ####
hb.gower.dist<-daisy(CLSA.full.stand.c, metric = "gower")

# Perform Cluster Analysis with 'Single Linkage'
hb.clust.single<-hclust(hb.gower.dist, method = "single")
# Perform Cluster Analysis with 'Complete Linkage'
hb.clust.complete<-hclust(hb.gower.dist, method = "complete")
# Perform Cluster Analysis with 'Average Linkage'
hb.clust.average<-hclust(hb.gower.dist, method = "average")
# Perform Cluster Analysis with 'Centroid Linkage'
hb.clust.centroid<-hclust(hb.gower.dist, method = "centroid")
# Perform Cluster Analysis with 'Ward Linkage'
hb.clust.ward<-hclust(hb.gower.dist, method = "ward.D2")


# plot dendograms ####
par(mfrow=c(2,3))
# Plot single
p1<-plot(hb.clust.single, hang = -1, labels = F, main = "Single Linkage")
# Plot complete
p2<-plot(hb.clust.complete, hang = -1, labels = FALSE, main = "Complete Linkage")
# Plot average
p3<-plot(hb.clust.average, hang = -1, labels = F, main = "Average Linkage")
# Plot centroid
p4<-plot(hb.clust.centroid, hang = -1, labels = F, main = "Centroid Linkage")
# Plot Ward
p5<-plot(hb.clust.ward, hang = -1, labels = F, main = "Ward Linkage")
par(mfrow=c(1,1))


# summary tables for k=4 for each linkage method ####

#single
cut.single <-cutree(hb.clust.single, k=4)

CLSA.full.stand.c %>%
  mutate(cluster = cut.single)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Single Linkage (K = 4)"))%>%
  gtsave("CLSA Full - Single - k4 table.png")

#centroid
cut.centroid <-cutree(hb.clust.centroid, k=4)

CLSA.full.stand.c %>%
  mutate(cluster = cut.centroid)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Centroid Linkage (K = 4)"))%>%
  gtsave("CLSA Full - Centroid - k4 table.png")

#centroid
cut.average <-cutree(hb.clust.average, k=4)

CLSA.full.stand.c %>%
  mutate(cluster = cut.average)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Average Linkage (K = 4)"))%>%
  gtsave("CLSA Full - Average - k4 table.png")

#complete
cut.complete <-cutree(hb.clust.complete, k=4)

CLSA.full.stand.c %>%
  mutate(cluster = cut.complete)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Complete Linkage (K = 4)"))%>%
  gtsave("CLSA Full - Complete - k4 table.png")


#ward
cut.ward <-cutree(hb.clust.ward, k=4)

CLSA.full.stand.c %>%
  mutate(cluster = cut.ward)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Ward Linkage (K = 4)"))%>%
  gtsave("CLSA Full - Ward - k4 table.png")

# ward vs complete linkage ####

#entanglement (omit?)
tanglegram(hb.clust.complete, hb.clust.ward, faster=TRUE, edge.lwd = 1, highlight_distinct_edges = FALSE,
           main = paste("entanglement =", round(entanglement(dend_list), 2)),
           lab.cex = 0)

#correlation
hb.clust.complete<-hclust(hb.gower.dist, method = "complete")%>% as.dendrogram()
hb.clust.ward<-hclust(hb.gower.dist, method = "ward.D2")%>% as.dendrogram()


dend_list <- dendlist("Complete" = hb.clust.complete,"Ward" = hb.clust.ward)

cors <- cor.dendlist(dend_list)

x <- data.frame("." = c("Ward", "Complete"), "Ward" = c(1, .34), "Complete" = c(.34, 1))

x %>% gt() %>% gtsave("CLSA Full - Correlation - WardCom.png")


#agglomerative coefficient 

hb.agnes.complete<-agnes(hb.gower.dist, method = "complete")
ac1<-hb.agnes.complete$ac

hb.agnes.ward<-agnes(hb.gower.dist, method = "ward")
ac2<-hb.agnes.ward$ac

ac<-c(ac1, ac2)

t3<-data.frame("Linkage" = c("Complete", "Ward"), "Agglomerative Coefficient" = ac)

t3 %>% 
  gt() %>%
  gtsave("CLSA Full - Agglomerative Coeff.png")


# Determining K (NBClust) ####

# complete
nbclust.comp<-NbClust(CLSA.full.stand.c, diss = hb.gower.dist, distance=NULL,
                      method = "complete")

# ward
nbclust.ward <- NbClust(CLSA.full.stand.c, diss = hb.gower.dist, distance=NULL,
                        method = "ward.D2")



# standardized means for complete linkage (k = 2-4)####

# complete k=2
cut.complete <-cutree(hb.clust.complete, k=2)

CLSA.full.stand.c %>%
  mutate(cluster = cut.complete)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Complete Linkage (K = 2)"))%>%
  data_color(
    columns = 3:9, 
    colors = scales::col_bin(
      bins = c(-Inf, 0, Inf),
      palette = c("red", "white","green"),
    )
  )%>%
  gtsave("CLSA Full - Complete - k2 table.png")


# complete k=3
cut.complete <-cutree(hb.clust.complete, k=3)

c3<-CLSA.full.stand.c %>%
  mutate(cluster = cut.complete)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Complete Linkage (K = 3)"))%>%
  data_color(
    columns = 3:9, 
    colors = scales::col_bin(
      bins = c(-Inf, 0, Inf),
      palette = c("red", "white","green"),
    )
  )%>%
  gtsave("CLSA Full - Complete - k3 table.png")


# complete k=4
cut.complete <-cutree(hb.clust.complete, k=4)

c4<-CLSA.full.stand.c%>%
  mutate(cluster = cut.complete)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Complete Linkage (K = 4)"))%>%
  data_color(
    columns = 3:9, 
    colors = scales::col_bin(
      bins = c(-Inf, 0, Inf),
      palette = c("red", "white","green"),
    )
  )%>%
  gtsave("CLSA Full - Complete - k4 table.png")

#ggthemes::Red-Green Diverging
#pals::kovesi.diverging_gwr_55_95_c38


# standardized means for Ward linkage (k = 2-4)####

# complete k=2
cut.ward <-cutree(hb.clust.ward, k=2)

c2<-CLSA.full.stand.c %>%
  mutate(cluster = cut.ward)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Ward Linkage (K = 2)"))%>%
  data_color(
    columns = 3:9, 
    colors = scales::col_bin(
      bins = c(-Inf, 0, Inf),
      palette = c("red", "white","green"),
    )
  )%>%
  gtsave("CLSA Full - Ward - k2 table.png")



# ward k=3
cut.ward <-cutree(hb.clust.ward, k=3)

c3<-CLSA.full.stand.c %>%
  mutate(cluster = cut.ward)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Ward Linkage (K = 3)"))%>%
  data_color(
    columns = 3:9, 
    colors = scales::col_bin(
      bins = c(-Inf, 0, Inf),
      palette = c("red", "white","green"),
    )
  )%>%
  gtsave("CLSA Full - Ward - k3 table.png")

# ward k=4
cut.ward <-cutree(hb.clust.ward, k=4)

c4<-CLSA.full.stand.c%>%
  mutate(cluster = cut.ward)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(walking),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Ward Linkage (K = 4)"))%>%
  data_color(
    columns = 3:9, 
    colors = scales::col_bin(
      bins = c(-Inf, 0, Inf),
      palette = c("red", "white","green"),
    )
  )%>%
  gtsave("CLSA Full - Ward - k4 table.png")

# ward k=7
cut.ward <-cutree(hb.clust.ward, k=7)

c8<-CLSA.full.stand.c%>%
  mutate(cluster = cut.ward)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(sitting),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))%>%
  round(2)%>%
  gt()%>%
  tab_style(style = list(cell_borders(sides = "left", color = "black",
                                      weight = px(3))), locations = list(cells_body(columns = vars(walking))))%>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black",
                                      weight = px(3))), locations = list(cells_column_labels(columns = gt::everything())))%>%
  tab_header(title = md("Standardized Means for Ward Linkage (K = 7)"))%>%
  data_color(
    columns = 3:9, 
    colors = scales::col_bin(
      bins = c(-Inf, 0, Inf),
      palette = c("red", "white","green"),
    )
  )%>%
  gtsave("CLSA Full - Ward - k7 table.png")

# link cluster data back with outcomes ####

#select outcomes, demographic variables, un-standardized health behaviour vars 
CLSA.indicators<-CLSA %>%
  select(entity_id, GEN_DHDI, GEN_DMHI, GEN_OWNAG.rev, SLS_DSCR, HWT_DBMI,
         HCU_EMEREG_MCQ, HCU_HLOVRNT_MCQ, HCU_NRSHM_MCQ, SEX_ASK, SDC_MRTL,
         INC_TOT, SSA_DPALL, RET_RTRD, LBF_CURR, PA2_WALK_MCQ, PA2_SIT_MCQ, light.m, exer, 
         NUR_FRTVEG_MCQ.rev, smoking, ALC_FREQ.rev, CAG_FPAS, GEN_BRD.rev, 
         GEN_MUSC.rev, SPA_DFRE, INT_SCLNTWRK_MCQ, CCT_F2
         )%>%
  rename(SMOKING_UNSTND=smoking)

#CLSA all health behaviours (drop_na) combined with health indicators (wind) etc
CLSA.hb.wind<-merge(CLSA.full.stand, CLSA.indicators, by = "entity_id")%>% as_tibble
head(CLSA.hb.wind)


# n should be 40,268 if merge successful (confirmed)
str(CLSA.hb.wind)

# Apply cluster variable (Ward k = 7)
cut.ward <-cutree(hb.clust.ward, k=7)

CLSA.hb.wind.k7 <- CLSA.hb.wind %>% 
  mutate(cluster = cut.ward,
         cluster = as.factor(cluster),
         Age.group = as.factor(Age.group),
         SDC_MRTL = as.factor(SDC_MRTL),
         INC_TOT = as.factor(INC_TOT),
         RET_RTRD = as.factor(RET_RTRD),
         LBF_CURR = as.factor(LBF_CURR), 
         HCU_EMEREG_MCQ = as.factor(HCU_EMEREG_MCQ),
         HCU_HLOVRNT_MCQ = as.factor(HCU_HLOVRNT_MCQ),
         HCU_NRSHM_MCQ = as.factor(HCU_NRSHM_MCQ),
         CAG_FPAS= as.factor(CAG_FPAS), 
         GEN_BRD.rev= as.factor(GEN_BRD.rev), 
         GEN_MUSC.rev= as.factor(GEN_MUSC.rev), 
         SPA_DFRE= as.factor(SPA_DFRE), 
         INT_SCLNTWRK_MCQ= as.factor(INT_SCLNTWRK_MCQ))
  

# CLSA.hb.wind.k7 is full dataset, (un/)standardized HBs, with clusters for Ward k=7
write_csv(CLSA.hb.wind.k7, "CLSA Baseline.csv")

CLSA.full.stand.c%>%
  mutate(cluster = cut.ward)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(walking),
            sitting = mean(sitting),
            light_PA = mean(light_PA),
            exercise = mean(exercise),
            fruit_vegetable = mean(fruit_vegetable),
            smoking = mean(smoking),
            alcohol = mean(alcohol))

CLSA.full.stand.c%>%
  mutate(cluster = cut.ward)%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = qwraps2::mean_sd(walking),
            sitting = qwraps2::mean_sd(sitting),
            light_PA = qwraps2::mean_sd(light_PA),
            exercise = qwraps2::mean_sd(exercise),
            fruit_vegetable = qwraps2::mean_sd(fruit_vegetable),
            smoking = qwraps2::mean_sd(smoking),
            alcohol = qwraps2::mean_sd(alcohol))

CLSA.hb.wind.k7%>%
  group_by(cluster)%>% 
  summarise(count = n(),
            walking = mean(PA2_WALK_MCQ),
            sitting = mean(PA2_SIT_MCQ),
            light_PA = mean(light.m),
            exercise = mean(exer),
            fruit_vegetable = mean(NUR_FRTVEG_MCQ.rev),
            smoking = mean(SMOKING_UNSTND),
            alcohol = mean(ALC_FREQ.rev))

scales<-data.frame("Variable" = c("walking", "sitting", "light_PA", "exercise",
                              "fruit_vegetable", "smoking", "alcohol"), 
               "Scale" = c("1 = never, 2 = seldom (1-2 days), 3 = sometimes (3-4 days), 4 = often (5-7 days)",
                           "1 = never, 2 = seldom (1-2 days), 3 = sometimes (3-4 days), 4 = often (5-7 days)",
                           "1 = never, 2 = seldom (1-2 days), 3 = sometimes (3-4 days), 4 = often (5-7 days)",
                           "1 = never, 2 = seldom (1-2 days), 3 = sometimes (3-4 days), 4 = often (5-7 days)",
                           "1 = less than two, 2 = two, 3 = three, 4 = four, 5 = five, 6 =six , 7 = seven or more",
                           "0 = have never smoked 1 = do not currently smoke at all 2 = occassionally smoke 3 = smoke daily",
                           "1 = less than once a month - 7 = almost every day"))
# Ridge plots x clusters ####

#walk
ridge.walk<-ggplot(CLSA.hb.wind.k7, aes(x=PA2_WALK_MCQ, y=cluster, fill = cluster))+ 
  geom_density_ridges(alpha=1.6)+
  xlab("Walking Frequency")+
  ylab("Cluster")+
  ggtitle(label = "Walking Frequencies by Cluster", 
          subtitle="Over the past 7 days, how often did you take a walk outside your home or yard for any
reason? For example, for pleasure or exercise, walking to work, walking the dog, etc. ")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("1", "2", "3", "4"),  
                   labels=c("1" = "Never", "2" = "Seldom (1-2 days)",
                            "3" = "Sometimes (3-4 days)", "4" = "Often (5-7 days)"))

ridge.walk.m<-ggplot(CLSA.hb.wind.k7, aes(x=PA2_WALK_MCQ)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(PA2_WALK_MCQ)),
               color="blue", linetype="dashed", size=1)+
  xlim(.5,4.5)+
  xlab("")+
  ylab("Density")+
  ggtitle(label = "Full Sample",
          subtitle = "Mean = 3.05, SD = 1.13, n = 40,268")+
  theme_classic()

pdf(file = "C:/...pdf",   # The directory 
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches

ggarrange(ridge.walk, ridge.walk.m, heights = c(2, 0.7),
          ncol = 1, nrow = 2)

dev.off()

#sit  
ridge.sit<-ggplot(CLSA.hb.wind.k7, aes(x=PA2_SIT_MCQ, y=cluster, fill = cluster))+ 
  geom_density_ridges(alpha=1.6)+
  xlab("Sitting Frequency")+
  ylab("Cluster")+
  ggtitle(label = "Sitting Activity Frequencies by Cluster", 
          subtitle="Over the past 7 days, how often did you participate in sitting activities such as reading,
watching TV, computer activities or doing handicrafts? ")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("1", "2", "3", "4"),  
                   labels=c("1" = "Never", "2" = "Seldom (1-2 days)",
                            "3" = "Sometimes (3-4 days)", "4" = "Often (5-7 days)"))

ridge.sit.m<-ggplot(CLSA.hb.wind.k7, aes(x=PA2_SIT_MCQ)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(PA2_SIT_MCQ)),
             color="blue", linetype="dashed", size=1)+
  xlim(.5,4.5)+
  xlab("")+
  ylab("Density")+
  ggtitle(label = "Full Sample",
          subtitle = "Mean = 3.90, SD = .38, n = 40,268")+
  theme_classic()

pdf(file = "C:/Users...pdf",   # The directory 
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches

ggarrange(ridge.sit, ridge.sit.m, heights = c(2, 0.7),
          ncol = 1, nrow = 2)

dev.off()

#light /moderate pa
ridge.pa<-ggplot(CLSA.hb.wind.k7, aes(x=light.m, y=cluster, fill = cluster))+ 
  geom_density_ridges(alpha=1.6)+
  xlab("Light Sports Frequency")+
  ylab("Cluster")+
  ggtitle(label = "Light Sports Frequencies by Cluster", 
          subtitle="Over the past 7 days, how often did you engage in light sports or recreational activities
 such as bowling, golf with a cart, shuffleboard, badminton, fishing or other similar
activities? Over the past 7 days, how often did you engage in moderate sports or recreational
activities such as ballroom dancing, hunting, skating, golf without a cart, softball or other
similar activities?" )+
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("1", "2", "3", "4"),  
                   labels=c("1" = "Never", "2" = "Seldom (1-2 days)",
                            "3" = "Sometimes (3-4 days)", "4" = "Often (5-7 days)"))

ridge.pa.m<-ggplot(CLSA.hb.wind.k7, aes(x=light.m)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(light.m)),
             color="blue", linetype="dashed", size=1)+
  xlim(.5,4.5)+
  xlab("")+
  ylab("Density")+
  ggtitle(label = "Full Sample",
          subtitle = "Mean = 1.29, SD = .51, n = 40,268")+
  theme_classic()

pdf(file = "C:/Users/...pdf",   # The directory 
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches

ggarrange(ridge.pa, ridge.pa.m, heights = c(2, 0.7),
          ncol = 1, nrow = 2)

dev.off()

# exercise
ridge.exer<-ggplot(CLSA.hb.wind.k7, aes(x=exer, y=cluster, fill = cluster))+ 
  geom_density_ridges(alpha=1.6)+
  xlab("Strenuous Sports / Exercise Frequency")+
  ylab("Cluster")+
  ggtitle(label = "Strenuous Sports / Exercise Frequencies by Cluster", 
          subtitle="Over the past 7 days, how often did you engage in strenuous sports or recreational
activities such as jogging, swimming, snowshoeing, cycling, aerobics, skiing, or other
similar activities? Over the past 7 days, how often did you do any exercises specifically to increase muscle
strength and endurance, such as lifting weights or push-ups, etc.?" )+
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("1", "2", "3", "4"),  
                   labels=c("1" = "Never", "2" = "Seldom (1-2 days)",
                            "3" = "Sometimes (3-4 days)", "4" = "Often (5-7 days)"))

ridge.exer.m<-ggplot(CLSA.hb.wind.k7, aes(x=exer)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(exer)),
             color="blue", linetype="dashed", size=1)+
  xlim(.5,4.5)+
  xlab("")+
  ylab("Density")+
  ggtitle(label = "Full Sample",
          subtitle = "Mean = 1.59, SD = .79, n = 40,268")+
  theme_classic()

pdf(file = "C:...pdf",   # The directory 
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches

ggarrange(ridge.exer, ridge.exer.m, heights = c(2, 0.7),
          ncol = 1, nrow = 2)

dev.off()

# fruit
ridge.fruit<-ggplot(CLSA.hb.wind.k7, aes(x=NUR_FRTVEG_MCQ.rev, y=cluster, fill = cluster))+ 
  geom_density_ridges(alpha=1.6)+
  xlab("Daily Fruit & Vegetable Consumption")+
  ylab("Cluster")+
  ggtitle(label = "Daily Fruit & Vegetable Consumption by Cluster", 
          subtitle="In general, how many servings of fruits and vegetables do you eat in a day?  ")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),  
                   labels=c("1" = "less than 2", "2" = "2",
                            "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7"="7 or more"))

ridge.fruit.m<-ggplot(CLSA.hb.wind.k7, aes(x=NUR_FRTVEG_MCQ.rev)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(NUR_FRTVEG_MCQ.rev)),
             color="blue", linetype="dashed", size=1)+
  xlim(0.5,7.5)+
  xlab("")+
  ylab("Density")+
  ggtitle(label = "Full Sample",
          subtitle = "Mean = 4.00, SD = 1.80, n = 40,268")+
  theme_classic()

pdf(file = "C:/Users...pdf",   # The directory 
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches

ggarrange(ridge.fruit, ridge.fruit.m, heights = c(2, 0.7),
          ncol = 1, nrow = 2)

dev.off()

#smoke
ridge.smoke<-ggplot(CLSA.hb.wind.k7, aes(x=SMOKING_UNSTND, y=cluster, fill = cluster))+ 
  geom_density_ridges(alpha=1.6)+
  xlab("")+
  ylab("Cluster")+
  ggtitle(label = "Smoking Frequency by Cluster", 
          subtitle="Have you ever smoked a whole cigarette? At the present time, 
          do you smoke cigarettes daily, occasionally or not at all??  ")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_continuous(labels=c("0" = "Never", "1" = "Not Currently",
                            "2" = "Occassionally", "3" = "Daily"))

ridge.smoke.m<-ggplot(CLSA.hb.wind.k7, aes(x=SMOKING_UNSTND)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(SMOKING_UNSTND)),
             color="blue", linetype="dashed", size=1)+
  xlim(-.25,3.25)+
  xlab("")+
  ylab("Density")+
  ggtitle(label = "Full Sample",
          subtitle = "Mean = 0.86, SD = .75, n = 40,268")+
  theme_classic()

pdf(file = "C:/Users.pdf",   # The directory 
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches

ggarrange(ridge.smoke, ridge.smoke.m, heights = c(2, 0.7),
          ncol = 1, nrow = 2)

dev.off()

#alcohol
ridge.alcohol<-ggplot(CLSA.hb.wind.k7, aes(x=ALC_FREQ.rev, y=cluster, fill = cluster))+ 
  geom_density_ridges(alpha=1.6)+
  xlab("Alcohol Consumption")+
  ylab("Cluster")+
  ggtitle(label = "Alcohol Consumption by Cluster", 
          subtitle="About how often during the past 12 months did you drink alcohol?")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7"),  
                   labels=c("1" = "< 1/Month", "2" = "2",
                            "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7"="Almost Every Day"))

ridge.alcohol.m<-ggplot(CLSA.hb.wind.k7, aes(x=ALC_FREQ.rev)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(ALC_FREQ.rev)),
             color="blue", linetype="dashed", size=1)+
  xlim(.5,7.5)+
  xlab("")+
  ylab("Density")+
  ggtitle(label = "Full Sample",
          subtitle = "Mean = 4.24, SD = 2.02, n = 40,268")+
  theme_classic()

mean(CLSA.hb.wind.k7$ALC_FREQ.rev)

pdf(file = "C:/Users..pdf",   # The directory 
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches

ggarrange(ridge.alcohol, ridge.alcohol.m, heights = c(2, 0.7),
          ncol = 1, nrow = 2)

dev.off()



