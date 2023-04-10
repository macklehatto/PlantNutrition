library(here)
library(readxl)
library(ggplot2)
library(tidyverse)
install.packages("car")
library(car)
install.packages("ggpubr")
library(ggpubr)
install.packages("gridExtra")
library(gridExtra)
install.packages("report")
library("easystats")
library("report")
install.packages("prettydoc")

data <- read_xlsx(
  here( "","Data.xlsx")
)

dat <- data %>%  
  replace(is.na(.), 0) %>%
  mutate(wetroottoshoot = RootWetWeight / ShootWetWeight) %>%
  mutate(dryroottoshoot = RootDryWeight / ShootDryWeight)

spp <- unique(dat$Species)
  

lespedeza <- data %>% replace(is.na(.),0) %>% filter(Species == "Lespedeza")
  
lespedeza$`RootDryWeight` <- as.numeric(lespedeza$`RootDryWeight`)

dalea <- data %>% replace(is.na(.),0) %>% filter(Species == "Dalea")

tuberosa <- data %>% replace(is.na(.),0) %>% filter(Species == "Asclepiastuberosa")

AMCA <- data %>% 
  replace(is.na(.),0) %>% 
  filter(Species == "Amorpha")

AMCA$wetroottoshoot = AMCA$RootWetWeight / AMCA$ShootWetWeight
AMCA$dryroottoshoot = AMCA$RootDryWeight / AMCA$ShootDryWeight

#leca <- ggplot(lespedeza, aes(roottoshoot))

## root:shoot graphs

lecabox <- lespedeza %>%
  ggplot(aes(x=Rate, y= roottoshoot, fill=Rate)
         ) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha=0.6) +
  geom_jitter(color="orange", size= 2.5, alpha=0.9) +
  ylab("Root to Shoot Ratio") +
  xlab("Osmocote Rate") +
  ggtitle("Lespedeza capitata Root:Shoot Ratio")
  
print(lecabox)

dacabox <- dalea %>%
  ggplot(aes(x=Rate, y= roottoshoot, fill=Rate)
  ) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha=0.6) +
  geom_jitter(color="orange", size= 2.5, alpha=0.9) +
  ylab("Root to Shoot Ratio") +
  xlab("Osmocote Rate") +
  ggtitle("Dalea candida Root:Shoot Ratio")

print(dacabox)

astubox <- tuberosa %>%
  ggplot(aes(x=Rate, y= roottoshoot, fill=Rate)
  ) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha=0.6) +
  geom_jitter(color="orange", size= 2.5, alpha=0.9) +
  ylab("Root to Shoot Ratio") +
  xlab("Osmocote Rate") +
  ggtitle("Asclepias tuberosa Root:Shoot Ratio")

print(astubox)

ggarrange(lecabox, dacabox, astubox, 
          ncol = 3, nrow = 1)

##Root:Shoot ALL
boxplotgrouproottoshootall = ggplot(data, aes(y= roottoshoot, x = Species, fill = Rate))+
  geom_boxplot(position=position_dodge(1.1), alpha = 0.1) +
  xlab('Species') +
  ylab('Root to Shoot Ratio') +
  ggtitle("Root to Shoot Ratios") +
  scale_fill_discrete(name = "Osmocote Rates")+
  geom_jitter(aes(col = Rate), size= 1.5, alpha=0.9, width = 0.2)+
  scale_x_discrete(breaks = c('Asclepiastuberosa', 'Dalea', 'Lespedeza'),
                    labels = c("ASTU", "DACA", "LECA"))

print(boxplotgrouproottoshootall)

##Root:Shoot ASTU and LECA only

lecaastu <- data %>% replace(is.na(.),0) %>% filter(Species != "Dalea")
    
boxplotgrouproottoshoottwo = ggplot(lecaastu, aes(y= roottoshoot, x = Species, fill = Rate))+
  geom_boxplot(position=position_dodge(1.1), alpha = 0.1) +
  xlab('Species') +
  ylab('Root to Shoot Ratio') +
  ggtitle("Root to Shoot Ratios") +
  scale_fill_discrete(name = "Osmocote Rates")+
  geom_jitter(aes(col = Rate), size= 1.5, alpha=0.9, width = 0.2)+
  scale_x_discrete(breaks = c('Asclepiastuberosa', 'Lespedeza'),
                   labels = c("ASTU", "LECA"))

print(boxplotgrouproottoshoottwo)

grid.arrange(boxplotgrouproottoshootall, boxplotgrouproottoshoottwo, ncol = 2)

##Root:Shoot Regression

#LECA
res_aov_leca_rootshoot <- aov(roottoshoot ~ Rate,
               data = lespedeza)

par(mfrow = c(1,2))

hist(res_aov_leca_rootshoot$residuals)

qqPlot(res_aov_leca_rootshoot$residuals,
       id = FALSE
        )

shapiro.test(res_aov_leca_rootshoot$residuals) # LESPEDEZA IS NORMAL p=0.0493 -> ANOVA

oneway.test(roottoshoot ~ Rate,
            data = lespedeza,
            var.equal = FALSE 
            )

posttest <- TukeyHSD(res_aov_leca_rootshoot)
plot(TukeyHSD(res_aov_leca_rootshoot))

#DACA
res_aov_daca_rootshoot <- aov(roottoshoot ~ Rate,
                              data = dalea)

par(mfrow = c(1,2))

hist(res_aov_daca_rootshoot$residuals)

qqPlot(res_aov_daca_rootshoot$residuals,
       id = FALSE
)

shapiro.test(res_aov_daca_rootshoot$residuals) # DALEA IS NOT NORMAL p=0.7

#ASTU
res_aov_astu_rootshoot <- aov(roottoshoot ~ Rate,
                              data = tuberosa)

par(mfrow = c(1,2))

hist(res_aov_astu_rootshoot$residuals)

qqPlot(res_aov_astu_rootshoot$residuals,
       id = FALSE
)

shapiro.test(res_aov_astu_rootshoot$residuals) # ASTU IS NOT NORMAL p=0.26

#sqrt transform
tuberosa$logroottoshoot = sqrt(tuberosa$roottoshoot)

res_aov_log_astu <- aov(logroottoshoot ~ Rate,
                        data = tuberosa)

par(mfrow = c(1,2))

hist(res_aov_log_astu$residuals)

qqPlot(res_aov_log_astu$residuals,
       id = FALSE
)

shapiro.test(res_aov_log_astu$residuals) #fail at p = 0.46

#AMCA
res_aov_AMCA_rootshoot <- aov(dryroottoshoot ~ Rate,
                              data = AMCA)

par(mfrow = c(1,2))

hist(res_aov_AMCA_rootshoot$residuals)

qqPlot(res_aov_AMCA_rootshoot$residuals,
       id = FALSE
)

shapiro.test(res_aov_AMCA_rootshoot$residuals) # ASTU IS NORMAL p-value = 3.065e-06

res_aov_AMCA <- aov(dryroottoshoot ~ Rate, 
                    data = AMCA
                    )
report(res_aov_AMCA)


## root-shoot correlations

lecacorr <- lespedeza %>%
  ggplot(aes(x=`RootDryWeight`, y= `ShootDryWeight`, color=Rate)) +
  geom_point() +
  geom_smooth(method="lm", fill= NA)+
  xlab("Root Dry Weight") +
  ylab("Shoot Dry Weight") +
  ggtitle("Lespedeza capitata Root to Shoot Correlation")
print(lecacorr)

dacacorr <- dalea %>%
  ggplot(aes(x=`RootDryWeight`, y= `ShootDryWeight`, color=Rate)) +
  geom_point() +
  geom_smooth(method="lm", fill= NA)+
  xlab("Root Dry Weight") +
  ylab("Shoot Dry Weight") +
  ggtitle("Dalea candida Root to Shoot Correlation")
print(dacacorr)

astucorr <- tuberosa %>%
  ggplot(aes(x=`RootDryWeight`, y= `ShootDryWeight`, color=Rate)) +
  geom_point() +
  geom_smooth(method="lm", fill= NA)+
  xlab("Root Dry Weight") +
  ylab("Shoot Dry Weight") +
  ggtitle("Asclepias tuberosa Root to Shoot Correlation")
print(astucorr)

grid.arrange(astucorr, dacacorr, lecacorr, ncol=3)
                        
                      

####### ROOT ONLY:

# ##Determining normality
#   
# plot(density(lespedeza$RootDryWeight))
# 
# ggqqplot(lespedeza$RootDryWeight,
#          main = "Density plot of Lespedeza capitata root dry weight",
#          xlab = "Dry weight (g)",
#          ylab = "density"
#          )  
# 
# shapiro.test(lespedeza$RootDryWeight)  



#ANOVA - normality

#leca:
res_aov <- aov(RootDryWeight ~ Rate,
               data = lespedeza)

par(mfrow = c(1,2))

hist(res_aov$residuals)

qqPlot(res_aov$residuals,
       id = FALSE
       )

shapiro.test(res_aov$residuals) # LESPEDEZA IS NOT NORMAL

#daca:
res_aov_daca <- aov(RootDryWeight ~ Rate,
               data = dalea)

par(mfrow = c(1,2))

hist(res_aov_daca$residuals)

qqPlot(res_aov_daca$residuals,
       id = FALSE
)

shapiro.test(res_aov_daca$residuals) # DALEA IS NOT NORMAL (p= 0.3731)

#astu
res_aov_astu <- aov(RootDryWeight ~ Rate,
                    data = tuberosa)

par(mfrow = c(1,2))

hist(res_aov_astu$residuals)

qqPlot(res_aov_astu$residuals,
       id = FALSE
)

shapiro.test(res_aov_astu$residuals) # ASTU IS NOT NORMAL (p=0.16)


###log transform root dry weight

#leca
lespedeza$logRootDryWeight = log(lespedeza$RootDryWeight)

res_aov_log <- aov(logRootDryWeight ~ Rate,
               data = lespedeza)

par(mfrow = c(1,2))

hist(res_aov_log$residuals)

qqPlot(res_aov_log$residuals,
       id = FALSE
)

shapiro.test(res_aov_log$residuals) #p = 0.08

#daca

dalea$logRootDryWeight = log(dalea$RootDryWeight)

res_aov_log_daca <- aov(logRootDryWeight ~ Rate,
                   data = dalea)

par(mfrow = c(1,2))

hist(res_aov_log_daca$residuals)

qqPlot(res_aov_log_daca$residuals,
       id = FALSE
)

shapiro.test(res_aov_log_daca$residuals) #p = 0.35

#astu

tuberosa$logRootDryWeight = log(tuberosa$RootDryWeight)

res_aov_log_astu <- aov(logRootDryWeight ~ Rate,
                        data = tuberosa)

par(mfrow = c(1,2))

hist(res_aov_log_astu$residuals)

qqPlot(res_aov_log_astu$residuals,
       id = FALSE
)

shapiro.test(res_aov_log_astu$residuals) #fail at p = 0.7


#Kruskal-Wallis + Dunn - LESPEDEZA

install.packages("FSA")
library(FSA)

kruskal.test(RootDryWeight ~ Rate, 
             data = lespedeza
             )

dunnTest(RootDryWeight ~ Rate, 
         data = lespedeza,
         method = "holm"
        )

library(ggstatsplot)

ggbetweenstats(
  data = lespedeza,
  x = Rate, 
  y = RootDryWeight,
  type = "nonparametric",
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  xlab = "Osmocote Rate",
  ylab = "Root Dry Weight",
  title = "Lespedeza capitata Treatment Comparisons"
  )

### DALEA ... student t test

kruskal.test(RootDryWeight ~ Rate, 
             data = dalea
            ) #FAIL

Dalea_Test <- t.test(RootDryWeight ~ Rate,
                     data = dalea,
                     var.equal = FALSE,
                     alternative = "less"
                     ) #FAIL


### ASTU ...student t test

kruskal.test(RootDryWeight ~ Rate, 
             data = tuberosa
              ) # significant at p = 0.045

Asclepias_Test <- t.test(RootDryWeight ~ Rate,
                     data = tuberosa,
                     var.equal = FALSE,
                     alternative = "less"
                      )
print(Asclepias_Test) # significant at p = 0.041

## root dry weight

lespedeza %>%
  ggplot()+
  aes(x = lespedeza$Rate, y = lespedeza$`RootDryWeight`, color = Rate) +
  geom_jitter()

dacaboxroot <- dalea %>%
  ggplot(aes(x=Rate, y= RootDryWeight, fill=Rate)
  ) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha=0.6) +
  geom_jitter(color="orange", size= 2.5, alpha=0.9) +
  ylab("Root Dry Weight") +
  xlab("Osmocote Rate") +
  ggtitle("Dalea candida Root Dry Weight")

print(dacaboxroot)

astuboxroot <- tuberosa %>%
  ggplot(aes(x=Rate, y= RootDryWeight, fill=Rate) 
  ) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha=0.6) +
  geom_jitter(color="orange", size= 2.5, alpha=0.9) +
  ylab("Root Dry Weight") +
  xlab("Osmocote Rate") +
  ggtitle("Asclepias tuberosa Root Dry Weight")

print(astuboxroot)

lecaboxroot <- lespedeza %>%
  ggplot(aes(x=Rate, y= RootDryWeight, fill=Rate) 
  ) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha=0.6) +
  geom_jitter(color="orange", size= 2.5, alpha=0.9) +
  ylab("Root Dry Weight") +
  xlab("Osmocote Rate") +
  ggtitle("Lespedeza capitata Root Dry Weight")

print(lecaboxroot)

## shoot dry weight

astuboxshoot <- tuberosa %>%
  ggplot(aes(x=Rate, y= ShootDryWeight, fill=Rate) 
  ) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha=0.6) +
  geom_jitter(color="orange", size= 2.5, alpha=0.9, height = 0.03) +
  ylab("Shoot Dry Weight") +
  xlab("Osmocote Rate") +
  ggtitle("Asclepias tuberosa Shoot Dry Weight")

print(astuboxshoot)

dacaboxshoot <- dalea %>%
  ggplot(aes(x=Rate, y= ShootDryWeight, fill=Rate)
  ) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha=0.6) +
  geom_jitter(color="orange", size= 2.5, alpha=0.9) +
  ylab("Shoot Dry Weight") + 
  xlab("Osmocote Rate") +
  ggtitle("Dalea candida Shoot Dry Weight")

print(dacaboxshoot)

lecaboxshoot <- lespedeza %>%
  ggplot(aes(x=Rate, y= ShootDryWeight, fill=Rate) 
  ) +
  geom_boxplot() +
  scale_fill_viridis_d(alpha=0.6) +
  geom_jitter(color="orange", size= 2.5, alpha=0.9) +
  ylab("Shoot Dry Weight") +
  xlab("Osmocote Rate") +
  ggtitle("Lespedeza capitata Shoot Dry Weight")

print(lecaboxshoot)

grid.arrange(astuboxshoot, astuboxroot, dacaboxshoot, dacaboxroot, lecaboxshoot, lecaboxroot, ncol=2)


boxplotgroup = ggplot(data, aes(y= RootDryWeight, x = Species, fill = Rate))+
  geom_boxplot() 