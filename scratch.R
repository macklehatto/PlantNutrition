install.packages("ggbeeswarm")
library(ggbeeswarm)

AMCA <- data %>% 
  replace(is.na(.),0) %>% 
  filter(Species == "Amorpha")

AMCA$wetroottoshoot = AMCA$RootWetWeight / AMCA$ShootWetWeight
AMCA$dryroottoshoot = AMCA$RootDryWeight / AMCA$ShootDryWeight

p <- ggplot(AMCA, aes(x = Rate, y = wetroottoshoot)) +
  geom_violin(fill = "grey92") +
  geom_quasirandom(
    size = 1.5,
    alpha = .4,
    width = .2
  ) +
  geom_violin(aes(y = dryroottoshoot))
  # geom_jitter(
  #   size = 2,
  #   alpha = .3,
  #   position = position_jitter(
  #     seed = 1, width = .1
  #   )
  # )
  
print(p)

hist <- ggplot(AMCA, aes()) +
  geom_histogram(alpha = 0.5, fill = "grey92")+
  geom_histogram(aes(x = dryroottoshoot))
print(hist)


  