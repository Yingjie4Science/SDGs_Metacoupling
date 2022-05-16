




library(cepiigeodist)
### filter countries that share borders
# dist_cepii[dist_cepii$contig == 1, ]

library(dplyr)
library(ggplot2)

d <- dist_cepii$dist %>% as.numeric()

hist(d)

plot(density(d))


ggplot(data = dist_cepii, aes(x=dist)) +
  geom_histogram(colour="black", 
                 aes(y=..density..),         # Histogram with density instead of count on y-axis
                 fill="white") +
  geom_density(alpha= 0.2, fill="yellow") +  # Overlay with transparent density plot
  geom_vline(aes(xintercept=mean(dist, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  theme_bw()


bw = 600
n_obs <- sum(!is.na(dist_cepii$dist))
dist_cepii %>% 
  ggplot(data = ., aes(x=dist)) +
  geom_histogram(colour="black", binwidth = bw,
                 fill="white") +
  
  stat_function(fun = function(x) 
    dnorm(x, mean = mean(dist_cepii$dist, na.rm = T), sd = sd(dist_cepii$dist, na.rm = T)) * bw * n_obs) +
  
  
  geom_vline(aes(xintercept=mean(dist, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  theme_bw()

