https://rpubs.com/haolunfu/701057


# load packages for later use
pacman::p_load(tidyverse, nlme)

data <- readRDS("data/all_data_wem.rds")

# set plot theme to black-n-white
ot <- theme_set(theme_bw())

# individual profiles on separate panels
ggplot(data, aes(year, energy_service)) +
  geom_point(size = rel(.8), pch = 20)+
  geom_line()+
  facet_wrap( ~ country_name) +
  labs(x = "Year", y = "Energy Service")

# Individaul Profiles in one Panel

# GDP/cap vs. ES

ggplot(data, aes(GDP_PPP_pcap, energy_service, group = country_name, colour = country_name)) +
  geom_point(pch = 20)+
  geom_line(alpha = .3) +
  labs(x = "GDP/cap", y = "Energy Service")

# Year vs. ES

ggplot(data, aes(year, energy_service, group = country_name, colour = country_name)) +
  geom_point(pch = 20)+
  geom_line(alpha = .3) +
  labs(x = "Year", y = "Energy Service")


data_g <- groupedData(energy_service ~ GDP_PPP_pcap | country_name, data = as.data.frame(data),
                     labels = list(x = "GDP/cap", y = "Energy Service"),
                     )

# lattice plot by subject
plot(data_g, pch = 20, aspect = .89)

model <- nlsList(SSasympOff, data = data_g)

# results of parameter estimates
coef(model)