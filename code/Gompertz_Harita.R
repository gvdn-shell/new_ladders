View(data)

#install.packages(c("dplyr", "ggplot2", "nls2"))
library(dplyr)
library(ggplot2)
library(nls2)


data <- data %>%
  mutate(across(c(GDP_PPP_pcap, EN.POP.DNST, SI.POV.GINI, energy_service), scale))


ggplot(data, aes(x = GDP_PPP_pcap, y = energy_service, colour = country_name)) +
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ a * exp(-b * exp(-c * x)),
              method.args = list(start = list(a = 10000, b = 1, c = 0.1)),
              se = FALSE) +
  labs(title = "Energy Service vs GDP per Capita",
       x = "GDP per Capita (scaled)",
       y = "Energy Service (scaled)",
       colour = "Country")+
      theme_minimal()



gompertz <- function(t, a, b, c) {
  a * exp(-b * exp(-c * t))
}

start_grid <- expand.grid(a = seq(5000, 15000, by = 1000),
                          b = seq(0.5, 1.5, by = 0.1),
                          c = seq(0.05, 0.15, by = 0.01))



  # Fit the model
  fit <- nls2(energy_service ~ gompertz(GDP_PPP_pcap, a, b, c), data = data,
              start = start_grid, algorithm = "brute-force")
  
  
  # Create the plot
  p <- ggplot(data, aes(x = GDP_PPP_pcap, y = energy_service)) +
    geom_point() +
    geom_line(aes(y = predict(fit)), color = "blue") +
    labs(title = paste("Energy Service vs GDP per Capita for"),
         x = "GDP per Capita (scaled)",
         y = "Energy Service (scaled)") +
    theme_minimal()
  



 