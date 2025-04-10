# From: https://jchiquet.github.io/MAP566/docs/mixed-models/map566-lab-nonlinear-mixed-model.html
# Remove all objects in R workspace (use with caution)
rm(list = ls())

# Check that required packages are installed and install if missing
ncpus <- 12
packages <- c("tidyverse",
              "ggplot2", 
              "saemix",
              "corrplot",
              "conflicted",
              "extrafont",
              "here",
              "sysfonts",
              "showtext",
              "ggfortify",
              "lme4",
              "lattice") #
installed_packages <- packages %in% rownames(installed.packages())

if (any(!installed_packages)) {
  install.packages(packages[!installed_packages], dependencies = TRUE, Ncpus = ncpus)
}

# Load required packages
invisible(lapply(packages, library, character.only = TRUE))
# Load the extrafont package
# font_import(paths = "C:/Windows/Fonts")


# syntax: font_add(family = "<family_name>", regular = "/path/to/font/file")
font_add("ShellMedium", "ShellMedium.ttf")
font_families()
## automatically use showtext for new devices
showtext_auto()
#Plot: need to open Windows graphics device as showtext does not work well with RStudio built-in graphics device
windows()
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

#######################################################################################
theme_set(theme_bw())


# Set seed for reproducibility
set.seed(42)

# Define Gompertz function
gompertz <- function(t, A, b, c) {
  return(A * exp(-exp(-b * (t - c))))
}

# Generate unique ids
ids <- 1:12

# Generate time points (14 weeks)
time_points <- 1:14

# Initialize data storage
data <- data.frame()

# Generate data for each id
for (id in ids) {
  # Assign group (control or treatment)
  group <- ifelse(id <= 6, 'control', 'treatment')
  
  # Generate Gompertz parameters
  A <- runif(1, 70, 100) # Asymptote
  b <- runif(1, 0.1, 0.3) # Growth rate
  c <- runif(1, 5, 10) # Inflection point
  
  # Generate weight data for each time point
  for (t in time_points) {
    weight <- gompertz(t, A, b, c)
    data <- rbind(data, data.frame(id = id, group = group, week = t, weight = weight))
  }
}

# Display the first few rows of the data
head(data)
# Add random noise to the weight data
data$weight <- data$weight + rnorm(nrow(data), mean = 0, sd = 2)

# Plot the data
data %>%
  ggplot(aes(x = week, y = weight, color = factor(id), group = factor(id))) +
  geom_point() +
  geom_line() +
  labs(title = "Weight Data with Gompertz Growth Model",
       x = "Week",
       y = "Weight") +
  theme_minimal()

# Non-linear mixed effects model with SAEMIX

saemix_data <- saemixData(
  name.data = data,
  name.group = c("id"),
  name.predictors = c("week"),
  name.response = c("weight"),
  #name.covariates = c("group"),
  units = list(x = "weeks", y = "grams")
)
# Define the model
gompertz <- function(psi, id, x) {
  t <- x[, 1] # Extract time points
  A <- psi[id, 1]
  b <- psi[id, 2]
  c <- psi[id, 3]
  
  # Gompertz model
  ypred <- A * exp(-exp(-b * (t - c)))
  
  return(ypred)
}

psi0 <- c(A = 80, b = 0.2, c = 7) # Initial parameter value guesses
distribParam <- c(1,1,1) # Using lognormal distribution for 3 parameters (setting transform.par=c(1,1,1))
# Assuming diagonal covariance matrix
NLMM_gompertz <- saemixModel(
  model = gompertz,
  description = "Gompertz model",
  psi0 = psi0,
  transform.par = distribParam # Log transformation
  #fixed.estim = c(1, 1, 1), # Fix all parameters
  #covariance.model = diag(distribParam),
  #error.model = "constant"
)
# Fit the model
saemix_options <- saemixControl(
  map   = TRUE,
  fim   = TRUE,
  ll.is = FALSE,
  displayProgress = FALSE,
  directory = "output_saemix",
  seed = 12345)
NLMM_gompertz_fit <- saemix(NLMM_gompertz , saemix_data, saemix_options)

summary(NLMM_gompertz_fit)

### Diagnostic plots:

# Individual plot for subject 1 to 9, 
saemix.plot.fits(NLMM_gompertz_fit, ilist = c(1:9), smooth=TRUE)

# Diagnostic plot: observations versus population predictions
saemix.plot.obsvspred(NLMM_gompertz_fit)

# Scatter plot of residuals
saemix.plot.scatterresiduals(NLMM_gompertz_fit, level=1)


fim <- NLMM_gompertz_fit@results@fim # Fisher information matrix
cov_hat <- solve(fim)                # covariance matrix of the estimates
d <- sqrt(diag(cov_hat))             # s.e. of the estimates
corrplot(cov_hat / (d %*% t(d)), type = 'upper', diag = FALSE)

cov_hat / (d %*% t(d))               # correlation matrix of the estimates

###############################################################################################
###############################################################################################

theophylline <- read_csv(here::here("data", "Theoph.csv")) %>%
  mutate(id = factor(Subject)) %>%
  select(-Subject) %>%
  # rename conc to concentration and Wt to wieght and Time to time
  rename(concentration = conc, weight = Wt, time = Time) 

theo_plot <- theophylline %>% 
  ggplot() + aes(x = time, y = concentration) + geom_point(color="#993399", size=2) +
  xlab("time (h)") + ylab("concentration (mg/l)")
theo_plot + geom_line(color="#993399", aes(group = id))

theo_plot + geom_line() + facet_wrap( ~ id)

# Fitting a nonlinear model to a single subject

subject1 <- theophylline %>% filter(id == 1) %>%
  select(time, concentration)

subject1_plot <- subject1 %>% 
  ggplot() + aes(x = time, y = concentration) + geom_point( color="#993399", size=3) + 
  xlab("time (h)") + ylab("concentration (mg/l)") + ylim(c(0,11))
subject1_plot + geom_line(color="#993399")

# Implementing a pharmocokinetic model

f1 <- function(psi, t) {
  D <- 320
  ka <- psi[1]
  V <- psi[2]
  ke <- psi[3]
  f <- D * ka / (V * (ka - ke)) * (exp(-ke * t) - exp(-ka * t))
  return(f)
}

# Fit model with nls function

model_1 <- nls(concentration ~ f1(psi, time), start = list(psi=c(ka=1, V=40, ke=0.1)), data=subject1)
coef(model_1)

dplot <- data.frame(time = seq(0, 40, by=0.2))
dplot$pred_1 <- predict(model_1, newdata = dplot)
subject1_plot + geom_line(data = dplot, aes(x = time, y = pred_1), colour = "#339900", linewidth=1)


# Fitting a UNIQUE non-linear model to several subjects

model_all <- nls(concentration ~ f1(psi, time), start = list(psi=c(ka=1, V=40, ke=0.1)), data=theophylline)
coef(model_all)

dplot$pred_all <- predict(model_all, newdata = dplot)
theo_plot + geom_line(data = dplot, aes(x = time, y = pred_all), colour="#339900", size=1)

theo_plot +  
  geom_line(data = dplot, aes(x = time, y=pred_all), colour="#339900", linewidth=1) + 
  facet_wrap(~ id)

# Fitting several nonlinear models to several subjects

res <- split(theophylline, theophylline$id) %>% 
  map(~{
    model_i <- nls(concentration ~ f1(psi, time), 
                   start = list(psi=c(ka=1, V=40,k=0.08)), 
                   data = .x)
    list(psi = coef(model_i),
         y_hat = predict(model_i, newdata = dplot),
         id = unique(.x$id))
  })
psi_hat <- map_df(res, "psi") %>% 
  setNames(c("ka","V","ke")) %>% 
  add_column(id = factor(map_dbl(res, "id"))) 
theo_pred <-
  map_df(res, "y_hat") %>% 
  pivot_longer(everything(), names_to = "id", values_to = "concentration") %>% 
  add_column(time = rep(dplot$time, each = length(res)))

theo_plot + geom_line(data = theo_pred, aes(x=time,y=concentration), colour="#339900", size=0.75) + facet_wrap(~id)

# For instance for individual 9

model_9 <- nls(concentration ~ f1(psi, time),  start = list(psi=c(ka=1, V=40,k=0.08)), 
               data = filter(theophylline, id == 9))
model_9


# Non-linear mixed effects (NLME) model

saemix_data <- saemixData(name.data       = theophylline,
                          name.group      = "id",
                          name.predictors = "time",
                          name.response   = "concentration")
# Define the model
model1_nlme <- function(psi,id,x) {
  D   <- 320
  t   <- x[,1]
  ka  <- psi[id,1]
  V   <- psi[id,2]
  ke  <- psi[id,3]
  fpred <- D*ka/(V*(ka-ke))*(exp(-ke*t)-exp(-ka*t))
  return(fpred)
}

saemix_model <- saemixModel(model = model1_nlme,
                            psi0  = c(ka=1,V=20,ke=0.5))

# map = TRUE estimation of individual parameters
saemix_options <- list(map=TRUE, fim=TRUE, ll.is=FALSE, displayProgress=FALSE, save=FALSE, seed=632545)
saemix_fit1    <- saemix(saemix_model, saemix_data, saemix_options)

saemix_fit1@results

# Individual parameter estimates

psi <- psi(saemix_fit1)
psi

saemix_fit <- saemix.predict(saemix_fit1)
saemix.plot.fits(saemix_fit1)

saemix.plot.obsvspred(saemix_fit1,level=1)
saemix.plot.scatterresiduals(saemix_fit1,level=1)

#############################################################################
data(theo.saemix)
saemix.data<-saemixData(name.data=theo.saemix,header=TRUE,sep=" ",na=NA,
                        name.group=c("Id"),name.predictors=c("Dose","Time"),
                        name.response=c("Concentration"),name.covariates=c("Weight","Sex"),
                        units=list(x="hr",y="mg/L",covariates=c("kg","-")), name.X="Time")
model1cpt<-function(psi,id,xidep) {
  dose<-xidep[,1]
  tim<-xidep[,2]
  ka<-psi[id,1]
  V<-psi[id,2]
  CL<-psi[id,3]
  k<-CL/V
  ypred<-dose*ka/(V*(ka-k))*(exp(-k*tim)-exp(-ka*tim))
  return(ypred)
}
saemix.model<-saemixModel(model=model1cpt,modeltype="structural",
                          description="One-compartment model with first-order absorption",
                          psi0=matrix(c(1.,20,0.5,0.1,0,-0.01),ncol=3, byrow=TRUE,
                                      dimnames=list(NULL, c("ka","V","CL"))),transform.par=c(1,1,1),
                          covariate.model=matrix(c(0,1,0,0,0,0),ncol=3,byrow=TRUE),fixed.estim=c(1,1,1),
                          covariance.model=matrix(c(0,0,0,0,1,0,0,0,0),ncol=3,byrow=TRUE), # Only second allowed to vary
                          #covariance.model=matrix(c(1,0,0,0,1,0,0,0,1),ncol=3,byrow=TRUE),
                          omega.init=matrix(c(1,0,0,0,1,0,0,0,1),ncol=3,byrow=TRUE),error.model="constant")
checkInitialFixedEffects(saemix.model, saemix.data, id=c(1:6))
checkInitialFixedEffects(saemix.model, saemix.data, id=c(1:6), psi=c(0.5, 30, 2)) # better fit


saemix_fit1    <- saemix(saemix.model, saemix.data)
psi <- psi(saemix_fit1)
print(psi)
## Keep ka and V constant across id

# Define the model
model1_nlme <- function(psi,id,x) {
  D   <- 320
  t   <- x[,1]
  ka  <- psi[id,1]
  V   <- psi[id,2]
  ke  <- psi[3]
  fpred <- D*ka/(V*(ka-ke))*(exp(-ke*t)-exp(-ka*t))
  return(fpred)
}


saemix_model <- saemixModel(
  model = model1_nlme,
  psi0 = c(ka = 1, V = 20, ke = 0.1)
)

# map = TRUE estimation of individual parameters
saemix_options <- list(map=TRUE, fim=TRUE, ll.is=FALSE, 
                       displayProgress=FALSE, save=FALSE, seed=632545)
saemix_fit1    <- saemix(saemix_model, saemix_data, saemix_options)


# Individual parameter estimates
psi <- psi(saemix_fit1)
print(psi)

# Predictions and plots
saemix_fit <- saemix.predict(saemix_fit1)
saemix.plot.fits(saemix_fit1)
saemix.plot.obsvspred(saemix_fit1, level = 1)
