library(exoplanets)

options(
  exoplanets.progress = FALSE, # hide progress
  readr.show_types = FALSE     # hide col spec, requires readr 2.0.0 >=
)

library(tidyverse)
library(rafalib)
library(ggrepel)

library(caret)


# ## PSCompPars

tabne_name = "PSCompPars"

# direct load
# (planets = exoplanets(tabne_name)

# cachedload
fields = "pl_name,pl_bmasse,pl_rade,pl_dens,pl_radeerr1,pl_radeerr2,pl_bmasseerr1,pl_bmasseerr2,discoverymethod,disc_facility,pl_controv_flag,sy_dist"
url <- paste0("https://exoplanetarchive.ipac.caltech.edu/TAP/sync?query=select+", fields,
              "+from+PSCompPars&format=csv")
filename <- paste0(tabne_name, ".cache.csv")
library(downloader)
if (!file.exists(filename)) download(url, filename)
planets_qry <- read.csv(filename) #, skip=1)
(planets = as_tibble(planets_qry))
(planets_train = planets %>% na.omit())


mass = planets$pl_bmasse
radius = planets$pl_rade
# volume = 4/3*pi*radius^3  # / 4/3*pi*radius_e^3  # = radius^3
volume = radius^3
# gravity = G*M/radius^2
gravity = 1/radius^2

## Histograms by density.

density_e=5.51

density = planets$pl_dens
range(density %>% na.omit())

log_density = log10(planets$pl_dens[planets$pl_dens>0])
log_density_e = log10(planets$pl_dens[planets$pl_dens>0] / density_e)


## Histograms by size.

col_earth = "#4040a0a0"
grid_col = col_earth

log.radius = log10(radius %>% na.omit())
(log.radius.range = range(log.radius))

# breaks = log10(c(.1, 1.25, 2, 6, 15, 100))
radius_stops = c(10^log.radius.range[1], 1.25, 2, 6, 15, 10^log.radius.range[2])
radius_breaks = log10(radius_stops)
radius_col = c("#0060a0", "#20c000", "#00a0e0", "#c0a060", "#c00000")
labels = c("Earth-size", "Super-Earth-size", "Neptune-size", "Jupiter-size", "Larger")


## Colors

col_water = "#0040FFa0"
col_earth = col_earth
col_iron = "#404050a0"
col_earth_core = "#f0f0f8a0"

## Density

density_water = 1
density_earth = density_e
density_iron = 8
density_earth_core = 13

# col=factor(round(log_density_e*2)/2)
col=factor(round(log_density_e))
levels(col)
label_value = 10^as.numeric(levels(col))



errorbar_size = 1e-1
errorbar_col = "#404040"
errorbar_alpha = .2
# planets %>%

planets.all = planets
nrow(planets.all)
# planets.clean = planets.all %>%
#   select(pl_name, pl_bmasse, pl_rade, pl_dens,
#          pl_radeerr1=0, pl_radeerr2=0, pl_bmasseerr1=0, pl_bmasseerr2=0,
#          discoverymethod, disc_facility, pl_controv_flag, sy_dist) %>%
#   na.omit()
# nrow(planets.clean)
planets.good = planets.all %>%
  # select(pl_name, pl_bmasse, pl_rade, pl_dens,
  #        pl_radeerr1, pl_radeerr2, pl_bmasseerr1, pl_bmasseerr2,
  #        discoverymethod, disc_facility, pl_controv_flag, sy_dist) %>%
  na.omit()
nrow(planets.good)

distance = Inf
# distance = 1000
# distance = 100
distance = 50
# distance = 20
# distance = 10
# distance = 8
# distance = 5

# planets.good.filtered = planets.clean %>%
planets.good.filtered = planets.good %>%
  # filter(str_detect(pl_name, 'TOI'))
  # filter(str_detect(pl_name, 'Kepler'))
  # filter(!str_detect(pl_name, 'Kepler')) %>%
  # filter(.3<=mass & mass<=40) %>%
  # filter(0<=radius & radius<=10) %>%
  # filter(sy_dist<distance) %>%
  # filter(sy_dist<100) %>%
  filter(sy_dist<distance)
nrow(planets.good.filtered)

# planets.target = planets.all
# planets.target = planets.good
planets.target = planets.good.filtered
nrow(planets.target)
planets.target.mutated = planets.target %>%
  mutate(name = pl_name,
         mass = pl_bmasse,
         radius = pl_rade,
         # volume = 4/3*pi*radius^3,
         volume = radius^3,
         # density = log10(.$pl_dens / density_e),
         density = .$pl_dens / density_e,
         diameter = 2*radius,
         controversial = pl_controv_flag,
         telescope = disc_facility
         )
planets.target.mutated %>%
  ggplot(aes(radius, mass, col=factor(round(density)), size=diameter, shape=discoverymethod)) +
  labs(col="density") +
  theme_light() +
  
  # ggplot(aes(radius, mass, col=factor(telescope), size=diameter, shape=discoverymethod)) +
  # labs(title = "Expolanets", col = "telescope") +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides="trbl") +
  geom_hline(yintercept = 1, col=grid_col, size=1.1) +
  geom_vline(xintercept = 1, col=grid_col, size=1.1) +
  geom_vline(xintercept = radius_stops, col=c(radius_col, radius_col[1])) +
  # geom_abline(slope = density_e, intercept = 0, col=grid_col) +
  geom_errorbar(aes(x=radius,
                    ymin=mass + pl_bmasseerr2,
                    ymax=mass + pl_bmasseerr1),
                size=errorbar_size, col=errorbar_col, alpha=errorbar_alpha) +
  geom_errorbar(aes(y=mass,
                    xmin=radius + pl_radeerr2,
                    xmax=radius + pl_radeerr1),
                size=errorbar_size, col=errorbar_col, alpha=errorbar_alpha) +
  geom_point(alpha=.4) +
  geom_text_repel(aes(radius, mass, label=name), max.overlaps = 20) +
  # geom_text_repel(aes(radius, mass, label=controversial), max.overlaps = 30) +
  # geom_text(nudge_x=0.1, cex = 2) +
  # geom_label(aes(label=planets.target$pl_name, alpha=.4)) +
  # scale_shape_manual(values=seq(65, 65 + length(levels(factor(planets.target$discoverymethod)))))
  scale_shape_manual(values=c(15, 4, 16, 3, 17)) +
  # geom_abline(slope = density_e, intercept = 0, col=grid_col) +
  # geom_curve(slope = density_e, intercept = 0, col=grid_col) +
  # geom_curve(aes(x = radius, y = mass, xend = radius, yend = mass*3)) +
  # geom_function(fun = ~ 0.5*exp(-abs(.x))) +
  geom_function(fun = ~ density_water/density_earth*.x^3, col=col_water, size=0) +
  geom_function(fun = ~ density_earth/density_earth*.x^3, col=col_earth, size=0, lty=2) +
  geom_function(fun = ~ density_iron/density_earth*.x^3, col=col_iron, size=0) +
  geom_function(fun = ~ density_earth_core/density_earth*.x^3, col=col_earth_core, size=0) +
  
  geom_function(fun = ~ (density_water/density_earth*.x)^-2, col=col_water, size=0) +
  geom_function(fun = ~ (density_earth/density_earth*.x)^-2, col=col_earth, size=0, lty=2) +
  geom_function(fun = ~ (density_iron/density_earth*.x)^-2, col=col_iron, size=0) +
  geom_function(fun = ~ (density_earth_core/density_earth*.x)^-2, col=col_earth_core, size=0) +
  # geom_function(fun = ~ .x) +
  ggtitle(paste("Exoplanets within", distance, "parsecs"))
# + 
#   theme(legend.position = "bottom") +
#   xlim(-4, 4) + ylim(-4,4)


library(caret)
# install.packages("gam")
modelLookup("gamLoess")

# grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
grid <- expand.grid(span = seq(0.30, 0.40, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1


fit = train(mass ~ radius,
            method = "gamLoess",
            tuneGrid=grid,
            data = planets.target.mutated)
# confusionMatrix(data = predict(fit, newdata=data.frame(radius=c(1,5,10))),
#                 reference = data.frame(radius=c(1,5,10))
#   )$overall["Accuracy"]

geom_abline(slope = fit$coef[2], intercept = fit$coef[1], col=grid_col)
geom_abline(fit$coef)

ggplot(fit, highlight = TRUE)
ggplot(fit, highlight = TRUE) +
  geom_abline(fit$coef[1], fit$coef[2]) +
  # geom_point(aes(fit$coef[1], fit$coef[2])) +
  scale_x_continuous() +
  scale_y_continuous()


mass_stops = c(.5, 10)
# target = planets.target.mutated %>%
#   mutate(radius_class = factor(-.5 < radius & radius < 10),
#          mass_class = factor(-.5 < mass & mass < 10),
#   )
target = planets.target.mutated %>%
  mutate(radius_class=as_factor(
           sapply(radius, function(x) ifelse(is.na(sd), NA, which.max(x < radius_stops))) %>% unlist()),
         mass_class=as_factor(
           sapply(mass, function(x) ifelse(is.na(sd), NA, which.max(x < mass_stops))) %>% unlist()))
planets.target.mutated %>% select(name, radius_class, mass_class)
str(target)

target %>%
  ggplot(aes(radius, mass, col = density, size = radius)) +
  geom_point()

fit = train(density ~ radius_class + mass_class,
            method = "gamLoess",
            tuneGrid=grid,
            data = target)

# planets.target.mutated %>% ggplot(aes(radius, mass, z = p, fill = p)) +
target %>%
  na.omit() %>%
  ggplot(aes(radius, mass, z = density, fill = density)) +
  # geom_raster() +
  geom_tile(aes(width=.1, height=100)) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")





library(dslabs)
mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)

head(mnist_27$train)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$table


mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("red","white","blue")) +
  stat_contour(breaks=c(0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875), color="darkgrey") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

head(mnist_27$train)
head(mnist_27$true_p)

planets.target.mutated %>%
  select(name, radius, mass, density, radius_class) %>%
  ggplot(aes(radius, mass,  z=radius_class, fill=radius_class)) +
  geom_tile(aes(radius, mass, width=.1, height=100))


plot(p_hat_glm)



# p_hat <- predict(fit, target, type = "prob")
p_hat <- predict(fit, target)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(data = y_hat, reference = target$radius_class)$overall["Accuracy"]
confusionMatrix(data = y_hat, reference = target$radius_class)$table
confusionMatrix(data = y_hat, reference = target$mass_class)$overall["Accuracy"]
confusionMatrix(data = y_hat, reference = target$mass_class)$table

target %>%
  na.omit() %>%
  ggplot(aes(radius, mass, z = p_hat, fill = p_hat)) +
  geom_raster()
  


library(MASS)
hm_col_scale<-colorRampPalette(c("transparent","blue","green","yellow","orange","red"))(1000)
# grid = with(target, kde2d(radius, mass, n=100))
grid = with(target, kde2d(log10(radius), log10(mass), n=100))
image(grid, col = hm_col_scale)
points(log10(radius), log10(mass), col = col)
points(log10(target$radius), log10(target$mass), col = "red", pch=8)
text(log10(target$radius), log10(target$mass), labels=target$name, cex=0.5, pos = 4)
contour(grid, nlevels = 10, add = TRUE, col=col_iron)
