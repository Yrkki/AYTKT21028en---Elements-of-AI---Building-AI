# fetch = function(package) { if (!requireNamespace(package, quietly = TRUE))
#   install.packages(package)
# }
# 
# # install.packages("exoplanets")
# fetch("exoplanets")
# 
# # install.packages("devtools")
# fetch("devtools")
# 
# devtools::install_github("ropensci/exoplanets")

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("Biobase")

library(exoplanets)

options(
  exoplanets.progress = FALSE, # hide progress
  readr.show_types = FALSE     # hide col spec, requires readr 2.0.0 >=
)
# 
# (k2names = exoplanets("k2names"))
# #> • https://exoplanetarchive.ipac.caltech.edu/TAP/sync?query=select+*+from+k2names&format=csv
# #> # A tibble: 449 x 3
# #>    epic_id        k2_name  pl_name     
# #>    <chr>          <chr>    <chr>       
# #>  1 EPIC 246199087 K2-112 f TRAPPIST-1 f
# #>  2 EPIC 246199087 K2-112 h TRAPPIST-1 h
# #>  3 EPIC 211331236 K2-117 c K2-117 c    
# #>  4 EPIC 212398486 K2-125 b K2-125 b    
# 
# (pl_name = exoplanets("ps", c("pl_name", "hostname")))
# #> • https://exoplanetarchive.ipac.caltech.edu/TAP/sync?query=select+pl_name,hostname+from+ps&format=csv
# #> # A tibble: 29,683 x 2
# #>    pl_name      hostname  
# #>    <chr>        <chr>     
# #>  1 Kepler-11 c  Kepler-11 
# #>  2 Kepler-11 f  Kepler-11 
# #>  3 HAT-P-1 b    HAT-P-1   
# 
# (keplernames = exoplanets("keplernames", limit = 5))
# #> • https://exoplanetarchive.ipac.caltech.edu/TAP/sync?query=select+*+from+keplernames+top+5&format=csv
# #> # A tibble: 5 x 4
# #>     kepid koi_name  kepler_name   pl_name      
# #>     <dbl> <chr>     <chr>         <chr>        
# #> 1 7515212 K00679.02 Kepler-212 b  Kepler-212 b 
# #> 2 8210018 K02762.01 Kepler-1341 b Kepler-1341 b
# #> 3 9008737 K02768.01 Kepler-404 b  Kepler-404 b 
# 
# tableinfo
# #> # A tibble: 546 x 13
# #>    table database_column_… table_label description  in_ps_table in_ps_comp_pars…
# #>    <chr> <chr>             <chr>       <chr>        <lgl>       <lgl>           
# #>  1 ps    default_flag      Default Pa… Boolean fla… TRUE        FALSE           
# #>  2 ps    soltype           Solution T… Disposition… TRUE        FALSE           
# #>  3 ps    pl_controv_flag   Controvers… Flag indica… TRUE        TRUE            
# #>  4 ps    pl_name           Planet Name Planet name… TRUE        TRUE            
# #>  5 ps    hostname          Host Name   Stellar nam… TRUE        TRUE           


library(MASS)
library(tidyverse)
library(rafalib)
library(ggrepel)
library(caret)

# ## PS
# (planets = exoplanets("ps"))
# str(planets)
# class(planets)
# 
# (cols = colnames(planets))
# (colslength = length(cols))
# (size = "size" %in% cols)
# planets$pl_massjlim %>% na.omit()
# (radj = planets$pl_radj %>% na.omit())
# (radjlim = planets$pl_radjlim %>% na.omit())
# plot(radj, radjlim)
# sum(abs(radjlim - radj) > 1)
# plot(radj, radjlim)
# radjlim - radj
# sum(radjlim > 0)
# planets$pl_radjlim
# which(planets$pl_radjlim==1)
# planets[327]


## PSCompPars

tabne_name = "PSCompPars"


## Load data

# direct load
# (planets = exoplanets(tabne_name)

# cached load
fields = "pl_name,pl_bmasse,pl_rade,pl_dens,pl_radeerr1,pl_radeerr2,pl_bmasseerr1,pl_bmasseerr2,discoverymethod,disc_facility,pl_controv_flag,sy_dist"
url <- paste0("https://exoplanetarchive.ipac.caltech.edu/TAP/sync?query=select+", fields,
              "+from+PSCompPars&format=csv")
filename <- paste0(tabne_name, ".cache.csv")
library(downloader)
if (!file.exists(filename)) download(url, filename)
planets_qry <- read.csv(filename) #, skip=1)
(planets = as_tibble(planets_qry))
(planets_train = planets %>% na.omit())
planets_test = setdiff(planets, planets_train)
# rownames(planets_test)

# planets = planets[str_detect(planets$disc_facility, "La Silla"),]  # HARPS


## Constants

e_radius=6371
e_density=5.51
j_mass=317.8

solar_system = tribble(
  ~pl_name, ~pl_bmasse, ~pl_rade, ~pl_dens, ~pl_radeerr1, ~pl_radeerr2, ~pl_bmasseerr1, ~pl_bmasseerr2, ~discoverymethod, ~disc_facility, ~pl_controv_flag, ~sy_dist,
  "Mercury", 0.055, 0.3826, 5.43/e_density, 0, 0, 0, 0, "", "", 0, 0,
  "Venus", 0.815, 0.9488, 5.24/e_density, 0, 0, 0, 0, "", "", 0, 0,
  "Earth", 1, 1, 1, 0, 0, 0, 0, "", "", 0, 0,
  "Mars", 0.107, 0.53247, 3.940/e_density, 0, 0, 0, 0, "", "", 0, 0,
  "Jupiter", j_mass, 11.209, 1.33/e_density, 0, 0, 0, 0, "", "", 0, 0,
  "Saturn", 95, 9.449, 0.70/e_density, 0, 0, 0, 0, "", "", 0, 0,
  "Uranus", 14.5, 4.007, 1.30/e_density, 0, 0, 0, 0, "", "", 0, 0,
  "Neptune", 17, 3.883, 1.76/e_density, 0, 0, 0, 0, "", "", 0, 0,
  "Pluto", 0.00218, 0.1868, 1.854/e_density, 0, 0, 0, 0, "", "", 0, 0,
  "Moon", 0.012300, 0.2727, 0.606, 0, 0, 0, 0, "", "", 0, 0,
  "Ganymede", 0.025, 0.413, 1.936/e_density, 0, 0, 0, 0, "", "", 0, 0,
  "Titan", 0.0225, 0.404, 1.8798/e_density, 0, 0, 0, 0, "", "", 0, 0,
  "Ceres", 0.00016, 469.73/e_radius, 2.162/e_density, 0, 0, 0, 0, "", "", 0, 0,
)


## Colors

col_water = "#0040FFa0"
col_earth = "#4040a0a0"
col_iron = "#8b0000a0"
col_earth_core = "#404050a0"


## Graphics colors

grid_col = "#c0c0c0a0"
contour_col = grid_col
bg_legend = "#ffffffc0"


## Density

density_water = 1
density_earth = e_density
density_iron = 7.874
density_earth_core = 13

mass = planets$pl_bmasse
mass1 = planets$pl_bmasseerr1
mass2 = planets$pl_bmasseerr2
radius = planets$pl_rade
radius1 = planets$pl_radeerr1
radius2 = planets$pl_radeerr2
# volume = 4/3*pi*radius^3  # / 4/3*pi*e_radius^3  # = radius^3
volume = radius^3
# gravity = G*M/radius^2  # 1/radius^2
gravity = radius^-2


## Histograms by density.

density = planets$pl_dens
range(density %>% na.omit())

log_density = log10(planets$pl_dens)
png(filename='plots/log_density.png')
hist(log_density, labels = T)
abline(0, 1e10, col = col_earth, lwd=3)
dev.off()

log_e_density = log10(planets$pl_dens / e_density)
png(filename='plots/log_e_density.png')
hist(log_e_density, labels = T)
abline(0, 1e10, col = col_earth, lwd=3)
dev.off()


## “Small” and “large” planets transitions

# We suggest that the transition between the two regimes of “small” and “large” planets 
# occurs at a mass of 124 ± 7M⊕ and a radius of 12.1 ± 0.5R⊕. Furthermore, the M-R relation 
# is R ∝ M0.55 ± 0.02 and R ∝ M0.01 ± 0.02 for small and large planets, respectively
# 
# # Ref: https://www.aanda.org/articles/aa/full_html/2017/08/aa29922-16/aa29922-16.html
transition_mass = 124
transition_radius = 12.1
mr_small = function(m) m^0.55
mr_small1 = function(m) m^(0.55 + .02)
mr_small2 = function(m) m^(0.55 - .02)
mr_large = function(m) m^0.01
mr_large1 = function(m) m^(0.01 + .02)
mr_large2 = function(m) m^(0.01 - .02)


## Histograms by mass and radius.

log.radius = log10(radius %>% na.omit())
(log.radius.range = range(log.radius))
log.mass = log10(mass %>% na.omit())
(log.mass.range = range(log.mass))

# breaks = log10(c(.1, 1.25, 2, 6, 15, 100))
radius_stops = c(10^log.radius.range[1], 1.25, 2, 4, 7, 10, 15, 17.25, 10^log.radius.range[2])
radius_breaks = log10(radius_stops)
radius_col = c("#4040a0", "#20c000", "#20d0d0", "#00a0e0", "#60a0a0", "#c0a060", "#c00000", "#964B00")
mass_stops = c(10^log.mass.range[1], 1.9, 10, 20, 50, 100, 6992, 25424, 10^log.mass.range[2])
mass_breaks = log10(mass_stops)
mass_col = radius_col
class_labels = c("Earth-size", 
           "Super-Earth/Rocky world", 
           "Mega-Earth/Mini-Neptune/Sub-Neptune/Water world", 
           "Neptune-size/Super-Neptune/Sub-Saturn", 
           "Transitional", 
           "Jupiter-size/Gas giant", 
           "Super-Jupiter", 
           "Brown dwarf/Larger")
class_labels_multiline = str_replace_all(class_labels,"/", "\n")

# Bivariate density color
hm_col_scale = colorRampPalette(c("transparent","blue","green","yellow","orange","red"))(1000)
hm_col_scale = paste0(hm_col_scale, "40")


## Unsupervised classification

k_means = function (dat, labels, nstart = 25, verbose=FALSE, col=grid_col) {
  dat <- dat %>% as.matrix()
  dat[is.na(dat)] <- 0
  
  centers = length(unique(labels)) + 1
  km <- kmeans(dat, centers = centers, nstart = nstart)
  print(km$size)
  
  if(verbose) {
    print(km$centers)
    print(km$iter)
    print(km$ifault)
    
    mds = cmdscale(dist(dat))
    
    plot(mds, col=km$cluster)
    abline(h=0, col=col, lwd=2)
    abline(v=0, col=col, lwd=2)
    abline(0, 3, lty=2, col=col)
  }
  
  km
}

## Data preparation

na_to_zero = function(x) ifelse(is.na(x), 0, x)

# na_to_one = function(x) ifelse(is.na(x), 1, x)
na_to_one = function(x) x

prepare = function(df) {
  df = df %>% mutate(
    pl_bmasse = na_to_one(pl_bmasse),
    mass = pl_bmasse,
    pl_bmasseerr1 = na_to_zero(pl_bmasseerr1),
    pl_bmasseerr2 = na_to_zero(pl_bmasseerr2),

    pl_rade = na_to_one(pl_rade),
    radius = pl_rade,
    pl_radeerr1 = na_to_zero(pl_radeerr1),
    pl_radeerr2 = na_to_zero(pl_radeerr2),

    pl_bmasse_log = log10(pl_bmasse),
    pl_bmasse1 = pl_bmasse + pl_bmasseerr1,
    pl_bmasse1_log = log10(pl_bmasse1),
    pl_bmasse2 = pl_bmasse + pl_bmasseerr2,
    pl_bmasse2_log = log10(pl_bmasse2),
    
    pl_rade_log = log10(pl_rade),
    pl_rade1 = pl_rade + pl_radeerr1,
    pl_rade1_log = log10(pl_rade1),
    pl_rade2 = pl_rade + pl_radeerr2,
    pl_rade2_log = log10(pl_rade2),

    pl_diametere = 2*pl_rade,
    # diameter = pl_diametere,

    # pl_volume = 4/3*pi*pl_rad^3,
    pl_volumee = pl_rade^3,
    # volume = pl_volumee,
    pl_volumee_log = log10(pl_volumee),
    
    pl_dens = na_to_one(pl_dens),
    pl_dens_log = log10(pl_dens),
    pl_dense = pl_dens / e_density,
    # density = pl_dense,
    pl_dense_log = pl_dens_log - log10(e_density),
    pl_dense_class = as_factor(round(pl_dense_log)),
    
    # pl_rade_class = as_factor(ifelse(is.na(pl_rade), NA, which.max(pl_rade < radius_stops))),
    # pl_bmasse_class = as_factor(ifelse(is.na(pl_bmasse), NA, which.max(pl_bmasse < mass_stops))),
    pl_rade_class=as_factor(
      sapply(pl_rade, function(x) ifelse(is.na(x), NA, which.max(x < radius_stops))) %>% unlist()),
    pl_bmasse_class=as_factor(
      sapply(pl_bmasse, function(x) ifelse(is.na(x), NA, which.max(x < mass_stops))) %>% unlist()),
    
    pl_class = as_factor(ifelse(is.na(pl_bmasse_class), as.numeric(levels(pl_rade_class)[pl_rade_class]), as.numeric(levels(pl_bmasse_class)[pl_bmasse_class]))),
    pl_incomplete = as_factor(is.na(pl_bmasse) | is.na(pl_rade)),
    discoverymethod = as_factor(discoverymethod)
  )
  
  km = k_means(df %>% select(pl_bmasse_log, pl_rade_log), class_labels, verbose = F)
  df[, "pl_class_kmeans"] <- factor(km$cluster)
  
  df
}

### Prepare the solar system
solar_system = prepare(solar_system)
# solar_system %>% select(-matches("disc|eerr|flag|sy_"))
# km = k_means(solar_system %>% select(pl_bmasse_log, pl_rade_log), class_labels, verbose = T)

# # Cap to Neptunes
# tops = c(5, 5)
# planets = planets[planets$pl_rade < radius_stops[tops[1]] & planets$pl_bmasse < mass_stops[tops[2]],]

## Naive classification
planets_mutated = prepare(planets)
# km = k_means(planets_mutated %>% select(pl_bmasse_log, pl_rade_log), class_labels, verbose = T)
planets_incomplete_ind <- which(as.logical(planets_mutated$pl_incomplete))
planets_incomplete = planets_mutated[planets_incomplete_ind,]

planets_good = planets_mutated[-planets_incomplete_ind,]
# planets_good = planets_mutated

# planets_incomplete %>% select(pl_name, pl_bmasse, pl_rade, pl_bmasse_class, pl_rade_class, pl_class, pl_controv_flag, pl_incomplete) %>% print(., n=nrow(.))

grid = planets_good %>% with(., kde2d(pl_rade_log, pl_bmasse_log, n=100))
grid_mr = planets_good %>% with(., kde2d(pl_bmasse_log, pl_rade_log, n=100))
# class(grid_mr)
# sum(grid_mr %>% unlist() - grid_mr %>% unlist())

# grid = with(target, kde2d(log10(radius), log10(mass), n=100))
# grid = with(target, kde2d(radius, mass, n=100))
# grid = kde2d(log10(radius), log10(mass), n=100)
# grid = kde2d(planets_train$pl_rade, planets_train$pl_bmasse, n=100)
# grid = with(planets_train, kde2d(log10(pl_rade), log10(pl_bmasse), n=100))
# grid = with(planets_train, kde2d(pl_rade, pl_bmasse, n=100))
# grid = with(planets.target.mutated, kde2d(log10(radius), log10(mass), n=100))
# grid = with(target, kde2d(log10(radius), log10(mass), n=100))



## By mass

par(mar = c(5.1, 4.1, 4.1, 2.1)) # default
png(filename='plots/log_mass_class_labels_multiline.png')
hist(log.mass, breaks = mass_breaks, col = mass_col, labels = class_labels_multiline)
lines(density(log.mass), lwd = 1, col = "grey")
abline(0, 1e10, col = col_earth, lwd=3)
dev.off()

png(filename='plots/log_mass.png')
hist(log.mass, breaks = mass_breaks, col = mass_col, labels = T)
lines(density(log.mass), lwd = 1, col = "grey")
abline(0, 1e10, col = col_earth, lwd=3)
legend("topleft", class_labels, col = mass_col, pch = 15, bg="transparent", bty = "n")
dev.off()

# Warning message:
#   In plot.histogram(r, freq = freq1, col = col, border = border, angle = angle,  :
#                       the AREAS in the plot are wrong -- rather use 'freq = FALSE'
png(filename='plots/log_mass_freq.png')
hist(log.mass, breaks = mass_breaks, col = mass_col, labels = T, freq = TRUE)
abline(0, 1e10, col = col_earth, lwd=3)
legend("topleft", class_labels, col = mass_col, pch = 15, bg="transparent", bty = "n")
dev.off()


radius_col_ext = c(radius_col[1], radius_col)
png(filename='plots/pl_rade_log.png')
planets_mutated %>%
  ggplot(aes(pl_rade_log)) +
  geom_histogram(col="black", binwidth = .1) +
  geom_rug(col=radius_col_ext[as.numeric(planets_mutated$pl_rade_class)]) +
  geom_density() +
  geom_vline(xintercept = 0, col=col_earth) +
  geom_vline(xintercept = radius_breaks, col=c(radius_col, radius_col[1])) +
  xlab("Radius") +
  ylab("Frequency") +
  ggtitle("Histogram of radius")
dev.off()


## By radius

par(mar = c(5.1, 4.1, 4.1, 2.1)) # default
png(filename='plots/log_radius_class_labels_multiline.png')
hist(log.radius, breaks = radius_breaks, col = radius_col, labels = class_labels_multiline)
lines(density(log.radius), lwd = 1, col = "grey")
abline(0, 1e10, col = col_earth, lwd=3)
dev.off()

png(filename='plots/log_radius.png')
hist(log.radius, breaks = radius_breaks, col = radius_col, labels = T)
lines(density(log.radius), lwd = 1, col = "grey")
abline(0, 1e10, col = col_earth, lwd=3)
legend("topleft", class_labels, col = radius_col, pch = 15, bg="transparent", bty = "n")
dev.off()

# Warning message:
#   In plot.histogram(r, freq = freq1, col = col, border = border, angle = angle,  :
#                       the AREAS in the plot are wrong -- rather use 'freq = FALSE'
png(filename='plots/log_radius_freq.png')
hist(log.radius, breaks = radius_breaks, col = radius_col, labels = T, freq = TRUE)
abline(0, 1e10, col = col_earth, lwd=3)
legend("topleft", class_labels, col = radius_col, pch = 15, bg="transparent", bty = "n")
dev.off()


mass_col_ext = c(mass_col[1], mass_col)
png(filename='plots/pl_bmasse_log.png')
planets_mutated %>%
  ggplot(aes(pl_bmasse_log)) +
  geom_histogram(col="black", binwidth = .1) +
  geom_rug(col=radius_col_ext[as.numeric(planets_mutated$pl_bmasse_class)]) +
  geom_density() +
  geom_vline(xintercept = 0, col=col_earth) +
  geom_vline(xintercept = mass_breaks, col=c(mass_col, mass_col[1])) +
  xlab("Mass") +
  ylab("Frequency") +
  ggtitle("Histogram of mass")
dev.off()


# col=factor(round(log_e_density*2)/2)
col=factor(round(log_e_density))
# levels(col)
label_value = 10^as.numeric(levels(col))

at.x <- outer(1:9, 10^(-2:5))
lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
at.y <- outer(1:9, 10^(-2:5))
lab.y <- ifelse(log10(at.y) %% 1 == 0, at.y, NA)
at.x1 <- outer(1:9, 10^((-2:5)))^(1/3)
lab.x1 <- ifelse(row(at.x1) == 1, round(at.x1^3,1), NA)

# coef=lm(mass ~ radius)$coef
# a = log10(coef[1])
# b = log10(coef[2])
# a = coef[1]
# b = coef[2]

# par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mar = c(5.1, 4.1, 6.1, 2.1))
png(filename='plots/pl_rade.png')
with(planets_mutated, plot(pl_rade, pl_bmasse, col=col, log="xy", xaxt="n", yaxt="n"))
# plot(radius, mass, col=col, log="xy", xaxt="n", yaxt="n")
# image(grid, col = hm_col_scale, add = TRUE, xlim=log10(range(radius)), ylim=log10(range(mass)))
# contour(grid, nlevels = 10, add = TRUE, col=contour_col, zlim=range(grid$z))
axis(1, at=at.x, labels=lab.x, las=1)
axis(2, at=at.y, labels=lab.y, las=1)
axis(3, at=at.x1, labels=lab.x1, las=1)
mtext("volume", side = 3, line = 2)
abline(h=1, col=col_earth, lwd=3)
abline(v=1, col=col_earth, lwd=3)
# curve(x^3, add=TRUE, col=col_earth, lty=2)
# curve(x^1, add=TRUE, col=col_earth, lty=2)
# curve(x^-2, add=TRUE, col=col_earth, lty=2)
# curve(density_water/density_earth*x^3, add=TRUE, col=col_water)
# curve(density_water/density_earth*x^1, add=TRUE, col=col_water)
# curve(density_water/density_earth*x^-2, add=TRUE, col=col_water)

curve(density_water/density_earth*x^3, add=TRUE, col=col_water)
curve(density_earth/density_earth*x^3, add=TRUE, col=col_earth, lty=2)
curve(density_iron/density_earth*x^3, add=TRUE, col=col_iron)
curve(density_earth_core/density_earth*x^3, add=TRUE, col=col_earth_core)
# curve((density_water/density_earth*x)^-2, add=TRUE, col=col_water)
# curve((density_earth/density_earth*x)^-2, add=TRUE, col=col_earth, lty=2)
# curve((density_iron/density_earth*x)^-2, add=TRUE, col=col_iron)
# curve((density_earth_core/density_earth*x)^-2, add=TRUE, col=col_earth_core)
curve(density_water/density_earth*x, add=TRUE, col=col_water)
curve(density_earth/density_earth*x, add=TRUE, col=col_earth, lty=2)
curve(density_iron/density_earth*x, add=TRUE, col=col_iron)
curve(density_earth_core/density_earth*x, add=TRUE, col=col_earth_core)
# curve(density_water/density_earth*x^-2, add=TRUE, col=col_water)
# curve(density_earth/density_earth*x^-2, add=TRUE, col=col_earth, lty=2)
# curve(density_iron/density_earth*x^-2, add=TRUE, col=col_iron)
# curve(density_earth_core/density_earth*x^-2, add=TRUE, col=col_earth_core)
points(solar_system$pl_rade, solar_system$pl_bmasse, col = "blue", pch=20)
text(solar_system$pl_rade, solar_system$pl_bmasse, labels=solar_system$pl_name, cex=0.6, adj = c(0, 0), col = "blue")
legend("bottomright",levels(col),col=seq_along(col),pch=1, label_value(label_value), title = "Density")
title("Density", line=4)
dev.off()

# grid = with(target, kde2d(10^radius, 1.01^mass, n=100))
# grid = with(target, kde2d(radius, mass, n=100))
# 
# grid = with(target, kde2d(log10(radius), log10(mass), n=100))
# # plot(NULL, xlim=c(0,100), ylim=c(0,100), axes=FALSE, xlab="", ylab="")
# # plot(NULL, col=col, log="xy")
# plot(NULL, col=col, log="xy", xaxt="n", yaxt="n")
# image(grid, col = hm_col_scale, xlab = "radius", ylab = "mass", main="Density", log="xy", xlim=c(0.1, max(target$radius)), ylim=c(0.1, max(target$mass)))
# # points(radius, mass, col = col)
# points(log10(radius), log10(mass), col = col)
# 
# IM=image(grid, col = hm_col_scale, xlab = "radius", ylab = "mass", main="Density")
# rasterImage(IM,0,0,100,100)
# rasterImage(image(grid, col = hm_col_scale, xlab = "radius", ylab = "mass", main="Density"),0,0,100,100)
# 
# with(target, {
#      image(with(target, kde2d(radius, mass, n=100)), col = hm_col_scale, xlab = "radius", ylab = "mass", main="Density",
#            # log="xy", xaxt="n", yaxt="n",
#            log="xy",
#            xlim=c(1, max(target$radius)), ylim=c(1, max(target$mass)))
# points(radius, mass, col = col)})
# 
# points(log10(radius), log10(mass), col = col)
# points(log10(target$radius), log10(target$mass), col = "red", pch=8)
# # text(grid, labels=rownames(radius),data=target, cex=0.9, font=2)
# text(log10(target$radius), log10(target$mass), labels=target$name, cex=0.5, pos = 4)
# contour(grid, grid, nlevels = 10, add = TRUE, col=contour_col)
# abline(h=0, col=col_earth, lwd=3)
# abline(v=0, col=col_earth, lwd=3)
# # curve(density_water/density_earth*x^3, add=TRUE, col=col_water)
# legend("bottomright",levels(col),col=seq_along(col),pch=1, label_value(label_value), title = "Density")


mr_relation = function(df, mr_title="Exoplanets Mass-Radius relation", log="xy", labels=FALSE) {
  par(mar = c(5.1, 4.1, 4.1, 5.1))
  with(df, {
    plot(pl_bmasse, pl_rade, col=pl_dense_class, log=log, xaxt="n", yaxt="n")
    axis(1, at=at.x, labels=lab.x, las=1)
    # axis(1, at=at.y, labels=lab.y, las=1)
    if (str_detect(log, "y")) {
      axis(2, at=at.x, labels=lab.x, las=1)
      # axis(2, at=at.y, labels=lab.y, las=1)
      axis(4, at=at.x1, labels=lab.x1, las=1)
      mtext("volume", side = 4, line = 3)
    } else {
      axis(2, las=1)
    }

    abline(h=1, col=col_earth, lwd=3)
    abline(v=1, col=col_earth, lwd=3)
    curve((x*density_earth/density_water)^(1/3), add=TRUE, col=col_water)
    curve((x*density_earth/density_earth)^(1/3), add=TRUE, col=col_earth, lty=2)
    curve((x*density_earth/density_iron)^(1/3), add=TRUE, col=col_iron)
    curve((x*density_earth/density_earth_core)^(1/3), add=TRUE, col=col_earth_core)
    # curve(1.25*mr_small(x/2), add=TRUE, col="red", lty=2)
    # curve(1.25*mr_small1(x/2), add=TRUE, col="red", lty=3)
    # curve(1.25*mr_small2(x/2), add=TRUE, col="red", lty=3)
    # curve(14*mr_large(x/120), add=TRUE, col="orange", lty=2)
    # curve(14*mr_large1(x/120), add=TRUE, col="orange", lty=3)
    # curve(14*mr_large2(x/120), add=TRUE, col="orange", lty=3)

    # curve(density_water/density_earth*x^-.5, add=TRUE, col=col_water)
    # curve(density_earth/density_earth*x^-.5, add=TRUE, col=col_earth, lty=2)
    # curve(density_iron/density_earth*x^-.5, add=TRUE, col=col_iron)
    # curve(density_earth_core/density_earth*x^-.5, add=TRUE, col=col_earth_core)
    curve(density_earth/density_water*x, add=TRUE, col=col_water)
    curve(density_earth/density_earth*x, add=TRUE, col=col_earth, lty=2)
    curve(density_earth/density_iron*x, add=TRUE, col=col_iron)
    curve(density_earth/density_earth_core*x, add=TRUE, col=col_earth_core)

    if (labels) text(pl_bmasse, pl_rade, labels=pl_name, cex=0.6, adj = c(0, 0))

    points(solar_system$pl_bmasse, solar_system$pl_rade, col = "blue", pch=20)
    text(solar_system$pl_bmasse, solar_system$pl_rade, labels=solar_system$pl_name, cex=0.6, adj = c(0, 0), col = "blue")
    
    legend("topleft",levels(col),col=seq_along(col),pch=1, label_value(label_value), title = "Density")
    title(mr_title)
    })
}

png(filename='plots/mr_relation.png')
mr_relation(planets_mutated)
# mr_relation(planets_mutated, log = "x")
dev.off()

planets_cropped = planets_mutated %>%
  filter(.3<=pl_bmasse & pl_bmasse<=40) %>%
  filter(0<=pl_rade & pl_rade<=10)
# mr_relation(planets_cropped, "Exoplanets Mass-Radius relation (cropped)")
mr_relation(planets_cropped, "Exoplanets Mass-Radius relation (cropped)", log = "x")


errorbar_size = 1e-1
errorbar_col = "#404040"
errorbar_alpha = .2
# planets %>%
#   ggplot(aes(radius, mass, col=col, size=2*radius)) +
#   labs(col="density") +
#   scale_x_log10() +
#   scale_y_log10() +
#   geom_hline(yintercept = 1, col=col_earth, size=1.1) +
#   geom_vline(xintercept = 1, col=col_earth, size=1.1) +
#   geom_errorbar(aes(x=radius,
#                     ymin=mass + pl_bmasseerr2,
#                     ymax=mass + pl_bmasseerr1),
#                 size=errorbar_size,
#                 col=errorbar_col,
#                 alpha=errorbar_alpha) +
#   geom_errorbar(aes(y=mass,
#                     xmin=radius + pl_radeerr2,
#                     xmax=radius + pl_radeerr1),
#                 size=errorbar_size,
#                 col=errorbar_col,
#                 alpha=errorbar_alpha) +
#   geom_point(alpha=.4)


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
# planets.good = setdiff(planets, planets.good)
nrow(planets.good)

## Distance cut-off
distance = Inf
# distance = 1000
# distance = 100
# distance = 50
# distance = 20
distance = 15
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
         # density = log10(.$pl_dens / e_density),
         density = .$pl_dens / e_density,
         diameter = 2*radius,
         controversial = pl_controv_flag,
         telescope = disc_facility
         )

target = planets.target.mutated %>%
  mutate(radius_class=as_factor(
    sapply(radius, function(x) ifelse(is.na(sd), NA, which.max(x < radius_stops))) %>% unlist()),
    mass_class=as_factor(
      sapply(mass, function(x) ifelse(is.na(sd), NA, which.max(x < mass_stops))) %>% unlist()))
grid = with(target, kde2d(log10(radius), log10(mass), n=100))
grid_mr = with(target, kde2d(log10(mass), log10(radius), n=100))


target = planets_mutated %>%
  # filter(str_detect(pl_name, 'TOI'))
  # filter(str_detect(pl_name, 'Kepler'))
  # filter(!str_detect(pl_name, 'Kepler')) %>%
  # filter(.3<=mass & mass<=40) %>%
  # filter(0<=radius & radius<=10) %>%
  # filter(sy_dist<distance) %>%
  # filter(sy_dist<100) %>%
  filter(sy_dist<distance)
nrow(target)


png(filename='plots/radius_mass.png')
planets.target.mutated %>%
  ggplot(aes(radius, mass, col=factor(round(density)), size=diameter, shape=discoverymethod)) +
  labs(col="density") +
  theme_light() +
  # image(grid, col = hm_col_scale) +
# points(log10(radius), log10(mass), col = col)
# contour(grid, nlevels = 10, add = TRUE, col=contour_col)

  # ggplot(aes(radius, mass, col=factor(telescope), size=diameter, shape=discoverymethod)) +
  # labs(title = "Expolanets", col = "telescope") +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides="trbl") +
  geom_hline(yintercept = 1, col=col_earth, size=1.1) +
  geom_vline(xintercept = 1, col=col_earth, size=1.1) +
  geom_hline(yintercept = mass_stops, col=c(mass_col, mass_col[1])) +
  geom_vline(xintercept = radius_stops, col=c(radius_col, radius_col[1])) +
  # geom_abline(slope = e_density, intercept = 0, col=col_earth) +
  geom_errorbar(aes(x=radius,
                    ymin=mass + pl_bmasseerr2,
                    ymax=mass + pl_bmasseerr1),
                size=errorbar_size, col=errorbar_col, alpha=errorbar_alpha) +
  geom_errorbar(aes(y=mass,
                    xmin=radius + pl_radeerr2,
                    xmax=radius + pl_radeerr1),
                size=errorbar_size, col=errorbar_col, alpha=errorbar_alpha) +
  geom_point(alpha=.4) +
  stat_ellipse(type="norm") +
  geom_text_repel(aes(radius, mass, label=name), max.overlaps = 20) +
  # geom_text_repel(aes(radius, mass, label=controversial), max.overlaps = 30) +
  # geom_text(nudge_x=0.1, cex = 2) +
  # geom_label(aes(label=planets.target$pl_name, alpha=.4)) +
  # scale_shape_manual(values=seq(65, 65 + length(levels(factor(planets.target$discoverymethod)))))
  scale_shape_manual(values=c(15, 4, 16, 3, 17)) +
  # geom_abline(slope = e_density, intercept = 0, col=col_earth) +
  # geom_curve(slope = e_density, intercept = 0, col=col_earth) +
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
dev.off()

# geom_curve(aes(x = x, y = y, xend = x.to, yend = y.to, color = x_gt_y_equal_xy_sign),
#            curvature = 0.75, angle = -45,
#            arrow = arrow(length = unit(0.25,"cm")))

# reverse
# https://exoplanetarchive.ipac.caltech.edu/exoplanetplots/exo_massradius.png
png(filename='plots/mass_radius.png')
planets.target.mutated %>%
  # filter(.3<=mass & mass<=40) %>%
  # filter(0<=radius & radius<=10) %>%
  ggplot(aes(mass, radius, col=factor(round(density)), size=diameter, shape=discoverymethod)) +
  labs(col="density") +
  theme_light() +
  
  scale_x_log10(limits= c(.3, 40)) +
  # scale_x_log10() +
  annotation_logticks(sides="tb") +
  
  # scale_y_continuous(limits= c(.3, 40), trans = 'log10') +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  # scale_y_continuous() +
  # scale_y_log10() +
  # annotation_logticks(sides="tblr") +
  
  geom_hline(yintercept = 1, col=col_earth, size=1.1) +
  geom_vline(xintercept = 1, col=col_earth, size=1.1) +
  geom_hline(yintercept = radius_stops, col=c(radius_col, radius_col[1])) +
  geom_vline(xintercept = mass_stops, col=c(mass_col, mass_col[1])) +
  geom_errorbar(aes(x=mass,
                    ymin=radius + pl_radeerr2,
                    ymax=radius + pl_radeerr1),
                size=errorbar_size, col=errorbar_col, alpha=errorbar_alpha) +
  geom_errorbar(aes(y=radius,
                    xmin=mass + pl_bmasseerr2,
                    xmax=mass + pl_bmasseerr1),
                size=errorbar_size, col=errorbar_col, alpha=errorbar_alpha) +
  geom_point(alpha=.4) +
  stat_ellipse(type="norm") +
  geom_text_repel(aes(mass, radius, label=name), max.overlaps = 20) +
  # scale_shape_manual(values=c(16, 3, 17)) +
  geom_function(fun = ~ (.x*density_earth/density_water)^(1/3), col=col_water, size=0) +
  geom_function(fun = ~ (.x*density_earth/density_earth)^(1/3), col=col_earth, size=0, lty=2) +
  geom_function(fun = ~ (.x*density_earth/density_iron)^(1/3), col=col_iron, size=0) +
  geom_function(fun = ~ (.x*density_earth/density_earth_core)^(1/3), col=col_earth_core, size=0) +
  # geom_function(fun = ~ (.x/density_water)^(1/3), col=col_water, size=0) +
  # geom_function(fun = ~ (.x/density_earth)^(1/3), col=col_earth, size=0, lty=2) +
  # geom_function(fun = ~ (.x/density_iron)^(1/3), col=col_iron, size=0) +
  # geom_function(fun = ~ (.x/density_earth_core)^(1/3), col=col_earth_core, size=0) +
  
  geom_function(fun = ~ density_water/density_earth*.x^-.5, col=col_water, size=0) +
  geom_function(fun = ~ density_earth/density_earth*.x^-.5, col=col_earth, size=0, lty=2) +
  geom_function(fun = ~ density_iron/density_earth*.x^-.5, col=col_iron, size=0) +
  geom_function(fun = ~ density_earth_core/density_earth*.x^-.5, col=col_earth_core, size=0) +
  ggtitle(paste("Exoplanets within", distance, "parsecs"))
dev.off()

# 
# # continuous
# # https://exoplanetarchive.ipac.caltech.edu/exoplanetplots/exo_massradius.png
# planets.target.mutated %>%
#   ggplot(aes(radius, mass, col=factor(round(density)), size=diameter, shape=discoverymethod)) +
#   labs(col="density") +
#   theme_light() +
#   scale_x_log10() +
#   scale_y_log10() +
#   annotation_logticks(sides="trbl") +
#   geom_hline(yintercept = 1, col=col_earth, size=1.1) +
#   geom_vline(xintercept = 1, col=col_earth, size=1.1) +
#   geom_point(alpha=.4) +
#   geom_text_repel(aes(radius, mass, label=name), max.overlaps = 5) +
#   scale_shape_manual(values=c(15, 4, 16, 3, 17)) +
#   ggtitle(paste("Exoplanets within", distance, "parsecs"))
# 
# # by volume
# planets.target.mutated %>%
#   ggplot(aes(volume, mass, col=factor(round(density)), size=diameter, shape=discoverymethod)) +
#   labs(col="density") +
#   theme_light() +
#   scale_x_continuous() +
#   scale_y_continuous() +
#   annotation_logticks(sides="trbl") +
#   geom_hline(yintercept = 1, col=col_earth, size=1.1) +
#   geom_vline(xintercept = 1, col=col_earth, size=1.1) +
#   geom_abline(slope = e_density, intercept = 0, col=col_earth) +
#   geom_point(alpha=.4) +
#   geom_text_repel(aes(volume, mass, label=name), max.overlaps = 5) +
#   scale_shape_manual(values=c(15, 4, 16, 3, 17)) +
#   ggtitle(paste("Exoplanets within", distance, "parsecs"))
# 
# density = planets.target.mutated$density
# plot(radius, mass, col=density)
# abline(h=0, col=col_earth, lwd=3)
# abline(v=0, col=col_earth, lwd=3)
# legend("bottomright",levels(col),col=seq_along(col),pch=1)
# # 
# # with(planets.target.mutated, {
# #   # plot(radius, mass, col=density)
# #   # abline(h=0, col=col_earth, lwd=3)
# #   # abline(v=0, col=col_earth, lwd=3)
# #   # legend("bottomright",levels(col),col=seq_along(density),pch=1)
# #   # 
# #   # plot(volume, mass, col=density)
# #   # abline(h=0, col=col_earth, lwd=3)
# #   # abline(v=0, col=col_earth, lwd=3)
# #   # legend("bottomright",levels(col),col=seq_along(density),pch=1)
# # 
# #   plot(log10(planets.target.mutated$volume), log10(planets.target.mutated$mass), col=planets.target.mutated$density)
# #   abline(h=0, col=col_earth, lwd=3)
# #   abline(v=0, col=col_earth, lwd=3)
# #   legend("bottomright",levels(col),col=seq_along(density),pch=1)
# # }
# # )


# # Initial demarcation attempts
# 
# radius = planets.target.mutated$radius
# mass = planets.target.mutated$mass
# volume = planets.target.mutated$volume
# density = planets.target.mutated$density
# 
# col=factor(round(log10(density)))
# 
# plot(volume, mass, col=col)
# abline(h=0, col=col_earth, lwd=3)
# abline(v=0, col=col_earth, lwd=3)
# abline(0, 1/e_density, col=col_earth)
# abline(0, 1, col=col_water)
# abline(0, 13/e_density, col=col_earth_core)
# # legend("bottomright",levels(col),col=seq_along(density),pch=1)
# legend("bottomright",levels(col),col=seq_along(col),pch=1)
# 

    # # with(planets_mutated, plot(pl_volumee_log, pl_bmasse_log, col=col))
    # plot(planets_mutated$pl_volumee_log, planets_mutated$pl_bmasse_log, col=col)
    # # plot(log10(volume), log10(mass), col=col)
    # abline(h=0, col=col_earth, lwd=3)
    # abline(v=0, col=col_earth, lwd=3)
    # # legend("bottomright",levels(col),col=seq_along(density),pch=1)
    # legend("bottomright",levels(col),col=seq_along(col),pch=1)


scales <- c(2:10 %o% 10^(-3:5))
at.x <- log10(scales)
lab.x <- 10^at.x
lab.x <- ifelse(at.x %% 1 == 0, lab.x, NA)
at.y <- at.x
lab.y <- 10^at.y
lab.y <- ifelse(at.y %% 1 == 0, lab.y, NA)
at.x1 <- log10(scales^(1/3))
lab.x1 <- 10^at.x
lab.x1 <- ifelse(at.x %% 1 == 0, lab.x1, NA)
at.y1 <- log10(scales*j_mass)
lab.y1 <- 10^at.y
lab.y1 <- ifelse(at.y %% 1 == 0, lab.y, NA)

# planets_qry %>% count(disc_facility)
# target %>% count(disc_facility)
sample = target
sample_clean = sample %>% filter(pl_bmasseerr1!=0 & pl_bmasseerr2!=0 & pl_radeerr1!=0 & pl_radeerr2!=0)
col_by_class = function(class) c(radius_col[1], radius_col)[as.numeric(class)]
# disc_locale=="Space"
# sample = target[target$disc_facility=="Kepler",]
# sample = target[target$disc_facility=="K2",]
# sample = target[target$disc_facility=="TESS",]
# sample = target[str_detect(target$disc_facility, "Spitzer"),]
# sample = target[str_detect(target$disc_facility, "CoRoT"),]

# disc_locale=="Ground"
# sample = target[str_detect(target$disc_facility, "WASP"),]
# sample = target[str_detect(target$disc_facility, "Cerro Tololo"),]
# sample = target[str_detect(target$disc_facility, "Paranal"),]
# sample = target[str_detect(target$disc_facility, "WKeck"),]
# sample = target[str_detect(target$disc_facility, "La Silla"),]  # HARPS
# sample = target[str_detect(target$disc_facility, "Arecibo"),]
# sample = target[str_detect(target$disc_facility, "European Southern Observatory"),]



total = nrow(sample)

# name = sample$name
# lradius = log10(sample$radius)
# lmass = log10(sample$mass)
name = sample$pl_name
lradius = log10(sample$pl_rade)
lmass = log10(sample$pl_bmasse)
lradius1 = log10(sample$radius + sample$pl_radeerr1)
lmass1 = log10(sample$mass + sample$pl_bmasseerr1)
lradius2 = log10(sample$radius + sample$pl_radeerr2)
lmass2 = log10(sample$mass + sample$pl_bmasseerr2)
discoverymethod = factor(sample$discoverymethod)
# discoverymethod = factor(sample$radius_class)
# discoverymethod = factor(sample$mass_class)

## By mass and radius

par(mar = c(5.1, 4.1, 6.1, 4.1))
image(grid_mr, col = hm_col_scale, xlab = "mass", ylab = "radius", xaxt="n", yaxt="n")
col_arrow = paste0(errorbar_col, "80")
lmass_stops = log10(mass_stops)
lradius_stops = log10(radius_stops)
radius_col_transparent = paste0(radius_col, "20")
for (i in 1:(length(mass_stops)-1)) {
  rect(lmass_stops[i], lradius_stops[i], lmass_stops[i+1], lradius_stops[i+1], border = radius_col[i], lty = 3, lwd = 3)
  text((lmass_stops[i] + lmass_stops[i+1])/2, (lradius_stops[i] + lradius_stops[i+1])/2, class_labels_multiline[i], col=radius_col[i])
}
abline(v=lmass_stops, col=c(mass_col, mass_col[1]), lwd=1.5)
abline(h=lradius_stops, col=c(radius_col, radius_col[1]), lwd=1.5)
text(min(lmass), lradius_stops+.01, class_labels, col = radius_col, adj = c(0, 0), cex=.55)
text(lmass_stops+.02, min(lradius), class_labels_multiline, col = mass_col, adj = c(0, 0), cex=.55)
text(min(lmass), 0+.01, class_labels[1], col = radius_col[1], adj = c(0, 0), cex=.55)
text(0+.02, min(lradius), class_labels_multiline[1], col = mass_col[1], adj = c(0, 0), cex=.55)

contour(grid_mr, grid_mr, nlevels = 10, add = TRUE, col=contour_col)
axis(1, at=at.y, labels=lab.y, las=1)
axis(2, at=at.x, labels=lab.x, las=1)
axis(3, at=at.y1, labels=lab.y1, las=1)
axis(4, at=at.x1, labels=lab.x1, las=1)
mtext("mass (J)", side = 3, line = 2)
mtext("volume", side = 4, line = 2)
abline(h=0, col=col_earth, lwd=3)
abline(v=0, col=col_earth, lwd=3)

legend("bottomright",levels(col),col=seq_along(col), pch=1, label_value(label_value), title = "Density", bg = bg_legend, cex=.8)
legend("topleft",levels(discoverymethod), pch=seq_along(levels(discoverymethod)), title = "Method", bg = bg_legend, col="#808080ff", cex=.8)
title(paste0("Exoplanets Mass-Radius relation
             Those within ", distance, " parsecs highlighted in red (",
             total, ")"), line=3.5)



par(mar = c(5.1, 4.1, 6.1, 4.1))
image(grid, col = hm_col_scale, xlab = "radius", ylab = "mass", xaxt="n", yaxt="n")
col_arrow = paste0(errorbar_col, "80")
abline(v=log10(radius_stops), col=c(radius_col, radius_col[1]), lwd=1.5)
abline(h=log10(mass_stops), col=c(mass_col, mass_col[1]), lwd=1.5)
arrows(x0=lradius, y0=lmass2, x1=lradius, y1=lmass1, code=3, angle=90, length=0.005, col=col_arrow, lwd=errorbar_size)
arrows(x0=lradius2, y0=lmass, x1=lradius1, y1=lmass, code=3, angle=90, length=0.005, col=col_arrow, lwd=errorbar_size)
points(log10(radius), log10(mass), col = col, pch=as.numeric(discoverymethod))
points(lradius, lmass, col = "red", pch=8)
text(lradius, lmass, labels=name, cex=0.6, adj = c(0, 0))
points(solar_system$pl_rade_log, solar_system$pl_bmasse_log, col = "blue", pch=20)
text(solar_system$pl_rade_log, solar_system$pl_bmasse_log, labels=solar_system$pl_name, cex=0.6, adj = c(0, 0), col = "blue")
contour(grid, grid, nlevels = 10, add = TRUE, col=contour_col)
axis(1, at=at.x, labels=lab.x, las=1)
axis(2, at=at.y, labels=lab.y, las=1)
axis(3, at=at.x1, labels=lab.x1, las=1)
axis(4, at=at.y1, labels=lab.y1, las=1)
mtext("volume", side = 3, line = 2)
mtext("mass (J)", side = 4, line = 2)
abline(h=0, col=col_earth, lwd=3)
abline(v=0, col=col_earth, lwd=3)
curve(log10(density_water/density_earth*(10^(x))^3), add=TRUE, col=col_water)
curve(log10(density_earth/density_earth*(10^(x))^3), add=TRUE, col=col_earth, lty=2)
curve(log10(density_iron/density_earth*(10^(x))^3), add=TRUE, col=col_iron)
curve(log10(density_earth_core/density_earth*(10^(x))^3), add=TRUE, col=col_earth_core)
# curve(log10((density_water/density_earth*(10^(x)))^-2), add=TRUE, col=col_water)
# curve(log10((density_earth/density_earth*(10^(x)))^-2), add=TRUE, col=col_earth, lty=2)
# curve(log10((density_iron/density_earth*(10^(x)))^-2), add=TRUE, col=col_iron)
# curve(log10((density_earth_core/density_earth*(10^(x)))^-2), add=TRUE, col=col_earth_core)
curve(log10(density_water/density_earth*(10^(x))), add=TRUE, col=col_water)
curve(log10(density_earth/density_earth*(10^(x))), add=TRUE, col=col_earth, lty=2)
curve(log10(density_iron/density_earth*(10^(x))), add=TRUE, col=col_iron)
curve(log10(density_earth_core/density_earth*(10^(x))), add=TRUE, col=col_earth_core)
legend("bottomright",levels(col),col=seq_along(col), pch=1, label_value(label_value), title = "Density", bg = "#ffffff40", bty = "n")
legend("topleft",levels(discoverymethod), pch=seq_along(levels(discoverymethod)), title = "Method", bg = "#ffffff40", bty = "n", col="#808080ff")
title(paste0("Exoplanets
             Those within ", distance, " parsecs highlighted in red (",
            total, ")"), line=3.5)

par(mar = c(5.1, 4.1, 6.1, 4.1))
image(grid_mr, col = hm_col_scale, xlab = "mass", ylab = "radius", xaxt="n", yaxt="n")
col_arrow = paste0(errorbar_col, "80")
lmass_stops = log10(mass_stops)
lradius_stops = log10(radius_stops)
radius_col_transparent = paste0(radius_col, "20")
for (i in 1:(length(mass_stops)-1)) {
  rect(lmass_stops[i], lradius_stops[i], lmass_stops[i+1], lradius_stops[i+1], border = radius_col[i], lty = 3, lwd = 3)
  text((lmass_stops[i] + lmass_stops[i+1])/2, (lradius_stops[i] + lradius_stops[i+1])/2, class_labels_multiline[i], col=radius_col[i])
}
abline(v=lmass_stops, col=c(mass_col, mass_col[1]), lwd=1.5)
abline(h=lradius_stops, col=c(radius_col, radius_col[1]), lwd=1.5)
text(min(lmass), lradius_stops+.01, class_labels, col = radius_col, adj = c(0, 0), cex=.55)
text(lmass_stops+.02, min(lradius), class_labels_multiline, col = mass_col, adj = c(0, 0), cex=.55)
text(min(lmass), 0+.01, class_labels[1], col = radius_col[1], adj = c(0, 0), cex=.55)
text(0+.02, min(lradius), class_labels_multiline[1], col = mass_col[1], adj = c(0, 0), cex=.55)
arrows(x0=lmass, y0=lradius2, x1=lmass, y1=lradius1, code=3, angle=90, length=0.005, col=col_arrow, lwd=errorbar_size)
arrows(x0=lmass2, y0=lradius, x1=lmass1, y1=lradius, code=3, angle=90, length=0.005, col=col_arrow, lwd=errorbar_size)
points(log10(mass), log10(radius), col = col, pch=as.numeric(discoverymethod))
points(lmass, lradius, col = "red", pch=8)
text(lmass, lradius, labels=name, cex=0.6, adj = c(0, 0))
points(solar_system$pl_bmasse_log, solar_system$pl_rade_log, col = "blue", pch=20)
text(solar_system$pl_bmasse_log, solar_system$pl_rade_log, labels=solar_system$pl_name, cex=0.6, adj = c(0, 0), col = "blue")
contour(grid_mr, grid_mr, nlevels = 10, add = TRUE, col=contour_col)
axis(1, at=at.y, labels=lab.y, las=1)
axis(2, at=at.x, labels=lab.x, las=1)
axis(3, at=at.y1, labels=lab.y1, las=1)
axis(4, at=at.x1, labels=lab.x1, las=1)
mtext("mass (J)", side = 3, line = 2)
mtext("volume", side = 4, line = 2)
abline(h=0, col=col_earth, lwd=3)
abline(v=0, col=col_earth, lwd=3)
curve(log10((10^(x)*density_earth/density_water)^(1/3)), add=TRUE, col=col_water)
curve(log10((10^(x)*density_earth/density_earth)^(1/3)), add=TRUE, col=col_earth, lty=2)
curve(log10((10^(x)*density_earth/density_iron)^(1/3)), add=TRUE, col=col_iron)
curve(log10((10^(x)*density_earth/density_earth_core)^(1/3)), add=TRUE, col=col_earth_core)
angle_density=25
text(2.01, log10((10^(2.05)*density_earth/density_water)^(1/3)), "water planet density", col = col_water, srt=angle_density, adj = c(0, 0), cex=.6)
text(2.01, log10((10^(2.05)*density_earth/density_earth)^(1/3)), "earth density", col = col_earth, srt=angle_density, adj = c(0, 0), cex=.6)
text(2.01, log10((10^(2.05)*density_earth/density_iron)^(1/3)), "iron planet density", col = col_iron, srt=angle_density, adj = c(0, 0), cex=.6)
text(2.01, log10((10^(2.05)*density_earth/density_earth_core)^(1/3)), "iron core planet density", col = col_earth_core, srt=angle_density, adj = c(0, 0), cex=.6)
curve(log10(density_earth/density_water*10^(x)), add=TRUE, col=col_water)
curve(log10(density_earth/density_earth*10^(x)), add=TRUE, col=col_earth, lty=2)
curve(log10(density_earth/density_iron*10^(x)), add=TRUE, col=col_iron)
curve(log10(density_earth/density_earth_core*10^(x)), add=TRUE, col=col_earth_core)
angle_gravity=55
text(.98+log10(density_water/density_earth), 1.01, "water planet gravity", col = col_water, srt=angle_gravity, adj = c(0, 0), cex=.6)
text(.98+log10(density_earth/density_earth), 1.01, "earth gravity", col = col_earth, srt=angle_gravity, adj = c(0, 0), cex=.6)
text(.98+log10(density_iron/density_earth), 1.01, "iron planet gravity", col = col_iron, srt=angle_gravity, adj = c(0, 0), cex=.6)
text(.98+log10(density_earth_core/density_earth), 1.01, "iron core planet gravity", col = col_earth_core, srt=angle_gravity, adj = c(0, 0), cex=.6)
curve(x-.5, add=TRUE, col="red", lty=2)
text(1.48, 1.01, "H/He escape threshold (?)", col = "red", srt=55, adj = c(0, 0), cex=.6)
legend("bottomright",levels(col),col=seq_along(col), pch=1, label_value(label_value), title = "Density", bg = bg_legend, cex=.8)
legend("topleft",levels(discoverymethod), pch=seq_along(levels(discoverymethod)), title = "Method", bg = bg_legend, col="#808080ff", cex=.8)
title(paste0("Exoplanets Mass-Radius relation
             Those within ", distance, " parsecs highlighted in red (",
             total, ")"), line=3.5)

mr_relation1 = function(df=sample, df_control=planets_mutated, df_errorbars=df, log="xy", labels=FALSE) {
  par(mar = c(5.1, 4.1, 6.1, 4.1))
  with(df, {
    # density
    image(grid_mr, col = hm_col_scale, xlab = "mass", ylab = "radius", xaxt="n", yaxt="n")
    
    # prep
    col_arrow = paste0(errorbar_col, "80")
    pl_bmasse_log_stops = log10(mass_stops)
    pl_rade_log_stops = log10(radius_stops)
    radius_col_transparent = paste0(radius_col, "20")
    
    # classification
    for (i in 1:(length(mass_stops)-1)) {
      rect(pl_bmasse_log_stops[i], pl_rade_log_stops[i], pl_bmasse_log_stops[i+1], pl_rade_log_stops[i+1], border = radius_col[i], lty = 3, lwd = 3)
      text((pl_bmasse_log_stops[i] + pl_bmasse_log_stops[i+1])/2, (pl_rade_log_stops[i] + pl_rade_log_stops[i+1])/2, class_labels_multiline[i], col=radius_col[i])
    }
    abline(v=pl_bmasse_log_stops, col=c(mass_col, mass_col[1]), lwd=1.5)
    abline(h=pl_rade_log_stops, col=c(radius_col, radius_col[1]), lwd=1.5)
    text(min(pl_bmasse_log), pl_rade_log_stops+.01, class_labels, col = radius_col, adj = c(0, 0), cex=.7)
    text(pl_bmasse_log_stops+.02, min(pl_rade_log), class_labels_multiline, col = mass_col, adj = c(0, 0), cex=.7)
    text(min(pl_bmasse_log), 0+.01, class_labels[1], col = radius_col[1], adj = c(0, 0), cex=.7)
    text(0+.02, min(pl_rade_log), class_labels_multiline[1], col = mass_col[1], adj = c(0, 0), cex=.7)

    # error bars
    with(df_errorbars, arrows(x0=pl_bmasse_log, y0=pl_rade2_log, x1=pl_bmasse_log, y1=pl_rade1_log, code=3, angle=90, length=0.005, col=col_arrow, lwd=errorbar_size))
    with(df_errorbars, arrows(x0=pl_bmasse2_log, y0=pl_rade_log, x1=pl_bmasse1_log, y1=pl_rade_log, code=3, angle=90, length=0.005, col=col_arrow, lwd=errorbar_size))
    # arrows(x0=pl_bmasse_log, y0=pl_rade2_log, x1=pl_bmasse_log, y1=pl_rade1_log, code=3, angle=90, length=0.005, col=col_arrow, lwd=errorbar_size)
    # arrows(x0=pl_bmasse2_log, y0=pl_rade_log, x1=pl_bmasse1_log, y1=pl_rade_log, code=3, angle=90, length=0.005, col=col_arrow, lwd=errorbar_size)
    
    # background points
    with(df_control, points(pl_bmasse_log, pl_rade_log, col = col_by_class(pl_class), pch=as.numeric(discoverymethod)))
    # with(df_control, points(pl_bmasse_log, pl_rade_log, col = pl_dense_class, pch=as.numeric(discoverymethod)))
    # with(df_control, text(pl_bmasse_log, pl_rade_log, labels=(df_control$discoverymethod), cex=0.5, adj = c(0, 0)))
    
    # sample points
    points(pl_bmasse_log, pl_rade_log, col = "red", pch=1, cex=3)
    if (labels) with(df, text(pl_bmasse_log+.04, pl_rade_log,
                              labels=paste0(pl_name, " @", round(sy_dist, 1), " pc"), cex=0.6, adj = c(0, 0)))

    # solar system points
    points(solar_system$pl_bmasse_log, solar_system$pl_rade_log, col = "blue", pch=20)
    text(solar_system$pl_bmasse_log, solar_system$pl_rade_log, labels=solar_system$pl_name, cex=0.6, adj = c(0, 0), col = "blue")

    # contours
    contour(grid_mr, grid_mr, nlevels = 10, add = TRUE, col=contour_col)

    # axes
    axis(1, at=at.y, labels=lab.y, las=1)
    axis(2, at=at.x, labels=lab.x, las=1)
    axis(3, at=at.y1, labels=lab.y1, las=1)
    axis(4, at=at.x1, labels=lab.x1, las=1)
    mtext("mass (J)", side = 3, line = 2)
    mtext("volume", side = 4, line = 2)
    
    # curves
    abline(h=0, col=col_earth, lwd=3)
    abline(v=0, col=col_earth, lwd=3)
    curve(log10((10^(x)*density_earth/density_water)^(1/3)), add=TRUE, col=col_water)
    curve(log10((10^(x)*density_earth/density_earth)^(1/3)), add=TRUE, col=col_earth, lty=2)
    curve(log10((10^(x)*density_earth/density_iron)^(1/3)), add=TRUE, col=col_iron)
    curve(log10((10^(x)*density_earth/density_earth_core)^(1/3)), add=TRUE, col=col_earth_core)
    angle_density=25
    x_pos = mean(pl_bmasse_log) + sd(pl_bmasse_log)
    text(x_pos+.01, log10((10^(x_pos+.05)*density_earth/density_water)^(1/3)), "water planet density", col = col_water, srt=angle_density, adj = c(0, 0), cex=.7)
    text(x_pos+.01, log10((10^(x_pos+.05)*density_earth/density_earth)^(1/3)), "earth density", col = col_earth, srt=angle_density, adj = c(0, 0), cex=.7)
    text(x_pos+.01, log10((10^(x_pos+.05)*density_earth/density_iron)^(1/3)), "iron planet density", col = col_iron, srt=angle_density, adj = c(0, 0), cex=.7)
    text(x_pos+.01, log10((10^(x_pos+.05)*density_earth/density_earth_core)^(1/3)), "iron core planet density", col = col_earth_core, srt=angle_density, adj = c(0, 0), cex=.7)
    curve(log10(density_earth/density_water*10^(x)), add=TRUE, col=col_water)
    curve(log10(density_earth/density_earth*10^(x)), add=TRUE, col=col_earth, lty=2)
    curve(log10(density_earth/density_iron*10^(x)), add=TRUE, col=col_iron)
    curve(log10(density_earth/density_earth_core*10^(x)), add=TRUE, col=col_earth_core)
    angle_gravity=55
    y_pos = mean(pl_rade_log) + sd(pl_rade_log)
    text(y_pos-.02+log10(density_water/density_earth), y_pos+.01, "water planet gravity", col = col_water, srt=angle_gravity, adj = c(0, 0), cex=.7)
    text(y_pos-.02+log10(density_earth/density_earth), y_pos+.01, "earth gravity", col = col_earth, srt=angle_gravity, adj = c(0, 0), cex=.7)
    text(y_pos-.02+log10(density_iron/density_earth), y_pos+.01, "iron planet gravity", col = col_iron, srt=angle_gravity, adj = c(0, 0), cex=.7)
    text(y_pos-.02+log10(density_earth_core/density_earth), y_pos+.01, "iron core planet gravity", col = col_earth_core, srt=angle_gravity, adj = c(0, 0), cex=.7)
    curve(x-.5, add=TRUE, col="red", lty=2)
    text(y_pos-.02+.5, y_pos+.01, "H/He escape threshold (?)", col = "red", srt=55, adj = c(0, 0), cex=.7)

    # legends
    with(df_control, legend("bottomright",levels(factor(discoverymethod)), pch=seq_along(discoverymethod), title = "Method", bg = bg_legend, col="#808080ff", cex=.7, ncol=1, x.intersp = 1, y.intersp = .5, bty = "o"))
    legend("bottomright",levels(col),col=seq_along(col), pch=15, label_value(label_value), title = "Density", bg = bg_legend, cex=.7, ncol=1, bty = "n")
    # legend("topleft",levels(factor(discoverymethod)), pch=seq_along(discoverymethod), title = "Method", bg = bg_legend, col="#808080ff", cex=.7)

    # titles
    title(paste0("Mass-Radius relation (", nrow(df_control), " exoplanets)"), line=4.5)
    title(paste0("Those within ", distance, " parsecs (", round(distance*3.26156), " ly) shown with error bars (", nrow(df_errorbars), "). Best ones circled in red and labeled (", nrow(df), ")."), line=3.3)
  })
}

mr_relation1(sample_clean, df_errorbars = sample, labels = T)
# mr_relation1(sample_clean, labels = T)
# mr_relation1(planets_mutated, log = "x")



# # col=factor(round(log10(density)))
# # col=factor(round(log10(radius / mass)))
# # 
# # coef=lm(radius ~ mass)$coef
# # a = log10(coef[1])
# # b = coef[2]
# # 
# # plot(log10(mass), radius, col=col)
# # abline(h=1, col=col_earth, lwd=3)
# # abline(v=0, col=col_earth, lwd=3)
# # curve(a + 1*b*10^x, add=TRUE, col=col_water)
# # curve(a + e_density*b*10^x, add=TRUE, col=col_earth, lty=2)
# # curve(a + 8*b*10^x, add=TRUE, col=col_iron)
# # curve(a + 13*b*10^x, add=TRUE, col=col_earth_core)
# # 
# # curve(((log10(coef[1]) + ((coef[2])*10^(x)))), add=TRUE)
# # for (i in seq(1, 500, 10))
# #   curve(((log10(coef[1]) + i*((coef[2])*10^(x)))), add=TRUE)
# # 
# # # abline(coef)
# # # abline(reg=lm(radius ~ log10(mass)))
# # # abline(coef[1], coef[2])
# # # legend("bottomright",levels(col),col=seq_along(density),pch=1)
# # legend("bottomright",levels(col),col=seq_along(col),pch=1)
# # 
# # 
# 
# 


# # log log attempts
# 
# mass = planets$pl_bmasse
# radius = planets$pl_rade
# # volume = 4/3*pi*radius^3  # / 4/3*pi*e_radius^3  # = radius^3
# volume = radius^3
# # gravity = G*M/radius^2
# gravity = 1/radius^2
# 
# ## Histograms by density.
# 
# (e_density=5.51)
# 
# density = planets$pl_dens
# 
# col=factor(round(log_e_density))
# levels(col)
# label_value = 10^as.numeric(levels(col))
# 
# 
# at.x <- outer(1:9, 10^(-2:5))
# lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
# at.y <- outer(1:9, 10^(-2:5))
# lab.y <- ifelse(log10(at.y) %% 1 == 0, at.y, NA)
# at.x1 <- outer(1:9, 10^((-2:5)))^(1/3)
# lab.x1 <- ifelse(row(at.x1) == 1, round(at.x1^3,1), NA)
# 
# # coef=lm(mass ~ radius)$coef
# # a = log10(coef[1])
# # b = log10(coef[2])
# # a = coef[1]
# # b = coef[2]
# 
# lradius = log10(radius)
# lmass = log10(mass)
# 
# par(mar = c(5.1, 4.1, 4.1, 2.1))
# # par(mar = c(5.1, 4.1, 6.1, 2.1))
# plot(lradius, lmass, col=col, log="xy", xlim = c(0.5001,2), ylim = c(0.5001,2))
# # plot(lradius, lmass, col=col, log="xy", xaxt="n", yaxt="n")
# text(lradius, lmass, planets.target.mutated$pl_name, cex=0.5, pos=4)
# # axis(1, at=at.x, labels=lab.x, las=1)
# # axis(2, at=at.y, labels=lab.y, las=1)
# # axis(3, at=at.x1, labels=lab.x1, las=1)
# # mtext("volume", side = 3, line = 2)
# abline(h=0.005, col=grid_col, lwd=3)
# abline(v=0.005, col=grid_col, lwd=3)
# abline(h=0.5, col=grid_col, lwd=3)
# abline(v=0.5, col=grid_col, lwd=3)
# abline(h=1, col=grid_col, lwd=3)
# abline(v=1, col=grid_col, lwd=3)
# curve(density_water/density_earth*x^3, add=TRUE, col=col_water)
# curve(density_earth/density_earth*x^3, add=TRUE, col=col_earth, lty=2)
# curve(density_iron/density_earth*x^3, add=TRUE, col=col_iron)
# curve(density_earth_core/density_earth*x^3, add=TRUE, col=col_earth_core)
# 
# curve((density_water/density_earth*x)^-2, add=TRUE, col=col_water)
# curve((density_earth/density_earth*x)^-2, add=TRUE, col=col_earth, lty=2)
# curve((density_iron/density_earth*x)^-2, add=TRUE, col=col_iron)
# curve((density_earth_core/density_earth*x)^-2, add=TRUE, col=col_earth_core)
# legend("topleft",levels(col),col=seq_along(col),pch=1, label_value(label_value), title = "Density")
# title("Density", line=4)
# 

# Load data
# planets_train = planets_good
planets_train = planets_good %>% na.omit()
planets_test = planets_good

# Estimate parameters from the data
params <- planets_train %>%
  group_by(pl_class) %>%
  summarize(avg_1 = mean(pl_bmasse_log), avg_2 = mean(pl_rade_log),
            sd_1 = sd(pl_bmasse_log), sd_2 = sd(pl_rade_log),
            r = cor(pl_bmasse_log, pl_rade_log))

# Contour plots
png(filename='plots/contour_plots.png')
planets_train %>%
  ggplot(aes(pl_bmasse_log, pl_rade_log, fill = pl_class, color = pl_class)) +
  scale_color_manual(values=c(radius_col[1], radius_col)) +
    geom_point(show.legend = FALSE, col=col_by_class(planets_train$pl_class)) +
  stat_ellipse(type="norm", lwd = 1.5)
dev.off()

# models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
models <- c("lda", "svmLinear", "knn", "multinom", "rf")
getModelInfo("lda")$rrlda$tags
getModelInfo("lda")$slda$label

# Fit model
# train_qda <- train(pl_class ~ pl_bmasse_log + pl_rade_log + (1|pl_rade_log), method = "qda", data = planets_train)
fit <- train(droplevels(pl_class) ~ pl_bmasse_log + pl_rade_log, method = "multinom", data = planets_train)
# Obtain predictors and accuracy
y_hat <- predict(fit, planets_test)
confusionMatrix(data = y_hat, reference = planets_test$pl_class)$overall["Accuracy"]



# # Fit model
# # train_qda <- train(pl_class ~ pl_bmasse_log + pl_rade_log + (1|pl_rade_log), method = "qda", data = planets_train)
# train_qda <- train(pl_class ~ pl_bmasse_log + pl_rade_log, method = "qda", data = planets_train)
# # train_qda <- train(pl_class ~ ., method = "qda", data = planets_train)
# # Obtain predictors and accuracy
# y_hat <- predict(train_qda, planets_test)
# confusionMatrix(data = y_hat, reference = planets_test$pl_class)$overall["Accuracy"]
# 
# 



# ---------------
# Other model testing routines
library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  info = getModelInfo(model)
  print(paste0(model, ":: ", info$rrlda$tags, ": ", info$slda$label))
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

length(mnist_27$test$y)
length(models)

matrix <- sapply(fits, function(m){predict(m, newdata = mnist_27$test, type = "raw")})
matrix
i <- 0
accuracy <- sapply(fits, function(m){
  i <<- i+1
  confusionMatrix(matrix[,i] %>% factor(), mnist_27$test$y)$overall["Accuracy"]
})
accuracy
mean(accuracy)

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)


acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)


library(Biobase)
rm <- rowMedians(matrix(as.integer(as.character(matrix)),nrow(matrix), ncol(matrix)))
confusionMatrix(factor(ifelse(rm >= 4.5, "7", "2")), mnist_27$test$y)$overall["Accuracy"]

votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)


ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

min_acc <- sapply(1:length(models), function(i){min(fits[[i]]$results$Accuracy)})
min_acc
mean(min_acc)



# votes <- rowMeans(matrix[min_acc > 0.8,] == "7")
votes <- rowMeans(matrix[min_acc > 0.7,] == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)
