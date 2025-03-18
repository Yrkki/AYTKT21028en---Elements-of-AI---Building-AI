# install.packages("exoplanets")

# install.packages("devtools")
# devtools::install_github("ropensci/exoplanets")

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


library(tidyverse)
library(rafalib)
library(ggrepel)

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
(planets = exoplanets("PSCompPars"))
str(planets)
class(planets)
nrow(planets)


(cols = colnames(planets))
(colslength = length(cols))

mass = planets$pl_bmasse
radius = planets$pl_rade


## Histograms by density.

(density_e=5.51)

density = planets$pl_dens
range(density %>% na.omit())
log_density = log10(planets$pl_dens[planets$pl_dens>0])
log_density_e = log10(planets$pl_dens[planets$pl_dens>0] / density_e)
hist(log_density)
hist(log_density_e)



## Histograms by size.

grid_col = "#4040a060"

log.radius = log10(radius %>% na.omit())
(log.radius.range = range(log.radius))

# breaks = log10(c(.1, 1.25, 2, 6, 15, 100))
breaks = log10(c(10^log.radius.range[1], 1.25, 2, 6, 15, 10^log.radius.range[2]))
col = c("#0060a0", "#20c000", "#00a0e0", "#c0a060", "#c00000")
labels = c("Earth-size", "Super-Earth-size", "Neptune-size", "Jupiter-size", "Larger")

hist(log.radius, breaks = breaks, col = col, labels = T,
     freq = FALSE) +
  abline(0, 1e10, col=grid_col) +
  legend("topright",levels(col),col=seq_along(col),pch=1)

hist(log.radius, breaks = breaks, col = col, labels = T,
     freq = TRUE) +
  abline(0, 1e10, col=grid_col)

data.frame(log.radius = log.radius) %>%
  ggplot(aes(log.radius)) +
  geom_histogram(col="black", binwidth = .1) +
  geom_vline(xintercept = 0, col=grid_col)


## Density

# col=factor(round(log_density_e*2)/2)
col=factor(round(log_density_e))
levels(col)
plot(log10(radius), log10(mass), col=col)
abline(h=0, col=grid_col)
abline(v=0, col=grid_col)
legend("bottomright",levels(col),col=seq_along(col),pch=1)

errorbar_size = 1e-1
errorbar_col = "#404040"
errorbar_alpha = .2
# planets %>%
#   ggplot(aes(radius, mass, col=col, size=2*radius)) +
#   labs(col="density") +
#   scale_x_log10() +
#   scale_y_log10() +
#   geom_hline(yintercept = 1, col=grid_col) +
#   geom_vline(xintercept = 1, col=grid_col) +
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
planets.clean = planets.all %>%
  select(pl_name, pl_bmasse, pl_rade, pl_dens,
         pl_radeerr1=0, pl_radeerr2=0, pl_bmasseerr1=0, pl_bmasseerr2=0,
         discoverymethod, disc_facility, pl_controv_flag) %>%
  na.omit()
nrow(planets.clean)
planets.good = planets.all %>%
  select(pl_name, pl_bmasse, pl_rade, pl_dens,
         pl_radeerr1, pl_radeerr2, pl_bmasseerr1, pl_bmasseerr2,
         discoverymethod, disc_facility, pl_controv_flag) %>%
  na.omit()
nrow(planets.good)

# planets.target = planets.all
planets.target = planets.good
nrow(planets.target)
planets.target.mutated = planets.target %>%
  mutate(name = pl_name,
         mass = pl_bmasse,
         radius = pl_rade,
         volume = 4/3*pi*radius^3,
         # density = log10(.$pl_dens / density_e),
         density = .$pl_dens / density_e,
         diameter = 2*radius,
         controversial = pl_controv_flag,
         telescope = disc_facility
         )
planets.target.mutated %>%
  ggplot(aes(radius, mass, col=factor(round(density)), size=diameter, shape=discoverymethod)) +
  labs(col="density") +
  # ggplot(aes(radius, mass, col=factor(telescope), size=diameter, shape=discoverymethod)) +
  # labs(title = "Expolanets", col = "telescope") +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides="trbl") +
  geom_hline(yintercept = 1, col=grid_col) +
  geom_vline(xintercept = 1, col=grid_col) +
  # geom_abline(slope = density_e, intercept = 0, col=grid_col) +
  # geom_errorbar(aes(x=radius,
  #                   ymin=mass + pl_bmasseerr2,
  #                   ymax=mass + pl_bmasseerr1),
  #               size=errorbar_size, col=errorbar_col, alpha=errorbar_alpha) +
  # geom_errorbar(aes(y=mass,
  #                   xmin=radius + pl_radeerr2,
  #                   xmax=radius + pl_radeerr1),
  #               size=errorbar_size, col=errorbar_col, alpha=errorbar_alpha) +
  geom_point(alpha=.4) +
  geom_text_repel(aes(radius, mass, label=name), max.overlaps = 15) +
  # geom_text_repel(aes(radius, mass, label=controversial), max.overlaps = 30) +
  # geom_text(nudge_x=0.1, cex = 2) +
  # geom_label(aes(label=planets.target$pl_name, alpha=.4)) +
  # scale_shape_manual(values=seq(65, 65 + length(levels(factor(planets.target$discoverymethod)))))
  scale_shape_manual(values=c(15, 4, 16, 3, 17)) +
  ggtitle("Exoplanets")

# reverse
# https://exoplanetarchive.ipac.caltech.edu/exoplanetplots/exo_massradius.png
planets.target.mutated %>%
  filter(.3<=mass & mass<=40) %>%
  filter(0<=radius & radius<=10) %>%
  ggplot(aes(mass, radius, col=factor(round(density)), size=diameter, shape=discoverymethod)) +
  labs(col="density") +
  theme_light() +
  scale_x_log10(limits= c(.3, 40)) +
  # geom_line(data = data.frame(x = seq(min(planets.target.mutated$mass), max(planets.target.mutated$mass), length.out = 100)),
  #           aes(y = x)) + 
  # scale_y_continuous(limits= c(.3, 40), trans = 'log10') +
  # scale_y_log10() +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  annotation_logticks(sides="trbl") +
  geom_hline(yintercept = 1, col=grid_col) +
  geom_vline(xintercept = 1, col=grid_col) +
  geom_point(alpha=.4) +
  geom_text_repel(aes(mass, radius, label=name), max.overlaps = 5) +
  scale_shape_manual(values=c(16, 3, 17)) +
  ggtitle("Exoplanets")



# continuous
# https://exoplanetarchive.ipac.caltech.edu/exoplanetplots/exo_massradius.png
planets.target.mutated %>%
  ggplot(aes(radius, mass, col=factor(round(density)), size=diameter, shape=discoverymethod)) +
  labs(col="density") +
  theme_light() +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides="trbl") +
  geom_hline(yintercept = 1, col=grid_col) +
  geom_vline(xintercept = 1, col=grid_col) +
  geom_point(alpha=.4) +
  geom_text_repel(aes(radius, mass, label=name), max.overlaps = 5) +
  scale_shape_manual(values=c(15, 4, 16, 3, 17)) +
  ggtitle("Exoplanets")

# by volume
planets.target.mutated %>%
  ggplot(aes(volume, mass, col=factor(round(density)), size=diameter, shape=discoverymethod)) +
  labs(col="density") +
  theme_light() +
  scale_x_continuous() +
  scale_y_continuous() +
  annotation_logticks(sides="trbl") +
  geom_hline(yintercept = 1, col=grid_col) +
  geom_vline(xintercept = 1, col=grid_col) +
  geom_abline(slope = density_e, intercept = 0, col=grid_col) +
  geom_point(alpha=.4) +
  geom_text_repel(aes(volume, mass, label=name), max.overlaps = 5) +
  scale_shape_manual(values=c(15, 4, 16, 3, 17)) +
  ggtitle("Exoplanets")

density = planets.target.mutated$density
plot(radius, mass, col=density)
abline(h=0, col=grid_col)
abline(v=0, col=grid_col)
legend("bottomright",levels(col),col=seq_along(col),pch=1)

with(planets.target.mutated, {
  # plot(radius, mass, col=density)
  # abline(h=0, col=grid_col)
  # abline(v=0, col=grid_col)
  # legend("bottomright",levels(col),col=seq_along(density),pch=1)
  # 
  # plot(volume, mass, col=density)
  # abline(h=0, col=grid_col)
  # abline(v=0, col=grid_col)
  # legend("bottomright",levels(col),col=seq_along(density),pch=1)

  plot(log10(planets.target.mutated$volume), log10(planets.target.mutated$mass), col=planets.target.mutated$density)
  abline(h=0, col=grid_col)
  abline(v=0, col=grid_col)
  legend("bottomright",levels(col),col=seq_along(density),pch=1)
}
)

plot(log10(planets.target.mutated$radius), log10(planets.target.mutated$mass), col=planets.target.mutated$density)
abline(h=0, col=grid_col)
abline(v=0, col=grid_col)
legend("bottomright",levels(col),col=seq_along(density),pch=1)

plot(log10(planets.target.mutated$volume), log10(planets.target.mutated$mass), col=planets.target.mutated$density)
abline(h=0, col=grid_col)
abline(v=0, col=grid_col)
legend("bottomright",levels(col),col=seq_along(density),pch=1)
