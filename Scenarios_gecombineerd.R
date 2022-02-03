

library(ggplot2)
library(readr)
library(dplyr)
library(sf)
library(shiny)
library(ggthemes)
library(scales)
library(biscale)
library(cowplot)
library(extrafont)


## Inlezen file 
kaart <- st_read("C:/Users/Bazenmm/Downloads/scenarios/data/basegridPZH500.gpkg")

scenarios <- read_csv("R:/ESRI/DATA/RUIMTELIJKE ONTWIKKELING/PROJECTEN/ONTWERPEND ONDERZOEK/THEMA/RUIMTE_2020_VERSCHUIVINGEN IN DE RUIMTEVRAAG EN -GEBRUIK/GIS/Oplevering bestanden VDM/scenarios.csv")

## Data combineren 
data_kaart <- left_join(kaart, scenarios, by = "c28992r500")

## Data plotten
ggplot(colour = 'white') + 
  ggtitle("Transformatiepotentie") +
  #   geom_sf(data = results() , aes(fill = VijfMinuten)) +
  geom_sf(data = data_kaart , aes(fill = WandelStad)) +
  coord_sf(datum = NA) +
  binned_scale(            "fill",
                           "foo",
                           ggplot2:::binned_pal(scales::manual_pal(c("#d7191b","#fdae61","#ffffbf", "#acdda5", "#2c83b9"))),
                           guide="coloursteps",
                           breaks=c(15, 30,45,60),
                           limits=c(0,100),
                           show.limits=TRUE)


###----------------------------------------------------------------------------------------

## Data bewerken
Scenarios_WandelHoogCompact <- data_kaart %>% 
  mutate(WandelStad01 = ifelse(WandelStad > 60, 1, 0)) %>% 
  mutate(HoogDroog01 =  ifelse(HoogDroog > 60, 1, 0))  %>%
  mutate(CompacteStad01 = ifelse(CompacteStad > 60, 1,0)) %>% 
  mutate(LivingLandscape01 = ifelse(LivingLandscape > 60,1,0)) %>% 
  mutate(WandelHoogCompact = WandelStad01+HoogDroog01+CompacteStad01) %>%
  mutate(WandelHoog = WandelStad01 +HoogDroog01) %>% 
  mutate(WandelCompact = WandelStad01+CompacteStad01 ) %>% 
  mutate(HoogCompact = HoogDroog01+CompacteStad01  )


####-----------------------------------------------------------------------------------------------------------------------------------------------------------

# Make a bivariate map dimentions = 3 
# create classes
data <- bi_class(data_kaart, x = WandelStad, y = HoogDroog, style = "quantile", dim = 3)

# create map
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Wandelstad & HoogDroog"
  ) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "> WandelStad ",
                    ylab = "> Hoogdroog",
                    size = 8)


# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, .65, 0.2, 0.2)



#####-------------------------------------------------------------------------------------------------------
### Bivariate card 01  WandelStad & HoogDroog
data01 <- bi_class(Scenarios_WandelHoogCompact, x = WandelStad01, y = HoogDroog01, style = "quantile", dim = 2)

map01 <- ggplot() +
  geom_sf(data = data01, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 2) +
  labs(
    title = "Wandelstad & HoogDroog"
  ) +
  bi_theme()

legend01 <- bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = " WandelStad > 60% ",
                      ylab = " HoogDroog > 60% ",
                      size = 10)

finalPlot01 <- ggdraw() +
  draw_plot(map01, 0, 0, 1, 1) +
  draw_plot(legend01, 0.2, .65, 0.2, 0.2)


#####---------------------------------------------------------------------------------------------------------
### Bivariate map 01    CompacteStad & HoogDroog  
dataCH01 <- bi_class(Scenarios_WandelHoogCompact, x = CompacteStad01, y = HoogDroog01, style = "quantile", dim = 2)

mapCH01 <- ggplot() +
  geom_sf(data = dataCH01, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 2) +
  labs(
    title = "CompacteStad & HoogDroog"
    # subtitle = "Dark Blue (DkBlue) Palette"
  ) +
  bi_theme()

legendCH01 <- bi_legend(pal = "GrPink",
                        dim = 2,
                        xlab = " CompacteStad > 60% ",
                        ylab = " HoogDroog > 60% ",
                        size = 10)

finalPlotCH01 <- ggdraw() +
  draw_plot(mapCH01, 0, 0, 1, 1) +
  draw_plot(legendCH01, 0.2, .65, 0.2, 0.2)


####-------------------------------------------------

## 
loadfonts(quiet = T)

fonts()



### Bivariate map 01 Wandelstad & CompacteStad  
dataWC01 <- bi_class(Scenarios_WandelHoogCompact, x = WandelStad01, y = CompacteStad01, style = "quantile", dim = 2)

mapWC01 <- ggplot() +
  geom_sf(data = dataWC01, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 2) +
  labs(
    title = "Wandelstad & CompacteStad"
    # subtitle = "Dark Blue (DkBlue) Palette"
  ) +
  bi_theme(base_family = "Calibri")

legendWC01 <- bi_legend(pal = "GrPink",
                        dim = 2,
                        xlab = "WandelStad > 60%",
                        ylab = "CompacteStad > 60%",
                        size = 10)

finalPlotWC01 <- ggdraw() +
  draw_plot(mapWC01, 0, 0, 1, 1) +
  draw_plot(legendWC01, 0.2, .65, 0.2, 0.2)

## Data combineren 
ggplot(data = Scenarios_WandelHoogCompact ,colour = 'white') + 
  ggtitle("Transformatiepotentie Wandelstad & Hoog Droog") +
  #   geom_sf(data = results() , aes(fill = VijfMinuten)) +
  geom_sf( aes(fill = as.factor(WandelHoog))) +
  coord_sf(datum = NA)  +
  scale_fill_manual(values=c("lightgrey","darkgrey","#238A8DFF", "#55C667FF", "#FDE725FF"), 
                    breaks=c(0,1,2,3,4))

## Data combineren 
ggplot(data = Scenarios_WandelHoogCompact ,colour = 'white') + 
  ggtitle("Transformatiepotentie Wandelstad & Compacte Stad") +
  #   geom_sf(data = results() , aes(fill = VijfMinuten)) +
  geom_sf( aes(fill = as.factor(WandelCompact))) +
  coord_sf(datum = NA)  +
  scale_fill_manual(values=c("lightgrey","darkgrey","#238A8DFF", "#55C667FF", "#FDE725FF"), 
                    breaks=c(0,1,2,3,4))


ggplot(data = Scenarios_WandelHoogCompact ,colour = 'white') + 
  ggtitle("Transformatiepotentie Compacte Stad  & HoogDroog") +
  #   geom_sf(data = results() , aes(fill = VijfMinuten)) +
  geom_sf( aes(fill = as.factor(HoogCompact))) +
  coord_sf(datum = NA)  +
  scale_fill_manual(values=c("lightgrey","darkgrey","#238A8DFF", "#55C667FF", "#FDE725FF"), 
                    breaks=c(0,1,2,3,4))




## Data combineren 
ggplot(data = Scenarios_WandelHoogCompact ,colour = 'white') + 
  ggtitle("Transformatiepotentie WandelStad, HoogDroog & CompacteStad") +
  #   geom_sf(data = results() , aes(fill = VijfMinuten)) +
  geom_sf( aes(fill = as.factor(WandelHoogCompact))) +
  coord_sf(datum = NA)  +
  scale_fill_manual(values=c("lightgrey","darkgrey","#238A8DFF", "#55C667FF", "#FDE725FF"), 
                    breaks=c(0,1,2,3,4))

## tmap package doet het niet 
tm_shape(Scenarios_WandelHoogCompact) + 
  tm_polygons("Wandelstad01", palette = "YlOrBr", n = 6, 
              legend.hist = TRUE, title = "% no school") + 
  tm_legend(outside = TRUE, hist.width = 2) 

### kaart maken met carthography package 
plot(st_geometry(Scenarios_WandelHoogCompact))
propTrianglesLayer(x=Scenarios_WandelHoogCompact, var1 = "CompacteStad01", var2 = "HoogDroog01", k = 0.0005)

####-----------------------------------------------------------------

## data wegschrijven als shapefile 
st_write(Scenarios_WandelHoogCompact, paste0("R:/ESRI/DATA/RUIMTELIJKE ONTWIKKELING/PROJECTEN/ONTWERPEND ONDERZOEK/THEMA/RUIMTE_2020_VERSCHUIVINGEN IN DE RUIMTEVRAAG EN -GEBRUIK/FME-R/scenarios/Scenarios_WandelHoogCompact.shp"), delete_layer = TRUE)
## en als gpkg 
st_write(Scenarios_WandelHoogCompact, paste0("R:/ESRI/DATA/RUIMTELIJKE ONTWIKKELING/PROJECTEN/ONTWERPEND ONDERZOEK/THEMA/RUIMTE_2020_VERSCHUIVINGEN IN DE RUIMTEVRAAG EN -GEBRUIK/FME-R/scenarios/Scenarios_WandelHoogCompact.gpkg"), delete_layer = TRUE)


## kaart weer inlezen 
## .sph
shape <- st_read("R:/ESRI/DATA/RUIMTELIJKE ONTWIKKELING/PROJECTEN/ONTWERPEND ONDERZOEK/THEMA/RUIMTE_2020_VERSCHUIVINGEN IN DE RUIMTEVRAAG EN -GEBRUIK/FME-R/scenarios/Scenarios_WandelHoogCompact.shp")

## .gpkg
gpkg <- st_read("R:/ESRI/DATA/RUIMTELIJKE ONTWIKKELING/PROJECTEN/ONTWERPEND ONDERZOEK/THEMA/RUIMTE_2020_VERSCHUIVINGEN IN DE RUIMTEVRAAG EN -GEBRUIK/FME-R/scenarios/Scenarios_WandelHoogCompact.gpkg")


plot(gpkg$WandelHoog)

## Kaart maken 
ggplot(colour = 'white') + 
  ggtitle("Transformatiepotentie") +
  #   geom_sf(data = results() , aes(fill = VijfMinuten)) +
  geom_sf(data = gpkg , aes(fill = as.factor(WandelHoog))) +
  coord_sf(datum = NA)  +
  scale_fill_manual(values=c("lightgrey","darkgrey","#238A8DFF", "#55C667FF", "#FDE725FF"), 
                    breaks=c(0,1,2,3,4))




cd Test
ls
head README.md
git remote show origin