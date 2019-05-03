# Existeix l'aplicació web mapbox

#install.packages('maptools')
#>> sudo apt install libgdal-dev
#install.packages('rgdal')

# https://www.arcgis.com/home/item.html?id=83d81d9336c745fd839465beab885ab7

library(dplyr)
library(rgdal)
library(maptools)
library(broom)
library(ggplot2)

m.prov = rgdal::readOGR('provincies_espanya/Provincias_ETRS89_30N.shp', GDAL1_integer64_policy = TRUE)

prov_info = m.prov %>%
  as_tibble() %>%
  mutate(
    id = as.character(1:n() - 1)
  )

df.prov = m.prov %>%
  tidy() %>%
  tbl_df() %>%
  left_join(prov_info, by = 'id')

library(readxl)
dades = read_xlsx('provincies_espanya/grafic_espanya.xlsx', col_names = c('prov', 'y2016', 'y2002'), skip = 2)

PROV_MAPA = unique(as.character(df.prov$Texto))
PROV_EXCEL = dades$prov

match.name = sapply(PROV_EXCEL, function(nm) which.min(adist(nm, PROV_MAPA, fixed = FALSE)))
rel_nom = tibble(
  prov = PROV_EXCEL,
  Texto = PROV_MAPA[match.name]
)

dplot = df.prov %>%
  left_join(rel_nom, by = 'Texto') %>%
  left_join(dades, by = 'prov') %>%
  mutate(
    lat = if_else(prov %in% c("Santa Cruz de Tenerife", "Palmas, Las"), lat + 600000, lat),
    long = if_else(prov %in% c("Santa Cruz de Tenerife", "Palmas, Las"), long + 500000, long))

RECT = apply(dplot[dplot$prov %in% c("Santa Cruz de Tenerife", "Palmas, Las"), c('lat', 'long')], 2, range)

ggplot() +
  geom_polygon(data = dplot, aes(x = long, y = lat, group = group, fill = y2016), col = 'white', size = 0.2) +
  geom_segment(aes(x = RECT[1,'long'], xend = RECT[2,'long']+ 50000, y = RECT[2,'lat']+50000, yend = RECT[2,'lat']+50000), col = 'grey') +
  geom_segment(aes(x = RECT[2,'long']+ 50000, xend = RECT[2,'long']+ 50000, y = RECT[2,'lat']+50000, yend = RECT[1,'lat']), col = 'grey') +
  theme_void() + labs(fill = '2006')
  coord_equal()
