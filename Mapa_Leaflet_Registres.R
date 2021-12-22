### Mapa registres iNaturalist online amb leaflet

#Preparem un pop-up per al mapa amb els registres per a poder veure informació de cadascun.
#Podrem afegir també la imatge introduint la localització dins del dataset del url. Es troba dins de photos --> small_url, on photos és una variable de tipus list dins del dataframe:
inat_obs_sf <-  actpnrf %>% # cal introduir el dataset amb les fotos (generat al final de l'script 1)
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)

barcelones <- st_read('shapefiles_catalunya_comarcas/shapefiles_catalunya_comarcas.shp')
inat_obs_pcsp_sf  <- inat_obs_sf %>% st_intersection(barcelones)

inat_obs_pcsp_popup_sf <- inat_obs_pcsp_sf %>% 
  mutate(popup_html = paste0("<p><b><i>", taxon.name, "</b><br/></i>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", photos[[1]]$small_url, "' style='width:100%;'/></p>"))



# Creem el mapa online amb leaflet. Només es visualitza correctament en R:
leaflet(barcelones) %>% 
  addProviderTiles("Esri.WorldStreetMap") %>% 
  addPolygons() %>% 
  addCircleMarkers(data = inat_obs_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 1)





