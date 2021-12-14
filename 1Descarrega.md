1.Descarrega
================
Erola Fenollosa
13/12/2021

## Introducció i descarrega de paquets

L’objectiu d’aquest codi és la descàrrega de les dades de iNaturalist i
el filtratge i neteja de les mateixes amb R. És el primer pas del cicle
d’anàlisi de les dades. L’adquisició i revisió d’aquestes. El projecte
de iNaturalist del qual volem descarregar la informació és:
<https://www.inaturalist.org/projects/especies-susceptibles-a-ser-invasores-a-barcelona-actuem-a-temps?tab=observations&subtab=table>.

Un dels paquets que podem requerir és ‘rinat’
<https://github.com/ropensci/rinat>, Un paquet desenvolupat per accedir
a les dades de ‘iNaturalist’ a través de APIs.

Carreguem els paquets necesaris:

``` r
#install.packages("rinat")
library(rinat)
```

    ## Warning: package 'rinat' was built under R version 4.1.2

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Més info i exemples sobre el paquet rinat:

``` r
# vignette("rinat-intro", package = "rinat")
```

### 1\. Descàrrega de les dades del Projecte a iNaturalist

Per a descarregar les dades del projecte podem utilitzar la funció
get\_inat\_obs\_project() del paquet rinat. Amb el parametre type
indiquem que volem descarregar les observacions i amb Raw = false
descarreguem un datarfame.

``` r
act <- get_inat_obs_project("especies-susceptibles-a-ser-invasores-a-barcelona-actuem-a-temps", type = "observations", raw=FALSE)
```

    ## 1435 records

    ## Getting records 0-200

    ## Getting records up to 400

    ## Getting records up to 600

    ## Getting records up to 800

    ## Getting records up to 1000

    ## Getting records up to 1200

    ## Getting records up to 1400

    ## Getting records up to 1600

    ## Done.

### 2\. Revisem el dataset

Visualitzem els noms de les variables del dataset descarregat

``` r
names(act)
```

    ##  [1] "id"                                "observed_on"                      
    ##  [3] "description"                       "latitude"                         
    ##  [5] "longitude"                         "map_scale"                        
    ##  [7] "timeframe"                         "species_guess"                    
    ##  [9] "user_id"                           "taxon_id"                         
    ## [11] "created_at"                        "updated_at"                       
    ## [13] "place_guess"                       "id_please"                        
    ## [15] "observed_on_string"                "iconic_taxon_id"                  
    ## [17] "num_identification_agreements"     "num_identification_disagreements" 
    ## [19] "time_observed_at"                  "time_zone"                        
    ## [21] "location_is_exact"                 "delta"                            
    ## [23] "positional_accuracy"               "private_latitude"                 
    ## [25] "private_longitude"                 "geoprivacy"                       
    ## [27] "quality_grade"                     "positioning_method"               
    ## [29] "positioning_device"                "out_of_range"                     
    ## [31] "license"                           "uri"                              
    ## [33] "observation_photos_count"          "comments_count"                   
    ## [35] "zic_time_zone"                     "oauth_application_id"             
    ## [37] "observation_sounds_count"          "identifications_count"            
    ## [39] "captive"                           "community_taxon_id"               
    ## [41] "site_id"                           "old_uuid"                         
    ## [43] "public_positional_accuracy"        "mappable"                         
    ## [45] "cached_votes_total"                "last_indexed_at"                  
    ## [47] "private_place_guess"               "uuid"                             
    ## [49] "taxon_geoprivacy"                  "tag_list"                         
    ## [51] "short_description"                 "user_login"                       
    ## [53] "iconic_taxon_name"                 "faves_count"                      
    ## [55] "created_at_utc"                    "updated_at_utc"                   
    ## [57] "time_observed_at_utc"              "owners_identification_from_vision"
    ## [59] "photos"                            "taxon.id"                         
    ## [61] "taxon.name"                        "taxon.rank"                       
    ## [63] "taxon.ancestry"                    "taxon.common_name.id"             
    ## [65] "taxon.common_name.name"            "taxon.common_name.is_valid"       
    ## [67] "taxon.common_name.lexicon"         "iconic_taxon.id"                  
    ## [69] "iconic_taxon.name"                 "iconic_taxon.rank"                
    ## [71] "iconic_taxon.rank_level"           "iconic_taxon.ancestry"            
    ## [73] "user.login"                        "user.user_icon_url"

Comptabilitzem el nombre de registres considerant la data actual:

``` r
print(paste("El nombre de registres actual (En data:", Sys.Date(), ") del projecte és: ", dim(act)[1]))
```

    ## [1] "El nombre de registres actual (En data: 2021-12-14 ) del projecte és:  1435"

### 3\. Explorem el contingut del dataset

Revisem els rangs de les diferents variables numèriques Fem un
histograma:

``` r
hist(act$positional_accuracy)
```

![](1Descarrega_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

És destacable el fet que hi ha algunes dades amb una precisió geogràfica
molt alta. Tenen poc sentit, Revisem el valor màxim:

``` r
max(act$positional_accuracy, na.rm = TRUE)
```

    ## [1] 2916

Penso que seria necesari filtrar i eliminar aquells registres que no
tenen una precisió inferior a 300m per exemple. Revisem quants registres
hi ha discretitzant la variable:

``` r
Categories <- cut(act$positional_accuracy, breaks = c(-Inf,10,50, 100, 300,Inf), labels = c("<10","10-50","50-100", "100-300", ">300"))
table(Categories)
```

    ## Categories
    ##     <10   10-50  50-100 100-300    >300 
    ##    1164     152      29      15      31

Sembla que hi ha 31 registres amb massa poca precisió (\> 300m).
Eliminem aquests registres:

``` r
actp <- act[act$positional_accuracy < 300,]
hist(actp$positional_accuracy)
```

![](1Descarrega_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Revisem els registres actuals:

``` r
print(paste("El nombre de registres actual (En data:", Sys.Date(), ") del projecte és: ", dim(actp)[1]))
```

    ## [1] "El nombre de registres actual (En data: 2021-12-14 ) del projecte és:  1402"

``` r
ggplot(actp, aes(x=positional_accuracy, fill=taxon.name)) +
  geom_density(alpha=0.4)
```

    ## Warning: Removed 44 rows containing non-finite values (stat_density).

    ## Warning: Groups with fewer than two data points have been dropped.
    
    ## Warning: Groups with fewer than two data points have been dropped.
    
    ## Warning: Groups with fewer than two data points have been dropped.

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
    ## -Inf
    
    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
    ## -Inf
    
    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
    ## -Inf

![](1Descarrega_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Sembla que tenim registres sene nom d’espècie, amb NA

``` r
unique(actp$taxon.name)
```

    ##  [1] "Cenchrus longisetus"          "Kalanchoe × houghtonii"      
    ##  [3] "Ligustrum lucidum"            "Dichondra micrantha"         
    ##  [5] "Senecio angulatus"            "Mirabilis jalapa"            
    ##  [7] "Mesembryanthemum cordifolium" NA                            
    ##  [9] "Lantana camara"               "Lantana"                     
    ## [11] "Lantana montevidensis"        "Ipomoea indica"              
    ## [13] "Lantana × urticoides"         "Lantana × hybrida"           
    ## [15] "Lantana achyranthifolia"      "Lantana velutina"

Contem-los:

``` r
sum(is.na(actp$taxon.name))
```

    ## [1] 44

Eliminem aquells registres que no tenen nom d’especie:

``` r
actpn <- actp[!is.na(actp$taxon.name),]
sum(is.na(actpn$taxon.name))
```

    ## [1] 0

Revisem el nombre de registres en el temps. Primer transformem la data

``` r
actpn$observed_on_DATE <- as.Date(actpn$observed_on)
```

``` r
ggplot(actpn, aes(observed_on_DATE))+stat_bin(aes(y=cumsum(..count..)),geom="line",bins=500)
```

    ## Warning: Removed 1 rows containing non-finite values (stat_bin).

![](1Descarrega_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Ara incloem una línia que delimiti la data d’inici del projecte (11 de
novembre de 2021) i comptabilitzem els registres anteriors i posteriors:

``` r
ggplot(actpn, aes(observed_on_DATE))+stat_bin(aes(y=cumsum(..count..)),geom="line",bins=500) + geom_vline(xintercept=as.Date("2021-11-06"), linetype=4)
```

    ## Warning: Removed 1 rows containing non-finite values (stat_bin).

![](1Descarrega_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Vegem-ho amb nombres absoluts:

``` r
a <- cut(actpn$observed_on_DATE, breaks = as.Date(c("2001-11-06", "2021-11-06", "2022-11-06")), labels = c("Abans d'iniciar el projecte","Després d'iniciar el projecte"))
table(a)
```

    ## a
    ##   Abans d'iniciar el projecte Després d'iniciar el projecte 
    ##                           245                          1112

Quants registres totals hi ha de cada espècie?

``` r
actpn %>% count(taxon.name, sort = TRUE)
```

    ##                      taxon.name   n
    ## 1  Mesembryanthemum cordifolium 336
    ## 2           Dichondra micrantha 266
    ## 3              Mirabilis jalapa 201
    ## 4             Ligustrum lucidum 134
    ## 5        Kalanchoe × houghtonii  86
    ## 6                Lantana camara  84
    ## 7           Cenchrus longisetus  79
    ## 8             Senecio angulatus  66
    ## 9                Ipomoea indica  59
    ## 10                      Lantana  32
    ## 11        Lantana montevidensis  10
    ## 12            Lantana × hybrida   2
    ## 13         Lantana × urticoides   1
    ## 14      Lantana achyranthifolia   1
    ## 15             Lantana velutina   1

Grafiquem:

``` r
ggplot(actpn, aes(taxon.name)) +
  geom_bar(fill = "#0073C2FF") + coord_flip()
```

![](1Descarrega_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Revisem els registres finals:

``` r
print(paste("El nombre de registres actual (En data:", Sys.Date(), ") del projecte és: ", dim(actpn)[1]))
```

    ## [1] "El nombre de registres actual (En data: 2021-12-14 ) del projecte és:  1358"
