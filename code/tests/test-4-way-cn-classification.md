

``` r
system("gh release list --repo=nptscot/npt")
# New coherent network with classification                Latest  CN_class                                                                     about 1 hour ago
#                                                         Draft                                                                                about 29 days ago
# osm_example                                                     osm_example 
```

``` r
system("gh release download CN_class --repo=nptscot/npt --clobber")
list.files("geojson")
```

    character(0)

``` r
# manually download from: https://github.com/nptscot/npt/releases/tag/CN_class
cn4 = sf::read_sf("https://github.com/nptscot/npt/releases/download/CN_class/city_of_edinburgh_2024-10-01_4_coherent_network.geojson")
library(tmap)
```


    Attaching package: 'tmap'

    The following object is masked from 'package:datasets':

        rivers

``` r
names(cn4)
```

    [1] "name_1"                       "road_function"               
    [3] "all_fastest_bicycle_go_dutch" "geometry"                    

``` r
m = tm_shape(cn4) +
  tm_lines("road_function", lwd = 4)
m
```

![](test-4-way-cn-classification_files/figure-commonmark/unnamed-chunk-2-1.png)

``` r
tmap_save(m, "cn4.html")
```

    Interactive map saved to /home/robin/github/nptscot/npt/code/tests/cn4.html

``` r
browseURL("cn4.html")
system("gh release upload CN_class --repo=nptscot/npt cn4.html")
```
