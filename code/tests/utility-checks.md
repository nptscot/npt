# QA on utility trips


    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
    tar_source(): these files do not exist: R

We’ll load data with `tar_load()` and check the utility trips.

The checks below apply to Scotland South.

The target that generates the graphs for each zone is `zones_stats`,
which was defined as follows:

``` r
  tar_target(zones_stats, {
    stats = dplyr::full_join(commute_stats, school_stats_from, by = "DataZone")
    stats = dplyr::full_join(stats, utility_stats, by = "DataZone")
    stats
  }),
```

The data in `zones_stats` is as follows:

    # A tibble: 378 × 301
       DataZone  comm_orig_all comm_orig_bicycle comm_orig_car comm_orig_foot
       <chr>             <dbl>             <dbl>         <dbl>          <dbl>
     1 S01007481        216.               3.89         184.            22.4 
     2 S01007482         99.2              2.92          72.0           18.5 
     3 S01007483        145.               3.89         108.            26.9 
     4 S01007484         74.4              1.46          56.9           14.1 
     5 S01007485         12.6              0             11.7            0   
     6 S01007486          8.75             0              6.81           0   
     7 S01007487         37.9              0.973         27.2            3.89
     8 S01007488         37.0              0             31.1            3.89
     9 S01007489        139.               4.86         116.            12.6 
    10 S01007490         38.9              0             33.1            5.84
    # ℹ 368 more rows
    # ℹ 296 more variables: comm_orig_public_transport <dbl>, comm_orig_taxi <dbl>,
    #   comm_dest_all <dbl>, comm_dest_bicycle <dbl>, comm_dest_car <dbl>,
    #   comm_dest_foot <dbl>, comm_dest_public_transport <dbl>,
    #   comm_dest_taxi <dbl>, comm_orig_bicycle_go_dutch_fastest <dbl>,
    #   comm_orig_car_go_dutch_fastest <dbl>,
    #   comm_orig_public_transport_go_dutch_fastest <dbl>, …

Let’s look at the shopping trips arriving at a specific zone: S01012355.

     [1] "shopping_dest_all"                               
     [2] "shopping_dest_car"                               
     [3] "shopping_dest_foot"                              
     [4] "shopping_dest_bicycle"                           
     [5] "shopping_dest_public_transport"                  
     [6] "shopping_dest_taxi"                              
     [7] "shopping_dest_bicycle_go_dutch_fastest"          
     [8] "shopping_dest_car_go_dutch_fastest"              
     [9] "shopping_dest_public_transport_go_dutch_fastest" 
    [10] "shopping_dest_foot_go_dutch_fastest"             
    [11] "shopping_dest_taxi_go_dutch_fastest"             
    [12] "shopping_dest_quietness_fastest"                 
    [13] "shopping_dest_route_hilliness_fastest"           
    [14] "shopping_dest_bicycle_ebike_fastest"             
    [15] "shopping_dest_car_ebike_fastest"                 
    [16] "shopping_dest_public_transport_ebike_fastest"    
    [17] "shopping_dest_foot_ebike_fastest"                
    [18] "shopping_dest_taxi_ebike_fastest"                
    [19] "shopping_dest_bicycle_go_dutch_quietest"         
    [20] "shopping_dest_car_go_dutch_quietest"             
    [21] "shopping_dest_public_transport_go_dutch_quietest"
    [22] "shopping_dest_foot_go_dutch_quietest"            
    [23] "shopping_dest_taxi_go_dutch_quietest"            
    [24] "shopping_dest_bicycle_ebike_quietest"            
    [25] "shopping_dest_car_ebike_quietest"                
    [26] "shopping_dest_public_transport_ebike_quietest"   
    [27] "shopping_dest_foot_ebike_quietest"               
    [28] "shopping_dest_taxi_ebike_quietest"               
    [29] "shopping_dest_quietness_quietest"                
    [30] "shopping_dest_route_hilliness_quietest"          

    [1] 640.0917

Let’s get the associated data from the website:

    [1] 640.0917

The equivalent value the `bicycle` column is:

    [1] 1.833888

Under the Go Dutch scenario, the value is:

    [1] 3.098478

The equivalent value for the `car` column is:

    [1] 422.5624

Under the Go Dutch scenario, the value is:

    [1] 16.33732

Let’s identify the same value in the `utility_stats` data:

      [1] "DataZone"                                        
      [2] "leisure_orig_all"                                
      [3] "shopping_orig_all"                               
      [4] "visiting_orig_all"                               
      [5] "leisure_orig_car"                                
      [6] "shopping_orig_car"                               
      [7] "visiting_orig_car"                               
      [8] "leisure_orig_foot"                               
      [9] "shopping_orig_foot"                              
     [10] "visiting_orig_foot"                              
     [11] "leisure_orig_bicycle"                            
     [12] "shopping_orig_bicycle"                           
     [13] "visiting_orig_bicycle"                           
     [14] "leisure_orig_public_transport"                   
     [15] "shopping_orig_public_transport"                  
     [16] "visiting_orig_public_transport"                  
     [17] "leisure_orig_taxi"                               
     [18] "shopping_orig_taxi"                              
     [19] "visiting_orig_taxi"                              
     [20] "leisure_dest_all"                                
     [21] "visiting_dest_all"                               
     [22] "shopping_dest_all"                               
     [23] "leisure_dest_car"                                
     [24] "visiting_dest_car"                               
     [25] "shopping_dest_car"                               
     [26] "leisure_dest_foot"                               
     [27] "visiting_dest_foot"                              
     [28] "shopping_dest_foot"                              
     [29] "leisure_dest_bicycle"                            
     [30] "visiting_dest_bicycle"                           
     [31] "shopping_dest_bicycle"                           
     [32] "leisure_dest_public_transport"                   
     [33] "visiting_dest_public_transport"                  
     [34] "shopping_dest_public_transport"                  
     [35] "leisure_dest_taxi"                               
     [36] "visiting_dest_taxi"                              
     [37] "shopping_dest_taxi"                              
     [38] "leisure_orig_bicycle_go_dutch_fastest"           
     [39] "shopping_orig_bicycle_go_dutch_fastest"          
     [40] "visiting_orig_bicycle_go_dutch_fastest"          
     [41] "leisure_orig_car_go_dutch_fastest"               
     [42] "shopping_orig_car_go_dutch_fastest"              
     [43] "visiting_orig_car_go_dutch_fastest"              
     [44] "leisure_orig_public_transport_go_dutch_fastest"  
     [45] "shopping_orig_public_transport_go_dutch_fastest" 
     [46] "visiting_orig_public_transport_go_dutch_fastest" 
     [47] "leisure_orig_foot_go_dutch_fastest"              
     [48] "shopping_orig_foot_go_dutch_fastest"             
     [49] "visiting_orig_foot_go_dutch_fastest"             
     [50] "leisure_orig_taxi_go_dutch_fastest"              
     [51] "shopping_orig_taxi_go_dutch_fastest"             
     [52] "visiting_orig_taxi_go_dutch_fastest"             
     [53] "leisure_orig_quietness_fastest"                  
     [54] "shopping_orig_quietness_fastest"                 
     [55] "visiting_orig_quietness_fastest"                 
     [56] "leisure_orig_route_hilliness_fastest"            
     [57] "shopping_orig_route_hilliness_fastest"           
     [58] "visiting_orig_route_hilliness_fastest"           
     [59] "visiting_dest_bicycle_go_dutch_fastest"          
     [60] "leisure_dest_bicycle_go_dutch_fastest"           
     [61] "shopping_dest_bicycle_go_dutch_fastest"          
     [62] "visiting_dest_car_go_dutch_fastest"              
     [63] "leisure_dest_car_go_dutch_fastest"               
     [64] "shopping_dest_car_go_dutch_fastest"              
     [65] "visiting_dest_public_transport_go_dutch_fastest" 
     [66] "leisure_dest_public_transport_go_dutch_fastest"  
     [67] "shopping_dest_public_transport_go_dutch_fastest" 
     [68] "visiting_dest_foot_go_dutch_fastest"             
     [69] "leisure_dest_foot_go_dutch_fastest"              
     [70] "shopping_dest_foot_go_dutch_fastest"             
     [71] "visiting_dest_taxi_go_dutch_fastest"             
     [72] "leisure_dest_taxi_go_dutch_fastest"              
     [73] "shopping_dest_taxi_go_dutch_fastest"             
     [74] "visiting_dest_quietness_fastest"                 
     [75] "leisure_dest_quietness_fastest"                  
     [76] "shopping_dest_quietness_fastest"                 
     [77] "visiting_dest_route_hilliness_fastest"           
     [78] "leisure_dest_route_hilliness_fastest"            
     [79] "shopping_dest_route_hilliness_fastest"           
     [80] "leisure_orig_bicycle_ebike_fastest"              
     [81] "shopping_orig_bicycle_ebike_fastest"             
     [82] "visiting_orig_bicycle_ebike_fastest"             
     [83] "leisure_orig_car_ebike_fastest"                  
     [84] "shopping_orig_car_ebike_fastest"                 
     [85] "visiting_orig_car_ebike_fastest"                 
     [86] "leisure_orig_public_transport_ebike_fastest"     
     [87] "shopping_orig_public_transport_ebike_fastest"    
     [88] "visiting_orig_public_transport_ebike_fastest"    
     [89] "leisure_orig_foot_ebike_fastest"                 
     [90] "shopping_orig_foot_ebike_fastest"                
     [91] "visiting_orig_foot_ebike_fastest"                
     [92] "leisure_orig_taxi_ebike_fastest"                 
     [93] "shopping_orig_taxi_ebike_fastest"                
     [94] "visiting_orig_taxi_ebike_fastest"                
     [95] "visiting_dest_bicycle_ebike_fastest"             
     [96] "leisure_dest_bicycle_ebike_fastest"              
     [97] "shopping_dest_bicycle_ebike_fastest"             
     [98] "visiting_dest_car_ebike_fastest"                 
     [99] "leisure_dest_car_ebike_fastest"                  
    [100] "shopping_dest_car_ebike_fastest"                 
    [101] "visiting_dest_public_transport_ebike_fastest"    
    [102] "leisure_dest_public_transport_ebike_fastest"     
    [103] "shopping_dest_public_transport_ebike_fastest"    
    [104] "visiting_dest_foot_ebike_fastest"                
    [105] "leisure_dest_foot_ebike_fastest"                 
    [106] "shopping_dest_foot_ebike_fastest"                
    [107] "visiting_dest_taxi_ebike_fastest"                
    [108] "leisure_dest_taxi_ebike_fastest"                 
    [109] "shopping_dest_taxi_ebike_fastest"                
    [110] "leisure_orig_bicycle_go_dutch_quietest"          
    [111] "visiting_orig_bicycle_go_dutch_quietest"         
    [112] "shopping_orig_bicycle_go_dutch_quietest"         
    [113] "leisure_orig_car_go_dutch_quietest"              
    [114] "visiting_orig_car_go_dutch_quietest"             
    [115] "shopping_orig_car_go_dutch_quietest"             
    [116] "leisure_orig_public_transport_go_dutch_quietest" 
    [117] "visiting_orig_public_transport_go_dutch_quietest"
    [118] "shopping_orig_public_transport_go_dutch_quietest"
    [119] "leisure_orig_foot_go_dutch_quietest"             
    [120] "visiting_orig_foot_go_dutch_quietest"            
    [121] "shopping_orig_foot_go_dutch_quietest"            
    [122] "leisure_orig_taxi_go_dutch_quietest"             
    [123] "visiting_orig_taxi_go_dutch_quietest"            
    [124] "shopping_orig_taxi_go_dutch_quietest"            
    [125] "leisure_orig_bicycle_ebike_quietest"             
    [126] "visiting_orig_bicycle_ebike_quietest"            
    [127] "shopping_orig_bicycle_ebike_quietest"            
    [128] "leisure_orig_car_ebike_quietest"                 
    [129] "visiting_orig_car_ebike_quietest"                
    [130] "shopping_orig_car_ebike_quietest"                
    [131] "leisure_orig_public_transport_ebike_quietest"    
    [132] "visiting_orig_public_transport_ebike_quietest"   
    [133] "shopping_orig_public_transport_ebike_quietest"   
    [134] "leisure_orig_foot_ebike_quietest"                
    [135] "visiting_orig_foot_ebike_quietest"               
    [136] "shopping_orig_foot_ebike_quietest"               
    [137] "leisure_orig_taxi_ebike_quietest"                
    [138] "visiting_orig_taxi_ebike_quietest"               
    [139] "shopping_orig_taxi_ebike_quietest"               
    [140] "leisure_orig_quietness_quietest"                 
    [141] "visiting_orig_quietness_quietest"                
    [142] "shopping_orig_quietness_quietest"                
    [143] "leisure_orig_route_hilliness_quietest"           
    [144] "visiting_orig_route_hilliness_quietest"          
    [145] "shopping_orig_route_hilliness_quietest"          
    [146] "visiting_dest_bicycle_go_dutch_quietest"         
    [147] "leisure_dest_bicycle_go_dutch_quietest"          
    [148] "shopping_dest_bicycle_go_dutch_quietest"         
    [149] "visiting_dest_car_go_dutch_quietest"             
    [150] "leisure_dest_car_go_dutch_quietest"              
    [151] "shopping_dest_car_go_dutch_quietest"             
    [152] "visiting_dest_public_transport_go_dutch_quietest"
    [153] "leisure_dest_public_transport_go_dutch_quietest" 
    [154] "shopping_dest_public_transport_go_dutch_quietest"
    [155] "visiting_dest_foot_go_dutch_quietest"            
    [156] "leisure_dest_foot_go_dutch_quietest"             
    [157] "shopping_dest_foot_go_dutch_quietest"            
    [158] "visiting_dest_taxi_go_dutch_quietest"            
    [159] "leisure_dest_taxi_go_dutch_quietest"             
    [160] "shopping_dest_taxi_go_dutch_quietest"            
    [161] "visiting_dest_bicycle_ebike_quietest"            
    [162] "leisure_dest_bicycle_ebike_quietest"             
    [163] "shopping_dest_bicycle_ebike_quietest"            
    [164] "visiting_dest_car_ebike_quietest"                
    [165] "leisure_dest_car_ebike_quietest"                 
    [166] "shopping_dest_car_ebike_quietest"                
    [167] "visiting_dest_public_transport_ebike_quietest"   
    [168] "leisure_dest_public_transport_ebike_quietest"    
    [169] "shopping_dest_public_transport_ebike_quietest"   
    [170] "visiting_dest_foot_ebike_quietest"               
    [171] "leisure_dest_foot_ebike_quietest"                
    [172] "shopping_dest_foot_ebike_quietest"               
    [173] "visiting_dest_taxi_ebike_quietest"               
    [174] "leisure_dest_taxi_ebike_quietest"                
    [175] "shopping_dest_taxi_ebike_quietest"               
    [176] "visiting_dest_quietness_quietest"                
    [177] "leisure_dest_quietness_quietest"                 
    [178] "shopping_dest_quietness_quietest"                
    [179] "visiting_dest_route_hilliness_quietest"          
    [180] "leisure_dest_route_hilliness_quietest"           
    [181] "shopping_dest_route_hilliness_quietest"          

    [1] 640.0917

    [1] 422.5624

Let’s go back another step to look at the inputs and code that generated
the `utility_stats` data.
