# optimization of marmap

## avenues to explore : 
- using binary data
- using library(bigmemory) very large matrices that exceed R’s in-memory limits:
- using parallel computing (eg parApply)
- improving memory managment
- using Rcpp for C++ Acceleration

## testing an profiling

use 
```
library(microbenchmark)
library(ggplot2)

res=microbenchmark(
    original = original_as_raster(bathy),
    parallel = as.raster(bathy)
)

print(res)
ggplot2::autoplot(res)
```

## attempts to optimize

`as.bathy`, 21/11/2024, improved
`getNOAA.bathy`21/11/2024, improved
