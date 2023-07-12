# transitioning from `sf`

## R spatial evolution reports
[First report](https://r-spatial.org/r/2022/04/12/evolution.html) | [2nd report](https://r-spatial.org/r/2022/12/14/evolution2.html) | [3rd report](https://r-spatial.org/r/2023/04/10/evolution3.html) | [4th report](https://r-spatial.org/r/2023/05/15/evolution4.html)  

Differences between `sp` and `sf` : https://www.jessesadler.com/post/gis-with-r-intro/ 

## list of concerned `sp` functions used in `marmap`

| `sp` function  | used in `marmap` functions | need replacement function? | help |
|--------------|----------------------|----------------------|----------------------|
|sp::Line      |                  dist2isobath.R               | no | |
|sp::Lines     |                  dist2isobath.R               | no | |
|sp::point.in.polygon       |     subsetBathy.R                | no | |
|sp::SpatialLines           |     dist2isobath.R               | no | |
|sp::SpatialPixelsDataFrame |     create.buffer.R              | yes? uses `CRS`  | [Stack Overflow](https://gis.stackexchange.com/questions/363109/sf-spatialgriddataframe-vs-spatialpixelsdataframe-processing-visualization-an) |
|sp::SpatialPoints          |     create.buffer.R ; lc.dist.R  | yes? uses `CRS` | |
|sp::SpatialPointsDataFrame |     create.buffer.R              | yes? uses `CRS` | [Stack Overflow](https://stackoverflow.com/questions/48152269/make-a-spatialpointsdataframe-with-sf-the-fast-way)|
