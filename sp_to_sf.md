# transitioning from sp to sf

## R spatial evolution reports
[First report](https://r-spatial.org/r/2023/05/15/evolution.html) | [2nd report](https://r-spatial.org/r/2023/05/15/evolution2.html) | [3rd report](https://r-spatial.org/r/2023/05/15/evolution3.html) | [4th report](https://r-spatial.org/r/2023/05/15/evolution4.html)  

Differences between `sp` and `sf` : https://www.jessesadler.com/post/gis-with-r-intro/ 

## list of concerned sp functions used in marmap

| `sp` function  | used in `marmap` functions | replacement function? |
|--------------|----------------------|----------------------|
|sp::Line      |                  dist2isobath.R               |  |
|sp::Lines     |                  dist2isobath.R               |  |
|sp::point.in.polygon       |     subsetBathy.R                |  |
|sp::SpatialLines           |     dist2isobath.R               |  |
|sp::SpatialPixelsDataFrame |     create.buffer.R              |  |
|sp::SpatialPoints          |     create.buffer.R ; lc.dist.R  |  |
|sp::SpatialPointsDataFrame |     create.buffer.R              |  |
