# Bike-Share-Viz

## About

Excercize in exploring bike share data This uses start/stop station 
ride location data from Chicago divvy bikes to create pseudo best-ride 
routes using OSM routing software routino.

The final visualization is a heat-map of routes split into regular 
subscribers / one-off users.

This is a full process file; data cleanup, mapping, routing, 
routino linux program must to be configured along with OSM maps
for city of choice.  

GGmap / ggplot plotting of the routes is done by overlaying likely
segments from each possible origin-destination.

Mapping builds on Code supplied by james cheshire Feb 2012, blog

## Final Output
 
<img src="\final\Likely Routes - Loop - reduced.jpg">

[full-size](https://github.com/azadag/Bike-Share-Viz/blob/master/final/Likely%20Routes%20-%20Loop.jpg)

## Basic take-away

Very different use case by  regular divvy members vs tourists.
Tourists were more likely to take shorter rides, within the loop 
and along the shore, while regular riders took longer home-work
rides that went further into non-loop neighborhoods.

## Notes

Most gpx files deleted for upload to github, in general not the most 
efficient way of making a likely route.  
