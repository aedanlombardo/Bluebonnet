# Mapping Senate District 14 with Leaflet for R

## Summary
I created this project as part of my work for [Aric Putnam's](https://www.senate.mn/members/member_bio.html?member_id=1249) campaign for Minnesota Senate District 14. If you follow the link from Aric's name you'll see that he was successful in unseating a Republican incumbent in an incredibly tight race. This mapping project was created to support voter contact planning in the summer leading up to the final push of the fall. Aric and his campaign manager wanted to better understand what the demographic makeup and political leaning of their district. Having tried using static choropleth maps in Python with geopandas (both with and without base layers), I realized it would be easier to interpret this information if it was presented over a base layer with monuments and street names and could be moved and zoomed in and out of. Thankfully I was pointed by a colleague at [Bluebonnet Data](https://www.bluebonnetdata.org/) to [Leaflet for R](https://rstudio.github.io/leaflet/) which allowed me to leverage the power of Leaflet while still working in my usual R environment that I used to clean and manipulate my raw data.

## Data
* MN_2016_Shapefile contains a precinct-level shapefile (w/ historical election returns and information on voter registration) for the senate district published by the [MGGG Redistricting Lab](https://mggg.org/)
* SD14_WNbyPcnt.xlsx contains calculations done by my colleague for the win number (number of votes needed to win) for each precinct
* SE_Block_Group.csv and SE_Tracts.csv contain 2018 ACS data pulled from [Social Explorer](https://www.socialexplorer.com/)

## Script
This script uses these files to produce Leaflet maps which can be saved an html files and hosted at any URL. While working with the campaign (and using GitLab) I used GitLab's website hosting template and GitLab Pages to host these files and used a link shortener to easily share them with campaign staff.
