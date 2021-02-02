library(tidyverse) # for piping
library(leaflet) # for map widgets
library(leaflet.providers) # for prettier basemap
library(RColorBrewer)
library(sf) # for messing with spatial data
library(tigris) # for census shapefiles
library(htmltools)

# Need Thunderforest key to use their basemaps
TF_API_KEY <- 'd0ae1dd235b346c2b873a7d371502ac7'

# Get shapefiles for census tracts, bgs and zipcodes and for district 14 and precincts 
mnbg <- block_groups("27", county = c('009', '141', '145'), class='sf')
mntracts <- tracts("27", county = c('009', '141', '145'), class='sf')
mnzcs <- zctas(starts_with = c('56301','56303','56304','56374','56377','56379',
                               '56387','55382','55319','55353','55320'),
               class = 'sf')
sd14 <- state_legislative_districts("27", class='sf')
mnprecincts <- st_read("../../data/raw/MN_2016_Shapefile")

# transform to same crs
mnbg <- st_transform(mnbg, "+proj=longlat +datum=WGS84")
sd14 <- st_transform(sd14, "+proj=longlat +datum=WGS84")
mnprecincts <- st_transform(mnprecincts, "+proj=longlat +datum=WGS84")
mntracts <- st_transform(mntracts, '+proj=longlat +datum=WGS84')
mnzcs <- st_transform(mnzcs, '+proj=longlat +datum=WGS84')

# Get just sd14
sd14 <- sd14[14,]
mnprecints14 <- mnprecincts %>% filter(MNSENDIST == "14")

# Get rid of most excess block groups by getting bgs covered by sd14 and bgs that overlap
sd14_covers <- mnbg[unlist(st_covers(sd14, mnbg)),]
sd14_overlaps <- mnbg[unlist(st_overlaps(sd14, mnbg)),]
sd14_bgs <- rbind(sd14_covers, sd14_overlaps)
rm(sd14_covers, sd14_overlaps)

sd14_tracts <- rbind(mntracts[unlist(st_covers(sd14, mntracts)),],
                     mntracts[unlist(st_overlaps(sd14, mntracts)),])

 # Get edge bgs to drop (from previous plot with popups to identify edge areas)
excess_bgs <- c('270090202021', '271450113011', '271450113014', '271450101023',
                '271450113012')
sd14_bgs <- sd14_bgs %>% filter(!is.element(GEOID, excess_bgs))

# Get centroid coords for setting the view and bbox for fitBounds
view_coords <- st_geometry(st_centroid(sd14))
bbx <- st_bbox(sd14)

# read in Social Explorer BG data
se_bgs <- read.csv('../../data/raw/SE_Block_Group.csv', as.is = TRUE,
                   colClasses = c(rep("character", 2),
                                  rep("numeric", 47)), skip = 1)
# Change colnames
colnames(se_bgs) <- gsub("SE_B01001", "Age", colnames(se_bgs))
colnames(se_bgs) <- gsub("SE_B12001", "Education", colnames(se_bgs))
colnames(se_bgs) <- gsub("SE_A17005", "Unemployment", colnames(se_bgs))
colnames(se_bgs) <- gsub("SE_A14006", "Median.HH.Inc", colnames(se_bgs))
colnames(se_bgs) <- gsub("SE_A10062B", "Renters", colnames(se_bgs))
colnames(se_bgs) <- gsub("SE_B18002", "Rent.Costs", colnames(se_bgs))
colnames(se_bgs) <- gsub("SE_A18003", "Rent.to.Income", colnames(se_bgs))
colnames(se_bgs) <- gsub("SE_B10040", "Home.Costs", colnames(se_bgs))
colnames(se_bgs) <- gsub("SE_A10037", 'HomeCosts.to.Income', colnames(se_bgs))
se_bgs$Geo_GEOID <- gsub("15000US", "", se_bgs$Geo_GEOID)

# Mutate variables to get percentage of people living in apartments (Renters/Total Pop)
# and percentage paying over 30% towards rent or housing, HS or Less
se_bgs <- se_bgs %>% mutate(Apartments = Renters_001/Age_001,
                            Rent.Distress = PCT_Rent.Costs_002 + PCT_Rent.Costs_003,
                            HS.or.Less = PCT_Education_002 + PCT_Education_003)

# Add social explorer data to shapes
sd14_bgs <- merge(sd14_bgs, se_bgs, by.x = "GEOID", by.y = "Geo_GEOID")


leaflet(sd14_bgs) %>%
  setView(lng = view_coords[[1]][1], lat = view_coords[[1]][2], zoom = 11) %>%
  setMaxBounds(bbx[[1]] - 0.05, bbx[[2]] - 0.05, bbx[[3]] + 0.05, bbx[[4]] + 0.05) %>% 
  addProviderTiles(provider = providers$OpenStreetMap.Mapnik,
                   options = providerTileOptions(minZoom = 11,
                                                 maxZoom = 14),
                   group = "Neighborhood") %>%
  addPolygons(group = "Under 18",
              stroke = FALSE,
              fillColor = ~colorNumeric("YlOrRd", domain = sd14_bgs$PCT_Age_002)(PCT_Age_002),
              fillOpacity = 0.5,
              label = ~PCT_Age_002) %>% 
  addLegend(group = "Under 18",
            position = "bottomright", pal = colorNumeric("YlOrRd", domain = sd14_bgs$PCT_Age_002),
            values = range(sd14_bgs$PCT_Age_002)) %>% 
  addPolygons(group = "Over 65",
              stroke = FALSE,
              fillColor = ~colorNumeric("YlOrBr", domain = sd14_bgs$PCT_Age_005)(PCT_Age_005),
              fillOpacity = 0.5,
              label = ~PCT_Age_005) %>% 
  addLegend(group = "Over 65",
            position = "bottomright", pal = colorNumeric("YlOrBr", domain = sd14_bgs$PCT_Age_005),
            values = range(sd14_bgs$PCT_Age_005)) %>% 
  addPolygons(group = "HS or Less",
              stroke = FALSE,
              fillColor = ~colorNumeric("YlOrRd", domain = sd14_bgs$HS.or.Less)(HS.or.Less),
              fillOpacity = 0.5,
              label = ~HS.or.Less) %>% 
  addLegend(group = "HS or Less",
            position = "bottomright", pal = colorNumeric("YlOrRd", domain = sd14_bgs$HS.or.Less),
            values = range(sd14_bgs$HS.or.Less)) %>% 
  addPolygons(group = "Bachelors or Greater",
              stroke = FALSE,
              fillColor = ~colorNumeric("GnBu", domain = sd14_bgs$PCT_Education_004)(PCT_Education_004),
              fillOpacity = 0.5,
              label = ~PCT_Education_004) %>% 
  addLegend(group = "Bachelors or Greater",
            position = "bottomright", pal = colorNumeric("GnBu", domain = sd14_bgs$PCT_Education_004),
            values = range(sd14_bgs$PCT_Education_004)) %>% 
  addPolygons(group = "Unemployed",
              stroke = FALSE,
              fillColor = ~colorNumeric("Oranges", domain = sd14_bgs$PCT_Unemployment_003)(PCT_Unemployment_003),
              fillOpacity = 0.5,
              label = ~PCT_Unemployment_003) %>% 
  addLegend(group = "Unemployed",
            position = "bottomright", pal = colorNumeric("Oranges", domain = sd14_bgs$PCT_Unemployment_003),
            values = range(sd14_bgs$PCT_Unemployment_003)) %>% 
  addPolygons(group = "Median Income",
              stroke = FALSE,
              fillColor = ~colorNumeric("Greens", domain = sd14_bgs$Median.HH.Inc_001)(Median.HH.Inc_001),
              fillOpacity = 0.5,
              label = ~Median.HH.Inc_001) %>% 
  addLegend(group = "Median Income",
            position = "bottomright", pal = colorNumeric("Greens", domain = sd14_bgs$Median.HH.Inc_001),
            values = range(sd14_bgs$Median.HH.Inc_001, na.rm = TRUE),
            labFormat = labelFormat(prefix = "$")) %>% 
  addPolygons(group = "Renter Population",
              stroke = FALSE,
              fillColor = ~colorNumeric("OrRd", domain = sd14_bgs$Apartments)(Apartments),
              fillOpacity = 0.5,
              label = ~Apartments) %>% 
  addLegend(group = "Renter Population",
            position = "bottomright", pal = colorNumeric("OrRd", domain = sd14_bgs$Apartments),
            values = range(sd14_bgs$Apartments)) %>% 
  addPolygons(group = "Renter Distress",
              stroke = FALSE,
              fillColor = ~colorNumeric("Reds", domain = sd14_bgs$Rent.Distress)(Rent.Distress),
              fillOpacity = 0.5,
              label = ~Rent.Distress) %>% 
  addLegend(group = "Renter Distress",
            position = "bottomright", pal = colorNumeric("Reds", domain = sd14_bgs$Rent.Distress),
            values = range(sd14_bgs$Rent.Distress, na.rm = TRUE)) %>% 
  addPolygons(group = "Homeowner Distress",
              stroke = FALSE,
              fillColor = ~colorNumeric("Oranges", domain = sd14_bgs$PCT_Home.Costs_002)(PCT_Home.Costs_002),
              fillOpacity = 0.5,
              label = ~PCT_Home.Costs_002) %>% 
  addLegend(group = "Homeowner Distress",
            position = "bottomright", pal = colorNumeric("Oranges", domain = sd14_bgs$PCT_Home.Costs_002),
            values = range(sd14_bgs$PCT_Home.Costs_002, na.rm = TRUE)) %>%
  addPolylines(data = sd14, color = "red",
               opacity = 3,
               group = "District Outline") %>%
  addPolylines(data = mnprecints14, color = "black",
               weight = 3,
               opacity = 5,
               group = "Precincts") %>% 
  addLayersControl(overlayGroups = c("Under 18", "Over 65", "HS or Less",
                                  "Bachelors or Greater", "Unemployed",
                                  "Median Income", "Renter Population",
                                  "Renter Distress", "Homeowner Distress"),
                   baseGroups = c("District Outline", "Precincts"),
                   position = "topleft",
                   options = layersControlOptions(collapsed = FALSE,
                                                  autoZIndex = TRUE)) %>% 
  hideGroup(c("Over 65", "HS or Less", "Bachelors or Greater", "Unemployed",
              "Median Income", "Renter Population", "Renter Distress",
              "Homeowner Distress"))

# Tract data
se_tracts <- read.csv("../../data/raw/SE_Tracts.csv", as.is = TRUE,
                      colClasses = c(rep('character', 2),
                                     rep('numeric', 31)))
sd14_tracts <- merge(sd14_tracts, se_tracts, by.x = "GEOID",
                     by.y = "FIPS")
# Tract leaflet
leaflet(sd14_tracts) %>%
  setView(lng = view_coords[[1]][1], lat = view_coords[[1]][2], zoom = 11) %>%
  setMaxBounds(bbx[[1]] - 0.05, bbx[[2]] - 0.05, bbx[[3]] + 0.05, bbx[[4]] + 0.05) %>% 
  addProviderTiles(provider = providers$OpenStreetMap.Mapnik,
                   options = providerTileOptions(minZoom = 11,
                                                 maxZoom = 14),
                   group = "Neighborhood") %>%
  addPolygons(group = "Gini Index",
              stroke = FALSE,
              fillColor = ~colorNumeric("Reds", domain = sd14_tracts$Gini.Index)(Gini.Index),
              fillOpacity = 0.5,
              label = ~Gini.Index) %>% 
  addLegend(group = "Gini Index",
            position = "bottomright", pal = colorNumeric("Reds", domain = sd14_tracts$Gini.Index),
            values = range(sd14_tracts$Gini.Index)) %>%
  addPolygons(group = "Naturalized Citizens",
              stroke = FALSE,
              fillColor = ~colorNumeric("Greens", domain = sd14_tracts$X..Total.Population..Foreign.Born..Naturalized.Citizen)(X..Total.Population..Foreign.Born..Naturalized.Citizen),
              fillOpacity = 0.5,
              label = ~X..Total.Population..Foreign.Born..Naturalized.Citizen) %>% 
  addLegend(group = "Naturalized Citizens",
            position = "bottomright", pal = colorNumeric("Greens", domain = sd14_tracts$X..Total.Population..Foreign.Born..Naturalized.Citizen),
            values = range(sd14_tracts$X..Total.Population..Foreign.Born..Naturalized.Citizen)) %>% 
  addPolylines(data = sd14, color = "red",
               opacity = 3,
               group = "District Outline") %>%
  addPolylines(data = mnprecints14, color = "black",
               weight = 3,
               opacity = 5,
               group = "Precincts") %>% 
  addLayersControl(overlayGroups = c("Gini Index", "Naturalized Citizens"),
                   baseGroups = c("District Outline", "Precincts"),
                   position = "topleft",
                   options = layersControlOptions(collapsed = FALSE,
                                                  autoZIndex = TRUE)) %>% 
  hideGroup(c("Naturalized Citizens"))
  
# Precincts 
pct_data <- readxl::read_xlsx("../../data/processed/SD14_WNbyPcnt.xlsx")
mnprecints14$PCTCODE <- as.numeric(as.character(mnprecints14$PCTCODE))
mnprecints14_d <- merge(mnprecints14, pct_data, by.x=c("COUNTYCODE", "PCTCODE"),
                        by.y=c("CountyCode", 'PrecinctCode'))
leaflet(mnprecints14_d) %>%
  setView(lng = view_coords[[1]][1], lat = view_coords[[1]][2], zoom = 11) %>%
  setMaxBounds(bbx[[1]] - 0.05, bbx[[2]] - 0.05, bbx[[3]] + 0.05, bbx[[4]] + 0.05) %>% 
  addProviderTiles(provider = providers$OpenStreetMap.Mapnik,
                   options = providerTileOptions(minZoom = 11,
                                                 maxZoom = 14),
                   group = "Neighborhood") %>%
  addPolygons(group = "Precinct Exp. Turnout",
              stroke = FALSE,
              fillColor = ~colorNumeric("Greens", domain = mnprecints14_d$`PCT turnout`)(`PCT turnout`),
              fillOpacity = 0.5,
              label = ~`PCT turnout`) %>% 
  addLegend(group = "Precinct Exp. Turnout",
            position = "bottomright", pal = colorNumeric("Greens", domain = mnprecints14_d$`PCT turnout`),
            values = range(mnprecints14_d$`PCT turnout`)) %>%
  addPolygons(group = "Precinct Vote Goal",
              stroke = FALSE,
              fillColor = ~colorNumeric("Blues", domain = mnprecints14_d$`PCT Vote Goal`)(`PCT Vote Goal`),
              fillOpacity = 0.5,
              label = ~`PCT Vote Goal`) %>% 
  addLegend(group = "Precinct Vote Goal",
            position = "bottomright", pal = colorNumeric("Blues", domain = mnprecints14_d$`PCT Vote Goal`),
            values = range(mnprecints14_d$`PCT Vote Goal`)) %>%
  addPolygons(group = "Exp. Turnout (MN state)",
              stroke = FALSE,
              fillColor = ~colorNumeric("Greens", domain = mnprecints14_d$`Exp. Turnout (based on MN turnout)`)(`Exp. Turnout (based on MN turnout)`),
              fillOpacity = 0.5,
              label = ~`Exp. Turnout (based on MN turnout)`) %>% 
  addLegend(group = "Exp. Turnout (MN state)",
            position = "bottomright", pal = colorNumeric("Greens", domain = mnprecints14_d$`Exp. Turnout (based on MN turnout)`),
            values = range(mnprecints14_d$`Exp. Turnout (based on MN turnout)`)) %>% 
  addPolygons(group = "Vote Goal",
              stroke = FALSE,
              fillColor = ~colorNumeric("Blues", domain = mnprecints14_d$`Vote Goal`)(`Vote Goal`),
              fillOpacity = 0.5,
              label = ~`Vote Goal`) %>% 
  addLegend(group = "Vote Goal",
            position = "bottomright", pal = colorNumeric("Blues", domain = mnprecints14_d$`Vote Goal`),
            values = range(mnprecints14_d$`Vote Goal`)) %>% 
  addPolylines(data = mnprecints14, color = "black",
               weight = 3,
               opacity = 5,
               group = "Precincts") %>% 
  addLayersControl(overlayGroups = c("Precinct Exp. Turnout", "Precinct Vote Goal",
                                     "Exp. Turnout (MN state)", "Vote Goal"),
                   position = "topleft",
                   options = layersControlOptions(collapsed = FALSE,
                                                  autoZIndex = TRUE)) %>%
  hideGroup(c("Precinct Vote Goal", "Exp. Turnout (MN state)", "Vote Goal"))


# Zip Codes
zillow <- readxl::read_xlsx("../../data/raw/Zillow Data/Zillow Home Values MN.xlsx")
zillow <- zillow %>% select(RegionName, (ncol(zillow) - 11):ncol(zillow))
zillow_d <- zillow %>% transmute(Zip = RegionName, ZPI = rowMeans(zillow[,-1]))
manu <- readxl::read_xlsx('../../data/raw/Manufacturing_2013_2017.xlsx',
                          col_types = 'numeric')
manu_d <- manu %>% filter(Year == 2017) %>% transmute(Year, `Zip Code`, Man = Total)
retail <- readxl::read_xlsx("../../data/raw/Business Data/Business Data/Number_of_businesses__by_size_MNSD14_Zipcode_retail.xlsx",
                            col_types = 'numeric')
accom <- readxl::read_xlsx('../../data/raw/Business Data/Business Data/Number_of_businesses__by_size_MNSD14_Zipcode_accomodation_and_food_service.xlsx',
                           col_types = 'numeric')
retail_d <- retail %>% select(`Zip Code`, Total) %>% rename(Ret = Total)
accom_d <- accom %>% select(`Zip Code`, Total) %>% rename(Acc = Total)
mnzcs_d <- mnzcs %>% mutate_at(vars(ZCTA5CE10), as.numeric) %>% 
  left_join(zillow_d, by = c('ZCTA5CE10' = 'Zip')) %>% 
  left_join(manu_d, by = c('ZCTA5CE10' = 'Zip Code')) %>% 
  left_join(retail_d, by = c('ZCTA5CE10' = 'Zip Code')) %>% 
  left_join(accom_d, by = c('ZCTA5CE10' = 'Zip Code'))

leaflet(mnzcs_d) %>%
  setView(lng = view_coords[[1]][1], lat = view_coords[[1]][2], zoom = 11) %>%
  setMaxBounds(bbx[[1]] - 0.05, bbx[[2]] - 0.05, bbx[[3]] + 0.05, bbx[[4]] + 0.05) %>% 
  addProviderTiles(provider = providers$OpenStreetMap.Mapnik,
                   options = providerTileOptions(minZoom = 11,
                                                 maxZoom = 14),
                   group = "Neighborhood") %>%
  addPolygons(group = "ZPI",
              stroke = FALSE,
              fillColor = ~colorNumeric("Greens", domain = mnzcs_d$ZPI)(ZPI),
              fillOpacity = 0.5,
              label = ~ZPI) %>% 
  addLegend(group = "ZPI",
            position = "bottomright", pal = colorNumeric("Greens", domain = mnzcs_d$ZPI),
            values = range(mnzcs_d$ZPI)) %>%
  addPolygons(group = "Manufacturing",
              stroke = FALSE,
              fillColor = ~colorNumeric("Blues", domain = mnzcs_d$Man)(Man),
              fillOpacity = 0.5,
              label = ~Man) %>% 
  addLegend(group = "Manufacturing",
            position = "bottomright", pal = colorNumeric("Blues", domain = mnzcs_d$Man),
            values = range(mnzcs_d$Man)) %>%
  addPolygons(group = "Retail",
              stroke = FALSE,
              fillColor = ~colorNumeric("Reds", domain = mnzcs_d$Ret)(Ret),
              fillOpacity = 0.5,
              label = ~Ret) %>% 
  addLegend(group = "Retail",
            position = "bottomright", pal = colorNumeric("Reds", domain = mnzcs_d$Ret),
            values = range(mnzcs_d$Ret)) %>%
  addPolygons(group = "Accomodation",
              stroke = FALSE,
              fillColor = ~colorNumeric("Oranges", domain = mnzcs_d$Acc)(Acc),
              fillOpacity = 0.5,
              label = ~Acc) %>% 
  addLegend(group = "Accomodation",
            position = "bottomright", pal = colorNumeric("Oranges", domain = mnzcs_d$Acc),
            values = range(mnzcs_d$Acc)) %>%
  addPolylines(data = sd14, color = "black",
               opacity = 3,
               group = "District Outline") %>% 
  addLayersControl(overlayGroups = c('ZPI', 'Manufacturing', 'Retail',
                                     'Accomodation'),
                   position = "topleft",
                   options = layersControlOptions(collapsed = FALSE,
                                                  autoZIndex = TRUE)) %>%
  hideGroup(c('Manufacturing', 'Retail', 'Accomodation'))







