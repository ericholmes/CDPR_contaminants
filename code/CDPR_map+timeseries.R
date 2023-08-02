# California Dpartment of pesticide regulation
# https://calpip.cdpr.ca.gov/main.cfm


library(tidyverse)
county <- read.table("data/pur1990/county.txt", sep = ",", header = T)
chemresults <- data.frame()

for(year in c(2009:2021)){
  print(year)
  files <- list.files(paste0("data/pur", year), pattern = "udc")
  for(file in files){
    print(file)
    temp <- read.table(paste0("data/pur", year, "/", file), sep = ",", header = T)
    chemical <- read.table(paste0("data/pur", year, "/chemical.txt"), sep = ",", header = T)
    tempmerge <- merge(temp[, c("chem_code", "lbs_chm_used")], chemical, by = "chem_code")
    
    if(nrow(temp)>=1){
      tempmerge$county_name <- county[county$county_cd == as.integer(substr(file, 7,8)), "couty_name"]
      tempmerge$Year <- year
      tempply <- tempmerge %>% group_by(chem_code, chemname, county_name, Year) %>% 
        summarize(lbs_applied = sum(lbs_chm_used, na.rm = T)) %>% data.frame()
      chemresults <- rbind(chemresults, tempply)
      }
    rm(temp, tempmerge, tempply)
  }
}

chemresults2 <- rbind(chemresults2008, chemresults)

save(chemresults2, file = "data/CPDR_chemresults.Rdata")

print(year)

chemresults <- chemresults2
# Mapping contaminants by county ------------------------------------------
chemresults$chemgroup <- ifelse(grepl(tolower(chemresults$chemname), pattern = "strobin"), "strobin", 
                                ifelse(grepl(tolower(chemresults$chemname), pattern = "copper"), "copper",
                                       ifelse(grepl(tolower(chemresults$chemname), pattern = "fipronil"),"fipronil","other")))

fipronil <- chemresults[chemresults$chemname %in% c("FIPRONIL"),]
fipronil$county_name <- tolower(fipronil$county_name)

counties <- map_data("county")
ca_counties <- counties[counties$region == "california",]


choro <- merge(ca_counties, fipronil, by.x = "subregion", by.y = "county_name", all.x =T)
# choro <- choro[order(choro$order), ]

ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = lbs_applied)) +
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) + facet_wrap(. ~ Year)

ggplot(choro[choro$subregion %in% c("yolo", "sacramento", "sutter", "colusa"),], aes(long, lat)) +
  geom_polygon(aes(group = group, fill = lbs_applied)) +
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) + facet_wrap(. ~ Year)

chemply <- chemresults %>% group_by(chemgroup, county_name, Year) %>% summarize(sum_lbs = sum(lbs_applied)) %>% data.frame()
chemply$county_name <- tolower(chemply$county_name)
choroply <- merge(ca_counties, chemply, by.x = "subregion", by.y = "county_name", all = T)

ggplot(choroply[choroply$subregion %in% c("yolo", "sacramento", "sutter", "colusa") & 
                  choroply$chemgroup != "other",], aes(long, lat)) +
  geom_polygon(aes(group = group, fill = sum_lbs)) +
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) + facet_wrap(. ~ Year)

ggplot(choroply[choroply$subregion %in% c("yolo", "sacramento", "sutter", "colusa") &
                  choroply$chemgroup == "strobin",], aes(long, lat)) +
  geom_polygon(aes(group = group, fill = sum_lbs)) +
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) + facet_wrap(. ~ Year)+
  theme_minimal()

ggplot(choroply[choroply$chemgroup == "copper",], aes(long, lat)) +
  geom_polygon(aes(group = group, fill = sum_lbs)) +
  scale_fill_viridis_c()+
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) + facet_wrap(. ~ Year)+
  theme_minimal()

ggplot(choroply[choroply$chemgroup == "strobin",], aes(long, lat)) +
  geom_polygon(aes(group = group, fill = sum_lbs)) +
  scale_fill_viridis_c()+
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) + facet_wrap(. ~ Year)+
  theme_minimal()



library(sfheaders)
library(sf)

choro_sf <- sf_polygon(choroply[choroply$chemgroup == "fipronil",],
                       x = "long",
                       y = "lat",
                       polygon_id = c("subregion"),
                       keep = T)

choro_sf$area <- st_area(choro_sf)

choro_sf$stand_lbs <- choro_sf$sum_lbs/choro_sf$area
choroply2 <- merge(choroply, choro_sf[, c("subregion", "area")],
                   by = "subregion")
choroply2$stand_lbs <- choroply2$sum_lbs/choroply2$area
counties_tomap <- c("yolo", "sacramento", "sutter", "colusa", "solano", "butte", "glenn","tehama", "contra costa", "alameda", "san joaquin")



ggplot(choroply[choroply$chemgroup == "strobin" & choroply$subregion %in% counties_tomap,], 
       aes(x = Year, y = sum_lbs)) + geom_line() + facet_wrap(subregion ~ .) + theme_bw() +
  labs(title = "Strobin")

ggplot(choroply[choroply$chemgroup == "fipronil" & choroply$subregion %in% counties_tomap,], 
       aes(x = Year, y = sum_lbs)) + geom_line() + facet_wrap(subregion ~ .) + theme_bw() +
  labs(title = "Fipronil")

ggplot(choroply[choroply$chemgroup == "copper" & choroply$subregion %in% counties_tomap,], 
       aes(x = Year, y = sum_lbs)) + geom_line() + facet_wrap(subregion ~ .) + theme_bw() +
  labs(title = "Copper products")

ggplot(choroply[choroply$chemgroup == "other" & choroply$subregion %in% counties_tomap,], 
       aes(x = Year, y = sum_lbs)) + geom_line() + facet_wrap(subregion ~ .) + theme_bw() +
  labs(title = "other")

png("Contam_counties_%03d.png", 
    family = "serif", height = 8, width = 6, units = "in", res = 1000)

cowplot::plot_grid(ggplot(choroply2[choroply2$subregion %in% counties_tomap &
                                      choroply2$chemgroup == "fipronil",], aes(long, lat)) +
                     geom_polygon(aes(group = group, fill = stand_lbs)) +
                     scale_fill_viridis_c()+
                     coord_map("albers",  lat0 = 45.5, lat1 = 29.5) + facet_grid(chemgroup ~ Year)+
                     theme_minimal(),
                   
                   ggplot(choroply2[choroply2$subregion %in% counties_tomap & 
                                      choroply2$chemgroup == "strobin",], aes(long, lat)) +
                     geom_polygon(aes(group = group, fill = stand_lbs)) +
                     scale_fill_viridis_c()+
                     coord_map("albers",  lat0 = 45.5, lat1 = 29.5) + facet_grid(chemgroup ~ Year)+
                     theme_minimal(),
                   
                   ggplot(choroply2[choroply2$subregion %in% counties_tomap & 
                                      choroply2$chemgroup == "copper",], aes(long, lat)) +
                     geom_polygon(aes(group = group, fill = stand_lbs)) +
                     scale_fill_viridis_c()+
                     coord_map("albers",  lat0 = 45.5, lat1 = 29.5) + facet_grid(chemgroup ~ Year)+
                     theme_minimal(),
                   
                   nrow = 3, labels = c("fipronil", "strobin", "copper"))

ggplot(choroply2[choroply$subregion == counties_tomap,], 
       aes(x = Year, y = stand_lbs, color = subregion)) + geom_line() + theme_bw() + 
  facet_wrap(chemgroup ~ ., scales = "free_y") + theme(legend.position = "bottom")

dev.off()

# Explore temporal trends by county ----------------------------------------
# counties: 57,YOLO; 51,SUTTER; 34,SACRAMENTO;06,COLUSA;11,GLENN
# temp1 <- read.table(paste0("C:/Users/eholmes/Downloads/pur", 2017, "/", "udc17_11.txt"), sep = ",", header = T)
# temp2 <- read.table(paste0("C:/Users/eholmes/Downloads/pur", 2018, "/", "udc18_11.txt"), sep = ",", header = T)
# temp3 <- read.table(paste0("C:/Users/eholmes/Downloads/pur", 2019, "/", "udc19_11.txt"), sep = ",", header = T)
# temp <- rbind(temp1,temp2,temp3)

chemdaily <- data.frame()

for(year in c(1990:2021)){
  for(cnty in c("57", "51", "34", "06", "11")){
    print(paste(year, cnty))
    temp <- read.table(paste0("data/pur", year, "/", "udc",substr(year, 3,4),"_", cnty,".txt"), sep = ",", header = T)
    chemdaily <- rbind(chemdaily, temp[, c(1:33)])
    rm(temp)
  }
}
chemdaily2 <- chemdaily[, 1:14]


# load(file = "data/CPDR_chemresults.Rdata")
save(chemresults2, file = "data/CPDR_chemresults.Rdata")
save(chemdaily2, file = "data/CPDR_chemdaily.Rdata")
save(chemweekly, file = "data/CPDR_chemweekly.Rdata")
chemdaily$applic_dt <- as.Date(chemdaily$applic_dt, "%m/%d/%Y")
chemdaily$Year <- format(chemdaily$applic_dt, format = "%Y")
chemdaily$applic_wk <- format(chemdaily$applic_dt, format = "%W")
chemdailymerge <- merge(chemdaily, chemical, by = "chem_code")
chemdailymerge <- merge(chemdailymerge, county, by = "county_cd")
chemweekly <- chemdailymerge %>% group_by(Year, applic_wk, couty_name, chemname) %>% 
  summarize(tot_lbs_chm_used = sum(lbs_chm_used))


chemdailymerge$chemgroup <- ifelse(grepl(tolower(chemdailymerge$chemname), pattern = "strobin"), "strobin", 
                              ifelse(grepl(tolower(chemdailymerge$chemname), pattern = "copper"), "copper",
                                     ifelse(grepl(tolower(chemdailymerge$chemname), pattern = "fipronil"),"fipronil","other")))

sutterfip <- chemdailyply[chemdailyply$chemgroup == "fipronil",]
sutteroth <- chemdailyply[chemdailyply$chemgroup == "other",]
chemdailyply <- chemdailymerge %>% group_by(county_cd, applic_dt, chemgroup) %>% summarize(sum_lbs = sum(lbs_chm_used))
chemdailyply$Year <- format(chemdailyply$applic_dt, format = "%Y")
chemdailyply$jday <- as.numeric(format(chemdailyply$applic_dt, format = "%j"))

ggplot(chemdailyply[chemdailyply$chemgroup == "copper",], aes(x = jday, y = sum_lbs)) + 
  geom_bar(stat = "identity", fill = "black") + geom_line(stat = "smooth") + 
  facet_wrap(Year ~.) +
  scale_x_continuous(breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 242, 272, 303, 333, 364),
                     labels = c(month.abb, month.abb[1]))

ggplot(chemdailyply[chemdailyply$chemgroup == "strobin",], aes(x = jday, y = sum_lbs)) + 
  geom_bar(stat = "identity", fill = "black") + geom_line(stat = "smooth") + 
  facet_wrap(Year ~.) +
  scale_x_continuous(breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 242, 272, 303, 333, 364),
                     labels = c(month.abb, month.abb[1]))

ggplot(chemdailyply[chemdailyply$chemgroup == "strobin",], aes(x = jday, y = sum_lbs)) + 
 stat_smooth(aes(color = Year), method = "loess", span = .3, se = F) + scale_color_viridis_d() +
  scale_x_continuous(breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 242, 272, 303, 333, 364),
                     labels = c(month.abb, month.abb[1]))

ggplot(chemdailyply[chemdailyply$chemgroup == "strobin",], aes(x = jday, y = sum_lbs)) + 
  geom_bar(stat = "identity", fill = "black") + geom_line(stat = "smooth") + 
  facet_wrap(Year ~., scales = "free_x") +
  scale_x_continuous(breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 242, 272, 303, 333, 364),
                     labels = c(month.abb, month.abb[1]))

ggplot(chemdailyply[chemdailyply$chemgroup == "strobin",], aes(x = jday, y = sum_lbs)) + 
  geom_bar(stat = "identity", fill = "black") + geom_line() + 
  facet_grid(Year ~chemgroup, scales = "free_x")

ggplot(chemdailyply[chemdailyply$chemgroup == "strobin",], aes(x = jday, y = sum_lbs)) + 
  geom_bar(stat = "identity", fill = "black") + stat_smooth() + 
  facet_grid(Year ~chemgroup, scales = "free_x") +
  scale_x_continuous(breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 242, 272, 303, 333, 364),
                     labels = c(month.abb, month.abb[1]))

ggplot(chemdailyply[chemdailyply$chemgroup == "other",], aes(x = jday, y = sum_lbs)) + 
  geom_bar(stat = "identity", fill = "black") + geom_line(stat = "smooth") + 
  facet_grid(Year ~chemgroup, scales = "free_x")

# create some data
d <- data.frame( 
  id=1:3, 
  qq=c('SW', 'SW', 'SE'), 
  q=c('NE', 'NW', 'SE'),
  s=c(17, 32, 30), 
  t=c('T36N', 'T35N', 'T35N'),
  r=c('R29W', 'R28W', 'R28W'),
  type='SN',
  m='MT20', stringsAsFactors = FALSE)

# add column names
names(d) <- c('id', 'qq', 'q', 's', 't', 'r', 'type', 'm')

# generate formatted PLSS codes
d$plssid <- formatPLSS(d)

# fetch lat/long coordinates
PLSS2LL(d)

}