#########################################################
#####  Data Incubator Challenge: Fisheries Project  #####
#####  Rachael E. Blake                             #####
#####  April 26 - 30 2018                           #####
#########################################################

###################
# load packages
library(tidyverse) ; library(httr) ; library(XML) ; library(rvest) ; library(lavaan)

###################
# download the fisheries data for all Pacific states
# NOTE: this comes from :
# https://www.st.nmfs.noaa.gov/commercial-fisheries/commercial-landings/monthly-landings/index
fish <- read.csv("./MF_MONTHLY_LANDINGS_PACIFIC.RESULTS", skip=5, header=TRUE, 
                 stringsAsFactors=FALSE, strip.white=TRUE)
fish$Metric.Tons <-  str_trim(fish$Metric.Tons)
fish$Pounds <- str_trim(fish$Pounds)
fish$X. <- str_trim(fish$X.)

fish1 <- fish %>% 
         rename(Revenue=X.) %>%
         filter(State != "Utah") %>% # remove Utah
         mutate(Pounds = as.numeric(gsub(",", "", Pounds)),
                Revenue = as.numeric(gsub(",", "", Revenue))) %>%
         mutate(Species_Group = ifelse(Species %in% c("CLAM, ATLANTIC JACKKNIFE", "CLAM, PACIFIC GEODUCK",
                                                      "CLAM, PACIFIC LITTLENECK", "CLAMS OR BIVALVES", 
                                                      "SHELLFISH", "MUSSEL, CALIFORNIA", "OYSTER, EASTERN",
                                                      "OYSTER, EUROPEAN FLAT", "OYSTER, KUMAMOTO", 
                                                      "OYSTER, OLYMPIA", "OYSTER, PACIFIC", "CLAM, BUTTER",
                                                      "CLAM, PACIFIC RAZOR", "CLAM, PACIFIC, GAPER", 
                                                      "CLAM, SOFTSHELL", "COCKLE, NUTTALL", "MUSSELS, FW",
                                                      "CLAM, MANILA", "MUSSEL, BLUE"), "Shellfish", 
                                ifelse(Species %in% c("CRAB, DUNGENESS", "CRAB, KING", "CRAB, SNOW", "CRAB, RED ROCK",
                                                      "CRAB, SOUTHERN TANNER", "CRABS", "CRAB, SNOW/TANNER",
                                                      "CRABS "), "Crabs", 
                                ifelse(Species %in% c("FINFISHES, GROUNDFISHES, OTHER", "FINFISHES, UNC FOR FOOD",
                                                      "FINFISHES, UNC GENERAL", "FINFISHES, FW, OTHER"), "Finfishes", 
                                ifelse(Species %in% c("FLATFISH", "FLOUNDER, ARROWTOOTH", "FLOUNDER, STARRY", 
                                                      "FLOUNDERS, RIGHTEYE", "FLOUNDER, PACIFIC, SANDDAB",
                                                      "FLOUNDER,PACIFIC,SANDDAB"), "Flatfish", 
                                ifelse(Species %in% c("HALIBUT, GREENLAND", "HALIBUT, PACIFIC", 
                                                      "HALIBUT, CALIFORNIA"), "Halibut", 
                                ifelse(Species %in% c("HERRING, PACIFIC", "HERRING, PACIFIC, ROE ON KELP", 
                                                      "HERRING, ROUND", "HERRING, LAKE OR CISCO", "HERRINGS",
                                                      "HERRING, ATLANTIC"), "Herring", 
                                ifelse(Species %in% c("ROCKFISH, BLACK", "ROCKFISH, BLUE", "ROCKFISH, BOCACCIO",
                                                      "ROCKFISH, CANARY", "ROCKFISH, CHINA", "ROCKFISH, COPPER", 
                                                      "ROCKFISH, DARKBLOTCHED", "ROCKFISH, GREENSTRIPED",
                                                      "ROCKFISH, PACIFIC OCEAN PERCH", "ROCKFISH, REDBANDED", 
                                                      "ROCKFISH, REDSTRIPE", "ROCKFISH, SHARPCHIN", "ROCKFISH, STARRY", 
                                                      "ROCKFISH, SILVERGRAY",  "ROCKFISH, VERMILION", "ROCKFISH, WIDOW", 
                                                      "ROCKFISH, YELLOWEYE", "ROCKFISH, YELLOWMOUTH", 
                                                      "ROCKFISH, YELLOWTAIL", "ROCKFISHES", "ROCKFISH, AURORA",
                                                      "ROCKFISH, BANK", "ROCKFISH, BLACK-AND-YELLOW", "ROCKFISH, BLACKGILL",
                                                      "ROCKFISH, BROWN", "ROCKFISH, CHILIPEPPER", "ROCKFISH, COWCOD",
                                                      "ROCKFISH, DEACON", "ROCKFISH, FLAG",  "ROCKFISH, GOPHER",
                                                      "ROCKFISH, GRASS", "ROCKFISH, GREENBLOTCHED", "ROCKFISH, GREENSPOTTED",
                                                      "ROCKFISH, HONEYCOMB", "ROCKFISH, KELP", "ROCKFISH, OLIVE",
                                                      "ROCKFISH, ROSY", "ROCKFISH, SHORTBELLY", "ROCKFISH, SPECKLED", 
                                                      "ROCKFISH, SPLITNOSE", "ROCKFISH, SQUARESPOT", "ROCKFISH, STRIPETAIL",
                                                      "ROCKFISH, TREEFISH"), "Rockfish", 
                                ifelse(Species %in% c("SALMON, CHINOOK", "SALMON, CHUM", "SALMON, COHO",
                                                      "SALMON, PACIFIC", "SALMON, PINK", "SALMON, SOCKEYE"), "Salmon", 
                                ifelse(Species %in% c("SCALLOP, SEA", "SCALLOP, WEATHERVANE", "SCALLOPS"), "Scallops", 
                                ifelse(Species %in% c("SHARK, DOGFISH", "SHARK, SPINY DOGFISH", "SHARKS",
                                                      "SHARK, BIGEYE THRESHER", "SHARK, BLUE", "SHARK, LEOPARD", 
                                                      "SHARK, PACIFIC ANGEL", "SHARK, SHORTFIN MAKO", "SHARK, SOUPFIN", 
                                                      "SHARK, THRESHER", "THRESHER SHARKS", "SHARK, HAMMERHEAD",
                                                      "SHARK, MAKOS", "SHARK, OCEANIC WHITETIP"), "Sharks", 
                                ifelse(Species %in% c("SMELT, EULACHON", "SMELTS"), "Smelts", 
                                ifelse(Species %in% c("SOLE, BUTTER", "SOLE, DOVER", "SOLE, ENGLISH", "SOLE, FLATHEAD", 
                                                      "SOLE, PETRALE", "SOLE, REX", "SOLE, ROCK", "SOLE, SAND", 
                                                      "SOLE, YELLOWFIN", "SOLE, BIGMOUTH", "SOLE, CURLFIN",
                                                      "SOLE, DEEPSEA", "SOLE, FANTAIL", "SOLE, SLENDER"), "Sole", 
                                ifelse(Species %in% c("SQUID, JUMBO", "SQUIDS", "SQUID, ROBUST CLUBHOOK", 
                                                      "SQUID, CALIFORNIA MARKET"), "Squids", 
                                ifelse(Species %in% c("ABALONE, BLACK",  "ABALONE, GREEN", "ABALONE, PINK", 
                                                      "ABALONE, RED", "ABALONES"), "Abalone", 
                                ifelse(Species %in% c("JACK MACKEREL", "MACKEREL, CHUB", "ATKA MACKEREL", 
                                                      "MACKEREL, FRIGATE", "SNAKE MACKEREL", "MACKEREL, ATLANTIC"), "Mackerel", 
                                ifelse(Species %in% c("SEAWEED, KELP", "SEAWEEDS", "ALGAE, MARINE"), "Algae",
                                ifelse(Species %in% c("SHRIMP, PENAEID", "SHRIMP, BRINE", "SHRIMP, GHOST", 
                                                      "SHRIMP, OCEAN", "SHRIMP, PACIFIC ROCK", "SHRIMP, SPOT",
                                                      "SHRIMP, MARINE, OTHER", "SHRIMP, BLUE MUD"), "Shrimp",
                                ifelse(Species %in% c("TUNA, BIGEYE", "TUNA, BLACK SKIPJACK", "TUNA, BLUEFIN PACIFIC", 
                                                      "TUNA, SKIPJACK", "TUNA, YELLOWFIN", "TUNAS", "TUNA, ALBACORE",
                                                      "TUNA, KAWAKAWA"), "Tuna",
                                ifelse(Species %in% c("YELLOWTAIL JACK", "AMBERJACK", "JACK, ALMACO", 
                                                      "JACK, BLACK", "JACKS"), "Jacks",
                                ifelse(Species %in% c("LOBSTER, BANDED SPINY", "LOBSTER, CARIBBEAN SPINY",
                                                      "LOBSTER, SLIPPER", "LOBSTER, CALIFORNIA SPINY"), "Lobster",
                                ifelse(Species %in% c("MARLIN, BLACK", "MARLIN, BLUE", "MARLIN, STRIPED", 
                                                      "SWORDFISH", "BILLFISHES"), "Billfish",
                                ifelse(Species %in% c("MULLET, STRIPED (LIZA)", "MULLETS"), "Mullet",
                                ifelse(Species %in% c("BARRACUDA, PACIFIC", "BARRACUDAS"), "Baracuda", 
                                ifelse(Species %in% c("GREENLING, KELP", "GREENLINGS"), "Greenling",
                                ifelse(Species %in% c("SCAD, BIGEYE", "SCAD, MACKEREL"), "Scad",
                                ifelse(Species %in% c("SEA BASS, GIANT", "SEABASS, WHITE"), "Seabass",
                                ifelse(Species %in% c("SKATE, BIG", "SKATE, CALIFORNIA"), "Skates",
                                ifelse(Species %in% c("STURGEON, GREEN", "STURGEON, WHITE"), "Sturgeon",
                                ifelse(Species %in% c("THORNYHEAD, LONGSPINE", "THORNYHEAD, SHORTSPINE"), "Thornyhead",
                                ifelse(Species %in% c("SNAPPER, RED", "SNAPPERS"), "Snapper", Species)))))))))))))))))))))))))))))))

fish2 <- fish1 %>%
         select(-Species, -Metric.Tons) %>%
         group_by(Species_Group, Year, State) %>%
         summarize(Pounds = sum(Pounds),
                   Revenue = sum(Revenue)) %>%  
         ungroup() %>%
         arrange(Year, State, Species_Group) 

fish3 <- fish2 %>%
         select(-Species_Group) %>%
         group_by(Year, State) %>%
         summarize(Pounds = sum(Pounds),
                   Revenue = sum(Revenue)) %>%
         ungroup()
         
  
         
###################
# download Multivariate El Nino Southern Oscillation Index 

URL_enso <- "http://www.esrl.noaa.gov/psd/enso/mei/table.html"
enso_pre <- xpathSApply(htmlParse(content(GET(URL_enso))),"//html/body/pre", xmlValue)
enso_cols <- scan(textConnection(enso_pre), skip=10, nlines=1, what=character()) # get header row
enso <- read.csv(file=textConnection(enso_pre), skip=11, stringsAsFactors=F, sep="\t", 
                 header=FALSE, col.names=enso_cols)
enso_df <- enso[1:69,]  # removes the text at bottom of file

enso_df1 <- enso_df %>%
            dplyr::rename(Year=YEAR) %>% # rename data columns
            gather(Months, ENSO, -Year) %>% # reshapes data to be column-wise
            filter(!is.na(ENSO)) # remove NA values

ENSO_ann <- enso_df1 %>%
            group_by(Year) %>%
            summarise(ENSO_anul_mn=mean(ENSO)) %>% # get annual means
            ungroup()  

###################
# download the Pacific Decadal Oscillation Index  

URL_pdo <- "http://jisao.washington.edu/pdo/PDO.latest"
pdo_raw <- read_html(URL_pdo)
pdo_pre <- pdo_raw %>% 
           html_node("p") %>%
           html_text()
pdo_cols <- scan(textConnection(pdo_pre), skip=31, nlines=1, what=character())# Get header row
pdo_df <- read.table(file=textConnection(pdo_pre), skip=32, nrows=118, stringsAsFactors=F, sep="", 
                     header=FALSE, col.names=pdo_cols, strip.white=TRUE, fill=TRUE)
pdo_df$YEAR <- substr(pdo_df$YEAR, 1, 4)  # removes asterisks from years 2002-2015
#
pdo_ann <- pdo_df %>% 
           dplyr::rename(Year=YEAR) %>% # rename data columns         
           gather(Month, PDO, -Year) %>% # reshapes data to be column-wise
           group_by(Year) %>%
           summarise(PDO_anul_mn=mean(as.numeric(as.character(PDO)), na.rm = TRUE)) %>% # get annual means
           ungroup() 

###################
# download Pacific Upwelling Anomalies data
# MONTHLY UPWELLING ANOMALIES (units are cubic meters/second/100 meters of coastline)
URL_upanom <- "http://www.pfeg.noaa.gov/products/PFELData/upwell/monthly/upanoms.mon"
upanom_raw <- read_html(URL_upanom)
upanom_pre <- upanom_raw %>% 
              html_node("p") %>%
              html_text()
upanom_cols <- scan(textConnection(upanom_pre), skip=2, nlines=1, what=character())# Get header row
upanom_cols <- c("Lat", "Long", upanom_cols[-1])# split position into lat and long 
upanom_df <- read.csv(file=textConnection(upanom_pre), skip=4, stringsAsFactors=F, sep="", 
                      header=FALSE, col.names=upanom_cols, strip.white=TRUE)

upanom_df1 <- upanom_df %>%
             # filter(Long %in% c("146W","149W")) %>%  # subsets for the two sites in the GOA
              dplyr::rename(Year=YEAR) %>% # rename data columns
              gather(Month, UpwelAnom,-Year,-Lat,-Long)  # reshapes data to be column-wise

upanom_ann <- upanom_df1 %>%
              mutate(Year = as.character(Year)) %>%
              group_by(Year) %>%
              summarise(UpWelAnom_anul_mn=mean(UpwelAnom, na.rm = TRUE)) %>% # get annual means
              ungroup() 

###################
# make one large dataset
fish_env <- fish2 %>%
            mutate(Year = as.character(Year)) %>%
            full_join(ENSO_ann, by="Year") %>%
            full_join(pdo_ann, by="Year") %>%
            full_join(upanom_ann, by="Year") %>%
            filter(Year %in% c(1990:2016)) %>%
            mutate(Year = as.numeric(Year)) #%>%
            #select(-Pounds)

###################
# make state-level datasets

AK_fe <- fish_env %>%
         filter(State == "Alaska")

ASP_fe <- fish_env %>%
          filter(State == "At-Sea Process, Pac.")

CA_fe <- fish_env %>%
         filter(State == "California")

HI_fe <- fish_env %>%
         filter(State == "Hawaii")

OR_fe <- fish_env %>%
         filter(State == "Oregon")

WA_fe <- fish_env %>%
         filter(State == "Washington")

###################
# which species is most valuable per state?
val_species <- function(df){
               df %>% 
               select(-Year, -ENSO_anul_mn, -PDO_anul_mn, -UpWelAnom_anul_mn) %>%
               group_by(Species_Group, State) %>%
               summarize(Pounds = sum(Pounds),
                         Revenue = sum(Revenue)) %>%
               ungroup() %>%
               slice(which.max(Revenue))
}

AK_max <- val_species(AK_fe)

CA_max <- val_species(CA_fe)

HI_max <- val_species(HI_fe)

OR_max <- val_species(OR_fe)

WA_max <- val_species(WA_fe)

all_st_max <- AK_max %>% full_join(CA_max) %>% full_join(HI_max) %>% full_join(OR_max) %>%
              full_join(WA_max)


###################
# does the most valuable species change over time?
val_sp_time <- function(df){
               df %>% 
               select(-ENSO_anul_mn, -PDO_anul_mn, -UpWelAnom_anul_mn) %>%
               group_by(Species_Group, Year, State) %>%
               summarize(Pounds = sum(Pounds),
                         Revenue = sum(Revenue)) %>%
               ungroup() %>%
               group_by(Year) %>%
               slice(which.max(Revenue)) %>%
               ungroup()
}

AK_yr_max <- val_sp_time(AK_fe)

CA_yr_max <- val_sp_time(CA_fe)

HI_yr_max <- val_sp_time(HI_fe)

OR_yr_max <- val_sp_time(OR_fe)

WA_yr_max <- val_sp_time(WA_fe)

all_time_max <- AK_yr_max %>% full_join(CA_yr_max) %>% full_join(HI_yr_max) %>% 
                full_join(OR_yr_max) %>% full_join(WA_yr_max) %>%
                mutate(Species_Group = str_to_title(Species_Group))

###################
# price per pound
ppp <- all_time_max %>%
       mutate(PricePPound = Revenue/Pounds)


###################
# species diversity per state

sp_div <- function(df, state){
          e <- df %>%
               filter(State == state) %>%
               group_by(Year, State) %>%
               count()
          
          f <- e %>%
               group_by(State) %>%
               summarize(MN_sp_num = mean(n),
                         SD_sp_num = sd(n))
          
          return(f)
}

sp_div2 <- function(df, state){
           e <- df %>%
           filter(State == state) %>%
           group_by(Year, State) %>%
           count()
 
           return(e)
}
  
AK_div <- sp_div(fish2, "Alaska")

CA_div <- sp_div(fish2, "California")

HI_div <- sp_div(fish2, "Hawaii")

OR_div <- sp_div(fish2, "Oregon")

WA_div <- sp_div(fish2, "Washington")

all_div <- AK_div %>% full_join(CA_div) %>% full_join(HI_div) %>% full_join(OR_div) %>%
           full_join(WA_div)


AK_div2 <- sp_div2(fish2, "Alaska")

CA_div2 <- sp_div2(fish2, "California")

HI_div2 <- sp_div2(fish2, "Hawaii")

OR_div2 <- sp_div2(fish2, "Oregon")

WA_div2 <- sp_div2(fish2, "Washington")

all_div2 <- AK_div2 %>% full_join(CA_div2) %>% full_join(HI_div2) %>% full_join(OR_div2) %>%
  full_join(WA_div2)


################################################################################################
## NOTE: This analysis below is still in development and not yet complete
################################################################################################
# do environmental variables influence highest revenue fisheries species?
val_env <- all_time_max %>%
           mutate(Year = as.character(Year)) %>%
           mutate(Sp_Dum_Var = ifelse(Species_Group == "Billfish", "1",
                               ifelse(Species_Group == "Crabs", "2",
                               ifelse(Species_Group == "Pollock, Walleye", "3",
                               ifelse(Species_Group == "Rockfish", "4",
                               ifelse(Species_Group == "Salmon", "5",
                               ifelse(Species_Group == "Sea Urchins", "6",
                               ifelse(Species_Group == "Shellfish", "7",
                               ifelse(Species_Group == "Shrimp", "8",
                               ifelse(Species_Group == "Squids", "9",
                               ifelse(Species_Group == "Tuna", "10", ""))))))))))) %>%
           full_join(ENSO_ann, by="Year") %>%
           full_join(pdo_ann, by="Year") %>%
           full_join(upanom_ann, by="Year") %>%
           filter(Year %in% c(1990:2016)) %>%
           mutate(Year = as.numeric(Year))

val_lm1 <- lm(Sp_Dum_Var~ENSO_anul_mn*UpWelAnom_anul_mn, data=val_env)

val_lm2 <- lm(Sp_Dum_Var~PDO_anul_mn*UpWelAnom_anul_mn, data=val_env)

val_lm3 <- lm(Revenue~Sp_Dum_Var, data=val_env)


val_env_time <- function(df, state){
                a <- df %>%
                     filter(State == state)
                b <- lm(Revenue~Sp_Dum_Var, data=a)
                c <- lm(Sp_Dum_Var~ENSO_anul_mn*UpWelAnom_anul_mn, data=a)
                d <- lm(Sp_Dum_Var~PDO_anul_mn*UpWelAnom_anul_mn, data=a)
                return(list(summary(b), summary(c), summary(d)))
}

AK_et <- val_env_time(val_env, "Alaska")

CA_et <- val_env_time(val_env, "California")

HI_et <- val_env_time(val_env, "Hawaii")

OR_et <- val_env_time(val_env, "Oregon")

WA_et <- val_env_time(val_env, "Washington")



###################
# 
AK_fe2 <- AK_fe %>%
         filter(Species_Group == "Salmon")


CA_fe2 <- CA_fe %>%
          filter(Species_Group == "Crabs")
moda <- lm(Pounds~PDO_anul_mn, data=CA_fe2)
routa <- list(paste('Fitted model:',round(coef(moda)[1],3),' + ',
                    round(coef(moda)[2],3), 'x',sep = ''),
              paste('R^2 == ',round(summary(moda)[['r.squared']],3),
                    sep=''))
ca_lm <- ggplot(data=CA_fe2, aes(y=Pounds, x=PDO_anul_mn)) + 
         geom_point() + 
         xlab("PDO") + ylab("Pounds of Crabs") +
         theme_bw() + geom_smooth(method=lm) +
         geom_text(aes(x=-1, y=1, label=routa[[2]]),
                   hjust=0, parse=TRUE) 
ca_lm


HI_fe2 <- HI_fe %>%
          filter(Species_Group == "Tuna")
modb <- lm(Pounds~PDO_anul_mn, data=HI_fe2) 
routb <- list(paste('Fitted model:',round(coef(modb)[1],3),' + ',
                    round(coef(modb)[2],3), 'x',sep = ''),
              paste('R^2 == ',round(summary(modb)[['r.squared']],3),
                    sep=''))
hi_lm <- ggplot(data=CA_fe2, aes(y=Pounds, x=PDO_anul_mn)) + 
         geom_point() + 
         xlab("PDO") + ylab("Pounds of Tuna") +
         theme_bw() + geom_smooth(method=lm) +
         geom_text(aes(x=-1, y=1, label=routb[[2]]),
                   hjust=0, parse=TRUE) 
hi_lm
  


OR_fe 
WA_fe 


###################

###################
