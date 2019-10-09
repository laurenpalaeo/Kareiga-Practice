library(tidyverse)
library(readxl)
library(vegan)

#Import Excel sheets, specify which sheet, use "skip" to get rid of "junk" rows
KariegaA <- read_excel("E:/Kariega.xls", sheet = "Transect A",skip = 2)
KariegaB <- read_excel("E:/Kariega.xls", sheet = "Transect B", skip = 2)
KariegaC <- read_excel("E:/Kariega.xls", sheet = "Transect C", skip = 2)


#This should be a loop but I am a dummy. This code filters out the NA's, renames `Species Name` to something easier to type, and "fill" in order to fill empty cells - used "direction=down" to fill the cell with the above cell, mutuate was used because there was a spelling error ("living" instead of "Living") which would have made a third table. This "third table has now been "corrected" and combined with the true table.The next three lots of code are the long way to do it. Do not run these. The right and efficient way is shown below them.
KareigaA <- KariegaA %>% filter(!is.na(Status)) %>% 
  rename(Species = `Species Name`) %>% 
  fill(Species, .direction = "down") %>% 
  mutate(Status = recode(Status, "living" = "Living")) %>% 
  split(f= .$Status)

#Adapted code above for Transect B
KareigaB <- KariegaB %>% filter(!is.na(Status)) %>% 
  rename(Species = `Species Name`) %>% 
  fill(Species, .direction = "down") %>% 
  mutate(Status = recode(Status, "living" = "Living")) %>% 
  split(f= .$Status)

#Adapted code above for Transect C
KareigaC <- KariegaC %>% filter(!is.na(Status)) %>% 
  rename(Species = `Species Name`) %>% 
  fill(Species, .direction = "down") %>% 
  mutate(Status = recode(Status, "living" = "Living")) %>% 
  split(f= .$Status)


#Lets loop: Calling it "my_fun". Filterig data set to only nclude data where "Status" does not equal "NA". Renaming a column name to make coding easier (Species). Blank cells under species are eliminated by merging them with the above cell (fill). "mutate" code helps correct for a case or spelling error. Split the data set by "Status". The map function is what creates the loop. The first map funtion selects everything but status, pivot longer rearranges data eliminating "species" and leaving sample IDs and counts. The table is then made wider again adding species names(ooposite of _wider). Then to filter using the grepl function: "!" means "not", then "pattern" is what you are selecting and "^T" means "starting with T" and x = column name.
my_fun <- . %>% filter(!is.na(Status)) %>% 
  rename(Species = `Species Name`) %>% 
  fill(Species, .direction = "down") %>% 
  mutate(Status = recode(Status, "living" = "Living")) %>% 
  split(f= .$Status) %>% 
  map(select, -Status) %>% 
  map(pivot_longer,-Species, names_to = "SampleID", values_to = "Count") %>% 
  map(pivot_wider, names_from = "Species", values_from = "Count") %>% 
  map(filter, !grepl(pattern = "^T", x = SampleID)) %>% 
  map(mutate, SampleID = as.numeric(SampleID))
  
 

Kraw <- excel_sheets("Data/Kariega.xls") %>% 
  grep("^Transect", x = .,value = TRUE) %>% 
  set_names() %>% 
  map(read_excel, path = "Data/Kariega.xls", skip = 2) %>% 
  map(my_fun)

#Making objects for two tables of the merged transect tables produced above based on status "living" and "non-living"
KrawL <- Kraw %>% map_df("Living", .id = "Transect")
KrawN <- Kraw %>% map_df("Non-Living", .id = "Transect")

# The final command makes all the "NA" values produced (because some species don't exist at other transects) and changes them to "0" because they weren't found and there wasn't a column for them in the first place.
KrawN[is.na(KrawN)] <- 0
KrawL[is.na(KrawL)] <- 0

#Next: convert counts into percents per species and bring in depth or age data to make a strat plot

#Bringing in species and elevation data
KES <- read_excel("E:/Kariega.xls", sheet = "All transect data with elevatio")

KESL <- KES %>% 
  pivot_longer(-(1:5), names_to = "species", values_to = "percent")  


KESW <- KESL %>% 
  filter(!grepl(pattern = "^Tot", x = species)) %>% 
  rename(MSL = `m above sea level`) %>% 
  mutate(percent) %>% group_by(species) %>% 
  filter(max(percent) > 10) %>% 
  select(MSL, species, percent) %>% 
  pivot_wider(names_from = "species",values_from = "percent")

  
  

p <- rioja::strat.plot(KESW, yvar = KESW$"m above sea level", y.rev = TRUE, scale.percent = TRUE, srt.xlabel = 45, ylabel = "m above sea level", yTop = 0.2, xRight = 0.85, xLeft =0.25)

p <- rioja::strat.plot(KESW, yvar = KESW$age, clust = my_cluster, y.rev = TRUE, scale.percent = TRUE, srt.xlabel = 45, ylabel = "Cal. yrs. BP.", yTop = 0.2, xRight = 0.85, xLeft =0.25) %>% addClustZone(my_cluster, 9)

?split
?excel_sheets
?map #used to make a loop
