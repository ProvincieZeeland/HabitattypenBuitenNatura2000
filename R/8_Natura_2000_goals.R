library(rvest)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(openxlsx)

### Niels van Hof, nielsvanhof@proton.me
### Internship Project "Habitat types outside of Natura 2000"
### Provincie Zeeland, Radboud University
### Script 8: Natura 2000 goals

### Makes an excel file with all Natura 2000 goals in Zeeland
### Information is gathered from www.natura2000.nl
### Code was last ran on 18-03-2025
### Always check if column names etc. are still right before running!


website_overzicht <- read_html('https://www.natura2000.nl/gebieden/zeeland/')

urls_beheertypes <-
  website_overzicht %>% 
  html_nodes("body") %>%
  html_nodes("*") %>% 
  html_attr("href") %>% 
  unique()


urls_beheertypes <- urls_beheertypes[grepl(
  'gebieden/zeeland',urls_beheertypes)]

urls_beheertypes <- paste0('https://www.natura2000.nl',urls_beheertypes)
n2000_gebieden <- gsub("^.+/","",urls_beheertypes)

urls_beheertypes <- paste0(urls_beheertypes ,'/',gsub("^.+/","",urls_beheertypes),'-doelstelling/')


beheertypes_df <- data.frame(urls_beheertypes,
                             n2000_gebieden)

beheertypes_tables_list <-
  sapply(beheertypes_df$urls_beheertypes, function(i){
    website <- read_html(i)
    tables <- website %>% html_table(fill=TRUE)
    lapply(tables, function(i){
      j <- i
      colnames(j) <- gsub("\\??","",colnames(i))
      j
    })
  }, simplify = F)

LUT_tabel <-
data.frame(cols =  c("Soort/Status doel/Populatie/Omvang leefgebied/Kwaliteit leefgebied/Relatieve bijdrage/Kernopgaven",
                     "Habitattype/Habitatsubtype/Status doel/Oppervlakte/Kwaliteit/Relatieve bijdrage/Kernopgave",
                     "Soort/Status doel/Aantal broedparen/Omvang leefgebied/Kwaliteit leefgebied/Relatieve bijdrage/Kernopgaven",
                     "Soort/Status doel/Populatie/Populatie waarde/Instandhoudingsdoelstelling/Omvang leefgebied/Kwaliteit leefgebied/Relatieve bijdrage/Kernopgaven"),
           Doeltype = c("Habitatrichtlijnsoorten", "Habitattypen",
                        "Broedvogels","Niet-broedvogels"))
           
doelstellingen_df <-
sapply(names(beheertypes_tables_list), function(naam_i){
  beheertypes_tables_i <-   beheertypes_tables_list[[naam_i]]
  
  subtabel_i <- beheertypes_tables_i[[1]]
  
  sapply(beheertypes_tables_i, function(subtabel_i){
    subtabel_j<- subtabel_i %>%
      mutate_all(as.character)
    
    
    
    colnames(subtabel_j)[colnames(subtabel_j) == 'Kernopgave'] <- 'Kernopgaven'
    
    subtabel_j <-     subtabel_j[!apply(subtabel_j,1,function(i){ all(i=="")}),]
     
    subtabel_j %>% 
      mutate(Kernopgaven = as.character(Kernopgaven),
             Doeltype = LUT_tabel$Doeltype[match(paste0(colnames(subtabel_i), collapse = "/"), LUT_tabel$cols)],
             gebiedsnaam = naam_i)
    
  }, simplify = F)  %>% bind_rows()
}, simplify = F) %>% bind_rows()

## add area name
doelstellingen_df$Gebiedsnaam <- beheertypes_df$n2000_gebieden[match(doelstellingen_df$gebiedsnaam, beheertypes_df$urls_beheertypes)]

Gebiedsnrs <-
tribble(
~Gebiedsnaam,                ~Gebiedsnummer,
"canisvliet",                125,
"grevelingen",               115,
"groote-gat",                124,
"kop-van-schouwen",          116,
"krammer-volkerak",          114,
"manteling-van-walcheren",   117,
"oosterschelde",             118,
"veerse-meer" ,              119,
"vlakte-van-de-raan",        163,
"vogelkreek" ,               126,
"voordelta" ,                113,
"westerschelde-saeftinghe",  122,
"yerseke-en-kapelse-moer" ,  121,
"zwin-kievittepolder",       123)

doelstellingen_df$Gebiedsnummer <- Gebiedsnrs$Gebiedsnummer[match(doelstellingen_df$Gebiedsnaam,Gebiedsnrs$Gebiedsnaam)]

## separate column for habitat type code
doelstellingen_df$Habitatcode <- sub("^(H\\S*?)(\\*?) -.*", "\\1", doelstellingen_df$Habitattype)
## put habitattype and subtype together
doelstellingen_df$Habitattype <- ifelse(doelstellingen_df$Habitatsubtype != "",
                                             paste0(doelstellingen_df$Habitattype, " (", doelstellingen_df$Habitatsubtype,")"),
                                             doelstellingen_df$Habitattype)
## separate column for species code
doelstellingen_df$Soortcode <- sub(".*?\\b(H\\S*|A\\d{3})\\b.*", "\\1", doelstellingen_df$Soort)

## change the order of columns
doelstellingen_df <- doelstellingen_df %>%
  dplyr::select(Gebiedsnaam, Gebiedsnummer, Doeltype, Habitatcode, Habitattype, Soortcode,
                Soort, `Status doel`, Oppervlakte, Kwaliteit, Populatie, , `Populatie waarde`,
                `Aantal broedparen`, Instandhoudingsdoelstelling, `Omvang leefgebied`,
                `Kwaliteit leefgebied`, `Relatieve bijdrage`, Kernopgaven)

## sort on different columns
doelstellingen_df$Doeltype <- factor(doelstellingen_df$Doeltype, levels = c("Habitattypen", "Habitatrichtlijnsoorten", "Broedvogels", "Niet-broedvogels"))
doelstellingen_df <- doelstellingen_df %>%
  arrange(Gebiedsnaam, Doeltype, Habitatcode, Soortcode)

## change names of areas
doelstellingen_df <- doelstellingen_df %>%
  mutate(Gebiedsnaam = recode(Gebiedsnaam,
                        "canisvliet" = "Canisvliet",
                        "grevelingen" = "Grevelingen",
                        "groote-gat" = "Groote Gat",
                        "kop-van-schouwen" = "Kop van Schouwen",
                        "krammer-volkerak" = "Krammer-Volkerak",
                        "manteling-van-walcheren" = "Manteling van Walcheren",
                        "oosterschelde" = "Oosterschelde",
                        "veerse-meer" = "Veerse Meer",
                        "vlakte-van-de-raan" = "Vlakte van de Raan",
                        "vogelkreek" = "Vogelkreek",
                        "voordelta" = "Voordelta",
                        "westerschelde-saeftinghe" = "Westerschelde & Saeftinghe",
                        "yerseke-en-kapelse-moer" = "Yerseke en Kapelse Moer",
                        "zwin-kievittepolder" = "Zwin & Kievittepolder",
                        ))

## make separate sheets for the different goals
## Habitat types
habitattypen_df <- doelstellingen_df %>%
  filter(Doeltype == "Habitattypen") %>%
  dplyr::select(Gebiedsnaam, Gebiedsnummer, Habitatcode, Habitattype, `Status doel`,
                Oppervlakte, Kwaliteit, `Relatieve bijdrage`, Kernopgaven)

## Habitat directive species
habitatrichtlijnsoorten_df <- doelstellingen_df %>%
  filter(Doeltype == "Habitatrichtlijnsoorten") %>%
  dplyr::select(Gebiedsnaam, Gebiedsnummer, Soortcode, Soort, `Status doel`, Populatie, 
                `Omvang leefgebied`, `Kwaliteit leefgebied`, `Relatieve bijdrage`, Kernopgaven)

## Breeding birds
broedvogels_df <- doelstellingen_df %>%
  filter(Doeltype == "Broedvogels") %>%
  dplyr::select(Gebiedsnaam, Gebiedsnummer, Soortcode, Soort, `Status doel`, `Aantal broedparen`, 
                `Omvang leefgebied`, `Kwaliteit leefgebied`, `Relatieve bijdrage`, Kernopgaven)

## Non-breeding birds
niet_broedvogels_df <- doelstellingen_df %>%
  filter(Doeltype == "Niet-broedvogels") %>%
  dplyr::select(Gebiedsnaam, Gebiedsnummer, Soortcode, Soort, `Status doel`, `Populatie`, 
                `Populatie waarde`, Instandhoudingsdoelstelling, `Omvang leefgebied`,
                `Kwaliteit leefgebied`, `Relatieve bijdrage`, Kernopgaven)

## To excel
## One sheet per dataframe
wb <- createWorkbook()
addWorksheet(wb, "Alle doelstellingen")
writeData(wb, "Alle doelstellingen", doelstellingen_df) ## all goals
addWorksheet(wb, "Habitattypen")
writeData(wb, "Habitattypen", habitattypen_df) ## habitat types

addWorksheet(wb, "Habitatrichtlijnsoorten")
writeData(wb, "Habitatrichtlijnsoorten", habitatrichtlijnsoorten_df) ## habitats directive species

addWorksheet(wb, "Broedvogels")
writeData(wb, "Broedvogels", broedvogels_df) ## breeding birds

addWorksheet(wb, "Niet-broedvogels")
writeData(wb, "Niet-broedvogels", niet_broedvogels_df) ## non-breeding birds

## Exporteren naar .xlsx
saveWorkbook(wb, "Natura2000_doelstellingen_ZLD_20250318_Niels.xlsx", overwrite = F) ## change date when running again

