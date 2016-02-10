
# Packages laden ----------------------------------------------------------
#workspace leegmaken.
rm(list = ls())

#benodigde packages laden.
library(data.table)
library(tidyr) 
library(lubridate)
library(dplyr)
library(readr)
library(stringr)
library(mySetwd) 


# Instellingen ----------------------------------------------------------------------------------------------------

# Pad naar werkmap 
root <- my_setwd(set = FALSE)
# Pad naar dropboxmap scripts
scriptmap <- my_setdropbox("scripts")



kolomspecificatie_sessies <- cols(
      Client_ID = col_skip(),
      Client_Naam = col_skip(),
      Client_GeboorteDatum = col_skip(),
      Dossier_ID = col_character(),
      Dossier_Aanmelddatum  = col_skip() ,
      Dossier_Aanmeld_Maand = col_skip(),
      Dossier_Aanmeld_Jaar = col_skip(),
      Dossier_Aanmeld_Week = col_skip(),
      Vestiging = col_skip(),
      behandellocatie = col_character(),
      Klant_Naam  = col_skip(),
      Product_ID = col_character(),
      Product_Naam = col_character(),
      Product_Type = col_character(),
      Product_Status = col_character(),
      Sessie_ID = col_character(),
      Sessie_Naam = col_character(),
      Behandelaar = col_skip(),
      Sessie_Status = col_character(),
      Sessie_Datum = col_datetime(format = "%d-%m-%Y %H:%M"),
      Sessie_Maand = col_skip(),
      Sessie_Jaar = col_skip(),
      Sessie_Week = col_skip(),
      bedrijfscode = col_character(),
      bedrijfscode_1e_oz = col_character(),
      bedrijfscode_1e_beh = col_character(),
      duur = col_integer(),
      UitwerktijdDuur = col_integer(),
      IndirecteTijd = col_integer(),
      Behandelaarcorrect = col_character(),
      bggz_activiteit = col_character(),
      sggz_activiteit = col_character()
)



# inlezen ---------------------------------------------------------------------------------------------------------

# sessies <- read_csv2(file = paste0(root, "/", "Data", "/", "Sessie Brongegevens.csv"), 
#                      col_types = kolomspecificatie_sessies)

sessies <- read.csv2(file = paste0(root, "/", "Data", "/", "Sessie Brongegevens.csv"), 
                     fileEncoding = "windows-1252", stringsAsFactors = FALSE)








# kolommen opschonen ----------------------------------------------------------------------------------------------

sessies <- sessies %>% 
      select(locatie = behandellocatie, sessienaam = Sessie_Naam, sessiestatus = Sessie_Status, 
             sessiedatum = Sessie_Datum, behandelaar = Behandelaarcorrect, tijd_direct = duur, 
             tijd_uitwerk = UitwerktijdDuur, tijd_indirect =  IndirecteTijd, productnaam = Product_Naam, 
             producttype = Product_Type, bggz_activiteit, sggz_activiteit, bedrijfscode, dossierid = Dossier_ID, 
             sessieid = Sessie_ID, productid = Product_ID)

# sessies$behandelaar <- iconv(sessies$behandelaar, to = 'windows-1252')



# Datums aanpassen --------------------------------------------------------

sessies$sessiedatum <- dmy_hm(sessies$sessiedatum)


# aanpassingen ----------------------------------------------------------------------------------------------------

# het woord "Vestiging" verwijderen uit de locatie. 
sessies$locatie <- str_trim(sub("Vestiging", "", sessies$locatie)) 


# type van de sessie afleiden
sessies <- sessies %>% 
      mutate(type = "privaat",  
             type = ifelse(!is.na(bggz_activiteit), "BGGZ", type), 
             type = ifelse(!is.na(sggz_activiteit), "SGGZ", type), 
             type = ifelse(bedrijfscode == "PiP BV", "ELP", type))




# Opslaan ---------------------------------------------------------------------------------------------------------

save(sessies, file = paste0(root, "/", "Data", "/", "sessies.Rdata"))




