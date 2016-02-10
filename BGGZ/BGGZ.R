
# Toelichting -----------------------------------------------------------------------------------------------------

# Script om de gegevens van BGGZ trajecten in te lezen. 
# Het doel is niet om via dit script analyses in te doen. Het moet een basis bieden voor
# verdere analyse. Het moet gegevens voorbereiden zodat deze makkelijk ingepast kunnen worden in de 
# datastructuur zoals deze is beschreven in het data_structuur document. Deze structuur is bedoeld
# om verdere bewerkingen veel eenvoudiger te maken. Bijvoorbeeld door: 
# een eenduidige benaming, 
# geen dubbeling in data behalve wanneer noodzakelijk. 
# 
# Verder zou het mooi zijn als in de scripts een aantal validaties kunnen worden aangebracht. Dat 
# is echter nu niet in scope.




# Packages laden --------------------------------------------------------------------------------------------------

# Eigen library om paden naar bestanden in te stellen
library(mySetwd)
# externe libraries
library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)
library(readr)
library(readxl)



# Instellingen ------------------------------------------------------------

# bronmap instellen
root <- paste0(my_setdropbox("scripts"), "/00 Werkmap/00 nieuw begin/")



# Data inlezen ----------------------------------------------------------------------------------------------------



# Er zijn verschillende databestanden relevant, welke hier worden ingelezen

# Specifcaties voor de kolommen
source(file = paste0(root, "BGGZ/", "kolomspecificaties_BGGZ.R"))


# trajecten
bggz_traject <- read_excel(path = paste0(my_setwd(set = FALSE), "/", "Data", "/", "bggz_traject_onderhanden_werk.xlsx"), 
                           col_types = colspecs_bggz_traject)

# sessies
bggz_sessies <- read_excel(path = paste0(my_setwd(set = FALSE), "/", "Data", "/", "BGGZ_Onderhanden_Werk.xlsx"),
                           col_types = colspecs_bggz_sessies)






# Opschonen -------------------------------------------------------------------------------------------------------

verwijder_null <- function(x) {
      if (is.character(x)) {
            # Check of er NULL voorkomt in de kolom
            hass_null <- FALSE
            hass_null <- sum(x == "NULL", na.rm = TRUE) > 0
            if(hass_null) {
                  x <- sub("NULL", "", x)
            }
            
      }
      return(x)
}

# Nulls vervangen in beide bestanden
bggz_traject <- bggz_traject %>% 
      mutate_each(funs(verwijder_null))


bggz_sessies <- bggz_sessies %>% 
      mutate_each(funs(verwijder_null))



# Datums ----------------------------------------------------------------------------------------------------------

# kolommem met datums in trajectenbestand
datums <- c(which(names(bggz_traject) %in% c("laatst_geplande_BGGZ_sessie", "laatst_uitgevoerde_BGGZ_sessie")),
            which(grepl("[Dd]atum", names(bggz_traject))))

# datums converteren
bggz_traject[, datums] <- lapply(bggz_traject[, datums], dmy)


# kolommen met datums in sessiebestand
datums <- c(which(grepl("[Dd]atum", names(bggz_sessies))))

# datums converteren
bggz_sessies[, datums] <- lapply(bggz_sessies[, datums], dmy)




# kolommen opschonen traject --------------------------------------------------------------------------------------

trajecten_bggz <- bggz_traject %>% 
      select(bsn = BurgerServiceNummer, startdatum = startdatumbggz, einddatum = einddatumbggz, 
             zvz_initieel = ZorgvraagzwaarteInitieel, zvz_actueel = ZorgvraagzwaarteActueel, 
             dossierid = Dossierid, bggzid = basisggztrajectid) %>% 
      as.data.table()


# kolommen opschonen sessies --------------------------------------------------------------------------------------

sessies_bggz <- bggz_sessies %>% 
      select(sessiedatum = starttijd, sessienaam, sessiestatus = Sessiestatus, behandelaar = Therapeut, 
             tijd_direct = duur, tijd_indirect = IndirecteTijd, tijd_uitwerk = UitwerktijdDuur, 
             dossierid = Dossierid, bggzid = basisggztrajectid, sessieid = sessie_id) %>% 
      as.data.table()


# aanvullen -------------------------------------------------------------------------------------------------------

# Om de een of andere reden is de verzekeraar vastgelegd op sessieniveau, 
# maar niet op dossierniveau. 
# Eerst apart zetten
uzovi <- bggz_sessies %>% 
      distinct(Dossierid) %>% 
      select(dossierid = Dossierid, uzovi = ZorgverzekeraarHuidigJaar) %>% 
      as.data.table()
# dan samenvoegen
trajecten_bggz <- merge(trajecten_bggz, uzovi, by = "dossierid", all.x = TRUE)

# dan lijst weer weggooien
rm(uzovi)


# Het is handig om per traject te kunnen zien hoeveel tijd er is geschreven. 
# onderverdeeld in uitgevoerde sessies en geplande
tijden <- sessies_bggz %>%
      mutate(status = ifelse(sessiestatus ==  "Uitgevoerd", "tijd_totaal", "tijd_totaal_planning")) %>% 
      group_by(dossierid, status) %>% 
      summarize(tijd = sum(tijd_direct + tijd_indirect + tijd_uitwerk)) %>% 
      spread(key = status, value = tijd, fill = 0)

# toevoegen aan trajecten
trajecten_bggz <- merge(trajecten_bggz, tijden, by = "dossierid", all.x = TRUE)

# dan lijst weer weggooien
rm(tijden)


# Opslaan -----------------------------------------------------------------
save()






