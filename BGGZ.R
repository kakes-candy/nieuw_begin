
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



# Data inlezen ----------------------------------------------------------------------------------------------------

# Er zijn verschillende databestanden relevant, welke hier worden ingelezen

# trajecten
bggz_traject <- read_excel(path = paste0(my_setwd(set = FALSE), "/", "Data", "/", "bggz_traject_onderhanden_werk.xlsx"))

# sessies
bggz_sessies <- read_excel(path = paste0(my_setwd(set = FALSE), "/", "Data", "/", "BGGZ_Onderhanden_Werk.xlsx"))





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
datums <- c(which(names(bggz_traject) %in% c("laatst_uitgevoerde_BGGZ_sessie")),
            which(grepl("[Dd]atum", names(bggz_traject))))


bggz_traject[, datums] <- lapply(bggz_traject[, datums], dmy)


# kolommen met datums in sessiebestand
datums <- c(which(grepl("[Dd]atum", names(bggz_traject))))

bggz_sessies[, datums] <- lapply(bggz_sessies[, datums], dmy)





