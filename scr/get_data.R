library(tidyverse)
library(xml2)

rss_link <- "https://www.parlament.gv.at/WWER/BREG/REG/filter.psp?view=RSS&jsMode=&xdocumentUri=&filterJq=&view=&REG=0&AST=J&FUNK=ALLE&RESS=ALLE&SUCH=&listeId=1016&FBEZ=FW_116"

xml_gov <- xml2::read_xml(rss_link)

df_gov <- xml_gov %>% 
  xml2::xml_find_all("//description") %>% 
  rvest::html_text() %>% 
  tibble::enframe(name=NULL, value="raw") %>% 
  mutate(raw=str_squish(raw) %>% str_trim(., side=c("both"))) %>% 
  separate(raw, 
         sep="<br />", 
         into=c("gov", "name", "position_date", "ministry", "empty"),
         fill="left",
         remove=T) %>% 
  select(-empty) %>% 
  separate(position_date,
           sep="<BR>",
           into=c("position", "date"),
           remove=T) %>% 
  separate(date,
           sep=" - ",
           into=c("date_start", "date_end"),
           remove=T
           ) %>% 
  filter(row_number()>1) %>% 
  mutate(gov=str_remove(gov, "Regierung:")) %>% 
  mutate(name=str_remove(name, "Name:")) %>% 
  mutate(name_clean=str_remove(name, regex(",.*$")) %>% #remove everything with/after comma (titles)
           str_remove_all(., regex("-?\\w*\\.")) %>% #remove titles ending with dots; hypen for eg. dipl-
           str_remove(., regex("\\(.*\\)")) %>% #remove brackets i.e. (FH)
           str_trim(., side=c("both")),
         .after=name) %>% 
  mutate(ministry=str_remove(ministry, "Bundesministerium:")) %>% 
  mutate(position=str_remove(position, "Funktion:")) %>% 
  mutate(across(.cols=everything(), 
                .fns=function(x) stringr::str_trim(x, side=c("both") %>% str_squish(.)))) %>% 
  mutate(across(.cols=contains("date"), lubridate::dmy))
  

readr::write_csv(df_gov, file=here::here("data", "df_gov.csv"))
