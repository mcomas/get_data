# Data is available at http://resultados.eleccionesgenerales19.es
# 
# download.file('https://resultados.eleccionesgenerales19.es/pdf/G2019-CO-MUNI_L1.pdf', 'elecciones_generales_28_abril_2019/municipis.pdf')

library(pdftools)

library(stringr)
library(readr)
library(dplyr)
library(tidyr)

l_text = pdf_text('elecciones_generales_28_abril_2019/municipis.pdf')
l_pages <- strsplit(l_text, "\n")
all_results = lapply(l_pages, function(page){
  # page = l_pages[[224]]
  to_skip = grepl("Congreso - Municipios", page, fixed = TRUE) |
    grepl("Resultados provisionales", page, fixed = TRUE) |
    grepl("Actualizado a las", page, fixed = TRUE) |
    grepl("Escrutado", page, fixed = TRUE) |
    grepl("Total votantes", page, fixed = TRUE) |
    grepl("Abstención", page, fixed = TRUE) |
    grepl("Votos nulos", page, fixed = TRUE) |
    grepl("Votos en blanco", page, fixed = TRUE) |
    grepl("Página", page, fixed = TRUE)
  
  prov_line = grep("Resultados provisionales", page, fixed = TRUE, value = T)
  prov = str_trim(gsub("Resultados provisionales", "", prov_line))
  
  page_width = max(sapply(page, nchar))
  page_cl = str_pad(page[!to_skip], page_width, 'right')
  
  is_reference = grepl("\\s+%", page_cl) # Reference point to find each table is going to be rows with dollars symbols
  is_vote = str_sub(page_cl, 1, 1) != " "
  
  p_ref = which(is_reference)
  p_from = lag(p_ref, default = 0)
  p_to = lead(p_ref, default = length(page_cl))
  
  get_information = function(from_, ref_, to_){
    # itab = 1
    # from_ = p_from[itab]
    # ref_ = p_ref[itab]
    # to_ = p_to[itab]
    
    iheader = which(!is_reference & !is_vote)
    iheader = iheader[from_ <= iheader & iheader < ref_]
    
    ivotes = which(is_vote)
    ivotes = ivotes[ref_ < ivotes & ivotes < to_]
    
    # page_cl[iheader]
    # page_cl[ivotes]
    
    line_votes = str_trim(str_sub(page_cl[ivotes], 36))
    votes = read_table2(paste(line_votes, collapse='\n'), col_names = FALSE, locale = locale(decimal_mark = ','))
    parties = str_trim(str_sub(page_cl[ivotes], 1, 35))
    
    line_percentage = page_cl[ref_-1]
    p_from = str_locate_all(line_percentage, "[:space:][:space:][^[:space:]]")[[1]][,2]
    p_to = lead(p_from, default = page_width + 1) - 1
    col_positions = fwf_positions(p_from, p_to)
    
    lines = c(str_pad("", page_width), page_cl[iheader])
    df_names = read_fwf(paste(lines, collapse = '\n'), col_positions = col_positions, skip = 1)
    municipis = apply(df_names, 2, function(x) paste(na.omit(x), collapse = " "))
    
    df = votes[,seq(1, ncol(votes), 2)]
    names(df) = municipis
    df$party = parties
    df %>%
      gather(municipi, vots, -party) %>%
      mutate(prov = prov)
  }
  votes = mapply(get_information, p_from, p_ref, p_to, SIMPLIFY = FALSE) %>% bind_rows()
  votes
})

elec.results = all_results %>%
  bind_rows()


# Filtering information in Girona
elec.results %>%
  filter(prov == 'Girona') %>%
  spread(party, vots, fill = 0)
