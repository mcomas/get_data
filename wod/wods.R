library(tidyverse)
library(rvest)

wods = tibble(id = numeric(0), type = character(0), name = character(0), url = character(0), wod = list())
if(file.exists('wods.RData')) load('wods.RData')

load('wod_info.RData')
wod_info_new = wod_info %>%
  anti_join(wods, by = 'url')

if(nrow(wod_info_new) > 0){
  login = "https://roxe.wodbuster.com/user"
  pgsession = html_session(login)
  pgform = html_form(pgsession)[[1]]
  
  filled_form = set_values(
    pgform, 
    `ctl00$ctl00$ctl00$ctl00$ctl00$body$body$body$body$body$IoEmail` = "", 
    `ctl00$ctl00$ctl00$ctl00$ctl00$body$body$body$body$body$IoPassword` = "")
  submit_form(pgsession, filled_form)
  
  get_wod = function(I, pgsession){
    url = sprintf("https://roxe.wodbuster.com/benchmarks/benchmark.aspx?id=%d", I)
    page = jump_to(pgsession, url)
    html_text(html_nodes(page, xpath = '//p//text()'))
  }
  wods_new = wod_info_new %>%
    mutate(wod = map(id, get_wod, pgsession))
  wods = bind_rows(wods,
                   wods_new)
  save(wods, file = 'wods.RData')
}

