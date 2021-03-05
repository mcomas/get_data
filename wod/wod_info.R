library(rvest)
library(purrr)

login = "https://roxe.wodbuster.com/user"
pgsession = html_session(login)
pgform = html_form(pgsession)[[1]]

filled_form = set_values(
  pgform, 
  `ctl00$ctl00$ctl00$ctl00$ctl00$body$body$body$body$body$IoEmail` = "", 
  `ctl00$ctl00$ctl00$ctl00$ctl00$body$body$body$body$body$IoPassword` = "")
submit_form(pgsession, filled_form)

get_wod_id = function(url){
  page = jump_to(pgsession, url)
  a_list = html_nodes(page, xpath = "//a[contains(@class, 'panel')]")
  tibble(
    name = map_chr(a_list, ~str_sub(html_text(.x), 1, -3)),
    url = map_chr(a_list, ~xml_attr(.x, 'href'))
  )
}

dwod = list(
  girls = get_wod_id("https://roxe.wodbuster.com/benchmarks/girls.aspx"),
  heroes = get_wod_id("https://roxe.wodbuster.com/benchmarks/heroes.aspx"),
  weights = get_wod_id("https://roxe.wodbuster.com/benchmarks/weights.aspx"),
  distances = get_wod_id("https://roxe.wodbuster.com/benchmarks/distances.aspx"),
  challenges = get_wod_id("https://roxe.wodbuster.com/benchmarks/challenges.aspx"),
  qualifiers = get_wod_id("https://roxe.wodbuster.com/benchmarks/qualifiers.aspx"),
  programs = get_wod_id("https://roxe.wodbuster.com/benchmarks/programs.aspx")
) %>% bind_rows(.id = 'type')

wod_info = dwod %>%
  mutate(
    id = as.numeric(str_sub(url, 31, -1))
  ) %>% select(id, type, name, url)

save(wod_info, file = "wod_info.RData")
