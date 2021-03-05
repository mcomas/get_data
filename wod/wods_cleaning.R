library(tidyverse)
load('wods.RData')

wods %>%
  arrange(id) %>%
  mutate(
    title = str_trim(map_chr(wod, first)),
    wod_lines = map_int(wod, length),
    wod = map(wod_lines, ~wod[1:(.x-5)]),
    wod_lines = map_int(wod, length),
    FTIME = as.integer(str_detect(title, regex("(for time|for tima)", ignore_case = TRUE))),
    AMRAP = as.integer(str_detect(title, regex("(AMRAP|As Many Rounds|As Many Reps|Max reps|Max \\(n\\) rep\\(s\\)|for reps)", ignore_case = TRUE))),
    EMOM = as.integer(str_detect(title, regex("EMOM", ignore_case = TRUE))),
    TABATA = as.integer(str_detect(title, regex("TABATA", ignore_case = TRUE))),
    rounds = str_replace(str_extract(title, regex(".* rounds?", ignore_case = TRUE)), regex("rounds?", ignore_case = TRUE), ""),
    rounds = str_replace(rounds, regex("two", ignore_case = TRUE), "2"),
    rounds = str_replace(rounds, regex("three", ignore_case = TRUE), "3"),
    rounds = str_replace(rounds, regex("four", ignore_case = TRUE), "4"),
    rounds = str_replace(rounds, regex("five", ignore_case = TRUE), "5"),
    rounds = str_replace(rounds, regex("six", ignore_case = TRUE), "6"),
    rounds = str_replace(rounds, regex("seven", ignore_case = TRUE), "7"),
    rounds = str_replace(rounds, regex("eight", ignore_case = TRUE), "8"),
    rounds = str_replace(rounds, regex("nine", ignore_case = TRUE), "9"),
    rounds = str_replace(rounds, regex("ten", ignore_case = TRUE), "10"),
    rounds = str_replace(rounds, regex("twelve", ignore_case = TRUE), "12"),
    rounds = str_replace(rounds, regex("thirty", ignore_case = TRUE), "30"),
    rounds = as.integer(rounds)
  ) 

wods %>%
  count(name) %>%
  filter(n > 1) %>%
  left_join(wods) %>%
  pull(wod)
