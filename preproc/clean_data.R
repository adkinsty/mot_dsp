library(tidyverse)

setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/MOT_DSP/")

data = read_csv(file = "data/raw/all/all_data.csv") %>% dplyr::select(-c(X1)) %>%
    rowwise() %>%
    mutate(sub = as.integer(subject),
        id = ifelse(group=="rew",sub+1e3,ifelse(group=="pen",sub+2e3,sub)),
        rt = one_IKI,
        keys = sum(!is.na(c(two_IKI,three_IKI,four_IKI,five_IKI,six_IKI,seven_IKI,eight_IKI))),
        mt =  sum(two_IKI,three_IKI,four_IKI,five_IKI,six_IKI,seven_IKI,eight_IKI,na.rm=T),
        mt_mu = mt/keys,
        tt = mt + rt) %>% ungroup() %>%
    mutate(lim = max_time,
        too_slow = ifelse(sess=="reward",mt >= max_time,FALSE),
        kerr = !too_slow & seq_keys != pressed,
        success = !too_slow & !kerr,
        val = ifelse(sequence=="A","low",ifelse(sequence=="B","med","high")),
        val = ifelse(group=="non","zero",val)) %>%
    group_by(id) %>%
    mutate(
        low = ifelse(group=="non",
                     sample(c("A","B","C"),1),
                     "A"),
        med = ifelse(group=="non",
                     ifelse(low=="A",
                            sample(c("B","C"),1),
                            ifelse(low=="B",
                                   sample(c("A","C"),1),
                                   sample(c("A","B"),1))),
                     "B"),
        high = ifelse(group=="non",
                      ifelse(low=="A",
                             ifelse(med=="B",
                                    "C",
                                    "B"),
                             ifelse(low=="B",
                                    ifelse(med=="A",
                                           "C",
                                           "A"),
                                    ifelse(med=="A",
                                           "B",
                                           "A"))),
                      "C"),
        size = ifelse(group=="non",
                      ifelse(sequence==low,
                             "low",
                             ifelse(sequence==med,
                                    "med",
                                    "high")),
                      val)) %>% ungroup()

write_csv(x = data, path = "data/clean/all_data_clean.csv")
