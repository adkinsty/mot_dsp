library(tidyverse)

setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/MOT_DSP/")

get_data = function(grp) {
    # possible values for grp: "rew", "pen", "non"
    files = list.files(path = sprintf("data/raw/%s/",grp),
                       pattern = "*_output.tsv")
    data = tibble()
    for (i in 1:length(files)) {
        fn = files[i]
        fn_split = str_split(string=fn,pattern="_",n = 4)[[1]]
        id = as.integer(fn_split[1])
        sess = fn_split[3]
        tmp = read_tsv(file = sprintf("data/raw/%s/%s",grp,fn)) %>% 
            group_by(sequence) %>%
            mutate(t = 1:n()) %>%
            mutate(sess = rep(sess,n()),
                   group = rep(grp,n()),
                   trial = 1:n()) %>%
            dplyr::select(-c('X1'))
        data = data %>% bind_rows(tmp)
    }
    return(data)
}

data_pen = get_data("pen")
write.csv(x = data_pen, file = "data/raw/all/all_pen_data.csv")

data_rew = get_data("rew")
write.csv(x = data_rew, file = "data/raw/all/all_rew_data.csv")

data_non = get_data("non")
write.csv(x = data_non, file = "data/raw/all/all_non_data.csv")

data_all = tibble()
data_all = data_all %>% bind_rows(data_pen) %>% bind_rows(data_rew) %>% bind_rows(data_non)

write.csv(x = data_all, file = "data/raw/all/all_data.csv")
