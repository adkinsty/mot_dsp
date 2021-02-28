library(tidyverse)
library(brms)
library(bayesplot)
library(bayestestR)
library(loo)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

bm_summary <- function(bm) {
  describe_posterior(bm,effects = 'fixed',component = 'all',ci = 0.95,test = c("p_direction"),centrality = 'all')
}

# orthogonal polynomial contrasts for block
poly_contr <- poly(1:9,2,simple=TRUE)  
poly_contr2 <- poly(1:3,2,simple=TRUE)
my_prior <- set_prior("normal(0,1)",class="b")

setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/mot_dsp/")

dat = read_csv(file = "data/clean/all_data_clean.csv") %>%
  mutate(zero_val = ifelse(group == "non","zero",size),
         zero_val = factor(zero_val,c("low","med","high","zero")),
         value = factor(size,c("low","med","high")),
         group = factor(group,levels = c("rew","pen","non")),
         kerr = ifelse(kerr == TRUE, 1, 0),
         kps = 8/(mt/1000),
         g = ifelse(group=="pen",.5,ifelse(group=="rew",.5,-.5)))

dat1 = filter(dat,sess == "reward") %>%
  mutate(acc = ifelse(kerr == 0, 1, 0)) %>%
  group_by(block,id,group,g,value,v) %>% summarise(acc = mean(acc)) %>% 
  ungroup() %>% mutate(bL = poly_contr[block,1],bQ = poly_contr[block,2])

dat1.1 = filter(dat,kerr == 0 & sess == "reward")  %>%
  group_by(block,id,group,g,value) %>% summarise(rt = mean(rt),kps = mean(kps)) %>% 
  ungroup() %>% mutate(rt_z = scale(rt)[,1], kps_z = scale(kps)[,1],bL = poly_contr[block,1],bQ = poly_contr[block,2])

dat2 = filter(dat,sess == "test") %>%
  mutate(acc = ifelse(kerr == 0, 1, 0)) %>%
  group_by(block,id,group,g,value) %>% summarise(acc = mean(acc)) %>%
  ungroup() %>% mutate(bL = poly_contr[block,1],bQ = poly_contr[block,2])

dat2.1 = filter(dat,kerr == 0 & sess == "test") %>%
  group_by(block,id,group,g,value) %>% summarise(rt = mean(rt),kps = mean(kps)) %>% 
  ungroup() %>% mutate(rt_z = scale(rt)[,1], kps_z = scale(kps)[,1],bL = poly_contr2[block,1],bQ = poly_contr2[block,2])


# **************************************** Train RT & KPS ****************************************

dat11_low_rew <- filter(dat1.1, group != "pen" & value %in% c("low","zero"))
dat11_low_pen <- filter(dat1.1, group != "rew" & value %in% c("low","zero"))
dat11_high_rew <- filter(dat1.1, group != "pen" & value %in% c("high","zero"))
dat11_high_pen <- filter(dat1.1, group != "rew" & value %in% c("high","zero"))

bm_rt_low_rew <- brm(bf(rt_z ~ bL*g + bQ),dat11_low_rew,prior=my_prior,warmup=4e3,iter = 1e4)
bm_rt_low_pen <- update(bm_rt_low_rew, newdata=dat11_low_pen)
bm_rt_high_rew <- update(bm_rt_low_rew, newdata=dat11_high_rew)
bm_rt_high_pen <- update(bm_rt_low_rew, newdata=dat11_high_pen)

ms_rt_low_rew <- bm_summary(bm_rt_low_rew)
ms_rt_low_pen <- bm_summary(bm_rt_low_pen)
ms_rt_high_rew <- bm_summary(bm_rt_high_rew)
ms_rt_high_pen <- bm_summary(bm_rt_high_pen)

bm_kps_low_rew <- brm(bf(kps_z ~ bL*g + bQ),dat11_low_rew,prior=my_prior,warmup=4e3,iter = 1e4)
bm_kps_low_pen <- update(bm_kps_low_rew, newdata=dat11_low_pen)
bm_kps_high_rew <- update(bm_kps_low_rew, newdata=dat11_high_rew)
bm_kps_high_pen <- update(bm_kps_low_rew, newdata=dat11_high_pen)

ms_kps_low_rew <- bm_summary(bm_kps_low_rew)
ms_kps_low_pen <- bm_summary(bm_kps_low_pen)
ms_kps_high_rew <- bm_summary(bm_kps_high_rew)
ms_kps_high_pen <- bm_summary(bm_kps_high_pen)

# **************************************** Train acc ****************************************

dat1_low_rew <- filter(dat1, group != "pen" & value %in% c("low","zero"))
dat1_low_pen <- filter(dat1, group != "rew" & value %in% c("low","zero"))
dat1_high_rew <- filter(dat1, group != "pen" & value %in% c("high","zero"))
dat1_high_pen <- filter(dat1, group != "rew" & value %in% c("high","zero"))

bm_acc_low_rew <- brm(bf(acc ~ bL*g + bQ),dat1_low_rew,family=zero_one_inflated_beta(),prior=my_prior,warmup=4e3,iter = 1e4)
bm_acc_low_pen <- update(bm_acc_low_rew, newdata=dat1_low_pen)
bm_acc_high_rew <- update(bm_acc_low_rew, newdata=dat1_high_rew)
bm_acc_high_pen <- update(bm_acc_low_rew, newdata=dat1_high_pen)

ms_acc_low_rew <- bm_summary(bm_acc_low_rew)
ms_acc_low_pen <- bm_summary(bm_acc_low_pen)
ms_acc_high_rew <- bm_summary(bm_acc_high_rew)
ms_acc_high_pen <- bm_summary(bm_acc_high_pen)


# **************************************** Retention ***************************************

dat_ret <- dat %>% 
  filter(kerr == 0 & sess != "training" & sequence != "random") %>%
  mutate(kps = 7/(mt/1000)) %>%
  mutate(block = ifelse(sess=="test",block+6,block)) %>%
  filter(block %in% 7:9) %>% 
  mutate(s = ifelse(sess=="reward",-.5,.5)) %>%
  group_by(g,group,id,s,value,block) %>%
  summarise(kps = mean(kps)) %>%
  ungroup() %>% mutate(kps_z = scale(kps)[,1],b = ifelse(block==7,-.5,ifelse(block==8,0,.5)))

dat_ret_low_rew <- filter(dat_ret, group != "pen" & value %in% c("low","zero"))
dat_ret_low_pen <- filter(dat_ret, group != "rew" & value %in% c("low","zero"))
dat_ret_high_rew <- filter(dat_ret, group != "pen" & value %in% c("high","zero"))
dat_ret_high_pen <- filter(dat_ret, group != "rew" & value %in% c("high","zero"))

bm_ret_low_rew <- brm(bf(kps_z ~ b*s*g),dat_ret_low_rew,prior=my_prior,warmup=4e3,iter = 1e4)
bm_ret_low_pen <- update(bm_ret_low_rew, newdata=dat_ret_low_pen)
bm_ret_high_rew <- update(bm_ret_low_rew, newdata=dat_ret_high_rew)
bm_ret_high_pen <- update(bm_ret_low_rew, newdata=dat_ret_high_pen)

ms_ret_low_rew <- bm_summary(bm_ret_low_rew)
ms_ret_low_pen <- bm_summary(bm_ret_low_pen)
ms_ret_high_rew <- bm_summary(bm_ret_high_rew)
ms_ret_high_pen <- bm_summary(bm_ret_high_pen)

