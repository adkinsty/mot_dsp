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
descale <- function(z,x) {
  y = z * sd(x) + mean(x)
  return(y)
}
v_coding <- function(bm) {
  # Function to compare models with different value contrasts

  bm_data <- bm$data
  encodings <- tibble("lin" = c(-.5, 0, .5),"bin" = c(-.25, -.25, .5)) #"tru" = scale(c(5,10,20))[,1]
  results <- list()

  for (group in c("rew","pen")) {

    if (group=="rew") {tmp_g <- .5} else {tmp_g <- -.5}
    loo_list <- list()
    
    for (code_name in colnames(encodings)) {

      code <- pull(encodings[,code_name])
      tmp_dat <- bm_data %>% filter(g==tmp_g) %>% 
        mutate(v = ifelse(v == -.5, code[1], ifelse(v == 0, code[2], code[3]))) %>% as_tibble()
      tmp_bm <- update(bm,formula. = ~ . - g - bL:g - bL:v:g - v:g,newdata=tmp_dat)
      log_lik <- log_lik(tmp_bm,pointwise=FALSE)
      LOO <- loo(log_lik, cores = 4)
      loo_list[[code_name]] <- LOO
    }
    results[[group]] <- loo_compare(loo_list)
  }
  return(results)
}

# orthogonal polynomials for block
poly_contr <- poly(1:9,2,simple=TRUE)  
poly_contr2 <- poly(c(0, 1, 5),2,simple=TRUE)
my_prior <- set_prior("normal(0,1)",class="b")

setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/mot_dsp/")

dat = read_csv(file = "data/clean/all_data_clean.csv") %>%
  mutate(size = ifelse(group == "non","zero",size),
         value = factor(size,c("low","med","high","zero")),
         group = factor(group,levels = c("rew","pen","non")),
         kerr = ifelse(kerr == TRUE, 1, 0),
         v = ifelse(size == "low", -.5, ifelse(size == "med", 0, .5)),
         g = ifelse(group=="pen",-.5,ifelse(group=="rew",.5,0)))
dat1 = filter(dat,sess == "reward")
dat1.1 = filter(dat1,kerr == 0) 
dat2 = filter(dat,sess == "test")
dat2.1 = filter(dat2,kerr == 0)

# **************************************** Train RT ****************************************

dat_rt <- dat1.1 %>% filter(group != "non") %>% 
  group_by(id,g,v,block) %>% summarise(rt = mean(rt)) %>% ungroup() %>% 
  mutate(bL = poly_contr[block,1],
         bQ = poly_contr[block,2],
         rt_z = scale(rt)[,1])

bm_rt <- read_rds("modeling/rds_files/bm_rt.rds")
bm_rt <- brm(bf(rt_z ~ bL*v*g + bQ + (bL + bQ + v | id)),dat_rt,prior=my_prior,warmup=4e3,iter = 1e4)
ms_rt <- bm_summary(bm_rt)
write_rds(bm_rt, "modeling/rds_files/bm_rt.rds")
v_code_rt <- v_coding(bm_rt)
write_rds(v_code_rt, "modeling/rds_files/v_code_rt.rds")

y <- dat_rt$rt_z
yrep <- posterior_predict(bm_rt)
color_scheme_set("brightblue")
ppc_dens_overlay(y, yrep[1:100, ], alpha = .5)
# **************************************** Train KPS ****************************************

dat_kps <- dat1.1 %>% filter(group != "non" & sequence != "random") %>% 
  mutate(kps = 8/(mt/1000)) %>%
  group_by(id,g,v,block) %>% summarise(kps = mean(kps)) %>% ungroup() %>% 
  mutate(bL = poly_contr[block,1],
         bQ = poly_contr[block,2],
         kps_z = scale(kps)[,1])

bm_kps <- read_rds("modeling/rds_files/bm_kps.rds")
bm_kps <- brm(bf(kps_z ~ bL*v*g + bQ + (bL + bQ + v | id)),dat_kps,prior=my_prior,warmup=4e3,iter = 1e4)
ms_kps <- bm_summary(bm_kps)
write_rds(bm_kps, "modeling/rds_files/bm_kps.rds")
v_code_kps <- v_coding(bm_kps)
write_rds(v_code_kps, "modeling/rds_files/v_code_kps.rds")

y <- dat_kps$kps_z
yrep <- posterior_predict(bm_kps)
color_scheme_set("blue")
ppc_dens_overlay(y, yrep[1:100, ], alpha = .5)

# **************************************** Train acc ****************************************

dat_acc <- dat1 %>% filter(group != "non") %>% 
  mutate(acc = ifelse(kerr==0,1,0)) %>%
  group_by(id,g,v,block) %>%
  summarise(acc = mean(acc,na.rm=T)) %>% ungroup() %>% 
  mutate(bL = poly_contr[block,1],
         bQ = poly_contr[block,2])

bm_acc <- brm(bf(acc ~ bL*v*g + bQ + (bL + bQ + v | id)),dat_acc,zero_one_inflated_beta(),prior=my_prior,iter=1e4,warmup=4e3)
ms_acc <- bm_summary(bm_acc)
write_rds(bm_acc, "modeling/rds_files/bm_acc.rds")
v_code_acc <- v_coding(bm_acc)
write_rds(v_code_acc, "modeling/rds_files/v_code_acc.rds")


# **************************************** Test RT ****************************************

dat_rt2 <- dat2.1 %>% filter(group != "non" & sequence != "random") %>% 
  group_by(id,g,v,block) %>% summarise(rt = mean(rt)) %>% ungroup() %>% 
  mutate(bL = poly_contr2[block,1],
         bQ = poly_contr2[block,2],
         rt_z = scale(rt)[,1])

bm_rt2 <- brm(bf(rt_z ~ bL*v*g + bQ + (bL + bQ + v | id)),dat_rt2,prior=my_prior,warmup=4e3,iter = 1e4)
ms_rt2 <- bm_summary(bm_rt2) 
write_rds(bm_rt2, "modeling/rds_files/bm_rt2.rds")



# **************************************** Test KPS ****************************************

dat_kps2 <- dat2.1 %>% filter(group != "non" & sequence != "random") %>% 
  mutate(kps = 8/(mt/1000)) %>%
  group_by(id,g,v,block) %>% summarise(kps = mean(kps)) %>% ungroup() %>% 
  mutate(bL = poly_contr2[block,1],
         bQ = poly_contr2[block,2],
         kps_z = scale(kps)[,1])

bm_kps2 <- brm(bf(kps_z ~ bL*v*g + bQ + (bL + bQ + v | id)),dat_kps2,prior=my_prior,warmup=4e3,iter = 1e4)
ms_kps2 <- bm_summary(bm_kps2)
write_rds(bm_kps2, "modeling/rds_files/bm_kps2.rds")



# **************************************** Test Er ****************************************
dat_acc2 <- dat2 %>% filter(group != "non" & sequence != "random") %>% 
  mutate(acc = ifelse(kerr==0,1,0)) %>%
  group_by(id,g,v,block) %>% summarise(acc = mean(acc)) %>% ungroup() %>%
    mutate(bL = poly_contr2[block,1],
           bQ = poly_contr2[block,2])

bm_acc2 <- brm(bf(acc ~ bL*v*g + bQ + (bL + bQ + v | id)),dat_acc2,zero_one_inflated_beta(),prior=my_prior,warmup=4e3,iter = 1e4)
ms_acc2 <- bm_summary(bm_acc2)
write_rds(bm_acc2, "modeling/rds_files/bm_acc2.rds")

dat_acc2 %>% ggplot(aes(x=factor(g),y=acc,colour=factor(v),group=v)) + stat_summary(position=position_dodge(.25))

# **************************************** Retention ***************************************

dat_ret_mt <- dat %>% 
  filter(kerr == 0 & sess != "training" & sequence != "random" & group!="non") %>%
  mutate(kps = 7/(mt/1000)) %>%
  mutate(block = ifelse(sess=="test",block+6,block)) %>%
  filter(block %in% 7:9) %>% 
  mutate(s = ifelse(sess=="reward",-.5,.5)) %>%
  group_by(g,id,s,v,block) %>%
  summarise(kps = mean(kps)) %>%
  ungroup() %>% mutate(kps_z = scale(kps)[,1],b = ifelse(block==7,-.5,ifelse(block==8,0,.5)))

bm_ret_mt <- read_rds("modeling/rds_files/bm_ret_mt.rds")
bm_ret_mt <- brm(bf(kps_z ~ b*v*g + s*v*g + (b + v + s | id)),dat_ret_mt,prior=my_prior,warmup=4e3,iter = 1e4)
ms_ret_mt <- bm_summary(bm_ret_mt)
write_rds(bm_ret_mt, "modeling/rds_files/bm_ret_mt.rds")


# **************************************** Post-hoc ****************************************

# Training Speed
dat_kps_rew <- dat_kps %>% filter(g == .5)
bm_kps_rew <- brm(bf(kps_z ~ bL*v + bQ + (bL + bQ + v | id)),dat_kps_rew,prior=my_prior,warmup=4e3,iter = 1e4)
ms_kps_rew <- bm_summary(bm_kps_rew)
write_rds(bm_kps_rew, "modeling/rds_files/bm_kps_rew.rds")

dat_kps_pen <- dat_kps %>% filter(g == -.5)
bm_kps_pen <- brm(bf(kps_z ~ bL*v + bQ + (bL + bQ + v | id)),dat_kps_pen,prior=my_prior,warmup=4e3,iter = 1e4)
ms_kps_pen <- bm_summary(bm_kps_pen)
write_rds(bm_kps_pen, "modeling/rds_files/bm_kps_pen.rds")

# Training Accuracy
dat_acc_rew <- dat_acc %>% filter(g == .5)
bm_acc_rew <- brm(bf(acc ~ bL*v + bQ + (bL + bQ + v | id)),dat_acc_rew,zero_one_inflated_beta(),prior=my_prior,iter=1e4,warmup=4e3)
ms_acc_rew <- bm_summary(bm_acc_rew)
write_rds(bm_acc_rew, "modeling/rds_files/bm_acc_rew.rds")

dat_acc_pen <- dat_acc %>% filter(g == -.5)
bm_acc_pen <- brm(bf(acc ~ bL*v + bQ + (bL + bQ + v | id)),dat_acc_pen,zero_one_inflated_beta(),prior=my_prior,iter=1e4,warmup=4e3)
ms_acc_pen <- bm_summary(bm_acc_pen)
write_rds(bm_acc_pen, "modeling/rds_files/bm_acc_pen.rds")

# Training RT
dat_rt_rew <- dat_rt %>% filter(g == .5)
bm_rt_rew <- brm(bf(rt_z ~ bL*v + bQ + (bL + bQ + v | id)),dat_rt_rew,prior=my_prior,warmup=4e3,iter = 1e4)
ms_rt_rew <- bm_summary(bm_rt_rew)
write_rds(bm_rt_rew, "modeling/rds_files/bm_rt_rew.rds")

dat_rt_pen <- dat_rt %>% filter(g == -.5)
bm_rt_pen <- brm(bf(rt_z ~ bL*v + bQ + (bL + bQ + v | id)),dat_rt_pen,prior=my_prior,warmup=4e3,iter = 1e4)
ms_rt_pen <- bm_summary(bm_rt_pen)
write_rds(bm_rt_pen, "modeling/rds_files/bm_rt_pen.rds")

# Retention
dat_ret_rew <- dat_ret_mt %>% filter(g == .5)
bm_ret_rew <- brm(bf(kps_z ~ b*v + s*v + (b + v + s | id)),dat_ret_rew,prior=my_prior,warmup=4e3,iter = 1e4)
ms_ret_rew <- bm_summary(bm_ret_rew)
write_rds(bm_ret_rew, "modeling/rds_files/bm_ret_rew.rds")

dat_ret_pen <- dat_ret_mt %>% filter(g == -.5)
bm_ret_pen <- brm(bf(kps_z ~ b*v + s*v + (b + v + s | id)),dat_ret_pen,prior=my_prior,warmup=4e3,iter = 1e4)
ms_ret_pen <- bm_summary(bm_ret_pen)
write_rds(bm_ret_pen, "modeling/rds_files/bm_ret_pen.rds")

