##MM sight word
set.seed(3456)

msw<-  MSW_complete
colnames(msw)
#without demographics and totals, with practice items
msw <- msw %>% dplyr::select(6:113)

colnames(msw)

#Descriptives and distributions
colMeans(msw)

#Compute percent correct
rowSums(msw) # total score

# Overall responses
RIallresp(msw)
#0= 80.1
#1= 19.9

#Floor/ceiling effects
RIrawdist(msw)
#EXCLUDED from item 71
#due to complete 0/full
#min= 15.93, max= 0%

#Guttman structure
RIheatmap(msw) +
  theme(axis.text.x = element_blank())

#108 items, so half= 54
msw %>% dplyr::select(1:54) %>% 
RItileplot()

msw %>% dplyr::select(55:108) %>% 
  RItileplot()
#from 66-70, at least one correct response
#from 71, no 1=correct

#barstack is better
#one half of the test
msw %>% dplyr::select(1:54) %>% 
RIbarstack() +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise() 

#second half
msw %>% dplyr::select(55:108) %>% 
  RIbarstack() +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise() 
cor.plot(msw)
#this is mainly parametric
#residual correlations
RIresidcorr(msw, cutoff = 0.2)
#item-pairs show residual correlations above the cutoff value:
#11 pairs

#first contrast loadings
RIloadLoc(msw)
#only two items above 2, at_different= 2.1, at_empty= 2.6

cor.plot(msw)

#1) MSA of msw

library(mokken)
library(tidyr)

## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.list <- check.monotonicity(msw)
summary(monotonicity.list)
monotonicity.list$Hi
#from item 71 till 108, all NaN
plot(monotonicity.list, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)

#z-sig all zeros

# B: scalability  
#Compute scalability coefficients
#as can be seen from the monotonicity output
#only till item 70 where there are at least
#1 correct response
msw %>% dplyr::select(1:70) %>% 
coefH(ci= .95)
#Scale H se      95% ci        
#0.938 (0.011) [0.916, 0.960]

sc_msw70<- msw %>% dplyr::select(1:70) %>% 
  coefH()
#0.938 (0.011)
sc_msw70$Hi

## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore- for the plot
#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.list <- check.iio(msw)
summary(iio.list)

iio.list$items.removed 
#item 9 
iio.list$violations
#$HT 	is the Coefficient HT for the remaining items.
#[1] 0.9654632

plot(iio.list)

# Compute the reliability of the scale
check.reliability(msw)

#$MS
[1] 0.93

#$alpha
[1]  0.92

#$lambda.2
[1] 0.93

reliability(msw)

#omega total= .93
#alpha= .92

tab_itemscale(msw)
#alpha= .93
#Mean inter-item-correlation=0.364

unidim(msw)  
#u= .76

#tam rasch model for WLE 
tam1 <- tam(msw, irtmodel = "1", verbose = FALSE) 
tam2 <- TAM::tam.mml.2pl(msw)

tam.wle(tam1) 
#  WLE Reliability=0.87
tam.wle(tam2) 
#  WLE Reliability=0.89

#Reliability: Test information - reflects item properties (not sample)
RItif(msw) + theme_rise()
#EXCLUDED many items

# an extended Person-Item Map termed PIccc
est<- RMX::plotPIccc(tam2,
                     classical=TRUE,  lmar=3,  ylas=2)

#2) Bayesian IRT
# load required packages

# ----------- Code for Bayesian IRT ------------
# Analysis of the M elision data set using dichotomous IRT models
msw<-  MSW_complete
colnames(msw)

#pivot the data frame into a long format
msw_l<- msw %>% pivot_longer(cols=c(6:113),
                             names_to='item',
                             values_to='scores')

# ---------- 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_msw_1pl <- bf(scores ~ 1 + (1 | item) + (1 | Student_ID ))


# specify some weakly informative priors
prior_msw_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "item")

# fit the 1PL model
fit_msw_1pl <- brm(
  formula = formula_msw_1pl,
  data = msw_l,
  family = brmsfamily("bernoulli"),
  prior = prior_msw_1pl)

# obtain basic summaries
summary(fit_msw_1pl)
#we learnt that fit 1pl is a better model 
#Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors. 


#investigate the posterior
plot(fit_msw_1pl)  

# extract person parameters
ranef_msw_1pl <- ranef(fit_msw_1pl)

(person_pars_msw_1pl <- ranef_msw_1pl$Student_ID)


# extract item parameters
(item_pars_msw_1pl <- coef(fit_msw_1pl)$item)

# plot item parameters
item_pars_msw_1pl[, , "Intercept"] %>%
  dplyr::as_tibble() %>%
  rownames_to_column() %>%
  dplyr::rename(item = "rowname") %>%
  dplyr::mutate(item = as.numeric(item)) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


# plot person parameters
person_pars_msw_1pl[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  arrange(Estimate) %>%
  mutate(id = seq_len(n())) %>%
  ggplot(aes(id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")


# ---------- 2PL models ----------------------
## specify a 2PL model
formula_msw_2pl <- bf(
  scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| item) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| item),
  nl = TRUE
)

# specify some weakly informative priors
prior_msw_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_msw_2pl <- brm(
  formula = formula_msw_2pl,
  data = msw_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_msw_2pl,
  seed = 1234,
  file = "models/fit_msw_2pl"
)

# obtain some basic summaries
summary(fit_msw_2pl)
#model 2PL did not converge
plot(fit_msw_2pl, ask = FALSE)

# extract item parameters
(item_pars_msw_2pl <- coef(fit_msw_2pl)$item)

# plot item parameters
# difficulties
eta <- item_pars_msw_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha <- item_pars_msw_2pl[, , "logalpha_Intercept"] %>%
  exp() %>%
  as_tibble() %>%
  rownames_to_column()

# plot difficulties and discrimination next to each other
bind_rows(eta, alpha, .id = "nlpar") %>%
  dplyr::rename(item = "rowname") %>%
  dplyr::mutate(item = as.numeric(item)) %>%
  dplyr::mutate(nlpar = factor(nlpar, labels = c("Easiness", "Discrimination"))) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  facet_wrap("nlpar", scales = "free_x") +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")

# extract person parameters
ranef_msw_2pl <- ranef(fit_msw_2pl)
(person_pars_msw_2pl <- ranef_msw_2pl$Student_ID)

# plot person parameters
person_pars_msw_2pl[, , "eta_Intercept"] %>%
  dplyr::as_tibble() %>%
  rownames_to_column() %>%
  dplyr::select(-Est.Error) %>%
  dplyr:: arrange(Estimate) %>%
  dplyr::mutate(id = seq_len(n())) %>%
  ggplot(aes(id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")

#compare person parameters of 1pl and 2pl

cor(person_pars_msw_1pl, person_pars_msw_2pl)
#= .90 
#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_msw_1pl <- loo(fit_msw_1pl)
loo_msw_2pl <- loo(fit_msw_2pl)

loo_msw_compare <- loo_compare(loo_msw_1pl, loo_msw_2pl)
print(loo_msw_compare, simplify = FALSE)
#            elpd_diff se_diff
#fit_msw_1pl    0.0       0.0  
#fit_msw_2pl   -934.9     23.4

#fit 2pl
Estimate   SE
elpd_loo   -747.1 28.0
p_loo       122.4  7.2
looic      1494.3 55.9
#FIT 1 is better (best fit model appears on top- all zeo)
#all models receive a difference score relative to the best model
#the LOO for fit 2 minus itself is 0, , the values in the top row are all zero.






