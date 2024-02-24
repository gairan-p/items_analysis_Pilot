##MM sight word
set.seed(3456)

esw<-  ESW_complete
colnames(esw)
#without demographics and totals, with practice items
esw <- esw %>% dplyr::select(6:113)
colnames(esw)

library(caret)
#identify variance near zero
nearZeroVar(esw, names = TRUE)
#22 items
#remove those items from the dataset
esw_variance<- esw %>% dplyr::select(-c(1:12,95,98:108)) 

colnames(esw_variance) #84 items 

#Descriptives and distributions
colMeans(esw)
#till item 103 some scores
#Compute percent correct
rowSums(esw) # total score

# Overall responses
RIallresp(esw)
#0= 47.6
#1= 52.4

#Floor/ceiling effects
RIrawdist(esw)
#EXCLUDED from item 104 (and item 1)
#due to complete 0/full
#min= 0, max= 0%

#Guttman structure
RIheatmap(esw) +
  theme(axis.text.x = element_blank())

#108 items, so half= 54
esw %>% dplyr::select(1:54) %>% 
  RItileplot()

esw %>% dplyr::select(55:108) %>% 
  RItileplot()
#from 66-70, at least one correct response
#from 103, no 1=correct

#barstack is better
#one half of the test
esw %>% dplyr::select(1:54) %>% 
  RIbarstack() +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise() 

#second half
esw %>% dplyr::select(55:108) %>% 
  RIbarstack() +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise() 

#inter-item correlation
cor.plot(esw)

#this is mainly parametric
#residual correlations
RIresidcorr(esw, cutoff = 0.2)
#item-pairs show residual correlations above the cutoff value:
#11 pairs

#first contrast loadings
RIloadLoc(esw)
#only two items above 2, at_different= 2.1, at_empty= 2.6


#1) MSA of esw
library(tidyr)

## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.list <- check.monotonicity(esw)
summary(monotonicity.list)
monotonicity.list$Hi
#from item 104 till 108, all NaN


plot(monotonicity.list, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)

# B: scalability  
#Compute scalability coefficients
#as can be seen from the monotonicity output
#only till item   where there are at least
#1 correct response
coefH(esw)
#items with covariance only
coefH(esw_variance, ci= .95)
#0.931 (0.011) 
#[0.909, 0.954]

sc_var<- coefH(esw_variance)
sc_var$H

## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore- for the plot
#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.list <- check.iio(esw)
summary(iio.list)
#HT= 0.9595275

#check the z-value

getwd()
iio.list$violations
#3 steps 
iio.var<- check.iio(esw_variance)
summary(iio.var)
#HT= .93
iio.list$items.removed 
#sw27 sw35 
#32   42 
iio.var$items.removed 
#same, 27 and 35

iio.list$violations

plot(iio.list)

aisp(esw)#does not work
aisp(esw_variance)
#1s for all

# Compute the reliability of the scale
check.reliability(esw_variance)
#same result as the original version with all items
#$MS
[1] 0.99

#$alpha
[1]  0.98

#$lambda.2
[1] 0.98

reliability(esw)
#items with missing values, cannot compute
reliability(esw_variance)
#alpha= .99
#omega total= .99

tab_itemscale(esw)
#alpha= .98
#Mean inter-item-correlation=NA

tab_itemscale(esw_variance)
#same alpha
#but, mean item-item correlation = .462

unidim(esw)  
#NAs, do not run
unidim(esw_variance)  
#alpha= .99
#u= .69

#tam rasch model for WLE 
##cannot run because dimnames with the original dataset
#so run with var-items removed
tam1 <- tam(esw_variance, irtmodel = "1", verbose = FALSE) 
tam2 <- TAM::tam.mml.2pl(esw_variance)
anova(tam1, tam2)
#tam 2 better, lower AIC

#Reliability: Test information - reflects item properties (not sample)
RItif(esw) + theme_rise()
#The following items were excluded due to
#complete 0/full responses:
#sw1 sw104 sw105 sw106 sw107 sw108

# an extended Person-Item Map termed PIccc
est<- RMX::plotPIccc(tam2,
                     classical=TRUE,  lmar=3,  ylas=2)

#2) Bayesian IRT
# load required packages

# ----------- Code for Bayesian IRT ------------
# Analysis of the M elision data set using dichotomous IRT models
esw<-  ESW_complete
colnames(esw)

#pivot the data frame into a long format
esw_l<- esw %>% pivot_longer(cols=c(6:113),
                             names_to='item',
                             values_to='scores')

# ---------- 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_esw_1pl <- bf(scores ~ 1 + (1 | item) + (1 | Student_ID ))


# specify some weakly informative priors
prior_esw_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "item")

# fit the 1PL model
fit_esw_1pl <- brm(
  formula = formula_esw_1pl,
  data = esw_l,
  family = brmsfamily("bernoulli"),
  prior = prior_esw_1pl)

# obtain basic summaries
summary(fit_esw_1pl)

#investigate the posterior
plot(fit_esw_1pl)  

# extract person parameters
ranef_esw_1pl <- ranef(fit_esw_1pl)

(person_pars_esw_1pl <- ranef_esw_1pl$Student_ID)


# extract item parameters
(item_pars_esw_1pl <- coef(fit_esw_1pl)$item)

# plot item parameters
item_pars_esw_1pl[, , "Intercept"] %>%
  dplyr::as_tibble() %>%
  rownames_to_column() %>%
  dplyr::rename(item = "rowname") %>%
  dplyr::mutate(item = as.numeric(item)) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


# plot person parameters
person_pars_esw_1pl[, , "Intercept"] %>%
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
formula_esw_2pl <- bf(
  scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| item) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| item),
  nl = TRUE
)

# specify some weakly informative priors
prior_esw_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_esw_2pl <- brm(
  formula = formula_esw_2pl,
  data = esw_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_esw_2pl,
  seed = 1234,
  file = "models/fit_esw_2pl"
)

# obtain some basic summaries
summary(fit_esw_2pl)
#parts of the model 2PL did not converge
plot(fit_esw_2pl, ask = FALSE)

# extract item parameters
(item_pars_esw_2pl <- coef(fit_esw_2pl)$item)

#plot item parameters
#difficulties
eta <- item_pars_esw_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

#discriminations
alpha <- item_pars_esw_2pl[, , "logalpha_Intercept"] %>%
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
ranef_esw_2pl <- ranef(fit_esw_2pl)
(person_pars_esw_2pl <- ranef_esw_2pl$Student_ID)

# plot person parameters
person_pars_esw_2pl[, , "eta_Intercept"] %>%
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

cor(person_pars_esw_1pl, person_pars_esw_2pl)
#= .97 
#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_esw_1pl <- loo(fit_esw_1pl)
loo_esw_2pl <- loo(fit_esw_2pl)

loo_esw_compare <- loo_compare(loo_esw_1pl, loo_esw_2pl)
print(loo_esw_compare, simplify = FALSE)
#            elpd_diff se_diff
#fit_esw_2pl     0.0       0.0
#fit_esw_1pl     -289.4      35.0
 #FIT 2 is better (best fit model appears on top- all zeo)
#all models receive a difference score relative to the best model
#the LOO for fit 2 minus itself is 0, , the values in the top row are all zero.






