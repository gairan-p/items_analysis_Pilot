##MM Word comprehension
mwc<- MWC_VD_complete
colnames(mwc)
#without demographics and totals, with practice items
mwc_items<- mwc %>% dplyr::select(6:14, 16:20, 22:34)

colnames(mwc_items)

#remove the following practice items
#antonyms- item 1 and 2
#synonyms- item 1 and 2
#analogies- item 1, 2, 3
#only test items

mwc_test<- mwc_items %>% dplyr::select(3:9, #antonyms
                                       13:14, #synonyms
                                       18:27) #analogies
#Descriptives and distributions
colMeans(mwc_test)

#Compute percent correct
rowSums(mwc_test) # total score

# Overall responses
RIallresp(mwc_test)

#Floor/ceiling effects
RIrawdist(mwc_test)
#min= 19.47, max= 0%

#Guttman structure
RIheatmap(mwc_test) +
  theme(axis.text.x = element_blank())

RItileplot(mwc_test)
#barstack is better

RIbarstack(mwc_test) +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise() 

#residual correlations
RIresidcorr(mwc_test, cutoff = 0.2)
#item-pairs show residual correlations above the cutoff value:
#11 pairs

#first contrast loadings
RIloadLoc(mwc_test)
#only two items above 2, at_different= 2.1, at_empty= 2.6

cor.plot(mwc_test)

#1) MSA of mwc

library(mokken)
library(tidyr)

## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.mwc <- check.monotonicity(mwc_test)
summary(monotonicity.mwc)
plot(monotonicity.mwc, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)

mono.mwc<- summary(monotonicity.mwc)
mono.mwc<- as.data.frame(mono.mwc)
write_xlsx(mono.mwc, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/MWCmono.xlsx")

# B: scalability  
#Compute scalability coefficients
coefH(mwc_test, ci= .95)
#   0.691 (0.036) [0.620, 0.762]

#Scale H      se 
sc_mwc<- coefH(mwc_test)
sc_mwc$Hi

## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore- for the plot
#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.mwc <- check.iio(mwc_test)
summary(iio.mwc)

iio.mwc$items.removed #NULL 
iio.mwc$violations
#$HT 	is the Coefficient HT for the remaining items.
#[1] 0.6520876

iio_mwc<- summary(iio.mwc)
iio_mwc<- as.data.frame(iio_mwc)
write_xlsx(iio_mwc, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/MWCiio.xlsx")

plot(iio.list)

# Compute the reliability of the scale
check.reliability(mwc_test)

#$MS
[1] 0.93

#$alpha
[1]  0.92

#$lambda.2
[1] 0.93

reliability(mwc_test)

#omega total= .93
#alpha= .92

tab_itemscale(mwc_test)
#alpha= .93
#Mean inter-item-correlation=0.364

unidim(mwc_test)  
#u= .76

#tam rasch model for WLE 
tam1 <- tam(mwc_test, irtmodel = "1", verbose = FALSE) 
tam2 <- TAM::tam.mml.2pl(mwc_test)

tam.wle(tam1) 
#  WLE Reliability=0.87
tam.wle(tam2) 
#  WLE Reliability=0.89

#Reliability: Test information - reflects item properties (not sample)
RItif(mwc_test) + theme_rise()

# an extended Person-Item Map termed PIccc
est<- RMX::plotPIccc(tam2,
                     classical=TRUE,  lmar=3,  ylas=2)

#2) Bayesian IRT
# load required packages

# ----------- Code for Bayesian IRT ------------
# Analysis of the M elision data set using dichotomous IRT models
mwc<- MWC_VD_complete
colnames(mwc)
mwc<- mwc %>% dplyr::select(1:14, 16:20, 22:34) #w/o totals of subscale


#pivot the data frame into a long format
mwc_l<- mwc %>% pivot_longer(cols=c(6:32),
                             names_to='item',
                             values_to='scores')

# ---------- 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_mwc_1pl <- bf(scores ~ 1 + (1 | item) + (1 | Student_ID ))


# specify some weakly informative priors
prior_mwc_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "item")

# fit the 1PL model
fit_mwc_1pl <- brm(
  formula = formula_mwc_1pl,
  data = mwc_l,
  family = brmsfamily("bernoulli"),
  prior = prior_mwc_1pl)

# obtain basic summaries
summary(fit_mwc_1pl)
#investigate the posterior
plot(fit_mwc_1pl)  

# extract person parameters
ranef_mwc_1pl <- ranef(fit_mwc_1pl)

(person_pars_mwc_1pl <- ranef_mwc_1pl$Student_ID)


# extract item parameters
(item_pars_mwc_1pl <- coef(fit_mwc_1pl)$item)

# plot item parameters
item_pars_mwc_1pl[, , "Intercept"] %>%
  dplyr::as_tibble() %>%
  rownames_to_column() %>%
  dplyr::rename(item = "rowname") %>%
  dplyr::mutate(item = as.numeric(item)) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


# plot person parameters
person_pars_mwc_1pl[, , "Intercept"] %>%
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
formula_mwc_2pl <- bf(
  scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| item) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| item),
  nl = TRUE
)

# specify some weakly informative priors
prior_mwc_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_mwc_2pl <- brm(
  formula = formula_mwc_2pl,
  data = mwc_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_mwc_2pl,
  seed = 1234,
  file = "models/fit_mwc_2pl"
)

# obtain some basic summaries
summary(fit_mwc_2pl)
plot(fit_mwc_2pl, ask = FALSE)

# extract item parameters
(item_pars_mwc_2pl <- coef(fit_mwc_2pl)$item)

# plot item parameters
# difficulties
eta <- item_pars_mwc_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha <- item_pars_mwc_2pl[, , "logalpha_Intercept"] %>%
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
ranef_mwc_2pl <- ranef(fit_mwc_2pl)
(person_pars_mwc_2pl <- ranef_mwc_2pl$Student_ID)

# plot person parameters
person_pars_mwc_2pl[, , "eta_Intercept"] %>%
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

cor(person_pars_mwc_1pl, person_pars_mwc_2pl)
#= .99 ~ 1
#so no need to refer to 2PL

#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_mwc_1pl <- loo(fit_mwc_1pl)
loo_mwc_2pl <- loo(fit_mwc_2pl)

loo_mwc_compare <- loo_compare(loo_mwc_1pl, loo_mwc_2pl)
print(loo_mwc_compare, simplify = FALSE)
#            elpd_diff se_diff
#fit_me_2pl   0.0       0.0  
#fit_me_1pl -5.4        4.8

#fit 2pl
Estimate   SE
elpd_loo   -747.1 28.0
p_loo       122.4  7.2
looic      1494.3 55.9
#FIT 2 is better (best fit model appears on top- all zeo)
#all models receive a difference score relative to the best model
#the LOO for fit 2 minus itself is 0, , the values in the top row are all zero.






