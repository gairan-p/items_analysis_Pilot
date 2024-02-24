set.seed(3468)
# English word comprehension (vocabulary depth) (vd)

ewc<- EWC_VD_complete
colnames(ewc)
#without demographics and totals, with practice items
ewc_items<- ewc %>% dplyr::select(6:14, 16:20, 22:34)

colnames(ewc_items)

#remove the following practice items
#antonyms- item 1 and 2
#synonyms- item 1 and 2
#analogies- item 1, 2, 3
#only test items

ewc_test<- ewc_items %>% dplyr::select(3:9, #antonyms
                                 13:14, #synonyms
                                 18:27) #analogies

#Descriptives and distributions
colMeans(ewc_test)

#Compute percent correct
rowSums(ewc_test) # total score

# Overall responses
RIallresp(ewc_test)

#Floor/ceiling effects
RIrawdist(ewc_test)
#min= 5.31, max= 20.35%

#Guttman structure
RIheatmap(ewc_test) +
  theme(axis.text.x = element_blank())

# For unidimensionality: 
#residual correlations

RIresidcorr(ewc_test, cutoff = 0.2)
#item-pairs show residual correlations above the cutoff value:
#11 pairs

#first contrast loadings
RIloadLoc(ewc_test)
#only two items above 2, at_different= 2.1, at_empty= 2.6

RItileplot(ewc_test)
RIbarstack(ewc_test)
cor.plot(ewc_test)
#1) MSA of EWC

library(mokken)
library(tidyr)

## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.list <- check.monotonicity(ewc_test)
summary(monotonicity.list)

mono_ewc<- summary(monotonicity.list)
mono_ewc<- as.data.frame(mono_ewc)
write_xlsx(mono_ewc, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/EWCmono.xlsx")

plot(monotonicity.list, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)

# B: scalability  
#Compute scalability coefficients
coefH(ewc_test, ci= .95)
#   0.620 (0.045) [0.531, 0.709]

sc_ewc<- coefH(ewc_test)

#Hi and SE 

#OVERALL test
#Scale H      se 
0.620 (0.045)

## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore- for the plot
restscore.list <- check.restscore(ewc_test)
summary(restscore.list)
plot(restscore.list, curves = "IRF",
     plot.ci=TRUE, color.ci = FALSE, ask=FALSE)

#C.2.#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.list <- check.iio(ewc_test)
summary(iio.list)

iio.list$items.removed #NULL 
#two items: 
#An_red  S_mug 
#3     13

#$HT 	is the Coefficient HT for the remaining items.
#[1]  0.384
iio.list$violations
plot(iio.list)


iio_ewc<- summary(iio.list)
iio_ewc<- as.data.frame(iio_ewc)
write_xlsx(iio_ewc, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/EWCiio.xlsx")

# Compute the reliability of the scale
check.reliability(ewc_test)

#$MS
[1] 0.9431351

#$alpha
[1]  0.9330003

#$lambda.2
[1] 0.9355095

library(psych)
reliability(ewc_test)

#omega total= .94
#alpha= .94

tab_itemscale(ewc_test)
#alpha= .93

unidim(ewc_test)  
#u= .90

#tam rasch model for WLE 

library(TAM)
tam1 <- tam(ewc_test, irtmodel = "1", verbose = FALSE) 
tam2 <- TAM::tam.mml.2pl(ewc_test)

tam.wle(tam1) 
#  WLE Reliability=0.83
tam.wle(tam2) 
#  WLE Reliability=0.84

#Reliability: Test information - reflects item properties (not sample)
RItif(ewc_test) + theme_rise()


#2) Bayesian IRT
# load required packages

# ----------- Code for Bayesian IRT ------------
# Analysis of the M elision data set using dichotomous IRT models
ewc<- EWC_VD_complete
colnames(ewc)
ewc<- ewc %>% dplyr::select(1:14, 16:20, 22:34) #w/o totals of subscale


#pivot the data frame into a long format
ewc_l<- ewc %>% pivot_longer(cols=c(6:32),
                    names_to='item',
                    values_to='scores')

# ---------- 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_ewc_1pl <- bf(scores ~ 1 + (1 | item) + (1 | Student_ID ))


# specify some weakly informative priors
prior_ewc_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "item")

# fit the 1PL model
fit_ewc_1pl <- brm(
  formula = formula_ewc_1pl,
  data = ewc_l,
  family = brmsfamily("bernoulli"),
  prior = prior_ewc_1pl)

# obtain basic summaries
summary(fit_ewc_1pl)
#investigate the posterior
plot(fit_ewc_1pl)  

# extract person parameters
ranef_ewc_1pl <- ranef(fit_ewc_1pl)

(person_pars_ewc_1pl <- ranef_ewc_1pl$Student_ID)


# extract item parameters
(item_pars_ewc_1pl <- coef(fit_ewc_1pl)$item)

# plot item parameters
item_pars_ewc_1pl[, , "Intercept"] %>%
  dplyr::as_tibble() %>%
  rownames_to_column() %>%
  dplyr::rename(item = "rowname") %>%
  dplyr::mutate(item = as.numeric(item)) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


# plot person parameters
person_pars_ewc_1pl[, , "Intercept"] %>%
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
formula_ewc_2pl <- bf(
  scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| item) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| item),
  nl = TRUE
)

# specify some weakly informative priors
prior_ewc_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_ewc_2pl <- brm(
  formula = formula_ewc_2pl,
  data = ewc_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_ewc_2pl,
  seed = 1234,
  file = "models/fit_ewc_2pl"
)

# obtain some basic summaries
summary(fit_ewc_2pl)
plot(fit_ewc_2pl, ask = FALSE)

# extract item parameters
(item_pars_ewc_2pl <- coef(fit_ewc_2pl)$item)

# plot item parameters
# difficulties
eta <- item_pars_ewc_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha <- item_pars_ewc_2pl[, , "logalpha_Intercept"] %>%
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
ranef_ewc_2pl <- ranef(fit_ewc_2pl)
(person_pars_ewc_2pl <- ranef_ewc_2pl$Student_ID)

# plot person parameters
person_pars_ewc_2pl[, , "eta_Intercept"] %>%
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

cor(person_pars_ewc_1pl, person_pars_ewc_2pl)
#= .99 ~ 1
#so no need to refer to 2PL

#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_ewc_1pl <- loo(fit_ewc_1pl)
loo_ewc_2pl <- loo(fit_ewc_2pl)
 
loo_ewc_compare <- loo_compare(loo_ewc_1pl, loo_ewc_2pl)
print(loo_ewc_compare, simplify = FALSE)
#            elpd_diff se_diff
#fit_me_2pl   0.0       0.0  
#fit_me_1pl -5.4        4.7
#FIT 2 is better (best fit model appears on top- all zeo)
#all models receive a difference score relative to the best model
#the LOO for fit 2 minus itself is 0. 




