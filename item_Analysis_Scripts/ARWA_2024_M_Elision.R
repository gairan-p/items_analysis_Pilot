#ARWA conference script for elision 

set.seed(6442)

me<- MElision_complete

colnames(me)
me<- me %>% dplyr::select(6:39)


#Descriptives and distributions

difficulty <- colMeans(me)

#Compute percent correct
total <- rowSums(me) # total score

# Overall responses
RIallresp(me)

#Floor/ceiling effects
RIrawdist(me) %>%  facet_grid(~Grade)
#Floor effect with 4.4% ~ .44 
#max score= 7.08% 

#Guttman structure
RIheatmap(me) +
  theme(axis.text.x = element_blank())
#item level descriptives
RItileplot(me)
#barstack is better

RIbarstack(me) +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise() 

#inter-item correlation

corPlot(me, upper= FALSE)  +
  theme_classic()

# For unidimensionality: 
#residual correlations
#and loadings on the first residual contrast

RIresidcorr(me, cutoff = 0.2)
#item-pairs show residual correlations above the cutoff value:


#first contrast loadings
RIloadLoc(me)

#1) MSA of elision 

## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.list_me <- check.monotonicity(me)

me_mono<- summary(monotonicity.list_me)
# vi/#ac= the relative number of violations 
#zsig= no. of significant violations

#items chronologically ordered, no fuss
me_mono<- as.data.frame(me_mono)
write_xlsx(me_mono, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/MEmono.xlsx")


plot(monotonicity.list, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)

# B: scalability  
#Compute scalability coefficients
#with CIs
coefH(me, ci = .95)
#   0.815 (0.027) [0.763, 0.868]

sc_me<- coefH(me)
#Hi and SE 
sc_me$Hi

#OVERALL test
#Scale H      se 
0.815 (0.027) 

#Automated Item Selection Procedure (AISP)
# Partition the the scale into mokken scales
aisp_me <- aisp(me)
#lowerbound=default= 0.3

## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore
restscore.list <- check.restscore(me)
summary(restscore.list)
plot(restscore.list, curves = "IRF",
     plot.ci=TRUE, color.ci = FALSE, ask=FALSE)

#C.2.#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.me <- check.iio(me)
iio.me$items.removed #NULL 
#$HT 	is the Coefficient HT for the remaining items.
[1] 0.7639738
plot(iio.list)

me_iio<- summary(iio.me)

me_iio<- as.data.frame(me_iio)
#items not ordered, so need to edit on excel

write_xlsx(me_iio, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/MEiio.xlsx")


# Compute the reliability of the scale
check.reliability(me)

#$MS
[1] 0.9810938

$alpha
[1] 0.9684055

$lambda.2
[1] 0.9727866

library(psych)
reliability(me)

#omega total= .98
tab_itemscale(me)

#dimensionality
unidim(me)
irt.fa(me)
fa.parallel(me)
EGA(me)

#tam rasch model for WLE or PSI
me_tam1 <- tam(as.matrix(me), irtmodel = "1", verbose = FALSE) 
me_tam2 <- TAM::tam.mml.2pl( me )
# Person Abilities
#generates a data frame - output related to estimation
me_WLE<- tam.wle(me_tam2)
#  WLE Reliability=0.968

#WLE is related to PSI
#Reliability: Test information - reflects item properties (not sample)
RItif(me) + theme_rise()

#though 4.4 % had floor effect based on the raw summed data
#the TIF 
library(ltm)
# an extended Person-Item Map termed PIccc
est<- RMX::plotPIccc(me_tam2,
               classical=TRUE,  lmar=3,  ylas=2)

#Bayesian IRT of MM elision 

# load required packages
bayes_pkgs <- c('TAM', 'RMX','mirt', 'ltm', 'psych', 'lme4', #basic psychometric 
          'brms', 'rstan') #bayseian IRT   

lapply(bayes_pkgs, library, character.only=TRUE)

me<- MElision_complete
colnames(me)


#pivot the data frame into a long format
me_l<- me %>% pivot_longer(c(6:39),
                           names_to='Melision_items', values_to='Melision_scores')

colnames(me_l)

me_l <- me_l %>% dplyr::select(1:5, 7:8)
#------ 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_me_1pl <- bf(Melision_scores ~ 1 + (1 | Melision_items) +
                       (1 | Student_ID ))


# specify some weakly informative priors
#truncated priors because M=0, SD=3 
prior_me_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "Melision_items")

# fit the 1PL model
fit_me_1pl <- brm(
  formula = formula_me_1pl,
  data = me_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_me_1pl)

# obtain basic summaries
summary(fit_me_1pl)
#investigate the posterior
plot(fit_me_1pl)  

# extract person parameters
ranef_me_1pl <- ranef(fit_me_1pl)

(person_pars_me_1pl <- ranef_me_1pl$Student_ID)


# extract item parameters
(item_pars_me_1pl <- coef(fit_me_1pl)$Melision_items)

# plot item parameters


item_pars_me_1pl[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(Melision_items = "rowname") %>%
  mutate(item = as.numeric(Melision_items)) %>%
  ggplot(aes(Melision_items, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


# plot person parameters
person_pars_me_1pl[, , "Intercept"] %>%
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
formula_me_2pl <- bf(
  Melision_scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| Melision_items) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| Melision_items),
  nl = TRUE
)

# specify some weakly informative priors
prior_me_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "Melision_items", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "Melision_items", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_me_2pl <- brm(
  formula = formula_me_2pl,
  data = me_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_me_2pl,
  seed = 1234,
  file = "models/fit_me_2pl"
)

# obtain some basic summaries
summary(fit_me_2pl)
plot(fit_me_2pl, ask = FALSE)


# extract item parameters
(item_pars_me_1pl <- coef(fit_me_2pl)$Melision_items)

# plot item parameters
# difficulties
eta <- item_pars_me_1pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha <- item_pars_me_1pl[, , "logalpha_Intercept"] %>%
  exp() %>%
  as_tibble() %>%
  rownames_to_column()

# plot difficulties and discrimination next to each other
ed_me<- 
  bind_rows(eta, alpha, .id = "nlpar") %>%
  rename(Melision_items = "rowname") %>%
  mutate(Melision_items = as.numeric(Melision_items)) %>%
  mutate(nlpar = factor(nlpar, labels = c("Easiness", "Discrimination"))) %>%
  ggplot(aes(Melision_items, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  facet_wrap("nlpar", scales = "free_x") +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number") +
  theme_doc()

# extract person parameters
ranef_me_2pl <- ranef(fit_me_2pl)
(person_pars_me_2pl <- ranef_me_2pl$Student_ID)

# plot person parameters
person_pars_me_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  dplyr:: select(-Est.Error) %>%
  arrange(Estimate) %>%
  mutate(id = seq_len(n())) %>%
  ggplot(aes(id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")+
  theme_doc()


#compare person parameters of 1pl and 2pl

cor(person_pars_me_1pl, person_pars_me_2pl)
#= .997 ~ 1
#so no need to refer to 2PL

#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_me_1pl <- loo(fit_me_1pl)
#Found 1 observations with a pareto_k > 0.7 in model 'fit_me_1pl'

loo_me_2pl <- loo(fit_me_2pl)
#Found 82 observations with a pareto_k > 0.7 in model 'fit_me_2pl'. 
#It is recommended to set 'moment_match = TRUE' in order to perform moment matching for problematic observations. 
#tried running with the 'moment_match' but kept crashing

loo_me_compare <- loo_compare(loo_me_1pl, loo_me_2pl)
print(loo_me_compare, simplify = FALSE)
#             elpd_diff se_diff elpd_loo se_elpd_loo
fit_me_2pl    0.0       0.0  -899.8     34.2     
fit_me_1pl  -31.7       7.6  -931.5     35.2     
p_loo  se_p_loo looic  se_looic
fit_me_2pl  137.5    7.1   1799.7   68.4  
fit_me_1pl  120.0    5.3   1863.1   70.4   

#FIT 2 is better (best fit model appears on top- all zeo)
#all models receive a difference score relative to the best model
#the LOO for fit 2 minus itself is 0. 

#we select and proceed with 2PL
# ---------- 2PL models with covariates ----------------------
# specify a model including item covariates

#DOES NOT WORK BECAUSE THE GRAPHS show mrv on y-axis
formula_me_2pl_cov1 <- bf(
  Melision_scores ~ Grade + Age + exp(logalpha) * eta,
  eta ~ 1 + (Grade |i| Melision_items) + (Grade | Student_ID),
  logalpha ~ 1 + (Grade |i| Melision_items),
  nl = TRUE
)

fit_me_2pl_cov1 <- brm(
  formula = formula_me_2pl_cov1,
  data = me_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_me_2pl,
  seed = 1234,
  file = "models/fit_me_2pl_cov1"
)
summary(fit_me_2pl_cov1)
conditional_effects(fit_me_2pl_cov1, "Age")
conditional_effects(fit_me_2pl_cov1, "Grade")
conditional_effects(fit_me_2pl_cov1, "Grade:Age")

#do not run the cov model 2 because too
#many warnings
