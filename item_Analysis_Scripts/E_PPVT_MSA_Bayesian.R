set.seed(5367)

erv<- English_PPVT_itemed
erv<- English_PPVT

colnames(erv) 

erv<- erv %>% dplyr::select(6:35)

#Descriptives and distributions
# Overall responses
RIallresp(erv)

#Guttman structure
RIheatmap(erv) +
  theme(axis.text.x = element_blank())#large n/ids,
#so to remove the x-axis text cos just blur

#Floor/ceiling effects
RIrawdist(erv)
#floor= .88%. max= 4.42%

#item level descriptives
RItileplot(erv)
RIbarstack(erv) +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise()

cor.plot(erv)
#1) MSA IRT 
library(mokken)
## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.erv <- check.monotonicity(erv)
erv_mono<- summary(monotonicity.erv)
erv_mono<- as.data.frame(erv_mono)
write_xlsx(erv_mono, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/EPPVTmono.xlsx")


plot(monotonicity.erv, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)

# B: scalability  
#Compute scalability coefficients
coefH(erv)
sc_erv<- coefH(erv, ci = .95)

#OVERALL test
#Scale H se      95% ci        
#   0.357 (0.057) [0.246, 0.469]

#Automated Item Selection Procedure (AISP)
# Partition the the scale into mokken scales
aisp_erv <- aisp(erv)
#lowerbound=default= 0.3

## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore
restscore.erv <- check.restscore(erv)
summary(restscore.erv)
plot(restscore.list, curves = "IRF",
     plot.ci=TRUE, color.ci = FALSE, ask=FALSE)

#C.2.#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.erv <- check.iio(erv)
summary(iio.erv)
#zsig= number of sig violations 

iio.erv$items.removed
#feather     jaw 
#21      19

#$HT 	is the Coefficient HT for the remaining items.
#HT= 0.38

iio_erv<- summary(iio.erv)
iio_erv<- as.data.frame(iio_erv)
write_xlsx(iio_erv, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/EPPVTiio.xlsx")

#Step 3
plot(iio.list)


# Compute the reliability of the scale
check.reliability(erv) 

  0.881349 #MS, if double monotonicity met


0.8767625 #alpha

0.8848036 #lambda.2

#Reliabilities
reliability(erv)
#omega_h  omega.tot 
#    0.49    0.91
#alpha= .9

tab_itemscale(erv) 
#alpha= .87
unidim(erv)  
#u= .61

#tam rasch model for WLE 
#Reliability: Test information - reflects item properties (not sample)
RItif(erv) + theme_rise()


library(TAM)
erv_tam1 <- tam(as.matrix(erv), irtmodel = "1", verbose = FALSE) 
erv_tam2 <- TAM::tam.mml.2pl(erv)

tam.wle(erv_tam1) 
#  WLE Reliability=0.84
tam.wle(erv_tam2) 
#  WLE Reliability=0.77

anova(erv_tam1, erv_tam2)
#tam2 has lower AIC


# an extended Person-Item Map termed PIccc
RMX::plotPIccc(erv_tam2, classical=TRUE,  lmar=3,  ylas=2)

#2) Bayesian IRT 
# load required packages
bayes_pkgs <- c('TAM', 'RMX','mirt', 'ltm', 'psych', 'lme4', #basic psychometric 
                'brms', 'rstan') #bayseian IRT   

lapply(bayes_pkgs, library, character.only=TRUE)

# set rstan options
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 2)


#pivot the data frame into a long format
erv_l<- erv %>% pivot_longer(cols=c(6:35),
                           names_to='erv_item',
                           values_to='erv_scores')

colnames(erv_l)
erv_l <- erv_l %>% dplyr::select(1:5, 7:8)

# ---------- 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_erv_1pl <- bf(erv_scores ~ 1 + (1 | erv_item) +
                       (1 | Student_ID ))


# specify some weakly informative priors
#truncated priors (small regularisation), half-normal, so M=0, SD=3 
prior_erv_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "erv_item")

# fit the 1PL model
fit_erv_1pl <- brm(
  formula = formula_erv_1pl,
  data = erv_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_erv_1pl)

# obtain basic summaries
summary(fit_erv_1pl)
#investigate the posterior
plot(fit_erv_1pl)  

# extract person parameters
ranef_erv_1pl <- ranef(fit_erv_1pl)
(person_pars_erv_1pl <- ranef_erv_1pl$Student_ID)

#can do all these (before 2pl CODES) after confirming
#a better fit- running it before hand is a waste of time

# extract item parameters
(item_pars_erv_1pl <- coef(fit_erv_1pl)$erv_item)

# plot item parameters
item_pars_erv_1pl[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(erv_item = "rowname") %>%
  mutate(erv_item = as.numeric(erv_item)) %>%
  ggplot(aes(erv_item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")

# ---------- 2PL models ----------------------
## specify a 2PL model
#nonlinear model in two parts-
#1) logalpha (discrimination) by item
#2) eta = the sum of person parameter and item easiness
#item easiness and discrimination correlated= |i| in both varying item terms

formula_erv_2pl <- bf(
  erv_scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| erv_item) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| erv_item),
  nl = TRUE
)

# specify some weakly informative priors
# both on the intercepts of eta and logalpha, and the SDs of both item and person
#imposing normal(0, 3) prior and regression coefficient, b on all item parameters
#Hyperparameters- SDs and correlation matrices
#preferably with mode 0 such as half-normal and half-Cauchy (0, 5) priors
#fixing SD=1 for person parameters(3rd line) ensures for model identification
#nlpar= nonlinear parameters

prior_erv_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "erv_item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "erv_item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_erv_2pl <- brm(
  formula = formula_erv_2pl,
  data = erv_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_erv_2pl,
  seed = 1234,
  file = "models/fit_erv_2pl"
)

# obtain some basic summaries
summary(fit_ee_2pl)
plot(fit_ee_2pl, ask = FALSE)

#posterior distribution of person and item parameters extracted with 'coef'
#extract item parameters
(item_pars_erv_1pl <- coef(fit_erv_2pl)$erv_item)

# plot item parameters
# difficulties
eta_rv <- item_pars_ee_1pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha_rv <- item_pars_ee_1pl[, , "logalpha_Intercept"] %>%
  exp() %>%
  as_tibble() %>%
  rownames_to_column()

# plot difficulties and discrimination next to each other
bind_rows(eta_rv, alpha_rv, .id = "nlpar") %>%
  rename(erv_item = "rowname") %>%
  mutate(erv_item = as.numeric(erv_item)) %>%
  mutate(nlpar = factor(nlpar, labels = c("Easiness", "Discrimination"))) %>%
  ggplot(aes(erv_item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  facet_wrap("nlpar", scales = "free_x") +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")  +
  theme_minimal()

# extract person parameters
ranef_erv_2pl <- ranef(fit_erv_2pl)
(person_pars_erv_2pl <- ranef_erv_2pl$Student_ID)

# plot person parameters
person_pars_erv_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  dplyr::select(-Est.Error) %>%
  arrange(Estimate) %>%
  mutate(id = seq_len(n())) %>%
  ggplot(aes(id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")  +
  theme_minimal()

#compare person parameters of 1pl and 2pl

cor(person_pars_erv_1pl, person_pars_erv_2pl)
#= .98 ~ 1

#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_erv_1pl <- loo(fit_erv_1pl)
loo_erv_2pl <- loo(fit_erv_2pl)

loo_erv_compare <- loo_compare(loo_erv_1pl, loo_erv_2pl)
print(loo_erv_compare, simplify = FALSE)

           #elpd_diff se_diff  
fit_erv_2pl     0.0      0      
fit_erv_1pl    -36.5       7.9 
#model 2 pl is better. proceed with 2PL


# ---------- 2PL models with covariates ----------------------
# specify a model including item covariates

#effect of grade to vary over items 
formula_erv_2pl_cov1 <- bf(
  erv_scores ~ Grade + Age + exp(logalpha) * eta,
  eta ~ 1 + (Grade |i| erv_item) + (Grade | Student_ID),
  logalpha ~ 1 + (Grade |i| erv_item),
  nl = TRUE
)

fit_erv_2pl_cov1 <- brm(
  formula = formula_erv_2pl_cov1,
  data = erv_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_erv_2pl,
  seed = 1234,
  file = "models/fit_erv_2pl_cov1"
)

summary(fit_ee_2pl_cov1)
conditional_effects(fit_ee_2pl_cov1, "Age")
conditional_effects(fit_ee_2pl_cov1, "Grade")
conditional_effects(fit_ee_2pl_cov1, "Grade:Age")

#PPVT has 4 images per item
#guessing could be involved

# ---------- 3PL models ----------------------
#from an older brms irt paper 
#https://github.com/paul-buerkner/SPM-IRT-models/blob/master/SPM_IRT_analysis.Rmd 
#referred in the 2021 paper for 3PL

# 3PL model with known guessing parameter
#out of 4, 1 is correct, so guessing probability of 25%
#fixed/known guessing probabality

### MCMC with hierarchical item priors
formula_erv_3pl <- bf(
  erv_scores ~ 0.25 + 0.75 * inv_logit(beta + exp(logalpha) * theta),
  nl = TRUE,
  theta ~ 0 + (1 | Student_ID),
  beta ~ 1 + (1 |i| erv_item),
  logalpha ~ 1 + (1 |i| erv_item),
  family = brmsfamily("bernoulli", link = "logit")
)


# specify some weakly informative priors
prior_erv_3pl <- 
  prior("normal(0, 2)", class = "b", nlpar = "beta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("normal(0, 1)", class = "sd", group = "Student_ID", nlpar = "theta") + 
  prior("normal(0, 3)", class = "sd", group = "erv_item", nlpar = "beta") +
  prior("normal(0, 1)", class = "sd", group = "erv_item", nlpar = "logalpha")


fit_erv_3pl <- brm(
  formula = formula_erv_3pl,
  data = erv_l,
  prior = prior_erv_3pl,
  seed = 1234,
  file = "models/fit_erv_3pl"
)


summary(fit_erv_3pl)
plot(fit_erv_3pl)

loo_erv_2pl  
loo_erv_3pl <- loo(fit_erv_3pl)

loo_erv23_compare <- loo_compare(loo_erv_2pl, loo_erv_3pl)
print(loo_erv23_compare, simplify = FALSE)

#            elpd_diff se_diff
fit_erv_2pl    0.0       0.0 
fit_erv_3pl -435.9      23.6

#fit 2 is better

#extract person parameters to compare correlations
ranef_erv_3pl <- ranef(fit_erv_3pl)
(person_pars_erv_3pl <- ranef_erv_3pl$Student_ID)

cor(person_pars_erv_2pl, person_pars_erv_3pl)
#.85



