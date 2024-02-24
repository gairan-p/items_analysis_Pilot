#ARWA conference script for PPVT
#MPPVT= MM Receptive vocabulary= mrv
mrv<- M_PPVT
colnames(mrv)
mrv<-  mrv %>% dplyr::select(6:35)

mrv<- M_PPVT_itemed
colnames(mrv)
mrv<-  mrv %>% dplyr::select(3:32)


#Descriptives and distributions

difficulty <- colMeans(mrv)

#Compute percent correct
total <- rowSums(mrv) # total score

# Overall responses
RIallresp(mrv)

#Floor/ceiling effects
RIrawdist(mrv)
#Floor effect= 0

#Guttman structure
RIheatmap(mrv) +
  theme(axis.text.x = element_blank())

#item level descriptives
RItileplot(mrv)
RIbarstack(mrv) +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise()
#inter-item
cor.plot(mrv)

#1) MSA IRT 

## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.ppvt <- check.monotonicity(mrv)

summary(monotonicity.ppvt)
# vi/#ac= the relative number of violations 
#zsig= no. of significant violations

plot(monotonicity.ppvt, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)

mppvt_mono<- summary(monotonicity.ppvt)

#items chronologically ordered, no fuss
mppvt_mono<- as.data.frame(mppvt_mono)
write_xlsx(mppvt_mono, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/MPPVTmono.xlsx")

# B: scalability  
#Compute scalability coefficients
sc_mrv<- coefH(mrv, ci = .95)
#Hi and SE 
sc_mrv$Hi

#OVERALL test
#Scale H se      95% ci        
#0.394 (0.051) [0.294, 0.494]

#Automated Item Selection Procedure (AISP)
# Partition the the scale into mokken scales
aisp_mrv <- aisp(mrv)
#lowerbound=default= 0.3

## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore
restscore.list <- check.restscore(mrv)
summary(restscore.list)
plot(restscore.list, curves = "IRF",
     plot.ci=TRUE, color.ci = FALSE, ask=FALSE)

#C.2.#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.list <- check.iio(mrv)
summary(iio.list)
#zsig= number of sig violations 
#HT= 0.3296339
iio.list$items.removed
#item 3, 26, 23
# 4  26   7 
#$HT 	is the Coefficient HT for the remaining items.
[1] 0.7639738
iio.list$violations

iio_ppvt<- summary(iio.list)
iio_ppvt<- as.data.frame(iio_ppvt)
#items not ordered, so need to edit on excel

write_xlsx(iio_ppvt, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/MPPVTiio.xlsx")

#plots
plot(iio.list)
#i3 i26 i23 


# Compute the reliability of the scale
check.reliability(mrv)

#$MS= irrelevant because double M not met.  
[1] 0.90 

$alpha
[1] 0.9020716

$lambda.2
[1] 0.9065375

library(psych)
reliability(mrv)

#omega total= .92
library(sjPlot)
tab_itemscale(mrv)
#Mean inter-item-correlation=0.247

#dimensionality
unidim(mrv)
irt.fa(mrv)
fa.parallel(mrv)
library(EGAnet)
ega<- EGA(mrv)
summary(ega)

#tam rasch model for WLE or PSI

library(TAM)
#tam rasch model 
tam1 <- tam(as.matrix(mrv), irtmodel = "1", verbose = FALSE) 
tam2 <- TAM::tam.mml.2pl(mrv)
# Person Abilities
#generates a data frame - output related to estimation
tam.wle(tam1) 
#  WLE Reliability=0.845
tam.wle(tam2) 
#  WLE Reliability=0.807

#WLE is related to PSI
#Reliability: Test information - reflects item properties (not sample)
RItif(mrv) + theme_rise()

#But, we know from Bayesian IRT that we will choose 2PL

library(ltm)
# an extended Person-Item Map termed PIccc
RMX::plotPIccc(tam2,
                     classical=TRUE,  lmar=3,  ylas=2)
#2) Bayesian IRT 
# load required packages
bayes_pkgs <- c('TAM', 'RMX','mirt', 'ltm', 'psych', 'lme4', #basic psychometric 
                'brms', 'rstan') #bayseian IRT   

lapply(bayes_pkgs, library, character.only=TRUE)

# set rstan options
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 2)


##Bayesian IRT 
#MM PPVT (mrv=M receptive  vocabulary)
mrv<- M_PPVT
colnames(mrv)
#pivot the data frame into a long format
mrv_l<- mrv %>% pivot_longer(cols=c(6:35),
                           names_to='rv_item',
                           values_to='rv_scores')

colnames(mrv_l)
mrv_l <- mrv_l %>% dplyr::select(1:5, 7:8)

# ---------- 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_mrv_1pl <- bf(rv_scores ~ 1 + (1 | rv_item) +
                        (1 | Student_ID ))


# specify some weakly informative priors
#truncated priors (small regularisation), half-normal, so M=0, SD=3 
prior_mrv_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "rv_item")

# fit the 1PL model
fit_mrv_1pl <- brm(
  formula = formula_mrv_1pl,
  data = mrv_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_mrv_1pl)

# obtain basic summaries
summary(fit_mrv_1pl)
#investigate the posterior
plot(fit_mrv_1pl)  

# extract person parameters
ranef_mrv_1pl <- ranef(fit_mrv_1pl)
(person_pars_mrv_1pl <- ranef_mrv_1pl$Student_ID)

# extract item parameters
(item_pars_mrv_1pl <- coef(fit_mrv_1pl)$rv_item)

# plot item parameters
item_pars_mrv_1pl[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(rv_item = "rowname") %>%
  mutate(rv_item = as.numeric(rv_item)) %>%
  ggplot(aes(rv_item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


# plot person parameters
person_pars_mrv_1pl[, , "Intercept"] %>%
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
#nonlinear model in two parts-
#1) logalpha (discrimination) by item
#2) eta = the sum of person parameter and item easiness
#item easiness and discrimination correlated= |i| in both varying item terms

formula_mrv_2pl <- bf(
  rv_scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| rv_item) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| rv_item),
  nl = TRUE
)

# specify some weakly informative priors
# both on the intercepts of eta and logalpha, and the SDs of both item and person
#imposing normal(0, 3) prior and regression coefficient, b on all item parameters
#Hyperparameters- SDs and correlation matrices
#preferably with mode 0 such as half-normal and half-Cauchy (0, 5) priors
#fixing SD=1 for person parameters(3rd line) ensures for model identification
#nlpar= nonlinear parameters

prior_mrv_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "rv_item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "rv_item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_mrv_2pl <- brm(
  formula = formula_mrv_2pl,
  data = mrv_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_mrv_2pl,
  seed = 1234,
  file = "models/fit_mrv_2pl"
)

# obtain some basic summaries
summary(fit_mrv_2pl)
plot(fit_mrv_2pl, ask = FALSE)

#posterior distribution of person and item parameters extracted with 'coef'
#extract item parameters
(item_pars_mrv_1pl <- coef(fit_mrv_2pl)$rv_item)

# plot item parameters
# difficulties
eta_rv <- item_pars_mrv_1pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha_rv <- item_pars_mrv_1pl[, , "logalpha_Intercept"] %>%
  exp() %>%
  as_tibble() %>%
  rownames_to_column()

# plot difficulties and discrimination next to each other
bind_rows(eta_rv, alpha_rv, .id = "nlpar") %>%
  rename(rv_item = "rowname") %>%
  mutate(rv_item = as.numeric(rv_item)) %>%
  mutate(nlpar = factor(nlpar, labels = c("Easiness", "Discrimination"))) %>%
  ggplot(aes(rv_item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  facet_wrap("nlpar", scales = "free_x") +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")  +
  theme_doc()

# extract person parameters
ranef_mrv_2pl <- ranef(fit_mrv_2pl)
(person_pars_mrv_2pl <- ranef_mrv_2pl$Student_ID)

# plot person parameters
person_pars_mrv_2pl[, , "eta_Intercept"] %>%
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

cor(person_pars_mrv_1pl, person_pars_mrv_2pl)
#= .996 ~ 1
#so no need to refer to 2PL

#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_mrv_1pl <- loo(fit_mrv_1pl)
loo_mrv_2pl <- loo(fit_mrv_2pl)

loo_mrv_compare <- loo_compare(loo_mrv_1pl, loo_mrv_2pl)
print(loo_mrv_compare, simplify = FALSE)

#elpd_diff se_diff  
fit_mrv_2pl     0.0            
fit_mrv_1pl    -6.9       
#model 2 pl is better. proceed with 2PL


# ---------- 2PL models with covariates ----------------------
# specify a model including item covariates

#effect of grade to vary over items 
formula_mrv_2pl_cov1 <- bf(
  rv_scores ~ Grade + Age + exp(logalpha) * eta,
  eta ~ 1 + (Grade |i| rv_item) + (Grade | Student_ID),
  logalpha ~ 1 + (Grade |i| rv_item),
  nl = TRUE
)

fit_mrv_2pl_cov1 <- brm(
  formula = formula_mrv_2pl_cov1,
  data = mrv_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_mrv_2pl,
  seed = 1234,
  file = "models/fit_me_2pl_cov1"
)

summary(fit_mrv_2pl_cov1)
conditional_effects(fit_mrv_2pl_cov1, "Age")
conditional_effects(fit_mrv_2pl_cov1, "Grade")
conditional_effects(fit_mrv_2pl_cov1, "Grade:Age")

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
formula_mrv_3pl <- bf(
  rv_scores ~ 0.25 + 0.75 * inv_logit(beta + exp(logalpha) * theta),
  nl = TRUE,
  theta ~ 0 + (1 | Student_ID),
  beta ~ 1 + (1 |i| rv_item),
  logalpha ~ 1 + (1 |i| rv_item),
  family = brmsfamily("bernoulli", link = "logit")
)

 
# specify some weakly informative priors
prior_mrv_3pl <- 
  prior("normal(0, 2)", class = "b", nlpar = "beta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("normal(0, 1)", class = "sd", group = "Student_ID", nlpar = "theta") + 
  prior("normal(0, 3)", class = "sd", group = "rv_item", nlpar = "beta") +
  prior("normal(0, 1)", class = "sd", group = "rv_item", nlpar = "logalpha")


fit_mrv_3pl <- brm(
  formula = formula_mrv_3pl,
  data = mrv_l,
  prior = prior_mrv_3pl,
  seed = 1234,
  file = "models/fit_mrv_3pl"
)

fit_mrv_3pl <- add_loo(fit_mrv_3pl)

summary(fit_mrv_3pl)
plot(fit_mrv_3pl)

loo_mrv_2pl <- loo(fit_mrv_2pl)
loo_mrv_3pl <- loo(fit_mrv_3pl)

loo_mrv_compare <- loo_compare(loo_mrv_2pl, loo_mrv_3pl)
print(loo_mrv_compare, simplify = FALSE)

#            elpd_diff se_diff
fit_mrv_2pl    0.0       0.0 
fit_mrv_3pl -435.9      23.6

#fit 2 is better

#extract person parameters to compare correlations
ranef_mrv_3pl <- ranef(fit_mrv_3pl)
(person_pars_mrv_3pl <- ranef_mrv_3pl$Student_ID)

cor(person_pars_mrv_2pl, person_pars_mrv_3pl)
#.85

