set.seed(5367)

ee<- EElision_complete
colnames(ee) 

# DIF demographic variables
dif.ee_Grade <- factor(ee$Grade)
dif.ee_Age <- factor(ee$Age)
dif.ee_Gender<- factor(ee$Gender)
dif.ee_Lang <- factor(ee$Language_Background)

#Then remove all from the item dataset
ee$Gender <- NULL
ee$Grade <- NULL
ee$Age<- NULL
ee$Language_Background<- NULL
ee$ee_Total<- NULL 
colnames(ee) 

ee<- ee %>% dplyr::select(6:39)
class(ee)

#Descriptives and distributions
# Overall responses
RIallresp(ee)

#Guttman structure
RIheatmap(ee) +
  theme(axis.text.x = element_blank())#large n/ids,
#so to remove the x-axis text cos just blur

#Floor/ceiling effects
RIrawdist(ee)
#floor= .88%. max= 12.39%

#item level descriptives
RItileplot(ee)
#barstack is better

RIbarstack(ee) +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise() 

cor.plot(ee) #too big
corPlot(ee, upper= FALSE)  +
  theme_classic()

#1) MSA IRT 
library(mokken)
## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.ee <- check.monotonicity(ee)

ee_mono<- summary(monotonicity.ee)
#items chronologically ordered, no fuss
ee_mono<- as.data.frame(ee_mono)
write_xlsx(ee_mono, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/EEmono.xlsx")

plot(monotonicity.ee, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)

# B: scalability  
#Compute scalability coefficients
#for table purpose without ci
coefH(ee)
sc_ee<- coefH(ee, ci = .95)
#Hi and SE 
sc_ee$Hi

#OVERALL test
#Scale H se      95% ci        
#0.781 (0.032) [0.718, 0.845]

#Automated Item Selection Procedure (AISP)
# Partition the the scale into mokken scales
aisp_ee <- aisp(ee)
#lowerbound=default= 0.3
#All= 1
# Use a significant test for criteria Hi > c (rather than the point estimate)

st_aisp<- aisp(ee,  type.z = "WB", test.Hi = TRUE, verbose = TRUE) 
#same as above, except item 4, 7

## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore
restscore.list <- check.restscore(ee)
summary(restscore.list)
plot(restscore.list, curves = "IRF",
     plot.ci=TRUE, color.ci = FALSE, ask=FALSE)

#C.2.#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.list <- check.iio(ee)
summary(iio.list)
#zsig= number of sig violations 

iio.list$items.removed
#i16 i21 i23 i18 
#20  18  19  24 
#$HT 	is the Coefficient HT for the remaining items.
#HT= 0.7419757

iio.list$violations

iio_ee<- summary(iio.list)
iio_ee<- as.data.frame(iio_ee)
#items not ordered, so need to edit on excel

write_xlsx(iio_ee, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/EEiio.xlsx")

#Step 5

plot(iio.list)

# Compute the reliability of the scale
check.reliability(ee) 
#if double monotonicity met

#MS [1] 0.9801374

#alpha
[1] 0.9653969

#lambda.2
[1] 0.9710067

#Reliabilities
reliability(ee)
#omega_h  omega.tot 
#    0.62    0.97

tab_itemscale(ee)
unidim(ee)  
#u= .80

#tam rasch model for WLE 

tam1 <- tam(as.matrix(ee), irtmodel = "1", verbose = FALSE) 
tam2 <- TAM::tam.mml.2pl(ee)

tam.wle(tam1) 
#  WLE Reliability=0.93
tam.wle(tam2) 
#  WLE Reliability=0.96

#Reliability: Test information - reflects item properties (not sample)
RItif(ee) + theme_rise()

# an extended Person-Item Map termed PIccc
RMX::plotPIccc(tam2, classical=TRUE,  lmar=3,  ylas=2)


classical=TRUE,  lmar=3,  ylas=2)
#2) Bayesian IRT 

ee<- EElision_complete
colnames(ee)

#pivot the data frame into a long format
ee_l<- ee %>% pivot_longer(cols=c(6:39),
                             names_to='elision_item',
                             values_to='ee_scores')

colnames(ee_l)
ee_l <- ee_l %>% dplyr::select(1:5, 7:8)

# ---------- 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_ee_1pl <- bf(ee_scores ~ 1 + (1 | elision_item) +
                        (1 | Student_ID ))


# specify some weakly informative priors
#truncated priors (small regularisation), half-normal, so M=0, SD=3 
prior_ee_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "elision_item")

# fit the 1PL model
fit_ee_1pl <- brm(
  formula = formula_ee_1pl,
  data = ee_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_ee_1pl)

# obtain basic summaries
summary(fit_ee_1pl)
#investigate the posterior
plot(fit_ee_1pl)  

# extract person parameters
ranef_ee_1pl <- ranef(fit_ee_1pl)
(person_pars_ee_1pl <- ranef_ee_1pl$Student_ID)

#can do all these (before 2pl CODES) after confirming
#a better fit- running it before hand is a waste of time

# extract item parameters
(item_pars_ee_1pl <- coef(fit_ee_1pl)$elision_item)

# plot item parameters
item_pars_ee_1pl[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(elision_item = "rowname") %>%
  mutate(elision_item = as.numeric(elision_item)) %>%
  ggplot(aes(elision_item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


# plot person parameters
person_pars_ee_1pl[, , "Intercept"] %>%
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

formula_ee_2pl <- bf(
  ee_scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| elision_item) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| elision_item),
  nl = TRUE
)

# specify some weakly informative priors
# both on the intercepts of eta and logalpha, and the SDs of both item and person
#imposing normal(0, 3) prior and regression coefficient, b on all item parameters
#Hyperparameters- SDs and correlation matrices
#preferably with mode 0 such as half-normal and half-Cauchy (0, 5) priors
#fixing SD=1 for person parameters(3rd line) ensures for model identification
#nlpar= nonlinear parameters

prior_ee_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "elision_item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "elision_item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_ee_2pl <- brm(
  formula = formula_ee_2pl,
  data = ee_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_ee_2pl,
  seed = 1234,
  file = "models/fit_ee_2pl"
)


# obtain some basic summaries
summary(fit_ee_2pl)
plot(fit_ee_2pl, ask = FALSE)

#posterior distribution of person and item parameters extracted with 'coef'
#extract item parameters
(item_pars_ee_2pl <- coef(fit_ee_2pl)$elision_item)

# plot item parameters
# difficulties
eta_ee <- item_pars_ee_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha_ee <- item_pars_ee_2pl[, , "logalpha_Intercept"] %>%
  exp() %>%
  as_tibble() %>%
  rownames_to_column()

library(ggplot2)
# plot difficulties and discrimination next to each other
bind_rows(eta_ee, alpha_ee, .id = "nlpar") %>%
  dplyr::rename(elision_item = "rowname") %>%
  dplyr::mutate(elision_item = as.numeric(elision_item)) %>%
  dplyr::mutate(nlpar = factor(nlpar, labels = c("Easiness", "Discrimination"))) %>%
  ggplot(aes(elision_item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  facet_wrap("nlpar", scales = "free_x") +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")  +
  theme_minimal()

# extract person parameters
ranef_ee_2pl <- ranef(fit_ee_2pl)
(person_pars_ee_2pl <- ranef_ee_2pl$Student_ID)

# plot person parameters
person_pars_ee_2pl[, , "eta_Intercept"] %>%
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

cor(person_pars_ee_1pl, person_pars_ee_2pl)
#= .993 ~ 1
#so no need to refer to 2PL?

#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_ee_1pl <- loo(fit_ee_1pl)
loo_ee_2pl <- loo(fit_ee_2pl)
#Found 52 observations with a pareto_k > 0.7 in model 'fit_ee_2pl'

loo_ee_compare <- loo_compare(loo_ee_1pl, loo_ee_2pl)
print(loo_ee_compare, simplify = FALSE)

             #elpd_diff se_diff  
fit_ee_2pl     0.0       0.0       
fit_ee_1pl    -59.6     9.1  
#model 2 pl is better. proceed with 2PL


# ---------- 2PL models with covariates ----------------------
# specify a model including item covariates

#effect of grade to vary over items 
formula_ee_2pl_cov1 <- bf(
  ee_scores ~ Grade + Age + exp(logalpha) * eta,
  eta ~ 1 + (Grade |i| elision_item) + (Grade | Student_ID),
  logalpha ~ 1 + (Grade |i| elision_item),
  nl = TRUE
)

fit_ee_2pl_cov1 <- brm(
  formula = formula_ee_2pl_cov1,
  data = ee_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_ee_2pl,
  seed = 1234,
  file = "models/fit_me_2pl_cov1"
)

summary(fit_ee_2pl_cov1)
