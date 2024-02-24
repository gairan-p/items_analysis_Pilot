set.seed(45457)

##E word identification 
ewi<- EWI_complete
colnames(ewi)
#without demographics and totals, with practice items
ewi <- ewi %>% dplyr::select(6:51)
colnames(ewi)

#Descriptives and distributions
colMeans(ewi)

#Compute percent correct
rowSums(ewi) # total score

# Overall responses
RIallresp(ewi)
#0= 37.8 
#1=  62.2

#Floor/ceiling effects
RIrawdist(ewi)
#ITEM 1 AND 8 excluded 
#min= 0%, max= 1.77%

#Guttman structure
RIheatmap(ewi) +
  theme(axis.text.x = element_blank())

#response category
RItileplot(ewi)

#barstack is better
RIbarstack(ewi) + theme_minimal() +  theme_rise() 

#inter-item correlations 
corPlot(ewi, upper= FALSE) 

#this is mainly parametric
#residual correlations
RIresidcorr(ewi, cutoff = 0.2)
#item-pairs show residual correlations above the cutoff value:
#

#first contrast loadings
RIloadLoc(ewi)
#  items above 2


#1) MSA of ewi
library(tidyr)

## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.list <- check.monotonicity(ewi)
mono_ewi<- summary(monotonicity.list)
monotonicity.list$Hi
mono_ewi<- as.data.frame(mono_ewi)
write_xlsx(mono_ewi, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/EWImono.xlsx")

plot(monotonicity.list, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)


# B: scalability  
#Compute scalability coefficients
#as can be seen from the monotonicity output
#only till item 70 where there are at least
#1 correct response

coefH(ewi, ci= .95)
#covariance errors
library(caret)
#identify variance near zero
nearZeroVar(ewi, names = TRUE)
#9 items
#remove those items from the dataset
ewi_variance<- ewi %>% dplyr::select(-c(1:9)) 

colnames(ewi_variance) #37 items 

coefH(ewi_variance, ci= .95)

#Scale H se      95% ci        
#0.792 (0.026) [0.740, 0.843]

sc_ewi<-    coefH(ewi_variance)
sc_ewi$Hi

#AISAP
aisp(ewi)
## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore- for the plot
#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.list <- check.iio(ewi)
summary(iio.list)
#$HT 	is the Coefficient HT for the remaining items.
#[1] 0.836

iio.list$items.removed 
#wi22 wi23 wi21 wi35 wi12 
31   19   38   41   14

iio.list$violations

iio_ewi<- summary(iio.list)
iio_ewi<- as.data.frame(iio_ewi)
write_xlsx(iio_ewi, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/EWIiio.xlsx")

#to get the item in the correct order as given 
#in the output, copy-paste the last 3 columns 
#with item order in excel then text-data and edit
plot(iio.list)

# Compute the reliability of the scale
check.reliability(ewi)

#$MS
[1] 0.98

#$alpha
[1]  0.97

#$lambda.2
[1] 0.97


tab_itemscale(ewi)
#alpha= . 9
#Mean inter-item-correlation=0.NA


#tam rasch model for WLE 
tam1 <- tam(ewi, irtmodel = "1", verbose = FALSE) 
tam2 <- TAM::tam.mml.2pl(ewi)

tam.wle(tam1) 
#  WLE Reliability=0.87
tam.wle(tam2) 
#  WLE Reliability=0.89
#without item 1 and 8
#Reliability: Test information - reflects item properties (not sample)
RItif(ewi) + theme_rise()
#EXCLUDED many items

# an extended Person-Item Map termed PIccc
RMX::plotPIccc(tam2, classical=TRUE, 
               lmar=3,  ylas=2)
#the piccc did not work
#2) Bayesian IRT
# load required packages

# ----------- Code for Bayesian IRT ------------
# Analysis of the M elision data set using dichotomous IRT models
ewi<- ewi_complete
colnames(ewi)

#pivot the data frame into a long format
ewi_l<- ewi %>% pivot_longer(cols=c(6:51),
                             names_to='item',
                             values_to='scores')

# ---------- 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_ewi_1pl <- bf(scores ~ 1 + (1 | item) + (1 | Student_ID ))


# specify some weakly informative priors
prior_ewi_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "item")

# fit the 1PL model
fit_ewi_1pl <- brm(
  formula = formula_ewi_1pl,
  data = ewi_l,
  family = brmsfamily("bernoulli"),
  prior = prior_ewi_1pl)

# obtain basic summaries
summary(fit_ewi_1pl)
#we learnt that fit 1pl is a better model 
#Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors. 


#investigate the posterior
plot(fit_ewi_1pl)  

# extract person parameters
ranef_ewi_1pl <- ranef(fit_ewi_1pl)

(person_pars_ewi_1pl <- ranef_ewi_1pl$Student_ID)


# extract item parameters
(item_pars_ewi_1pl <- coef(fit_ewi_1pl)$item)

# plot item parameters
item_pars_ewi_1pl[, , "Intercept"] %>%
  dplyr::as_tibble() %>%
  rownames_to_column() %>%
  dplyr::rename(item = "rowname") %>%
  dplyr::mutate(item = as.numeric(item)) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


# plot person parameters
person_pars_ewi_1pl[, , "Intercept"] %>%
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
formula_ewi_2pl <- bf(
  scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| item) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| item),
  nl = TRUE
)

# specify some weakly informative priors
prior_ewi_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_ewi_2pl <- brm(
  formula = formula_ewi_2pl,
  data = ewi_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_ewi_2pl,
  seed = 1234,
  file = "models/fit_ewi_2pl"
)

# obtain some basic summaries
summary(fit_ewi_2pl)
#model 2PL converge
plot(fit_ewi_2pl, ask = FALSE)

# extract item parameters
(item_pars_ewi_2pl <- coef(fit_ewi_2pl)$item)

# plot item parameters
# difficulties
eta <- item_pars_ewi_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha <- item_pars_ewi_2pl[, , "logalpha_Intercept"] %>%
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
ranef_ewi_2pl <- ranef(fit_ewi_2pl)
(person_pars_ewi_2pl <- ranef_ewi_2pl$Student_ID)

# plot person parameters
person_pars_ewi_2pl[, , "eta_Intercept"] %>%
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

cor(person_pars_ewi_1pl, person_pars_ewi_2pl)
#= .90 
#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_ewi_1pl <- loo(fit_ewi_1pl)
loo_ewi_2pl <- loo(fit_ewi_2pl)

loo_ewi_compare <- loo_compare(loo_ewi_1pl, loo_ewi_2pl)
print(loo_ewi_compare, simplify = FALSE)
#            elpd_diff se_diff
#fit_ewi_2pl    0.0       0.0  
#fit_ewi_1pl   -38.5       8.8

#fit 2pl
#         Estimate   SE
# elpd_loo  -1161.8 36.7
p_loo       166.1  7.6
looic      2323.6 73.4
#1PL
Estimate   SE
elpd_loo  -1200.3 37.6
p_loo       145.0  5.8
looic      2400.7 75.2

#FIT 2 is better (best fit model appears on top- all zeo)
#all models receive a difference score relative to the best model
#the LOO for fit 2 minus itself is 0, , the values in the top row are all zero.






