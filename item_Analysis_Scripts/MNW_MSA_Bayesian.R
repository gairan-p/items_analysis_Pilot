set.seed(45457)
##MM nonword
mnw<- MNW_complete
colnames(mnw)
#without demographics and totals, with practice items
mnw <- mnw %>% dplyr::select(6:71)
colnames(mnw)

#Descriptives and distributions
colMeans(mnw)

#Compute percent correct
rowSums(mnw) # total score

# Overall responses
RIallresp(mnw)
#0= 76%
#1= 24%

#Floor/ceiling effects
RIrawdist(mnw)
#min= 20.35, max= 0%

#Guttman structure
RIheatmap(mnw) +
  theme(axis.text.x = element_blank())

#response category
RItileplot(mnw)

#barstack is better
RIbarstack(mnw) + theme_minimal() +  theme_rise() 

#inter-item correlations 
corPlot(mnw, upper= FALSE)  +
  theme_minimal()

#this is mainly parametric
#residual correlations
RIresidcorr(mnw, cutoff = 0.2)
#item-pairs show residual correlations above the cutoff value:
#11 pairs

#first contrast loadings
RIloadLoc(mnw)
#only two items above 2, at_different= 2.1, at_empty= 2.6

cor.plot(mnw)

#1) MSA of mnw
library(tidyr)

## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.list <- check.monotonicity(mnw)
mono_mnw<- summary(monotonicity.list)

#z-max with values !0 = nonsignificant
#z-sig  
mono_mnw<- as.data.frame(mono_mnw)
write_xlsx(mono_mnw, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/MNWmono.xlsx")

plot(monotonicity.list, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)


# B: scalability  
#Compute scalability coefficients
#as can be seen from the monotonicity output
#only till item 70 where there are at least
#1 correct response

coefH(mnw, ci= .95)
#Scale H se      95% ci        
#  0.894 (0.018) [0.858, 0.930]

sc_mnw<-    coefH(mnw)
sc_mnw$Hi

#AISAP
aisp(mnw)
## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore- for the plot
#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.list <- check.iio(mnw)
summary(iio.list)
#$HT 	is the Coefficient HT for the remaining items.
#[1] 0.9024785

iio.list$items.removed 
#item 11, 1

iio.list$violations

iio_mnw<- summary(iio.list)
iio_mnw<- as.data.frame(iio_mnw)
write_xlsx(iio_mnw, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/MNWiio.xlsx")

#to get the item in the correct order as given 
#in the output, copy-paste the last 3 columns 
#with item order in excel then text-data and edit
plot(iio.list)

# Compute the reliability of the scale
check.reliability(mnw)

#$MS
[1] 0.93

#$alpha
[1]  0.92

#$lambda.2
[1] 0.93

reliability(mnw)

#omega total= .93
#alpha= .92

tab_itemscale(mnw)
#alpha= .93
#Mean inter-item-correlation=0.364

unidim(mnw)  
#u= .76

#tam rasch model for WLE 
tam1 <- tam(mnw, irtmodel = "1", verbose = FALSE) 
tam2 <- TAM::tam.mml.2pl(mnw)

tam.wle(tam1) 
#  WLE Reliability=0.87
tam.wle(tam2) 
#  WLE Reliability=0.89

#Reliability: Test information - reflects item properties (not sample)
RItif(mnw) + theme_rise()
#EXCLUDED many items

# an extended Person-Item Map termed PIccc
est<- RMX::plotPIccc(tam2,
                     classical=TRUE,  lmar=3,  ylas=2)

#2) Bayesian IRT
# load required packages

# ----------- Code for Bayesian IRT ------------
# Analysis of the M elision data set using dichotomous IRT models
mnw<- MNW_complete
colnames(mnw)

#pivot the data frame into a long format
mnw_l<- mnw %>% pivot_longer(cols=c(6:71),
                             names_to='item',
                             values_to='scores')

# ---------- 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_mnw_1pl <- bf(scores ~ 1 + (1 | item) + (1 | Student_ID ))


# specify some weakly informative priors
prior_mnw_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "item")

# fit the 1PL model
fit_mnw_1pl <- brm(
  formula = formula_mnw_1pl,
  data = mnw_l,
  family = brmsfamily("bernoulli"),
  prior = prior_mnw_1pl)

# obtain basic summaries
summary(fit_mnw_1pl)
#we learnt that fit 1pl is a better model 
#Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors. 


#investigate the posterior
plot(fit_mnw_1pl)  

# extract person parameters
ranef_mnw_1pl <- ranef(fit_mnw_1pl)

(person_pars_mnw_1pl <- ranef_mnw_1pl$Student_ID)


# extract item parameters
(item_pars_mnw_1pl <- coef(fit_mnw_1pl)$item)

# plot item parameters
item_pars_mnw_1pl[, , "Intercept"] %>%
  dplyr::as_tibble() %>%
  rownames_to_column() %>%
  dplyr::rename(item = "rowname") %>%
  dplyr::mutate(item = as.numeric(item)) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


# plot person parameters
person_pars_mnw_1pl[, , "Intercept"] %>%
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
formula_mnw_2pl <- bf(
  scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| item) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| item),
  nl = TRUE
)

# specify some weakly informative priors
prior_mnw_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_mnw_2pl <- brm(
  formula = formula_mnw_2pl,
  data = mnw_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_mnw_2pl,
  seed = 1234,
  file = "models/fit_mnw_2pl"
)

# obtain some basic summaries
summary(fit_mnw_2pl)
#model 2PL did not converge
plot(fit_mnw_2pl, ask = FALSE)

# extract item parameters
(item_pars_mnw_2pl <- coef(fit_mnw_2pl)$item)

# plot item parameters
# difficulties
eta <- item_pars_mnw_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha <- item_pars_mnw_2pl[, , "logalpha_Intercept"] %>%
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
ranef_mnw_2pl <- ranef(fit_mnw_2pl)
(person_pars_mnw_2pl <- ranef_mnw_2pl$Student_ID)

# plot person parameters
person_pars_mnw_2pl[, , "eta_Intercept"] %>%
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

cor(person_pars_mnw_1pl, person_pars_mnw_2pl)
#= .90 
#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_mnw_1pl <- loo(fit_mnw_1pl)
loo_mnw_2pl <- loo(fit_mnw_2pl)

loo_mnw_compare <- loo_compare(loo_mnw_1pl, loo_mnw_2pl)
print(loo_mnw_compare, simplify = FALSE)
#            elpd_diff se_diff
#fit_mnw_2pl    0.0       0.0  
#fit_mnw_1pl   -140.9     22.2 

#fit 2pl
Estimate   SE
elpd_loo   -887.0 37.8
p_loo       171.6 11.5
looic      1774.1 75.5
#FIT 2 is better (best fit model appears on top- all zeo)
#all models receive a difference score relative to the best model
#the LOO for fit 2 minus itself is 0, , the values in the top row are all zero.






