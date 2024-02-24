set.seed(45457)
##ENW nonword

enw<- ENW_complete
colnames(enw)
#without demographics and totals, with practice items
enw <- enw %>% dplyr::select(6:71)

colnames(enw)

#Descriptives and distributions
colMeans(enw)

#Compute percent correct
rowSums(enw) # total score

# Overall responses
RIallresp(enw)
#0= 55%
#1= 45%

#Floor/ceiling effects
RIrawdist(enw)
#min= 0%, max= 0%

#Guttman structure
RIheatmap(enw) +
  theme(axis.text.x = element_blank())

#scores category/frequency
RItileplot(enw)

#barstack is better
#one half of the test
  RIbarstack(enw) +
  theme_minimal() + # theming is optional, see section 11 for more on this
  theme_rise() 

#inter-item 
cor.plot(enw)
#this is mainly parametric
#residual correlations
RIresidcorr(enw, cutoff = 0.2)
#item-pairs show residual correlations above the cutoff value:
#11 pairs

#first contrast loadings
RIloadLoc(enw)
#only two items above 2, at_different= 2.1, at_empty= 2.6

cor.plot(enw)

#1) MSA of enw

library(mokken)
library(tidyr)

## A. Monotonicity:
#Z statistic adjusted monotonicity for each item:
monotonicity.list <- check.monotonicity(enw)
monotonicity.list$Hi
#Na for item 66- the last item 

mono_enw<- summary(monotonicity.list)
#z-max with values !0 = nonsignificant
#z-sig  
class(mono)
mono_enw<- as.data.frame(mono_enw)
write_xlsx(mono_enw, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/ENWmono.xlsx")

plot(monotonicity.list, curves = "IRF", 
     plot.ci=TRUE, color.ci = TRUE, ask=FALSE)

# B: scalability  
#Compute scalability coefficients
#as can be seen from the monotonicity output
#only till item 70 where there are at least
#1 correct response

coefH(enw, ci= .95)
#One or more variables have zero variance

library(caret)
#identify variance near zero
nearZeroVar(enw, names = TRUE)
#10 items
#remove those items from the dataset
enw_variance<- enw %>% dplyr::select(-c(1,58:66)) 

colnames(enw_variance) #56 items 

coefH(enw_variance, ci= .95)
#H
#Scale H se      95% ci        
#0.806 (0.023) [0.760, 0.851]

sc_enw_var<-  coefH(enw_variance)
sc_enw_var$Hi

#AISAP
aisp(enw)
## C. Invariant Ordering:
# Investigate the assumption of non-intersecting ISRFs 
#using method restscore- for the plot
#better alternative is Investigate invariant item orderings (IIO)
#MIIO= default
iio.list <- check.iio(enw)
summary(iio.list)
#$HT 	is the Coefficient HT for the remaining items.
#[1] 0.8525876

iio.list$items.removed 
#item 21 and 10

iio.list$violations


iio.enw<- summary(iio.list)
iio.enw<- as.data.frame(iio.enw)
write_xlsx(iio.enw, "C:/Users/psyuser/Documents/Reading_Pilot_Scores/items_analysis_Pilot/ENWiio.xlsx")

plot(iio.list)

# Compute the reliability of the scale
check.reliability(enw)

#$MS
[1] 0.93

#$alpha
[1]  0.92

#$lambda.2
[1] 0.93

reliability(enw)

#omega total= .93
#alpha= .92

tab_itemscale(enw)
#alpha= .93
#Mean inter-item-correlation=0.364

unidim(enw)  
#u= .76

#tam rasch model for WLE 
tam1 <- tam(enw, irtmodel = "1", verbose = FALSE) 
tam2 <- TAM::tam.mml.2pl(enw)

tam.wle(tam1) 
#  WLE Reliability=0.87
tam.wle(tam2) 
#  WLE Reliability=0.89

#Reliability: Test information - reflects item properties (not sample)
RItif(enw) + theme_rise()
#EXCLUDED many items

# an extended Person-Item Map termed PIccc
est<- RMX::plotPIccc(tam2,
                     classical=TRUE,  lmar=3,  ylas=2)

#2) Bayesian IRT
# load required packages

# ----------- Code for Bayesian IRT ------------
# Analysis of the M elision data set using dichotomous IRT models
enw<- ENW_complete
colnames(enw)

#pivot the data frame into a long format
enw_l<- enw %>% pivot_longer(cols=c(6:71),
                             names_to='item',
                             values_to='scores')

# ---------- 1PL models ----------------------
# specify a 1PL model in brms
#Both item and person parameters have hierarchical priors:

formula_enw_1pl <- bf(scores ~ 1 + (1 | item) + (1 | Student_ID ))


# specify some weakly informative priors
prior_enw_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "Student_ID") + 
  prior("normal(0, 3)", class =  "sd", group = "item")

# fit the 1PL model
fit_enw_1pl <- brm(
  formula = formula_enw_1pl,
  data = enw_l,
  family = brmsfamily("bernoulli"),
  prior = prior_enw_1pl)

# obtain basic summaries
summary(fit_enw_1pl)
#we learnt that fit 1pl is a better model 
#Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors. 


#investigate the posterior
plot(fit_enw_1pl)  

# extract person parameters
ranef_enw_1pl <- ranef(fit_enw_1pl)

(person_pars_enw_1pl <- ranef_enw_1pl$Student_ID)


# extract item parameters
(item_pars_enw_1pl <- coef(fit_enw_1pl)$item)

# plot item parameters
item_pars_enw_1pl[, , "Intercept"] %>%
  dplyr::as_tibble() %>%
  rownames_to_column() %>%
  dplyr::rename(item = "rowname") %>%
  dplyr::mutate(item = as.numeric(item)) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")


# plot person parameters
person_pars_enw_1pl[, , "Intercept"] %>%
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
formula_enw_2pl <- bf(
  scores ~ exp(logalpha) * eta,
  eta ~ 1 + (1 |i| item) + (1 | Student_ID),
  logalpha ~ 1 + (1 |i| item),
  nl = TRUE
)

# specify some weakly informative priors
prior_enw_2pl <- 
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "Student_ID", nlpar = "eta") + 
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

# fit the 2PL model
# this models throws some convergence warnings which are false
# positives and can be safely ignored
fit_enw_2pl <- brm(
  formula = formula_enw_2pl,
  data = enw_l,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_enw_2pl,
  seed = 1234,
  file = "models/fit_enw_2pl"
)

# obtain some basic summaries
summary(fit_enw_2pl)
#model 2PL did not converge
plot(fit_enw_2pl, ask = FALSE)

# extract item parameters
(item_pars_enw_2pl <- coef(fit_enw_2pl)$item)

# plot item parameters
# difficulties
eta <- item_pars_enw_2pl[, , "eta_Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()

# discriminations
alpha <- item_pars_enw_2pl[, , "logalpha_Intercept"] %>%
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
ranef_enw_2pl <- ranef(fit_enw_2pl)
(person_pars_enw_2pl <- ranef_enw_2pl$Student_ID)

# plot person parameters
person_pars_enw_2pl[, , "eta_Intercept"] %>%
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

cor(person_pars_enw_1pl, person_pars_enw_2pl)
#= .90 
#The loo method implements approximate leave-one-out
#cross-validation via Pareto-Smoothed importance sampling:
loo_enw_1pl <- loo(fit_enw_1pl)
loo_enw_2pl <- loo(fit_enw_2pl)

loo_enw_compare <- loo_compare(loo_enw_1pl, loo_enw_2pl)
print(loo_enw_compare, simplify = FALSE)
#            elpd_diff se_diff
#fit_enw_2pl    0.0       0.0  
#fit_enw_1pl   -148.5      17.2 

#fit 2pl
Estimate   SE
elpd_loo   -887.0 37.8
p_loo       171.6 11.5
looic      1774.1 75.5
#FIT 2 is better (best fit model appears on top- all zeo)
#all models receive a difference score relative to the best model
#the LOO for fit 2 minus itself is 0, , the values in the top row are all zero.

