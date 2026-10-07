# Mediation effects calculated over distinct covariate patterns rather than
# per person, to avoid mediation::mediate() running out of memory.
#
# Follows mediate()'s quasi-Bayesian approach (boot = FALSE):
#   - coefficients for both models drawn from a multivariate normal using
#     vcov() (so survreg's robust variance is used if fitted with robust = TRUE)
#   - for polr mediators, only the coefficients are drawn; the cut-points stay
#     fixed at model.m$zeta, as in mediate()
#   - survreg predictions are passed through the distribution's inverse
#     transform (exp for Weibull), so effects are on the time scale (days)
#   - effects averaged over everyone in the data; point estimates are means of
#     the draws, except proportion mediated which uses medians; CIs are
#     percentiles of the draws
#
# Two differences from mediate():
#   - predictions only depend on the covariate pattern (treatment is set
#     counterfactually for everyone), so they are made once per pattern and
#     averaged using the number of people with each pattern. Memory scales
#     with patterns x sims instead of people x sims
#   - instead of drawing a random mediator value per person, the outcome is
#     averaged over mediator categories weighted by their predicted
#     probability. Same estimand, but less Monte Carlo noise
#
# Supports glm (binomial, 0/1 numeric mediator) or MASS::polr (ordered factor
# mediator) mediator models, and lm or survreg outcome models. Assumes all
# covariates are categorical, and that both models were fitted to `data`.
mediate_collapsed <- function(med_fit, out_fit, data, treat, mediator,
                              treat.value, control.value,
                              sims = 1000, conf.level = 0.95) {
  is_polr <- inherits(med_fit, "polr")
  is_survreg <- inherits(out_fit, "survreg")

  # covariates other than treatment and mediator, from both models
  covs <- setdiff(
    union(all.vars(delete.response(terms(med_fit))), all.vars(delete.response(terms(out_fit)))),
    c(treat, mediator)
  )

  # distinct covariate patterns among people used in the models, and how
  # many people have each one
  model_vars <- union(all.vars(terms(med_fit)), all.vars(terms(out_fit)))
  data <- data[complete.cases(data[, model_vars]), ]
  pat <- data %>% count(across(all_of(covs)), name = "n_pat")
  w <- pat$n_pat

  # set a column to one value for every pattern, keeping its factor levels
  set_value <- function(df, var, value) {
    template <- data[[var]]
    df[[var]] <- if (is.factor(template)) {
      factor(rep(value, nrow(df)), levels = levels(template), ordered = is.ordered(template))
    } else {
      rep(value, nrow(df))
    }
    df
  }

  design <- function(fit, newdata) {
    tt <- delete.response(terms(fit))
    mf <- model.frame(tt, newdata, xlev = fit$xlevels)
    model.matrix(tt, mf, contrasts.arg = fit$contrasts)
  }

  # coefficient draws
  m_names <- names(coef(med_fit))
  y_names <- names(coef(out_fit))
  if (anyNA(coef(med_fit)) || anyNA(coef(out_fit))) {
    stop("NA in model coefficients; rerun models with nonsingular design matrix")
  }
  bm <- MASS::mvrnorm(sims, coef(med_fit), vcov(med_fit)[m_names, m_names, drop = FALSE])
  by <- MASS::mvrnorm(sims, coef(out_fit), vcov(out_fit)[y_names, y_names, drop = FALSE])

  if (is_polr) {
    m_levels <- med_fit$lev
    linkfn <- switch(med_fit$method,
      logistic = plogis,
      probit = pnorm,
      cauchit = pcauchy,
      loglog = function(x) exp(-exp(-x)),
      cloglog = function(x) 1 - exp(-exp(x))
    )
  } else {
    m_levels <- c(0, 1)
  }

  itrans <- if (is_survreg) survival::survreg.distributions[[out_fit$dist]]$itrans else NULL
  if (is.null(itrans)) itrans <- identity

  arms <- c(`1` = treat.value, `0` = control.value)

  # P(M = each category) for each treatment arm: list of patterns x sims matrices
  med_probs <- lapply(arms, function(a) {
    X <- design(med_fit, set_value(pat, treat, a))[, m_names, drop = FALSE]
    eta <- X %*% t(bm)
    if (is_polr) {
      cum <- c(list(0 * eta), lapply(med_fit$zeta, function(z) linkfn(z - eta)), list(0 * eta + 1))
      lapply(seq_along(m_levels), function(j) cum[[j + 1]] - cum[[j]])
    } else {
      p1 <- med_fit$family$linkinv(eta)
      list(1 - p1, p1)
    }
  })

  # predicted outcome for each treatment arm x mediator category: patterns x sims
  y_pred <- lapply(arms, function(a) {
    lapply(m_levels, function(m) {
      nd <- set_value(set_value(pat, treat, a), mediator, m)
      X <- design(out_fit, nd)[, y_names, drop = FALSE]
      itrans(X %*% t(by))
    })
  })

  # average outcome over everyone with treatment t and mediator distribution
  # from treatment t_m, for each draw
  expected <- function(t, t_m) {
    ey <- Reduce(`+`, Map(`*`, y_pred[[t]], med_probs[[t_m]]))
    colSums(w * ey) / sum(w)
  }
  e11 <- expected("1", "1")
  e10 <- expected("1", "0")
  e01 <- expected("0", "1")
  e00 <- expected("0", "0")

  delta.1 <- e11 - e10
  delta.0 <- e01 - e00
  zeta.1 <- e11 - e01
  zeta.0 <- e10 - e00

  tau <- (zeta.1 + delta.0 + zeta.0 + delta.1) / 2
  nu.0 <- delta.0 / tau
  nu.1 <- delta.1 / tau
  delta.avg <- (delta.1 + delta.0) / 2
  zeta.avg <- (zeta.1 + zeta.0) / 2
  nu.avg <- (nu.1 + nu.0) / 2

  d.avg <- (mean(delta.0) + mean(delta.1)) / 2
  z.avg <- (mean(zeta.0) + mean(zeta.1)) / 2
  tau.coef <- mean(tau)
  n.avg <- (median(nu.0) + median(nu.1)) / 2

  low <- (1 - conf.level) / 2
  ci <- function(x) quantile(x, c(low, 1 - low), na.rm = TRUE)
  pval <- function(x, xhat) {
    if (xhat == 0) return(1)
    min(2 * min(mean(x > 0), mean(x < 0)), 1)
  }

  data.frame(
    term      = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
    estimate  = c(d.avg, z.avg, tau.coef, n.avg),
    conf.low  = c(ci(delta.avg)[1], ci(zeta.avg)[1], ci(tau)[1], ci(nu.avg)[1]),
    conf.high = c(ci(delta.avg)[2], ci(zeta.avg)[2], ci(tau)[2], ci(nu.avg)[2]),
    p.value   = c(pval(delta.avg, d.avg), pval(zeta.avg, z.avg), pval(tau, tau.coef), pval(nu.avg, n.avg)),
    row.names = NULL
  )
}
