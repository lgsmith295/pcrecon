#' Stepwise AIC corrected for small samples sizes
#'
#' @param object an object representing a model of an appropriate class. This is used as the initial model in the stepwise search.
#' @param scope defines the range of models examined in the stepwise search. This should be either a single formula, or a list containing components upper and lower, both formulae. See the details for how to specify the formulae and how they are used.
#' @param scale used in the definition of the AIC statistic for selecting the models, currently only for lm and aov models (see extractAIC for details).
#' @param direction the mode of stepwise search, can be one of "both", "backward", or "forward", with a default of "both". If the scope argument is missing the default for direction is "backward".
#' @param trace if positive, information is printed during the running of stepAICc. Larger values may give more information on the fitting process.
#' @param keep a filter function whose input is a fitted model object and the associated AIC statistic, and whose output is arbitrary. Typically keep will select a subset of the components of the object and return them. The default is not to keep anything.
#' @param steps the maximum number of steps to be considered. The default is 1000 (essentially as many as required). It is typically used to stop the process early.
#' @param starts if true the updated fits are done starting at the linear predictor for the currently selected model. This may speed up the iterative calculations for glm (and other fits), but it can also slow them down. Not used in R.
#' @param k the multiple of the number of degrees of freedom used for the penalty. Only k = 2 gives the genuine AIC: k = log(n) is sometimes referred to as BIC or SBC.
#' @param ... any additional arguments to extractAIC. (None are currently used.)
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow
#'   \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @details Function adapted from the MASS package to calculate AICc rather than AIC.
#'
#'     The set of models searched is determined by the scope argument. The right-hand-side of its lower component is always included in the model, and right-hand-side of the model is included in the upper component. If scope is a single formula, it specifies the upper component, and the lower model is empty. If scope is missing, the initial model is used as the upper model. Models specified by scope can be templates to update object as used by update.formula.
#'     There is a potential problem in using glm fits with a variable scale, as in that case the deviance is not simply related to the maximized log-likelihood. The glm method for extractAIC makes the appropriate adjustment for a gaussian family, but may need to be amended for other cases. (The binomial and poisson families have fixed scale by default and do not correspond to a particular maximum-likelihood problem for variable scale.)
#'     Where a conventional deviance exists (e.g. for lm, aov and glm fits) this is quoted in the analysis of variance table: it is the unscaled deviance.
#'     The stepwise-selected model is returned, with up to two additional components. There is an "anova" component corresponding to the steps taken in the search, as well as a "keep" component if the keep= argument was supplied in the call. The "Resid. Dev" column of the analysis of deviance table refers to a constant minus twice the maximized log likelihood: it will be a deviance only in cases where a saturated model is well-defined (thus excluding lm, aov and survreg fits, for example).
#' @note The model fitting must apply the models to the same dataset. This may be a problem if there are missing values and an na.action other than na.fail is used (as is the default in R). We suggest you remove the missing values first.
#' @references Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @import stats
#' @import MASS
#' @export
# @importFrom MuMIn AICc
#' @examples
#' \dontrun{
#' quine.hi <- aov(log(Days + 2.5) ~ .^4, quine)
#' quine.nxt <- update(quine.hi, . ~ . - Eth:Sex:Age:Lrn)
#' quine.stp <- stepAIC(quine.nxt,
#'                     scope = list(upper = ~Eth*Sex*Age*Lrn, lower = ~1),
#'                     trace = FALSE)
#' quine.stp$anova
#'
#' cpus1 <- cpus
#' for(v in names(cpus)[2:7])
#'  cpus1[[v]] <- cut(cpus[[v]], unique(quantile(cpus[[v]])),
#'                    include.lowest = TRUE)
#' cpus0 <- cpus1[, 2:8]  # excludes names, authors' predictions
#' cpus.samp <- sample(1:209, 100)
#' cpus.lm <- lm(log10(perf) ~ ., data = cpus1[cpus.samp,2:8])
#' cpus.lm2 <- stepAIC(cpus.lm, trace = FALSE)
#' cpus.lm2$anova
#' }
stepAICc <- function(object, scope, scale = 0, direction = c("both", "backward", "forward"), trace = 1, keep = NULL, steps = 1000, use.start = FALSE, k = 2, ...) {
  mydeviance <- function(x, ...) {
    dev <- deviance(x)
    if (!is.null(dev))
      dev
    else MuMIn::AICc(x, k=0)
  }
  cut.string <- function(string) {
    if (length(string) > 1L)
      string[-1L] <- paste("\n", string[-1L], sep = "")
    string
  }
  re.arrange <- function(keep) {
    namr <- names(k1 <- keep[[1L]])
    namc <- names(keep)
    nc <- length(keep)
    nr <- length(k1)
    array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr,
                                                           namc))
  }
  step.results <- function(models, fit, object, usingCp = FALSE) {
    change <- sapply(models, "[[", "change")
    rd <- sapply(models, "[[", "deviance")
    dd <- c(NA, abs(diff(rd)))
    rdf <- sapply(models, "[[", "df.resid")
    ddf <- c(NA, abs(diff(rdf)))
    AIC <- sapply(models, "[[", "AIC")
    heading <- c("Stepwise Model Path \nAnalysis of Deviance Table",
                 "\nInitial Model:", deparse(formula(object)), "\nFinal Model:",
                 deparse(formula(fit)), "\n")
    aod <- if (usingCp)
      data.frame(Step = change, Df = ddf, Deviance = dd,
                 `Resid. Df` = rdf, `Resid. Dev` = rd, Cp = AIC,
                 check.names = FALSE)
    else data.frame(Step = change, Df = ddf, Deviance = dd,
                    `Resid. Df` = rdf, `Resid. Dev` = rd, AIC = AIC,
                    check.names = FALSE)
    attr(aod, "heading") <- heading
    class(aod) <- c("Anova", "data.frame")
    fit$anova <- aod
    fit
  }
  Terms <- terms(object)
  object$formula <- Terms
  if (inherits(object, "lme"))
    object$call$fixed <- Terms
  else if (inherits(object, "gls"))
    object$call$model <- Terms
  else object$call$formula <- Terms
  if (use.start)
    warning("'use.start' cannot be used with R's version of 'glm'")
  md <- missing(direction)
  direction <- match.arg(direction)
  backward <- direction == "both" | direction == "backward"
  forward <- direction == "both" | direction == "forward"
  if (missing(scope)) {
    fdrop <- numeric()
    fadd <- attr(Terms, "factors")
    if (md)
      forward <- FALSE
  }
  else {
    if (is.list(scope)) {
      fdrop <- if (!is.null(fdrop <- scope$lower))
        attr(terms(update.formula(object, fdrop)), "factors")
      else numeric()
      fadd <- if (!is.null(fadd <- scope$upper))
        attr(terms(update.formula(object, fadd)), "factors")
    }
    else {
      fadd <- if (!is.null(fadd <- scope))
        attr(terms(update.formula(object, scope)), "factors")
      fdrop <- numeric()
    }
  }
  models <- vector("list", steps)
  if (!is.null(keep))
    keep.list <- vector("list", steps)
  n <- nobs(object, use.fallback = TRUE)
  fit <- object
  bAIC <- extractAIC(fit, scale, k = k, ...)
  edf <- bAIC[1L]
  bAIC <- MuMIn::AICc(fit, k=k)
  if (is.na(bAIC))
    stop("AIC is not defined for this model, so 'stepAIC' cannot proceed")
  if (bAIC == -Inf)
    stop("AIC is -infinity for this model, so 'stepAIC' cannot proceed")
  nm <- 1
  Terms <- terms(fit)
  if (trace) {
    cat("Start:  AIC=", format(round(bAIC, 2)), "\n", cut.string(deparse(formula(fit))),
        "\n\n", sep = "")
    utils::flush.console()
  }
  models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n -
                         edf, change = "", AIC = bAIC)
  if (!is.null(keep))
    keep.list[[nm]] <- keep(fit, bAIC)
  usingCp <- FALSE
  while (steps > 0) {
    steps <- steps - 1
    AIC <- bAIC
    ffac <- attr(Terms, "factors")
    if (!is.null(sp <- attr(Terms, "specials")) && !is.null(st <- sp$strata))
      ffac <- ffac[-st, ]
    scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
    aod <- NULL
    change <- NULL
    if (backward && length(scope$drop)) {
      aod <- MASS::dropterm(fit, scope$drop, scale = scale,
                      trace = max(0, trace - 1), k = k, ...)
      rn <- row.names(aod)
      row.names(aod) <- c(rn[1L], paste("-", rn[-1L],
                                        sep = " "))
      if (any(aod$Df == 0, na.rm = TRUE)) {
        zdf <- aod$Df == 0 & !is.na(aod$Df)
        nc <- match(c("Cp", "AIC"), names(aod))
        nc <- nc[!is.na(nc)][1L]
        ch <- abs(aod[zdf, nc] - aod[1, nc]) > 0.01
        if (any(is.finite(ch) & ch)) {
          warning("0 df terms are changing AIC")
          zdf <- zdf[!ch]
        }
        if (length(zdf) > 0L)
          change <- rev(rownames(aod)[zdf])[1L]
      }
    }
    if (is.null(change)) {
      if (forward && length(scope$add)) {
        aodf <- addterm(fit, scope$add, scale = scale,
                        trace = max(0, trace - 1), k = k, ...)
        rn <- row.names(aodf)
        row.names(aodf) <- c(rn[1L], paste("+", rn[-1L],
                                           sep = " "))
        aod <- if (is.null(aod))
          aodf
        else rbind(aod, aodf[-1, , drop = FALSE])
      }
      attr(aod, "heading") <- NULL
      if (is.null(aod) || ncol(aod) == 0)
        break
      nzdf <- if (!is.null(aod$Df))
        aod$Df != 0 | is.na(aod$Df)
      aod <- aod[nzdf, ]
      if (is.null(aod) || ncol(aod) == 0)
        break
      nc <- match(c("Cp", "AIC"), names(aod))
      nc <- nc[!is.na(nc)][1L]
      o <- order(aod[, nc])
      if (trace) {
        print(aod[o, ])
        utils::flush.console()
      }
      if (o[1L] == 1)
        break
      change <- rownames(aod)[o[1L]]
    }
    usingCp <- match("Cp", names(aod), 0) > 0
    fit <- update(fit, paste("~ .", change), evaluate = FALSE)
    fit <- eval.parent(fit)
    nnew <- nobs(fit, use.fallback = TRUE)
    if (all(is.finite(c(n, nnew))) && nnew != n)
      stop("number of rows in use has changed: remove missing values?")
    Terms <- terms(fit)
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1L]
    bAIC <- MuMIn::AICc(fit, k=k)
    if (trace) {
      cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n",
          cut.string(deparse(formula(fit))), "\n\n", sep = "")
      utils::flush.console()
    }
    if (bAIC >= AIC + 1e-07)
      break
    nm <- nm + 1
    models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n -
                           edf, change = change, AIC = bAIC)
    if (!is.null(keep))
      keep.list[[nm]] <- keep(fit, bAIC)
  }
  if (!is.null(keep))
    fit$keep <- re.arrange(keep.list[seq(nm)])
  step.results(models = models[seq(nm)], fit, object, usingCp)
}
