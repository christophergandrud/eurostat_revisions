#' Plot marginal effects from two-way interactions in linear regressions
#' 
#' @param obj fitted model object from \code{lm}
#' @param term1 the first constitutive term's variable name. Note b1 and b2 must 
#' be entered in the order in which they are entered into the \code{lm} model.
#' @param term2 character string of the second constitutive variable's name.
#' @param fitted numeric vector of fitted values of \code{term2} to plot for.
#' 
#' @return a \code{gg} class ggplot2 object
#'
#' @source Modified from:
#' http://www.carlislerainey.com/2013/08/27/creating-marginal-effect-plots-for-linear-regression-models-in-r/
#' 
#' @export

plot_me <- function(obj, term1, term2, fitted2, ci = 90) {
    library(ggplot2)
    
    beta_hat <- coef(obj)
    cov <- vcov(obj)
    
    int_term <- sprintf('%s:%s', term1, term2)
    
    if (!(term1 %in% names(beta_hat))) stop(sprintf('%s not found.', term1), 
                                            call. = F)
    if (!(int_term %in% names(beta_hat))) {
        stop(sprintf('Interaction term-- %s --not found.', int_term), call. = F)
    }
    
    term2_dist <- obj$model[, term2]
    term2_dist <- data.frame(fake_y = 0, real_x = term2_dist)
    names(term2_dist) <- c('term1', 'term2')
    
    # Estimated marginal effect
    dy_dx <- beta_hat[term1] + beta_hat[int_term]*fitted2
    
    # Standard error
    se_dy_dx <- sqrt(cov[term1, term1] + fitted2 ^ 2 * cov[int_term, int_term] + 
                         2 * fitted2 *cov[term1, int_term])    
    
    if (ci == 90) {
        # 90% confidence intervals
        upper <- dy_dx + 1.64*se_dy_dx
        lower <- dy_dx - 1.64*se_dy_dx
    }
    
    else if (ci == 95) {
        # 90% confidence intervals
        upper <- dy_dx + 1.96*se_dy_dx
        lower <- dy_dx - 1.96*se_dy_dx
    }
    
    parts <- cbind(fitted2, dy_dx, lower, upper) %>% data.frame
    
    ggplot(parts, aes(fitted2, dy_dx)) +
        geom_rug(data = term2_dist, aes(x = term2, y = term1), sides = 'b',
                 alpha = 0.1) +
        geom_hline(yintercept = 0, linetype = 'dotted') +
        geom_line() +
        geom_ribbon(ymin = lower, ymax = upper, alpha = 0.1) +
        xlab(sprintf('\n%s', term2)) +
        theme_bw()
}