
## shamelessly copied from:
## https://susanejohnston.wordpress.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
## excellent resource !
## http://docs.ggplot2.org/0.9.3.1/fortify.lm.html
#' ggplot regression
#' @export
ggplot_reg <- function (fit, title = "") {
    require(ggplot2)
    p <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
        geom_point()
    p <- p + stat_smooth(method = "lm", col = "red") +
        labs(title = paste(title, "Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                 "Intercept =",signif(fit$coef[[1]],5 ),
                 " Slope =",signif(fit$coef[[2]], 5),
                 " P =",signif(summary(fit)$coef[2,4], 5)))
    return(p)
}
