generateNormalData <- function() {
  x <- rnorm(100)
  hist(
    x,
    main = "Histogram of Simulated Normal Data",
    xlab = "x",
    col = "lightblue"
  )
}

runTtestExample <- function() {
  x <- rnorm(20)
  y <- rnorm(20)
  print(t.test(x, y))
}

generateTwoGroupData <- function() {
  group <- rep(c("A", "B"), each = 50)
  score <- c(
    rnorm(50, mean = 10, sd = 2),
    rnorm(50, mean = 12, sd = 2)
  )
  
  dat <- data.frame(group = group, score = score)
  
  print(head(dat))
  boxplot(
    score ~ group,
    data = dat,
    main = "Scores by Group",
    xlab = "Group",
    ylab = "Score",
    col = c("lightblue", "lightgreen")
  )
}


#' Enhanced mean plot using the active R Commander dataset
#' @export
meanPlotEnhanced <- function() {
  
  ds_name <- ActiveDataSet()
  
  if (is.null(ds_name) || ds_name == "") {
    stop("No active dataset found in R Commander.")
  }
  
  dat <- get(ds_name, envir = .GlobalEnv)
  
  numeric_vars <- names(dat)[sapply(dat, is.numeric)]
  group_vars <- names(dat)[sapply(dat, function(x) is.factor(x) || is.character(x) || is.numeric(x))]
  
  if (length(numeric_vars) == 0) {
    stop("No numeric response variables found in the active dataset.")
  }
  
  if (length(group_vars) == 0) {
    stop("No grouping variables found in the active dataset.")
  }
  
  response_var <- utils::select.list(
    numeric_vars,
    title = "Select the response variable",
    graphics = TRUE
  )
  
  if (response_var == "") return(invisible(NULL))
  
  group_var <- utils::select.list(
    group_vars,
    title = "Select the grouping variable",
    graphics = TRUE
  )
  
  if (group_var == "") return(invisible(NULL))
  
  interval_choice <- utils::select.list(
    c("None", "95% CI"),
    title = "Show interval bars?",
    graphics = TRUE
  )
  
  if (interval_choice == "") return(invisible(NULL))
  
  show_ci <- interval_choice == "95% CI"
  
  y <- dat[[response_var]]
  g <- as.factor(dat[[group_var]])   # force grouping to factor
  
  keep <- complete.cases(y, g)
  y <- y[keep]
  g <- droplevels(g[keep])
  
  .drawEnhancedMeanPlot(
    y = y,
    g = g,
    ylab = response_var,
    xlab = group_var,
    show_ci = show_ci
  )
}


# Internal plotting function
.drawEnhancedMeanPlot <- function(y, g, ylab = "Response", xlab = "Group", show_ci = TRUE) {
  
  g <- as.factor(g)
  levs <- levels(g)
  k <- length(levs)
  
  # tighter spacing between groups
  xpos <- 1 + 0.5 * (0:(k - 1))
  
  means <- tapply(y, g, mean)
  sds   <- tapply(y, g, sd)
  ns    <- tapply(y, g, length)
  ses   <- sds / sqrt(ns)
  ci    <- qt(0.975, df = pmax(ns - 1, 1)) * ses
  
  group_index <- match(g, levs)
  
  ymin <- min(y, na.rm = TRUE)
  ymax <- max(y, na.rm = TRUE)
  
  interval_low  <- if (show_ci) means - ci else means
  interval_high <- if (show_ci) means + ci else means
  
  plot_min <- min(ymin, interval_low, na.rm = TRUE)
  plot_max <- max(ymax, interval_high, na.rm = TRUE)
  pad <- 0.08 * (plot_max - plot_min)
  if (pad == 0) pad <- 1
  
  # colors for raw points only
  base_cols <- c("firebrick", "steelblue3", "darkgreen", "purple", "orange3", "brown")
  group_cols <- base_cols[seq_len(k)]
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(
    mar = c(9.2, 5.6, 4.8, 1.5),
    oma = c(0, 0, 0, 5.5),
    xaxs = "i",
    yaxs = "r"
  )
  
  plot(
    xpos, means,
    type = "n",
    xaxt = "n",
    xlab = "",
    ylab = ylab,
    ylim = c(plot_min - pad, plot_max + pad),
    xlim = c(min(xpos) - 0.05, max(xpos) + 0.05),
    main = "Mean Plot"
  )
  
  axis(1, at = xpos, labels = levs)
  abline(h = pretty(c(plot_min, plot_max)), col = "gray85", lty = 1)
  
  # x-axis title placed manually to avoid overlap
  mtext(xlab, side = 1, line = 1.6, cex = 1)
  
  # confidence interval bars
  if (show_ci) {
    for (i in seq_along(xpos)) {
      segments(xpos[i], means[i] - ci[i], xpos[i], means[i] + ci[i], lty = 2)
      segments(xpos[i] - 0.03, means[i] - ci[i], xpos[i] + 0.03, means[i] - ci[i], lty = 2)
      segments(xpos[i] - 0.03, means[i] + ci[i], xpos[i] + 0.03, means[i] + ci[i], lty = 2)
    }
  }
  
  # line joining means
  lines(xpos, means, lwd = 1.2)
  
  # larger gold diamonds
  diamond_half_width <- 0.025
  diamond_half_height <- 0.04 * diff(par("usr")[3:4])
  
  for (i in seq_along(xpos)) {
    polygon(
      x = c(xpos[i], xpos[i] + diamond_half_width, xpos[i], xpos[i] - diamond_half_width),
      y = c(means[i] + diamond_half_height, means[i], means[i] - diamond_half_height, means[i]),
      col = "gold",
      border = "black",
      lwd = 1
    )
  }
  
  # stacked points: centered on each group, with only tied/near-tied values spread slightly
  for (i in seq_along(levs)) {
    yi <- y[g == levs[i]]
    
    stripchart(
      yi,
      method = "stack",
      at = xpos[i],
      add = TRUE,
      vertical = TRUE,
      pch = 19,
      col = group_cols[i],
      cex = 1.25,
      offset = 0.35
    )
  }
  
  # mean labels: put the last group's label on the left so it doesn't get clipped
  for (i in seq_along(xpos)) {
    if (i == length(xpos)) {
      text(
        xpos[i] - 0.045,
        means[i],
        labels = round(means[i], 1),
        pos = 2,
        cex = 0.95
      )
    } else {
      text(
        xpos[i] + 0.045,
        means[i],
        labels = round(means[i], 1),
        pos = 4,
        cex = 0.95
      )
    }
  }
  
  # ANOVA p-value
  anova_p <- NA
  if (k >= 2) {
    fit <- try(aov(y ~ g), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      anova_p <- summary(fit)[[1]][["Pr(>F)"]][1]
    }
  }
  
  usr <- par("usr")
  
  # p-value in top-right outer margin
  if (!is.na(anova_p)) {
    text(
      x = usr[2] + 0.02,
      y = usr[4],
      labels = paste0(
        "One-way ANOVA\np = ",
        format.pval(anova_p, digits = 3, eps = 0.001)
      ),
      adj = c(0, 1),
      xpd = NA,
      cex = 0.95
    )
  }
  
  # legend in bottom-right outer margin
  legend(
    x = usr[2] + 0.02,
    y = usr[3] + 0.28 * diff(usr[3:4]),
    legend = levs,
    col = group_cols,
    pch = 19,
    pt.cex = 1.2,
    bty = "n",
    cex = 0.9,
    title = "Groups",
    xpd = NA,
    yjust = 0
  )
  
  # aligned summary values under each group
  y_bottom1 <- usr[3] - 0.18 * diff(usr[3:4])
  y_bottom2 <- usr[3] - 0.25 * diff(usr[3:4])
  
  # row labels on the left; moved right so they don't get cut off
  text(
    x = min(xpos) - 0.06,
    y = y_bottom1,
    labels = paste(ylab, "Mean"),
    adj = 1,
    xpd = NA,
    cex = 0.9
  )
  
  text(
    x = min(xpos) - 0.06,
    y = y_bottom2,
    labels = paste(ylab, "StdDev"),
    adj = 1,
    xpd = NA,
    cex = 0.9
  )
  
  # values centered under each group
  text(
    x = xpos,
    y = rep(y_bottom1, length(xpos)),
    labels = format(round(means, 3), nsmall = 3),
    xpd = NA,
    cex = 0.9
  )
  
  text(
    x = xpos,
    y = rep(y_bottom2, length(xpos)),
    labels = format(round(sds, 3), nsmall = 3),
    xpd = NA,
    cex = 0.9
  )
}