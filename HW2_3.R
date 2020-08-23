df <- data.frame(time = as.integer(seq(0, 10, 1)),
                 conc = c(6.84, 4.07, 2.82, 1.01, 2.76, 1.04,
                          0.31, 0.24, 0.92, 0.02, 0.02))

df$logC <- log(df$conc)

inter <- df$logC[1]
slr <- lm(logC ~ 0 + time, data = df, offset = rep(inter, length(df$logC)))
beta0 <- round(df$logC[1], 3)
beta1 <- round(-coef(slr), 3)
beta1var <- round(summary(slr)$coefficients[2] ** 2, 6)
res <- round(summary(slr)$sigma ** 2, 4)
fit <- paste("log C(t) = ", beta0, " - ", beta1, " * t", sep = "")
slopvari <- paste("Variance of slope:", beta1var)
resvari <- paste("Residual variance:", res)

png(filename = "HW2Q3.png", width = 500, height = 500, units = "px")
plot(df$time, df$logC, pch = 19,
     main = expression("Time vs"~italic("log C(t)")),
     xlab = expression("Time"~italic("t")~"(in hr)"),
     ylab = "Log - Drug concentration")
     
abline(inter, coef(slr), col = "red")
legend(2, -1.6, legend = c(fit, slopvari, resvari), box.lty = 0, text.font = 3)
dev.off()