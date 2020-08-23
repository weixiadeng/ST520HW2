df <- data.frame(time = as.integer(seq(0, 10, 1)),
                 conc = c(6.84, 4.07, 2.82, 1.01, 2.76, 1.04,
                          0.31, 0.24, 0.92, 0.02, 0.02))

df$logC <- log(df$conc)

inter <- df$logC[1]
slr <- lm(logC ~ 0 + time, data = df, offset = rep(inter, length(df$logC)))
beta0 <- round(df$logC[1], 3)
beta1 <- round(-coef(slr), 3)
fit <- paste("log C(t) = ", beta0, " - ", beta1, " * t", sep = "")

png(filename = "HW2Q3.png", width = 500, height = 500, units = "px")
plot(df$time, df$logC, pch = 19,
     main = expression("Time vs"~italic("log C(t)")),
     xlab = expression("Time"~italic("t")~"(in hr)"),
     ylab = "Log - Drug concentration")
     
abline(inter, coef(slr), col = "red")
legend(0.5, -1.6, legend = c(fit), box.lty = 0, text.font = 3,
       pt.cex = 10, cex = 1.5)
dev.off()