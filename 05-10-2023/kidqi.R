iq = read.csv("./kidiq.csv", header=T)

# head(iq)
#
library(ggplot2)

# p = ggplot(iq, aes(x=mom_iq, y=kid_score, fill=factor(mom_work)))
#
# p + geom_point(aes(col=factor(mom_work))) + geom_smooth() +
# theme_bw() + xlab("Mom's IQ") + ylab("Kid's Score") +
# ggtitle("Hoang Kim Viet Tan") + theme(legend.position="bottom")
#
#


attach(iq)

library(ggplot2); library(gridExtra)

plot(kid_score ~ mom_iq, pch=16, col="blue")

plot(kid_score ~ mom_iq, pch=16, col="blue", xlab="Mom
IQ", ylab="Kid Score")

m = lm(kid_score ~ mom_iq)
abline(m, col="red")



