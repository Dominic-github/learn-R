P = ggplot(pisa, aes(x=WEALTH, y=PV1SCIE, color=Gender))

P1 = p + geom_point() + ggtitle("Tran Thi Trang 37")

p2 = p + geom_point() + geom_smooth(method="lm") + ggtitle("Tran Thi Trang 372")

grid.arrange(p1, p2, nrow=2)