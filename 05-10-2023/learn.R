data = read.csv("does_vn07.csv",header = T);

# dim(data);
# names(data)
# fix(data)

# install.packages(c("psych"))

attach(data)

# # need attach(data)
# # Gia tri trung binh
# mean(age)

library(psych)
library(gmodels)


# describe(data)

# men = subset(data,gender="Male")
# women = subset(data,gender="Female")

# fix(men)
# fix(women)

# describe(women)



# attach(men)

# describleby(men,fracture,range=F)

# CrossTable(gender)

# CrossTable(gender ,fracture, digits=3)

CrossTable(gender ,fracture, digits=3, chisq=T, fisher=T)



