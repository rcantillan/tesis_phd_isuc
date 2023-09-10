
#install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")
library(mlogit)
library(gmnl)
data("Electricity", package = "mlogit")
Electr <- mlogit.data(Electricity, id.var = "id", choice = "choice", varying = 3:26, shape = "wide", sep = "")
Elec.lc <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 | 0 | 1, data = Electr, subset = 1:3000, model = "lc", panel = TRUE, Q = 2)
summary(Elec.lc)




Elec.lc <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 | 0 | 1,
                  data = Electr, subset = 1:3000, model = 'lc', panel = TRUE, Q = 2)
