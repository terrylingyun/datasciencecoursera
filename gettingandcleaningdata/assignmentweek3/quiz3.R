url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
dat1 <- fread(url1, select = c(1, 2, 4, 5), skip = 5, nrows = 190)
dat2 <- fread(url2)
dat <- merge(dat1, dat2, by.x = "V1", by.y = "CountryCode")
nrow(dat)
arrange(dat, desc(V2))[13, 3]
dat %>% group_by(`Income Group`) %>% summarise(mean(`V2`, na.rm = T))
dat$quant <- cut(dat$V2, breaks = quantile(dat$V2, probs = seq(0, 1, 0.2), na.rm = TRUE))
table(dat$`Income Group`, dat$quant)