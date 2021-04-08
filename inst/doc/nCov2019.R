## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github("yulab-smu/nCov2019", dependencies = TRUE)

## -----------------------------------------------------------------------------
library("nCov2019")
res <- query()

## -----------------------------------------------------------------------------
names(res)

## -----------------------------------------------------------------------------
x = res$global
x$affectedCountries # total affected countries
summary(x)

## -----------------------------------------------------------------------------
x = res$latest

## -----------------------------------------------------------------------------
print(x) # check update time

## -----------------------------------------------------------------------------
head(x["Global"]) # return all global countries.
x[c("USA","India")] # return only for USA and India 

## ----message=FALSE, warning=FALSE---------------------------------------------
df = x["Global"]
head(df[order(df$cases, decreasing = T),])  

## ----message=FALSE, warning=FALSE---------------------------------------------
x = res$latest
head(x$detail)  # more detail data 

## ---- warning=FALSE-----------------------------------------------------------
Z = res$historical
print(Z) # update time

head(Z["Global"])
head(Z[c("China","UK","USA")]) 

## ---- warning=FALSE-----------------------------------------------------------
 head(Z['China','hubei'])

## ----eval=FALSE---------------------------------------------------------------
#  userowndata <- read.csv("path_to_user_data.csv")
#  # userowndata, it should contain these 6 column:
#  # "country","province","date","cases","deaths","recovered"
#  Z = convert(data=userowndata)
#  head(Z["Global"])

## ---- warning=FALSE-----------------------------------------------------------
X <- res$vaccine
summary(X)

head(X["all"])

# check for the details about the mRNA-based vaccine, id3
X[ID="id3"]

## ---- warning=FALSE-----------------------------------------------------------
X <- res$therapeutics
summary(X)
head(X["All"])
X[ID="id1"] 

## ----fig.height=8, fig.width=12, warning=FALSE--------------------------------
X <- res$latest
plot(X)

## ----fig.height=8, fig.width=12, warning=FALSE--------------------------------
plot(X, type="tests",palette="Green")

## ----fig.height=8, fig.width=12, warning=FALSE--------------------------------
library(ggplot2)
library(dplyr)
X <- res$historical
tmp <- X["global"] %>%
  group_by(country) %>%
  arrange(country,date) %>%
  mutate(diff = cases - lag(cases, default =  first(cases))) %>%
  filter(country %in% c("Australia", "Japan", "Italy", "Germany",  "China")) 

ggplot(tmp,aes(date, log(diff+1), color=country)) + geom_line() +
  labs(y="Log2(daily increase cases)") + 
  theme(axis.text = element_text(angle = 15, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d") + 
  theme_minimal()

## ----fig.height=8, fig.width=12, warning=FALSE--------------------------------
Y <- res$historical
plot(Y, region="Global" ,date = "2020-08-01", type="cases")

## ----eval=FALSE---------------------------------------------------------------
#  library(nCov2019)
#  res = query()
#  from = "2020-03-01"
#  to = "2020-08-01"
#  y = res$historical
#  plot(y, from = from, to=to)

## ----fig.height=8, fig.width=12, warning=FALSE--------------------------------
library(ggplot2)
x <- res$historical
d = x['Japan' ] # you can replace Anhui with any province
d = d[order(d$cases), ]

ggplot(d, 
       aes(date, cases)) +
  geom_col(fill = 'firebrick') + 
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) + 
  scale_x_date(date_labels = "%Y/%m/%d") +
  labs(caption = paste("accessed date:", max(d$date)))


## ----fig.height=8, fig.width=10, warning=FALSE--------------------------------
library("dplyr")
library("ggrepel")

x <- res$latest
y <- res$historical

country_list =  x["global"]$country[1:10]

y[country_list]  %>%
subset( date > as.Date("2020-10-01") ) %>%
group_by(country) %>%
arrange(country,date) %>%
mutate(increase = cases - lag(cases, default =  first(cases))) -> df

ggplot(df, aes(x=date, y=increase, color=country  ))+
  geom_smooth() + 
  geom_label_repel(aes(label = paste(country,increase)), 
    data = df[df$date == max(df$date), ], hjust = 1) + 
  labs(x=NULL,y=NULL)+ 
  theme_bw() + theme(legend.position = 'none') 
  

## ---- warning=FALSE-----------------------------------------------------------
library('tidyr')
library('ggrepel')
library('ggplot2')
y <- res$historical
country = "India"

y[country] -> d
d <- gather(d, curve, count, -date, -country)

ggplot(d, aes(date, count, color = curve)) + geom_point() + geom_line() + 
  labs(x=NULL,y=NULL,title=paste("Trend of cases, recovered and deaths in", country)) +
    scale_color_manual(values=c("#f39c12", "#dd4b39", "#00a65a")) +
    theme_bw() +   
  geom_label_repel(aes(label = paste(curve,count)), 
                   data = d[d$date == max(d$date), ], hjust = 1) + 
  theme(legend.position = "none",
        axis.text = element_text(angle = 15, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d")

## ----fig.height=15, fig.width=6, warning=FALSE--------------------------------
library('tidyr')
library('ggrepel')
library('ggplot2')
y <- res$historical
d <- y["global"]

d <- d[d$cases > 0,]
length(unique(d$country))
d <- subset(d,date <= as.Date("2020-3-19"))
max_time <- max(d$date)
min_time <- max_time - 7
d <-  d[d$date >= min_time,]
dd <- d[d$date == max(d$date,na.rm = TRUE),]

d$country <- factor(d$country, 
  levels=unique(dd$country[order(dd$cases)]))
breaks = c(0,1000, 10000, 100000, 10000000)

ggplot(d, aes(date, country)) + 
  geom_tile(aes(fill = cases), color = 'black') + 
  scale_fill_viridis_c(trans = 'log', breaks = breaks, 
  labels = breaks) + 
  xlab(NULL) + ylab(NULL) +
  scale_x_date(date_labels = "%Y-%m-%d") + theme_minimal()


## ----fig.height=10, fig.width=10, warning=FALSE-------------------------------

require(dplyr)

y <- res$historical
d <- y["global"]

time = as.Date("2020-03-19")
dd <- filter(d, date == time) %>% 
    arrange(desc(cases)) 

dd = dd[1:40, ]
dd$country = factor(dd$country, levels=dd$country)

dd$angle = 1:40 * 360/40
require(ggplot2)
p <- ggplot(dd, aes(country, cases, fill=cases)) + 
    geom_col(width=1, color='grey90') + 
    geom_col(aes(y=I(5)), width=1, fill='grey90', alpha = .2) +       
    geom_col(aes(y=I(3)), width=1, fill='grey90', alpha = .2) +    
    geom_col(aes(y=I(2)), width=1, fill = "white") +
    scale_y_log10() + 
    scale_fill_gradientn(colors=c("darkgreen", "green", "orange", "firebrick","red"), trans="log") + 
    geom_text(aes(label=paste(country, cases, sep="\n"), 
                  y = cases *.8, angle=angle), 
            data=function(d) d[d$cases > 700,], 
            size=3, color = "white", fontface="bold", vjust=1)  + 
     geom_text(aes(label=paste0(cases, " cases ", country), 
                  y = max(cases) * 2, angle=angle+90), 
            data=function(d) d[d$cases < 700,], 
            size=3, vjust=0) + 
    coord_polar(direction=-1) + 
    theme_void() + 
    theme(legend.position="none") +
    ggtitle("COVID19 global trend", time)
p

## ----fig.height=8, fig.width=12, warning=FALSE--------------------------------
require(dplyr)
require(ggplot2)
require(shadowtext)
 

y <- res$historical
d <- y["global"]



dd <- d %>% 
  as_tibble %>%
  filter(cases > 1000000) %>%
  group_by(country) %>%
  mutate(days_since_1m = as.numeric(date - min(date))) %>%
  ungroup 
  

  

breaks=c(1000, 10000, 20000, 50000, 500000,500000,5000000,20000000)


p <- ggplot(dd, aes(days_since_1m, cases, color = country)) +
  geom_smooth(method='lm', aes(group=1),
              data = dd, 
              color='grey10', linetype='dashed') +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks = breaks, labels = breaks) +
  scale_x_continuous(expand = expansion(add = c(0,1))) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm")
  ) +
  coord_cartesian(clip = "off") +
  geom_shadowtext(aes(label = paste0(" ",country)), hjust=0, vjust = 0, 
                  data = . %>% group_by(country) %>% top_n(1,days_since_1m),
                  bg.color = "white") +
  labs(x = "Number of days since 1,000,000th case", y = "", 
       subtitle = "Total number of cases")
print(p)


## ----eval=FALSE---------------------------------------------------------------
#  dashboard()

## -----------------------------------------------------------------------------
sessionInfo()

