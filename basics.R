library(modelr)
library(splines)
library(tidyverse)
library(gapminder)
 
# explore life expectancy over time

# case study
ch <- filter(gapminder, country == 'Switzerland')
ggplot(ch, aes(year, lifeExp)) +
  geom_line() +
  ggtitle('Data')

model <- lm(lifeExp ~ year, ch)
ch %>%
  add_predictions(model) %>% 
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle('Trend')

ch %>%
  add_residuals(model) %>% 
  ggplot(aes(year, resid)) +
  geom_line() +
  ggtitle('Residuals')

# all countries
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

country_model <- function(df) {
  lm(lifeExp ~ year, df)
}

by_country <- by_country %>% 
  mutate(model = map(data, country_model)) %>% 
  mutate(resid = map2(data, model, add_residuals))

by_country %>% 
  unnest(resid) %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~continent)



# simple model visualisation with cont and cat variable
(df <- sim3)
mod1 <- lm(y ~ x1 + x2, df)
mod2 <- lm(y ~ x1 * x2, df)
# models
grid <- df %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2) 
ggplot(df, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(x1, pred)) +
  facet_wrap(~model)
# residuals
(resid <- gather_residuals(df, mod1, mod2))
ggplot(resid, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model ~ x2)


# approximating a non-linear model
df <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)
ggplot(df, aes(x, y)) + 
  geom_point()
mod1 <- lm(y ~ ns(x, 1), df)
mod2 <- lm(y ~ ns(x, 2), df)
mod3 <- lm(y ~ ns(x, 3), df)
mod4 <- lm(y ~ ns(x, 4), df)
mod5 <- lm(y ~ ns(x, 5), df)
mod6 <- lm(y ~ ns(x, 6), df)
grid <- df %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6, .pred = "y")
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, color = 'blue') +
  facet_wrap(~model)


# tidy up who tuberculosis dataset
df <- tidyr::who
df %>% 
  gather(contains('new'), key = 'key', value = 'cases', na.rm = TRUE) %>%
  mutate(key = str_replace(key, 'newrel', 'new_rel')) %>% 
  separate(key, c('new', 'type', 'sexage')) %>% 
  separate(sexage, c('sex', 'age'), sep = 1) %>% 
  select(-iso2, -iso3, -new)
  

# most often occurring colors in sentences
data = stringr::sentences
df = tibble(sentences=data)
colors <- c('red', 'orange', 'yellow', 'green', 'blue', 'purple')
regex <- str_c(colors, collapse = '|')
df %>% 
  filter(str_detect(sentences, regex)) %>% 
  mutate(colors = str_extract(sentences, regex)) %>% 
  ggplot(aes(x = colors)) +
    geom_bar()


# simple for loops
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10)
)
r <- vector("double", ncol(df))
for (i in seq_along(df)){
  r[[i]] <- median(df[[i]])
}
r

# quickly explore tibble 
iris %>% summarise_all(n_distinct)
iris %>% select(-Species) %>% summarise_all(sum)

for (name in names(iris)) {
  print(name)
}

# useful helper functions
rule <- function(..., pad = '-') {
  title <- paste0(...)
  width <- getOption('width') - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
  

# misc useful stuff

# find objects in global name space (if you can't remember function name)
apropos('repla')

# distinct values of var in pipe
distinct(varname)

# print n rows of dataset inside pipe
df %>% print(n=100)