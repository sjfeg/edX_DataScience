## ed.X HarvardX Data Science - Visualization

library(dslabs)
data("heights")

names(heights)

x <- heights$height
length(unique(x))
tab <- table(x)

male_heigths <- filter(heights, sex == "Male")
y <- male_heigths$height
sum((y > 62.5) & (y < 65.5))
sum((y >= 63.5) & (y <= 65.5))

mean((y > 69) & (y <= 72))

avg <- mean(y)
std <- sd(y)
pnorm(72, mean = avg, sd = std) - pnorm(69, mean = avg, sd = std)

p <- 1 - pnorm(7*12, 69, 3)
round(p*(10^9))

vp <- seq(0.05, 0.95, 0.05)
z <- scale(y)
observed_quantiles <- quantile(z, vp)
theoretical_quantiles <- qnorm(vp) 
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
s <- c(0.1, 0.3, 0.5, 0.7, 0.9)
male_percentiles <- quantile(male, s)
female_percentiles <- quantile(female, s)
df <- data.frame(female = female_percentiles, male = male_percentiles)
print(df)

r <- murders %>% summarize(rate = sum(total) / sum(population) * 10^6) %>% .$rate

params <- heights %>% filter(sex=="Male") %>% summarize(mean=mean(height), sd=sd(height))

library(ggplot2)
p <- murders %>% ggplot(aes(population, total, label=abb, color=region)) +
    geom_label()
p + scale_x_log10() +
    scale_y_log10() +
    ggtitle("Gun murder data")

p <- heights %>% ggplot(aes(x = height))
p + geom_histogram(binwidth = 1)

heights %>% ggplot(aes(height, fill = sex)) + geom_density(alpha=0.2)

library(NHANES)
data("NHANES")
str(NHANES)
tab <- NHANES %>% filter(Gender == "female" & AgeDecade == " 20-29")
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))
ref_avg <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%
  .$average
NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>%
  summarize(min = min(BPSysAve, na.rm = TRUE), max = max(BPSysAve, na.rm = TRUE))

data("gapminder")
gapminder <- mutate(gapminder, dollars_per_day = gdp / population / 365)
p <- gapminder %>%
  filter(year == 1979 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
p + scale_y_continuous(trans = "log2")

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
country_list1 <- gapminder %>% filter(year == 1970 & !is.na(gdp)) %>% .$country
country_list2 <- gapminder %>% filter(year == 2010 & !is.na(gdp)) %>% .$country
country_list = intersect(country_list1, country_list2)
p <- gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developping")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1/2, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

p <- gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  geom_boxplot(aes(region, dollars_per_day, fill = factor(year))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p <- gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developping")) %>%
  ggplot(aes(x = dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75) +
  facet_grid(year ~ .)

gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Carribean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"  ))

gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

test <- gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population / (sum(population) * 2)) %>%
  ungroup()

p <- gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population / (sum(population) * 1)) %>%
  ungroup() %>%
  ggplot(aes(x = dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"  ))

surv_income <- gapminder %>%
  filter(year == 2010 & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp) / sum(population) / 365, 
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

p <- surv_income %>%
  ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, 0.9981), breaks = c(.85, .90, .95, .99, .995, .998)) + 
  geom_label(size = 3, show.legend = FALSE)

surv_income2 <- gapminder %>%
  filter(year == 2010 & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  mutate(income = gdp / population / 365, 
         infant_survival_rate = 1 - infant_mortality/1000)

p <- surv_income2 %>%
  ggplot(aes(income, infant_survival_rate, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, 0.9981), breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_bw()

f <- gapminder %>% filter(year == 2012 & continent == "Africa")
f1 <- f %>% filter(fertility <= 3 & life_expectancy >= 70)
f2 <- gapminder %>% filter(year == 2012 & continent == "Africa" & fertility <= 3 & life_expectancy >= 70)
df <- gapminder %>% 
      filter(year == 2012 & continent == "Africa" & fertility <= 3 & life_expectancy >= 70) %>%
      select(country, region)

murders %>% mutate(rate = total/population*100000) %>%
  mutate(region = reorder(region, rate, FUN = median)) %>%
  ggplot(aes(x = region, y = rate)) +
  geom_boxplot() +
  geom_point()