library(tidyverse)    # load the tidyverse package

iris <- as_tibble(iris) # so it prints a little nicer


#Question 1

i2 <- rename(
  iris,
  sepal_length = Sepal.Length,
  sepal_width = Sepal.Width,
  petal_length = Petal.Length,
  petal_width = Petal.Width,
  species = Species
  )
i2

#Question 2

numerical_mm <- mutate(i2,
                sepal_length = sepal_length * 10,
                sepal_width = sepal_width * 10,
                petal_length = petal_length * 10,
                petal_width = petal_width * 10
)

numerical_mm


#Question 3 

i3 <- mutate(i2, sepal_area = sepal_length * sepal_width,
             petal_area = petal_length * petal_width)
i3

#Question 4

i4 <- summarize (i3,
            sl_sample_size = n(),
            sl_max = max(sepal_length), 
            sl_min = min(sepal_length),
            sl_range = (sl_max - sl_min),
            sl_median = median(sepal_length),
            sl_iqr = IQR(sepal_length),
            q1 = quantile(sepal_length, probs = 0.25),
            q3 = quantile(sepal_length, probs = 0.75)
             )
            i4
            
#Question 5

i5 <- group_by(i2, species)

            
i6 <- summarize(i5, sample_size = n(),
             pw_mean = mean(petal_width),
             pw_sd = sd(petal_width),
             pw_var = var(petal_width), 
             pw_sem = pw_sd/ sqrt (n()),
             pw_ciupper = pw_mean + 1.96 * pw_sem, 
             pw_cilower = pw_mean - 1.96 * pw_sem
             )
i6
# Question 6 

ggplot(data = i2) + 
  geom_jitter(mapping = aes(x = species, y = petal_width))

#Question 7 

i7 <- summarize (i5,
pw_mean = mean(petal_width),
pw_sem = sd(petal_width)/ sqrt (n()),
pw_ciupper = pw_mean + 1.96 * pw_sem, 
pw_cilower = pw_mean - 1.96 * pw_sem)
i7

ggplot(data = i2) +
  geom_jitter(mapping = aes(x = species, y = petal_width)) +
  geom_crossbar(
    data = i7, 
    mapping = aes(x = species, y = pw_mean, ymax = pw_ciupper, 
                  ymin = pw_cilower),
    color = "red"
  )



#Question 8 

ggplot(data = i2) +
  geom_point(mapping = aes(x = petal_length, y = petal_width , color = species))









