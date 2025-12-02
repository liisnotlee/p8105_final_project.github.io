sleep_stress_EDA
================

Load the data

View the sleep time for different participants in each study interval

``` r
# plot the mean timein bed in hour
sleep |>
  group_by(id, study_interval)|>
  summarize(mean_sleep = mean(timeinbed/60),
            sd_sleep = sd(timeinbed),
            mean_efficiency = mean(efficiency),
            sd_efficiency = sd(efficiency)) |>
  ggplot(aes(x = id, y = mean_sleep, color = id))+
  geom_point()+
  facet_grid(study_interval~.)
```

    ## `summarise()` has grouped output by 'id'. You can override
    ## using the `.groups` argument.

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The average sleep time for each person is different each study interval
and varied across participants. In 2022, some participants slept less
than 5 hours and some slept more than 8 hours. In 2024, most
participants have better sleep length, 7-8 hours.

Combine the sleep and stress score

``` r
# join two data sets, using inner join
sleep_score1 = sleep_score |>
  select(id,study_interval, day_in_study, overall_score, deep_sleep_in_minutes)

sleep1 = sleep|>
   select(id,study_interval, sleep_start_day_in_study, timeinbed) |>
   mutate(day_in_study = sleep_start_day_in_study) |>
  select(-sleep_start_day_in_study)
stress_score1 = stress_score |>
  filter(calculation_failed == "FALSE")|>
  select(id, study_interval, day_in_study, stress_score)|>
  distinct(id, study_interval, day_in_study, .keep_all = TRUE)

sleep_stress = inner_join(sleep_score1, stress_score1, join_by("id", "study_interval", "day_in_study"))
 # cleaning exercise from Anu
exercise_clean = exercise|>
  distinct()|>
  select(id, study_interval, start_day_in_study, 
         activityname, calories, duration, activeduration, 
         averageheartrate, steps, elevationgain)

# Aggregate to daily level
exercise_daily = exercise_clean %>%
  group_by(id, study_interval, start_day_in_study) %>%
  summarise(
    total_calories = sum(calories, na.rm = TRUE),
    total_duration_min = sum(duration, na.rm = TRUE) / 60000,
    total_active_min = sum(activeduration, na.rm = TRUE) / 60000,
    total_steps = sum(steps, na.rm = TRUE),
    avg_heartrate = mean(averageheartrate, na.rm = TRUE),
    max_heartrate = max(averageheartrate, na.rm = TRUE),
    total_elevation = sum(elevationgain, na.rm = TRUE),
    n_sessions = n(),
    activity_types = paste(unique(activityname), collapse = ", "),
    .groups = "drop"
  ) %>%
  rename(day_in_study = start_day_in_study)
```

    ## Warning: There were 6 warnings in `summarise()`.
    ## The first warning was:
    ## ℹ In argument: `max_heartrate = max(averageheartrate,
    ##   na.rm = TRUE)`.
    ## ℹ In group 924: `id = 30`, `study_interval = 2024`,
    ##   `start_day_in_study = 861`.
    ## Caused by warning in `max()`:
    ## ! no non-missing arguments to max; returning -Inf
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 5
    ##   remaining warnings.

``` r
exercise_daily1 = exercise_daily |>
  select(id, day_in_study, total_active_min, study_interval)
# check of na values
sum(is.na(sleep_stress$overall_score))
```

    ## [1] 0

``` r
sum(is.na(sleep_stress$stress_score))
```

    ## [1] 0

Showing the sleep score and stress score changes

``` r
# graphs to show the score across ids
sleep_stress|>
  group_by(id) |> 
  summarize(
    mean_sleep_score = mean(overall_score, na.rm = TRUE),
    mean_stress_score = mean(stress_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = id, mean_sleep_score)) +
  geom_smooth(aes(y = mean_sleep_score, color = "mean_sleep_score"), se= FALSE )+
  geom_smooth(aes(y = mean_stress_score, color = "mean_stress_score"), se = FALSE) 
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~
    ## x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~
    ## x'

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Load and clean the hormone data for menstrual phases

``` r
hormone1 = hormone |>
  select(id,phase, day_in_study, lh, estrogen,fatigue,moodswing)|>
  mutate(
    phase = factor(phase, levels = c("Follicular", "Fertility", "Luteal", "Menstrual")),
    fatigue = factor(fatigue, 
                     levels = c("Low", "Moderate", "High", "Very High")),
    moodswing = factor(moodswing,
                       levels = c("Very Low/Little", "Low", "Moderate", "High","Very High","Not at all"))
  )
phase_order <- c("Menstrual", "Follicular", "Fertility", "Luteal")
```

Joined data with the phase in hormone dataset

``` r
sleep_stress_hormone =
  inner_join(sleep_stress,hormone1, join_by(id, day_in_study))
```

Now explore the changes of the stress score with the menstrual cycle.
The participants slept best during the menstrual time, probably a good
sign to rest well during this time. The lowest sleep score were recorded
during ovulation/ fertility time in which the increased estrogen and lh
levels may contribute to this low sleep quality. The higher stress score
the less stressful people experience. In this study, the participants
showed the least stressful during folicular period , as the bodies are
full of energy. The most stressful time is during luteal period.

``` r
plot_1 = 
  sleep_stress_hormone |>
  group_by(phase) |>
  summarize(
    mean_sleep_score = mean(overall_score, na.rm = TRUE),
    mean_stress_score = mean(stress_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = recode(metric,
                    mean_sleep_score  = "Sleep Score",
                    mean_stress_score = "Stress Score"),
    phase = factor(phase, levels = phase_order)
  ) |>
  ggplot(aes(x = phase, y = value, color = metric, group = metric)) +
    geom_smooth(linewidth= 1) +
    geom_point(size = 3) +
    labs(color = "Metric", y = "Mean Score", x = "Phase") +
    labs(
    x = "Menstrual Cycle Phase",
    y = "Score"
  ) +
    theme_minimal() +
  theme(legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

plot_2  =
  sleep_stress_hormone |>
  group_by(phase) |>
  summarize(
    mean_lh = mean(lh, na.rm = TRUE),
    mean_estrogen = mean( estrogen, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = recode(metric,
                    mean_lh           = "LH",
                    mean_estrogen     = "Estrogen"),
    phase = factor(phase, levels = phase_order)
  ) |>
  ggplot(aes(x = phase, y = value, color = metric, group = metric)) +
    geom_smooth(linewidth= 1) +
    geom_point(size = 3) +
    labs(color = "Metric", y = "level", x = "Phase") +
    labs(
    x = "Menstrual Cycle Phase",
    y = "Level"
  ) +
    theme_minimal() +
  theme(legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

plot_1+ plot_2
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~
    ## x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~
    ## x'

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Variation of sleep score across menstrual cycle phases

``` r
sleep_stress_hormone |>
  ggplot(aes(x = phase, y = overall_score, fill = phase))+
  geom_violin(alpha = 0.5)+
  geom_boxplot(width = 0.2)+
      labs(
    title = "Sleep Score Across Menstrual Cycle Phases",
    x = "Menstrual Cycle Phase",
    y = "score"
  ) +
    theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Variation of stress score across menstrual cycle phases

``` r
sleep_stress_hormone |>
  ggplot(aes(x = phase, y = stress_score, fill = phase))+
  geom_violin(alpha = 0.5)+
  geom_boxplot(width = 0.2)+
      labs(
    title = "Stress Score Across Menstrual Cycle Phases",
    x = "Menstrual Cycle Phase",
    y = "score"
  ) +
    theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Explore the variation among different participants. adding the id,
ethnicity and birthyear of the subjects.

``` r
sleep_stress_hormone2 = subject |>
  select(id, birth_year, ethnicity) |>
  mutate(age = 2025- birth_year) |>
  full_join(sleep_stress_hormone, join_by(id)) 
```

View the stress and sleep score across menstrual cycle phases for each

``` r
sleep_stress_hormone2 |>
  group_by(phase, study_interval, ethnicity) |>
  summarize(
    mean_sleep_score = mean(overall_score, na.rm = TRUE),
    mean_stress_score = mean(stress_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "metric",
    values_to = "value"
  ) |>
  drop_na()|>
  mutate(
    metric = recode(metric,
                    mean_sleep_score  = "Sleep Score",
                    mean_stress_score = "Stress Score"),
    phase = factor(phase, levels = phase_order)
  ) |>
  ggplot(aes(x = phase, y = value, color = ethnicity, group = ethnicity)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    facet_grid(study_interval ~ metric) +
    labs(color = "Ethnicity", y = "Mean Score", x = "Phase") +
    labs(
    title = "stress and sleep score Across Menstrual Cycle Phases",
    x = "Menstrual Cycle Phase",
    y = "score"
  ) +
    theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
variation of sleep score and stress score across different ethnicity

``` r
dodge <- position_dodge(width = 0.5)
sleep_stress_hormone2 |>
  select(id, ethnicity, stress_score, overall_score) |>
  drop_na()|>
  pivot_longer(
    cols = c(stress_score, overall_score),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = recode(metric,
      overall_score = "Sleep Score",
      stress_score  = "Stress Score",
    )
  ) |>
  ggplot(aes(x = ethnicity, y = value, fill = metric)) +
  geom_violin(position = dodge,alpha = 0.5, trim = TRUE) +
  geom_boxplot(width = 0.5, alpha= 0.1)+
    labs(y = "Score", fill = "Metric")
```

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
sleep_stress_hormone_add |>
  group_by(phase, id) |>
  summarize(
    timeinbed = mean(timeinbed/60)
  )|> 
   mutate(phase = factor(phase, levels = phase_order))|>
  ggplot(aes(x = phase, y = timeinbed, fill = phase)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2) +
  labs(
    title = "Sleep time Across Menstrual Cycle Phases",
    x = "Menstrual Cycle Phase",
    y = "Time in bed (hrs)"
  ) +
    theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

    ## `summarise()` has grouped output by 'phase'. You can
    ## override using the `.groups` argument.

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

See the correlation in sleep score file

``` r
sleep_score |>
  group_by(id)|>
  summarise(mean_deep_sleep = mean(deep_sleep_in_minutes),
            mean_overall_score = mean(overall_score))|>
  ggplot(aes(x= mean_overall_score, y = mean_deep_sleep )) +
  geom_point()
```

    ## Warning: Removed 2 rows containing missing values or values outside
    ## the scale range (`geom_point()`).

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

NOT WORKING_try to put all in one scale

See time in bed and deep sleep vs menstrual cycle

``` r
coeff = 5.5 # 5 is the best overlap
timeinbed_color = "red"
deep_sleep_color = "blue"
plot_3 = sleep_stress_hormone_add |>
  mutate(phase = factor(phase, levels = phase_order))|>
  group_by(phase)|>
   summarize(timeinbed = mean(timeinbed/60, na.rm = TRUE),
             deep_sleep_in_minutes = mean((deep_sleep_in_minutes/60)*coeff, na.rm = TRUE)
             )|>
   ggplot(aes(x= phase)) +
  geom_smooth(aes(y = timeinbed,group = 1), color = timeinbed_color, linewidth = 0.8) +
  geom_smooth(aes( y = deep_sleep_in_minutes, group = 1), color = deep_sleep_color, linewidth = 0.8)+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Time in bed (hours)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Deep sleep (hours)")
  ) +
  theme(
    axis.title.y = element_text(color = timeinbed_color, size=13),
    axis.title.y.right = element_text(color = deep_sleep_color, size=13)
  )
plot_3
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~
    ## x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~
    ## x'

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
plot_1 + plot_2 + plot_3
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~
    ## x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~
    ## x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~
    ## x'
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~
    ## x'

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

Mesntruators spent more time in bed during menstrual time but their deep
sleep hours were lowest. This showed that the physical exhaustion during
menstrual days requiring them to rest but preventing them from having a
restorative sleep quality. Follicular time is the when menstruators feel
most energetic and spent less time in bed but having much better deep
sleep. It will be good to link with exercising activities.

``` r
coeff = 1 # 5 is the best overlap
total_active_min_color= "red"
stress_score_color = "blue"
sleep_stress_hormone_add |>
  mutate(phase = factor(phase, levels = phase_order))|>
  group_by(phase)|>
   summarize(total_active_min = mean(total_active_min, na.rm = TRUE),
             stress_score = mean(stress_score*coeff, na.rm = TRUE)
             )|>
   ggplot(aes(x= phase)) +
  geom_line(aes(y = total_active_min,group = 1), color = total_active_min_color, linewidth = 0.8) +
  geom_line(aes( y = stress_score , group = 1), color = deep_sleep_color, linewidth = 0.8)+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Active time(minutes))",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Stress score")
  ) +
  theme(
    axis.title.y = element_text(color = total_active_min_color, size=13),
    axis.title.y.right = element_text(color = stress_score_color, size=13)
  )
```

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
We need to check if the exercise activities/ time link to stress score?

We have `stress score`, `sleep score`, `phase`, `excersise time`,
`estrogen`, `lh`

We can try to predict the stress score based on phase, time in bed, deep
sleep time? Is there a way to link exercise vs symptom? Maybe Anu did
this analysis? Do we include study_interval as the random effect? Maybe
not.

``` r
#  model
m1 <- lmer(stress_score ~ phase + timeinbed + total_active_min +(1 | id) + moodswing, data = sleep_stress_hormone_add)
summary(m1) 
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: stress_score ~ phase + timeinbed + total_active_min + (1 | id) +  
    ##     moodswing
    ##    Data: sleep_stress_hormone_add
    ## 
    ## REML criterion at convergence: 1883.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8147 -0.6366 -0.0164  0.6341  2.6182 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 13.15    3.627   
    ##  Residual             24.24    4.924   
    ## Number of obs: 309, groups:  id, 14
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)          74.229760   1.590207  56.584455  46.679  < 2e-16 ***
    ## phaseFertility        0.168304   0.933411 290.091342   0.180  0.85703    
    ## phaseLuteal          -0.106444   0.864192 291.883437  -0.123  0.90206    
    ## phaseMenstrual        0.590857   0.927992 293.370027   0.637  0.52481    
    ## timeinbed             0.007760   0.001846 290.588897   4.203 3.51e-05 ***
    ## total_active_min     -0.006606   0.009311 290.722569  -0.709  0.47859    
    ## moodswingLow          3.832021   1.423897 297.031982   2.691  0.00752 ** 
    ## moodswingModerate    -0.250613   1.089636 297.982244  -0.230  0.81825    
    ## moodswingHigh        -1.207654   1.331457 297.425939  -0.907  0.36513    
    ## moodswingVery High    3.871488   1.892216 297.562305   2.046  0.04163 *  
    ## moodswingNot at all   1.683572   1.383950 284.726062   1.216  0.22480    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) phsFrt phsLtl phsMns timnbd ttl_c_ mdswnL mdswnM mdswnH
    ## phaseFrtlty -0.233                                                        
    ## phaseLuteal -0.174  0.547                                                 
    ## phaseMnstrl -0.254  0.469  0.589                                          
    ## timeinbed   -0.379 -0.047 -0.062 -0.049                                   
    ## totl_ctv_mn -0.254  0.076  0.114  0.051 -0.114                            
    ## moodswingLw -0.301 -0.120 -0.248 -0.058 -0.045  0.013                     
    ## modswngMdrt -0.381 -0.083 -0.217 -0.063 -0.061 -0.014  0.534              
    ## moodswngHgh -0.336 -0.088 -0.295 -0.095  0.000  0.045  0.455  0.637       
    ## mdswngVryHg -0.282  0.053 -0.069 -0.002 -0.016  0.055  0.276  0.426  0.420
    ## mdswngNtata -0.337 -0.121 -0.182  0.055 -0.018  0.045  0.397  0.445  0.405
    ##             mdswVH
    ## phaseFrtlty       
    ## phaseLuteal       
    ## phaseMnstrl       
    ## timeinbed         
    ## totl_ctv_mn       
    ## moodswingLw       
    ## modswngMdrt       
    ## moodswngHgh       
    ## mdswngVryHg       
    ## mdswngNtata  0.285

``` r
# etracting coefficient
coefficients_values = fixed.effects(m1)
intercept_values = random.effects(m1)
fitted_stress_score = fitted.values(m1)
# check the residuals
qqnorm(residuals(m1)) 
```

![](sleep_stress_khue_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
