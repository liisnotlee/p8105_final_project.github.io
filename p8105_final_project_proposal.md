P8105 FINAL PROJECT PROPOSAL
================
2025-11-07

## Group members

Muying Li (ml5218), Yue Zhao (yz5290), Khue Nguyen (nnk2114), Anu Singh
(as7923), Emily Brynn Weaver (ebw2145)

## Tentative project title

Exploring the relationship between lifestyle factors and the menstrual
cycle among young adult menstruators using multimodal data

## Motivation

Half of the world’s population experiences menstruation, yet irregular
symptoms often lack clear underlying causes while significantly
disrupting daily life. By analyzing data from young adult menstruators,
we aim to understand relationships between physiological menstrual
characteristics and lifestyle factors such as stress, sleep, and
exercise.

## Intended final products

- **Webpage** with Home, Exploratory Analysis, Models, Report, and
  Screencast pages
- **Exploratory analysis** of lifestyle factors (exercise, sleep,
  stress) and menstrual health patterns, including cycle length analysis
  and symptom correlations
- **Statistical models** using multivariable linear regression to
  evaluate associations between lifestyle factors and menstrual health
- **Screencast** demonstrating webpage navigation and highlighting key
  findings

## Anticipated data sources

The [mcPHASES dataset](https://physionet.org/content/mcphases/1.0.0/)
contains multimodal data from 42 menstruating young adults in Canada
tracked over two 3-month intervals. Participants wore Fitbit Sense
smartwatches and Dexcom G6 glucose monitors, used Mira Plus kits for
hormone tracking, and self-reported daily symptoms. The original dataset
contains 23 structured tables. We will focus on:

- `hormones_and_selfreport.csv`
- `height_and_weight.csv`
- `subject_info.csv`
- `stress_score.csv sleep.csv`
- `sleep_score.csv`

## Planned analyses / visualizations

- **Descriptive statistics:** Study population characteristics,
  menstrual cycle data (hormones, symptoms, duration), and lifestyle
  factors (sleep, stress, exercise)
- **Exploratory Data Analysis:** Distribution visualizations
  (histograms, boxplots, heatmaps) by demographics; correlation analysis
  between lifestyle factors and symptoms; hormone-symptom trajectories
  across cycle phases (line plots, bar charts); exercise and sleep
  pattern analysis across menstrual phases; analysis of sleep quality
  during cycle phases (boxplots)
- **Prediction and ROC Analysis:** Predictive models using hormone
  biomarkers for ovulation and symptom onset; multiple linear regression
  predicting symptom severity using lifestyle predictors and cycle phase

## Coding challenges

- **Data cleaning:** Handling missing data, standardizing variables,
  converting categorical to numeric values
- Managing duplicate entries and aggregating multiple daily measurements
- Merging multiple datasets via common identifiers without data loss
- **Statistical modeling:** Testing assumptions and accounting for
  repeated measures

## Planned timeline

- **Nov 1–7:** Dataset selection, determine analyses, and proposal
  submission by Nov 7.
- **Nov 8–14:** Data cleaning, merging, and preliminary dataset
  exploration. Attend project meeting.
- **Nov 15–22:** Conduct exploratory data analyses and main analyses.
  Attend project meeting.
- **Nov 23–29:** Run advanced models. Attend project meeting.
- **Nov 30–Dec 5:** Finalize results, figures, report, webpage, and
  screencast.
- **Dec 6:** Final submission of report, webpage, screencast, and peer
  assessment.
