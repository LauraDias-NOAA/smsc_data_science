# Title: Lorena and the Gilled green iguana

# Background: Lorena is studying the respiration rate (I believe?) of some
# species, let's call it a "Gilled green iguana") in a tank of water. To do
# so, Lorena adds, dissolved oxygen to the tank and measures DO (response 
# variable) over time (predictor variable). She adds oxygen until the DO
# reads 10 mg/L (sorry if I'm butchering your methods Lorena!).

# The problem: Lorena wants to calculate the slope of dissolved oxygen
# after each event in which oxygen was added. The data are continuous
# and thus need to be grouped.


# setup -------------------------------------------------------------------

library(tidyverse)

# Simulate data -----------------------------------------------------------

# Write a function to generate a sine wave:

make_sineWave <-
  function(timeVector, y_offset, amplitude, angular_freq, phase){
    tibble(
      time = timeVector,
      dissOx = y_offset + amplitude * sin(angular_freq*time + phase)
    )
  }

# Note: The real data are not a sine curve but rather a sort of reverse 
# sawtooth wave.

# Generate a sine wave:

sineWave <-
  make_sineWave(
    timeVector = 1:120,
    y_offset = 8.75,
    amplitude = 1.25,
    angular_freq = 0.15,
    phase = .5)

# Plot the data:

sineWave %>%
  ggplot(
    aes(x = time, y = dissOx)
  ) +
  geom_line() +
  labs(
    title = 'Dissolved oxygen concentration over time',
    x = 'Time (seconds)',
    y = 'Dissolved oxygen (mg/L)'
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    title = element_text(size = rel(1.5))
  )

# Try changing the y-offset, amplitude, angular frequency, and phase. How does this influence the sine wave?

# Write a function to generature a noisy wave:

make_noisyWave <-
  function(sineWave_frame, noiseValue) {
    sineWave_frame %>%
      mutate(
        dissOx = rnorm(nrow(.), mean = dissOx, sd = noiseValue * .007))
  }

noisyWave <-
  make_noisyWave(sineWave, noiseValue = 20)

noisyWave


# Plot simulated data to determine most appropriate noise values:

noisyWave %>%
  ggplot(aes(x = time, y = dissOx)) +
  geom_point() + 
  geom_line() +
  labs(
    title = 'Dissolved oxygen concentration over time',
    x = 'Time (seconds)',
    y = 'Dissolved oxygen (mg/L)'
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    title = element_text(size = rel(1.5))
  )

## Try different noise values -- how does this change the output?

# Generate a smooth -------------------------------------------------------

# We will assign a function that uses loess (local polynomial regression 

# fitting -- in the stats package of base R) to smooth our noisy data:

add_smooth <-
  function(noisyData, smoothingSpan){
    loess(
      dissOx ~ time,
      data = noisyData,
      # Span will control how smoothed the data will be:
      span = smoothingSpan) %>%
      # Predict will return predicted values for a given time:
      predict()
  }

noisyWave %>%
  mutate(dissOx_smooth = add_smooth(., smoothingSpan = 0.3))

# View smoothing to determine which smoothing span is best:

noisyWave %>%
  mutate(dissOx_smooth = add_smooth(., smoothingSpan = 0.3)) %>%
  ggplot(aes(x = time, y = dissOx)) +
  geom_point() + 
  geom_line() +
  # A new line with our smoothed dissOx_smooth data:
  geom_line(aes(y = dissOx_smooth), color = 'blue', size = 1) +
  labs(
    title = 'Dissolved oxygen concentration over time',
    x = 'Time (seconds)',
    y = 'Dissolved oxygen (mg/L)'
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    title = element_text(size = rel(1.5))
  )

## Try different smoothing spans. How does this influence the output?

# The best smoothing parameter is probably dependent on both the noise in
# the data and the smoothing span:

make_noisyWave(sineWave, noiseValue = 20) %>%
  mutate(dissOx_smooth = add_smooth(., smoothingSpan = 0.3)) %>%
  ggplot(aes(x = time, y = dissOx)) +
  geom_point() + 
  geom_line() +
  geom_line(aes(y = dissOx_smooth), color = 'blue', size = 1) +
  labs(
    title = 'Dissolved oxygen concentration over time',
    x = 'Time (seconds)',
    y = 'Dissolved oxygen (mg/L)'
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    title = element_text(size = rel(1.5))
  )

## Try different noise and smoothing spans. How does this influence the 
# output?

# Assigning groups --------------------------------------------------------

# Generate data:

wave_trials <-
  noisyWave %>%
  mutate(dissOx_smooth = add_smooth(., smoothingSpan = 0.3)) %>%
  mutate(
    # Look forward to determine dissOx is increasing or decreasing:
    slope = ifelse(
      lead(dissOx_smooth) > dissOx_smooth,
      'positive',
      'negative'),
    # Define the turning points in the data:
    trial_group = ifelse(
      lag(slope) == 'positive' & slope == 'negative',
      1, 0
    ),
    # Assign group identities:
    trial_group = 
      ifelse(
        slope == 'negative',
        # What does this do?
        cumsum(trial_group),
        NA))

wave_trials

# Plot the data:

wave_trials %>%
  ggplot(aes(x = time, y = dissOx)) +
  geom_ribbon(
    # Why do we need to add a new data argument?
    data = wave_trials %>%
      filter(!is.na(trial_group)) %>%
      group_by(trial_group) %>%
      mutate(minTime = min(time), maxTime = max(time)),
    # Look closely at these aesthetics:
    aes(
      ymin = min(dissOx_smooth),
      ymax = dissOx_smooth,
      fill = factor(trial_group)),
    alpha = .9
  ) +
  geom_point(size = 2) + 
  geom_line() +
  geom_line(
    aes(y = dissOx_smooth),
    color = 'blue',
    size = 1) +
  labs(
    title = 'Dissolved oxygen concentration over time',
    x = 'Time (seconds)',
    y = 'Dissolved oxygen (mg/L)'
  ) +
  scale_fill_manual(
    values = c('#B02909', '#DF993A', '#F24A47'),
    name = 'Trial') +
  scale_y_continuous(
    limits = c(7.5, 10),
    expand = c(0,0)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    title = element_text(size = rel(1.5))
  )

# Subset the data ---------------------------------------------------------

# Subset data to observations (rows) associated with trial groups and the
# columns of interest:

gilled_iguana <-
  wave_trials %>%
  filter(!is.na(trial_group)) %>%
  select(trial_group, time, dissOx)

gilled_iguana

# Lorena is interested in the slope of decrease after adding the DO. Here's
# what that might look like:

map(
  unique(gilled_iguana$trial_group),
  function(x){
    gilled_iguana %>%
      filter(trial_group == x) %>%
      lm(dissOx ~ time, data = .) %>%
      summary()
  }
)

# Plot the models:

gilled_iguana %>%
  mutate(Trial = factor(trial_group)) %>%
  group_by(Trial) %>%
  mutate(time = time - min(time)) %>%
  ungroup %>%
  ggplot(
    aes(time, dissOx)) +
  stat_smooth(method = "lm", aes(col = Trial)) +
  geom_point(
    aes(fill = Trial),
    size = 3, 
    alpha = .5, 
    pch=21) +
  labs(
    title = 'Dissolved oxygen concentration over time',
    x = 'Time since treatment (seconds)',
    y = 'Dissolved oxygen (mg/L)'
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    title = element_text(size = rel(1.5))
  )


   
