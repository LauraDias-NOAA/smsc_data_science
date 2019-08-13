# Title: Lorena and the Gilled green iguana

# Background: Lorena is studying the respiration rate (I believe?) of some
# species, let's call it a "Gilled green iguana") in a tank of water. To do
# so, Lorena adds, dissolved oxygen to the tank and measures DO (response 
# variable) over time (predictor variable). She adds oxygen until the DO
# reads 10 mg/L (sorry if I'm butchering your methods Lorena!).

# The problem: Lorena wants to calculate the slope of dissolved oxygen
# after each event in which oxygen was added. The data are continuous
# and thus need to be grouped.


# Simulate data -----------------------------------------------------------

# Write a function to generature a noisy wave:

make_noisyWave <-
  function(noiseValue) {
    purrr::map_dfr(
      # Generate a sequence of points by 0.1:
      seq(0, 12, .1),
      function(x){
        # Create some gaussian noise:
        noise <-
          rnorm(n = 1, sd = noiseValue * .007)
        # Output tibble of time and oxygen concentration:
        tibble(
          time = x * 10,
          # Sin wave, shifted up, with noise:
          dissOx = mean(c(7.5, 10)) + sin(x) + noise)
      }
    )
  }

# Note: The real data are not a sine curve but rather a sort of reverse 
# sawtooth wave.

noisyWave <-
  make_noisyWave(noiseValue = 20)

# Plot simulated data to determine most appropriate noise values:

ggplot(noisyWave, aes(x = time, y = dissOx)) +
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
    noisyData %>%
      mutate(
        # See ?stats::loess for details:
        dissOx_smooth = loess(
          dissOx ~ time,
          data = .,
          # Span will control how smoothed the data will be:
          span = smoothingSpan) %>%
          # Predict will return predicted values for a given time:
          predict())
  }

# View smoothing to determine which smoothing span is best:

noisyWave %>%
  add_smooth(.3) %>%
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

make_noisyWave(noiseValue = 10) %>%
  add_smooth(.3) %>%
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

wave_noisySmooth <-
  make_noisyWave(noiseValue = 10) %>%
  add_smooth(.3) %>%
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

# Plot the data:

wave_noisySmooth %>%
  ggplot(aes(x = time, y = dissOx)) +
  geom_ribbon(
    # Why do we need to add a new data argument?
    data = wave_noisySmooth %>%
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
  geom_line(aes(y = dissOx_smooth), color = 'blue', size = 1) +
  labs(
    title = 'Dissolved oxygen concentration over time',
    x = 'Time (seconds)',
    y = 'Dissolved oxygen (mg/L)'
  ) +
  scale_fill_manual(values = c('#B02909', '#DF993A'), name = 'Trial') +
  scale_y_continuous(limits = c(7.5, 10), expand = c(0,0)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    title = element_text(size = rel(1.5))
  )

# Subset the data ---------------------------------------------------------

gilled_iguana <-
  wave_noisySmooth %>%
  filter(!is.na(trial_group)) %>%
  select(trial_group, time, dissOx)

# Lorena is intersted in the slope of decrease after adding the DO. Here's
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


   
