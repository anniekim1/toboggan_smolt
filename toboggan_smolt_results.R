
# Libraries ---------------------------------------------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(knitr)
library(tidyverse)
library(kableExtra)
library(scales)
library(tidyr)
library(flextable)
library(FSA)
library(magrittr)
library(dplyr)
library(nnet)


file_path <- "C:/Users/KIMS/Documents/toboggan__smolt/toboggan_smolt_dataentry_COPY.csv"
df_fish <- read_csv(file_path)


# Data Labeling -----------------------------------------------------------

df_fish <- df_fish %>%
  mutate(
    count = as.numeric(count),
    date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")),
    mort = tolower(ifelse(is.na(mort), "no", mort)),
    sacrifice = tolower(ifelse(is.na(sacrifice), "no", sacrifice)),
    species = recode(species,
                     "co-w" = "Coho (wild)",
                     "st/rb" = "Steelhead or Rainbow trout",
                     "co-a" = "Coho (hatchery)",
                     "bt/dv" = "Bull trout or Dolly varden",
                     "ct" = "Cutthroat",
                     "mw" = "Mountain whitefish",
                     "ch" = "Chinook",
                     "pk" = "Pink"
    )
  )


# Total count of each species caught --------------------------------------

summary_table <- df_fish %>%
  filter(!is.na(species) & species != "co-f") %>%
  group_by(species) %>%
  summarise(`Total Count` = sum(count, na.rm = TRUE), .groups = "drop") %>%
  rename(Species = species) %>%
  mutate(Species = factor(Species, 
                          levels = c("Coho (wild)", "Coho (hatchery)", 
                                     "Steelhead or Rainbow trout", 
                                     "Bull trout or Dolly varden", 
                                     "Cutthroat", 
                                     "Mountain whitefish", 
                                     "Chinook", 
                                     "Pink"))) %>%
  arrange(Species)

summary_table %>%
  kable(caption = "Table 1. Total count of fish for each species.", format = "pandoc") %>%
  kable_styling(full_width = TRUE, position = "center", bootstrap_options = c("striped", "hover"))



# Daily count of wild and hatchery smolts at barn and fence sites ---------

daily_counts_cow <- df_fish %>%
  filter(species == "Coho (wild)", site %in% c("barn", "fence")) %>%
  group_by(date, site) %>%
  summarise(daily_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

if (nrow(daily_counts_cow) == 0) stop("Error: No data available for CO-W at Barn and Fence sites.")

# Plot daily counts for CO-W
ggplot(daily_counts_cow, aes(x = date, y = daily_count, color = site, linetype = site)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  scale_x_date(
    breaks = seq(min(daily_counts_cow$date, na.rm = TRUE), max(daily_counts_cow$date, na.rm = TRUE), by = "8 days"),
    date_labels = "%d-%b"
  ) +
  labs(
    x = "Date",
    y = "Daily Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Total count of wild coho smolts by size category ------------------------

size_order <- c("little", "small", "big", "bigger", "biggest")
co_w_size_counts <- df_fish %>%
  filter(species == "Coho (wild)", !is.na(size_cwt)) %>%
  group_by(size_cwt) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  mutate(size_cwt = factor(size_cwt, levels = size_order))

if (nrow(co_w_size_counts) == 0) stop("Error: No size data for Coho (wild) available.")

ggplot(co_w_size_counts, aes(size_cwt, total_count, fill = size_cwt)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    x = "Size Category", 
    y = "Total Count",
    fill = "Size Category"  # Change legend title
  ) +
  scale_x_discrete(
    labels = c(
      "little" = "60-69 mm",
      "small" = "70-85 mm",
      "big" = "86-115 mm",
      "bigger" = "116-190 mm",
      "biggest" = ">190 mm"
    )
  ) +
  scale_fill_manual(
    values = c(
      "little" = "lightblue", 
      "small" = "blue", 
      "big" = "darkblue", 
      "bigger" = "purple", 
      "biggest" = "red"
    ),
    labels = c(
      "little" = "60-69 mm",
      "small" = "70-85 mm",
      "big" = "86-115 mm",
      "bigger" = "116-190 mm",
      "biggest" = ">190 mm"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(breaks = pretty_breaks(n = 6))


# Age-length key ----------------------------------------------------------

alk <- df_fish %>%
  filter(!is.na(`GR AGE`), !is.na(fork_length_mm), species == "Coho (wild)") %>%
  filter(`GR AGE` != "0M") %>% 
  mutate(LengthGroup = cut(fork_length_mm, breaks = seq(0, max(fork_length_mm, na.rm = TRUE), by = 5))) %>%
  group_by(LengthGroup, `GR AGE`) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(LengthGroup) %>%
  mutate(Freq = Count / sum(Count))

assign_age <- function(length_group) {
  probs <- alk %>% filter(as.character(LengthGroup) == as.character(length_group))
  if (nrow(probs) > 0) {
    sample(probs$`GR AGE`, 1, prob = probs$Freq)
  } else {
    NA
  }
}

df_fish_age <- df_fish %>% 
  filter(species == "Coho (wild)") %>%
  mutate(LengthGroup = cut(fork_length_mm, breaks = seq(0, max(fork_length_mm, na.rm = TRUE), by = 5))) %>%
  mutate(Assigned_Age = ifelse(is.na(`GR AGE`), sapply(LengthGroup, assign_age), `GR AGE`))

bubble_data <- alk %>%
  mutate(LengthMid = as.numeric(gsub("[()\\[\\]]", "", str_extract(LengthGroup, "\\d+"))) + 2.5)

ggplot(bubble_data, aes(x = LengthMid, y = as.factor(`GR AGE`), size = Freq)) +
  geom_point(alpha = 0.6, color = "blue") +
  scale_size_area(max_size = 10) +
  labs(x = "Fork Length (mm)",
       y = "Age (GR)",
       size = "Proportion") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove the legend
    panel.grid.major.x = element_line(size = 0.3, color = "gray80", linetype = "dashed"),  # Dashed grid lines on x-axis
    panel.grid.minor.x = element_line(size = 0.2, color = "gray90", linetype = "dashed"), # Dashed minor grid lines on x-axis
    panel.grid.major.y = element_line(size = 0.3, color = "gray80", linetype = "dashed"),  # Dashed grid lines on y-axis
    panel.grid.minor.y = element_line(size = 0.2, color = "gray90", linetype = "dashed")  # Dashed minor grid lines on y-axis
  ) +
  scale_x_continuous(
    limits = c(50, max(bubble_data$LengthMid, na.rm = TRUE)),  # Force axis to start at 50
    breaks = seq(50, max(bubble_data$LengthMid, na.rm = TRUE), by = 25),  # Labels at 50, 75, 100, etc.
    minor_breaks = seq(0, max(bubble_data$LengthMid, na.rm = TRUE), by = 5)  # Grid lines every 10 mm
  )


ggsave("age_length_key_bubble_plot.png", width = 7, height = 5)



# Age-length key 2.0 ------------------------------------------------------

df_co_w <- df_fish %>% 
  filter(species == "Coho (wild)") %>% 
  select(fork_length_mm, `GR AGE`)

df_co_w <- df_co_w %>% mutate(`GR AGE` = ifelse(`GR AGE` == "", NA, `GR AGE`))
df_co_w <- df_co_w %>% mutate(lcat5 = lencat(fork_length_mm, w = 5))

aged <- df_co_w %>% filter(!is.na(`GR AGE`))
unaged <- df_co_w %>% filter(is.na(`GR AGE`))

aged <- aged%>% mutate(`GR AGE` = as.numeric(`GR AGE`))

aged <- aged %>% filter(`GR AGE` %in% c(22, 33, 44))

alk.freq <- xtabs(~ lcat5 + `GR AGE`, data = aged)
rowSums(alk.freq)
alk <- prop.table(alk.freq, margin=1)

mlr <- multinom(`GR AGE`~lcat5, data=aged,maxit=500)
lens <- seq (50, 200, 5)
alk.sm <- predict(mlr,data.frame(lcat5=lens), type="probs")
row.names(alk.sm) <- lens
row(alk)

alkPlot(alk, type="bubble", xlab="Fork Length (mm)", ylab="Age", yaxt="n", col = "skyblue")  # Suppress default y-axis labels
axis(2, at=c(22, 33, 44), labels=c(22, 33, 44), tick=FALSE)

alkPlot(alk, type="area", pal="gray", showLegend=TRUE,
        leg.cex=0.7, xlab="Total Length (mm)")

# Weight-length model for coho smolts -------------------------------------

df_fish_coho <- df_fish %>% 
  filter(
    species %in% c("Coho (wild)", "Coho (hatchery)"),  # Keep only co-w and co-a
    !is.na(fork_length_mm), 
    !is.na(weight_g), 
    weight_g > 0, 
    fork_length_mm > 0
  )

length_weight_model <- lm(log(weight_g) ~ log(fork_length_mm), data = df_fish_coho)
summary(length_weight_model)

ggplot(df_fish_coho, aes(x = fork_length_mm, y = weight_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  scale_x_log10() +  # Apply log scale to x-axis
  scale_y_log10() +  # Apply log scale to y-axis for consistency
  labs(
    x = "Fork Length (mm)",
    y = "Weight (g)"
  ) +
  annotate("text", 
           x = max(df_fish_coho$fork_length_mm) * 0.9,  # Move closer to right
           y = min(df_fish_coho$weight_g) * 3,  # Move lower
           label = "log(Weight) = -10.8 + 2.86 * log(Length)", 
           size = 3.5,  # Make text smaller
           hjust = 1, 
           fontface = "italic") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
```

```{r save-lw-plot, echo=FALSE}
ggsave("co_w_size_histogram.png", width = 7, height = 5)


# Mortalities -------------------------------------------------------------

mortality_summary <- df_fish %>%
  group_by(species) %>%
  summarise(
    `Total Fish per Species` = sum(count, na.rm = TRUE),  
    `Total Accidental` = sum(ifelse(mort == "yes", count, 0), na.rm = TRUE),
    `Total Sacrifice` = sum(ifelse(sacrifice == "yes", count, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `% Accidental` = round((`Total Accidental` / `Total Fish per Species`) * 100, 2),
    `% Sacrifice` = round((`Total Sacrifice` / `Total Fish per Species`) * 100, 2),
    `% Accidental` = paste0(replace_na(`% Accidental`, 0), "%"),
    `% Sacrifice` = paste0(replace_na(`% Sacrifice`, 0), "%")
  ) %>%
  filter(`Total Accidental` > 0 | `Total Sacrifice` > 0)  # Remove species with zero mortality

# Add Coho (hatchery) data (all sacrificed)
coho_hatchery_data <- df_fish %>%
  filter(species == "Coho (hatchery)") %>%
  summarise(
    species = "Coho (hatchery)",
    `Total Fish per Species` = sum(count, na.rm = TRUE),
    `Total Accidental` = 0,
    `Total Sacrifice` = sum(count, na.rm = TRUE),
    `% Accidental` = "0%",
    `% Sacrifice` = "100%",
    .groups = "drop"
  )

# Bind Coho (hatchery) data and update column names and row order
mortality_summary <- bind_rows(mortality_summary, coho_hatchery_data) %>%
  rename(Species = species) %>%
  arrange(match(Species, c("Coho (wild)", "Coho (hatchery)")), Species)

# Display the table
mortality_summary %>%
  kable(format = "pandoc", booktabs = TRUE, align = c("l", "r", "r", "r", "r", "r"),
        col.names = c("Species", 
                      "Total Fish", 
                      "Total Accidental", 
                      "Total Sacrifice", 
                      "% Accidental", 
                      "% Sacrifice"),
        caption = "Table 2. Accidental and sacrificed mortalities. Note that the percentages are calculated in comparison to the respective species, not the total count of all species."
  )


