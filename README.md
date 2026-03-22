# r-codes
r code scripts for results section in thesis: Twenty-Four-Hour Rhythms of Physical Activity, Sleep, and Glucose Across Shift Types in Healthcare Workers With Type 2 Diabetes

Analysis uses data from KCL study,  Gibson, R., D’Annibale, M., Palla, L., Tejo, F., McGowan, B., Vetter, C., Lorencatto, F., Oliver, N. and Guess, N. (2026) ‘Characterising the impact of shift work on diet and glucose variability in healthcare employees living with type 2 diabetes: The Shift-Diabetes study’, Diabetic Medicine. Advance online publication. https://onlinelibrary.wiley.com/doi/10.1111/dme.70262?af=R

This repository contains R scrips used to generate all figures and statistical analyses for a study examining the temporal relationship between sleep, physical activity, and glucose regulation in healthcare shift workers with type 2 diabetes. 

The project uses data from:
- continiuous glucose monitoring (CGM)
- Wrist-worn accelerometry
- device-derived sleep metrics 
- shift schedule classification

r version 4.2.0 or above and requires packages:
-tidyverse
-ggplot2
-dplyr
-tidyr
-lme4
-lmerTest
-emmeans
-DiagrammeR
-DiagrammeRsvg
-rsvg

Figure Outputs:
- Figure 1-4 24-hour profiles of physical activity, sleep, and glucose
- Figure 5 + 5B Daily MVPA glucose outcomes across shift types
- Figure 6 + 6B Between-participant associations MVPA and glucose outcomes (mean, CV%)
- Figure 7 + 7B MVPA timing around night shifts
- Figure 8 + 8B 18-hour MVPA timing around night shifts

Statistical approach: Linear mixed-effects models
- outcome ~ predictor x shift_type + (1 participant)
- outcome ~ hour x shift_type + (1 participant)
