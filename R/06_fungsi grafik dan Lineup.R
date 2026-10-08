# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# CP3 - BAGIAN 1 & 2
# FUNGSI GRAFIK + LINEUP
# ============================================================
# ============================================================
# CP3 - LINEUP 20 PANEL
# Mengikuti Grafik 3
# ============================================================

library(tidyverse)
library(nullabor)

# Theme dari CP2
source("R/04_theme_palet_tim.R")


# ============================================================
# 1. DATA
# ============================================================

data_bersih <- readRDS(
  "data/data-bersih/atlas_sulsel.rds"
)


# ============================================================
# 2. SIAPKAN DATA LINEUP
#    Tahun 2024 seperti Grafik 3
# ============================================================

data_lineup <- data_bersih %>%
  filter(tahun == 2024) %>%
  select(
    kabupaten_kota,
    ipm,
    kemiskinan
  ) %>%
  drop_na() %>%
  mutate(
    jenis_wilayah = case_when(
      str_detect(
        kabupaten_kota,
        regex("^Kota", ignore_case = TRUE)
      ) ~ "Kota",
      
      TRUE ~ "Kabupaten"
    )
  )


# ============================================================
# 3. BUAT LINEUP
#    1 DATA ASLI + 19 DATA NULL
# ============================================================

set.seed(123)

lineup_20 <- lineup(
  null_permute("ipm"),
  true = data_lineup,
  n = 20
)


# ============================================================
# 4. GRAFIK LINEUP
# ============================================================

grafik_lineup <- ggplot(
  lineup_20,
  aes(
    x = kemiskinan,
    y = ipm
  )
) +
  
  # ----------------------------------------------------------
# TITIK KABUPATEN
# ----------------------------------------------------------

geom_point(
  data = ~ dplyr::filter(
    .x,
    jenis_wilayah == "Kabupaten"
  ),
  aes(
    color = "Kabupaten",
    shape = "Kabupaten"
  ),
  size = 2.2,
  alpha = 0.85
) +
  
  # ----------------------------------------------------------
# TITIK KOTA
# ----------------------------------------------------------

geom_point(
  data = ~ dplyr::filter(
    .x,
    jenis_wilayah == "Kota"
  ),
  aes(
    color = "Kota",
    shape = "Kota"
  ),
  size = 2.5,
  alpha = 0.9
) +
  
  # ----------------------------------------------------------
# GARIS REGRESI
# ----------------------------------------------------------

geom_smooth(
  method = "lm",
  se = TRUE,
  linewidth = 0.8,
  color = "#163A5C",
  fill = palet_pita,
  inherit.aes = TRUE
) +
  
  # ----------------------------------------------------------
# 20 PANEL
# ----------------------------------------------------------

facet_wrap(
  ~ .sample,
  ncol = 5
) +
  
# ----------------------------------------------------------
# PALET CP2
# ----------------------------------------------------------

scale_color_manual(
  name = "Jenis wilayah",
  values = c(
    "Kabupaten" = "#5B8DB8",
    "Kota" = "#E69F5B"
  )
) +
  
  scale_shape_manual(
    name = "Jenis wilayah",
    values = c(
      "Kabupaten" = 16,
      "Kota" = 17
    )
  ) +
  
  # ----------------------------------------------------------
# LABEL
# ----------------------------------------------------------

labs(
  title = "Manakah panel yang paling berbeda dari yang lain?",
  subtitle = paste(
    "Dari 20 panel, satu memuat data asli.",
    "Pilih panel yang menurut Anda paling berbeda."
  ),
  x = "Persentase Penduduk Miskin",
  y = "IPM"
) +
  
  # ----------------------------------------------------------
# THEME CP2
# ----------------------------------------------------------

theme_tim() +
  
  # ----------------------------------------------------------
# PERAPIAN PANEL
# ----------------------------------------------------------

theme(
  
  # Judul
  plot.title = element_text(
    size = 16,
    face = "bold"
  ),
  
  plot.subtitle = element_text(
    size = 10
  ),
  
  # Legend
  legend.position = "top",
  
  legend.title = element_text(
    face = "bold"
  ),
  
  # Nomor panel
  strip.text = element_text(
    size = 10,
    face = "bold"
  ),
  
  # GARIS PEMBATAS SETIAP PANEL
  panel.border = element_rect(
    color = "#7A7A7A",
    fill = NA,
    linewidth = 0.7
  ),
  
  # Jarak antar-panel
  panel.spacing = unit(
    0.7,
    "lines"
  ),
  
  # Sumbu
  axis.text = element_text(
    size = 8
  ),
  
  axis.title = element_text(
    size = 10,
    face = "bold"
  )
)


# ============================================================
# 5. TAMPILKAN
# ============================================================

print(grafik_lineup)


# ============================================================
# 6. SIMPAN
# ============================================================

ggsave(
  filename = "keluaran/lineup_20_panel.png",
  plot = grafik_lineup,
  width = 14,
  height = 11,
  units = "in",
  dpi = 300
)
attr(lineup_20, "pos")
