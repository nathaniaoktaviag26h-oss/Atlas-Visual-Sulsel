# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# CP3 - BAGIAN 1 & 2
# FUNGSI GRAFIK + LINEUP
# ============================================================


# ============================================================
# 0. PACKAGE DAN THEME
# ============================================================

library(tidyverse)
library(nullabor)

# Theme dan palet dari CP2
source("R/04_theme_palet_tim.R")


# ============================================================
# 1. FUNGSI CP3
#    Minimal 2 fungsi menggunakan {{ }}
# ============================================================


# ------------------------------------------------------------
# FUNGSI 1
# Membuat scatterplot dengan nama kolom fleksibel
# ------------------------------------------------------------

grafik_hubungan <- function(data, x, y) {
  
  ggplot(
    data,
    aes(
      x = {{ x }},
      y = {{ y }}
    )
  ) +
    geom_point(
      size = 2.5,
      alpha = 0.85
    ) +
    theme_tim()
}


# ------------------------------------------------------------
# FUNGSI 2
# Membuat ringkasan statistik suatu variabel
# ------------------------------------------------------------

ringkasan_variabel <- function(data, variabel) {
  
  data %>%
    summarise(
      mean = mean({{ variabel }}, na.rm = TRUE),
      median = median({{ variabel }}, na.rm = TRUE),
      minimum = min({{ variabel }}, na.rm = TRUE),
      maksimum = max({{ variabel }}, na.rm = TRUE),
      sd = sd({{ variabel }}, na.rm = TRUE)
    )
}


# ============================================================
# 2. DATA
# ============================================================

data_bersih <- readRDS(
  "data/data-bersih/atlas_sulsel.rds"
)


# ============================================================
# 3. CONTOH PENGGUNAAN FUNGSI
#    Untuk menunjukkan fungsi dapat menerima nama kolom
# ============================================================

# Fungsi 1
cek_grafik_fungsi <- grafik_hubungan(
  data_bersih,
  kemiskinan,
  ipm
)

# Fungsi 2
cek_ringkasan <- ringkasan_variabel(
  data_bersih,
  ipm
)


# ============================================================
# 4. SIAPKAN DATA LINEUP
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
# 5. BUAT LINEUP
#    1 DATA ASLI + 19 DATA NULL
# ============================================================

set.seed(123)

lineup_20 <- lineup(
  null_permute("ipm"),
  true = data_lineup,
  n = 20
)


# ============================================================
# 6. GRAFIK LINEUP
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
    fill = "Kabupaten",
    shape = "Kabupaten"
  ),
  color = "#163A5C",
  size = 2.2,
  stroke = 0.6,
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
    fill = "Kota",
    shape = "Kota"
  ),
  color = "#163A5C",
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
  alpha = 0.30,
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

scale_fill_manual(
  name = "Jenis wilayah",
  values = c(
    "Kabupaten" = "#5B8DB8",
    "Kota" = "#E69F5B"
  )
) +
  
  
  # ----------------------------------------------------------
# BENTUK TITIK
# 21 = lingkaran dengan fill + outline
# 24 = segitiga dengan fill + outline
# ----------------------------------------------------------

scale_shape_manual(
  name = "Jenis wilayah",
  values = c(
    "Kabupaten" = 21,
    "Kota" = 24
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
  
  # Garis pembatas setiap panel
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
  ),
  
  # Hilangkan kotak legend
  legend.key = element_blank()
)


# ============================================================
# 7. TAMPILKAN LINEUP
# ============================================================

print(grafik_lineup)


# ============================================================
# 8. SIMPAN LINEUP
# ============================================================

ggsave(
  filename = "keluaran/lineup_20_panel.png",
  plot = grafik_lineup,
  width = 14,
  height = 11,
  units = "in",
  dpi = 300
)


# ============================================================
# 9. CEK POSISI DATA ASLI
#    HANYA UNTUK PENELITI
# ============================================================

attr(lineup_20, "pos")
