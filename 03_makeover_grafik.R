# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# MAKEOVER GRAFIK KEMISKINAN
# Indikator: Persentase Penduduk Miskin
# Tahun: 2024
# ============================================================


# ------------------------------------------------------------
# 1. PACKAGE
# ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(stringr)


# ------------------------------------------------------------
# 2. MEMANGGIL DATA BERSIH
# ------------------------------------------------------------

data_bersih <- readRDS(
  "data/data-bersih/atlas_sulsel.rds"
)


# ------------------------------------------------------------
# 3. MENGAMBIL DATA KEMISKINAN TAHUN 2024
# ------------------------------------------------------------

kemiskinan_2024 <- data_bersih %>%
  filter(tahun == 2024)


# ------------------------------------------------------------
# 4. MEMISAHKAN DATA SULAWESI SELATAN
# ------------------------------------------------------------

kemiskinan_sulsel <- kemiskinan_2024 %>%
  filter(
    kabupaten_kota == "sulawesi selatan"
  )


kemiskinan_kabkota <- kemiskinan_2024 %>%
  filter(
    kabupaten_kota != "sulawesi selatan"
  )


# ------------------------------------------------------------
# 5. MENENTUKAN STATUS WARNA
# ------------------------------------------------------------

kemiskinan_kabkota <- kemiskinan_kabkota %>%
  mutate(
    status = case_when(
      
      kemiskinan == max(
        kemiskinan,
        na.rm = TRUE
      ) ~ "Kemiskinan tertinggi",
      
      kemiskinan == min(
        kemiskinan,
        na.rm = TRUE
      ) ~ "Kemiskinan terendah",
      
      TRUE ~ "Kabupaten/Kota lainnya"
    )
  )


# ------------------------------------------------------------
# 6. MENGURUTKAN KABUPATEN/KOTA
#    TERTINGGI -> TERENDAH
# ------------------------------------------------------------

kemiskinan_kabkota <- kemiskinan_kabkota %>%
  arrange(kemiskinan) %>%
  mutate(
    kabupaten_kota = factor(
      kabupaten_kota,
      levels = kabupaten_kota
    )
  )


# ------------------------------------------------------------
# 7. MEMBUAT GRAFIK MAKEOVER
# ------------------------------------------------------------

grafik_kemiskinan <- ggplot(
  kemiskinan_kabkota,
  aes(
    x = kemiskinan,
    y = kabupaten_kota,
    fill = status
  )
) +
  
  
  # ----------------------------------------------------------
# BATANG
# ----------------------------------------------------------

geom_col(
  width = 0.70
) +
  
  
  # ----------------------------------------------------------
# NILAI KEMISKINAN
# ----------------------------------------------------------

geom_text(
  aes(
    label = sprintf(
      "%.2f%%",
      kemiskinan
    )
  ),
  hjust = -0.15,
  size = 3.5
) +
  
  
  # ----------------------------------------------------------
# GARIS BENCHMARK SULAWESI SELATAN
# ----------------------------------------------------------

geom_vline(
  xintercept = kemiskinan_sulsel$kemiskinan,
  linetype = "dashed",
  linewidth = 0.7
) +
  
  
  # ----------------------------------------------------------
# LABEL BENCHMARK
# ----------------------------------------------------------

annotate(
  "text",
  x = kemiskinan_sulsel$kemiskinan,
  y = Inf,
  label = paste0(
    "Sulawesi Selatan = ",
    sprintf(
      "%.2f%%",
      kemiskinan_sulsel$kemiskinan
    )
  ),
  hjust = 1.05,
  vjust = 1.5,
  size = 3.5
) +
  
  
  # ----------------------------------------------------------
# INDEKS / LEGENDA WARNA
# ----------------------------------------------------------

scale_fill_manual(
  
  values = c(
    "Kemiskinan tertinggi" = "#1D4ED8",
    "Kemiskinan terendah" = "#93C5FD",
    "Kabupaten/Kota lainnya" = "#DBEAFE"
  ),
  
  breaks = c(
    "Kemiskinan tertinggi",
    "Kemiskinan terendah",
    "Kabupaten/Kota lainnya"
  ),
  
  labels = c(
    "Kemiskinan tertinggi",
    "Kemiskinan terendah",
    "Kabupaten/Kota lainnya"
  ),
  
  name = "Indeks warna"
) +
  
  
  # ----------------------------------------------------------
# SKALA SUMBU X
# ----------------------------------------------------------

scale_x_continuous(
  
  limits = c(
    0,
    max(
      kemiskinan_kabkota$kemiskinan,
      na.rm = TRUE
    ) + 3
  ),
  
  expand = expansion(
    mult = c(0, 0)
  )
) +
  
  
  # ----------------------------------------------------------
# JUDUL DAN KETERANGAN
# ----------------------------------------------------------

labs(
  
  title = "Persentase Penduduk Miskin Kabupaten/Kota",
  
  subtitle = "Sulawesi Selatan, 2024",
  
  x = "Persentase penduduk miskin (%)",
  
  y = NULL,
  
  caption = paste0(
    "Sumber: Badan Pusat Statistik\n",
    "Catatan: Garis putus-putus menunjukkan persentase ",
    "penduduk miskin Sulawesi Selatan."
  )
) +
  
  
  # ----------------------------------------------------------
# TEMA
# ----------------------------------------------------------

theme_minimal(
  base_size = 12
) +
  
  
  theme(
    
    # --------------------------------------------------------
    # LEGENDA
    # --------------------------------------------------------
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    
    # --------------------------------------------------------
    # GRID
    # --------------------------------------------------------
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    
    # --------------------------------------------------------
    # SUMBU
    # --------------------------------------------------------
    
    axis.text.y = element_text(
      size = 10
    ),
    
    axis.text.x = element_text(
      size = 9
    ),
    
    
    # --------------------------------------------------------
    # JUDUL
    # --------------------------------------------------------
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 12
    ),
    
    
    # --------------------------------------------------------
    # SUMBER
    # --------------------------------------------------------
    
    plot.caption = element_text(
      hjust = 0,
      size = 9
    ),
    
    
    # --------------------------------------------------------
    # MARGIN
    # --------------------------------------------------------
    
    plot.margin = margin(
      10, 30, 10, 10
    )
  )


# ------------------------------------------------------------
# 8. MENAMPILKAN GRAFIK
# ------------------------------------------------------------

grafik_kemiskinan