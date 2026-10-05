# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# MAKEOVER GRAFIK KEMISKINAN (TANPA BENCHMARK PROVINSI)
# Indikator: Persentase Penduduk Miskin
# Tahun: 2024
# ============================================================

library(ggplot2)
library(dplyr)
library(stringr)

# 1. MEMANGGIL DATA BERSIH
data_bersih <- readRDS("data/data-bersih/atlas_sulsel.rds")

# 2. MENGAMBIL DATA KABUPATEN/KOTA TAHUN 2024
kemiskinan_kabkota <- data_bersih %>%
  filter(tahun == 2024) %>%
  filter(!str_detect(tolower(kabupaten_kota), "sulawesi selatan"))

# 3. MENENTUKAN STATUS WARNA (TERTINGGI / TERENDAH / LAINNYA)
kemiskinan_kabkota <- kemiskinan_kabkota %>%
  mutate(
    status = case_when(
      kemiskinan == max(kemiskinan, na.rm = TRUE) ~ "Kemiskinan tertinggi",
      kemiskinan == min(kemiskinan, na.rm = TRUE) ~ "Kemiskinan terendah",
      TRUE ~ "Kabupaten/Kota lainnya"
    )
  )

# 4. MENGURUTKAN KABUPATEN/KOTA (TERTINGGI -> TERENDAH)
kemiskinan_kabkota <- kemiskinan_kabkota %>%
  arrange(kemiskinan) %>%
  mutate(
    kabupaten_kota = factor(kabupaten_kota, levels = kabupaten_kota)
  )

# 5. MEMBUAT GRAFIK MAKEOVER
grafik_kemiskinan <- ggplot(
  kemiskinan_kabkota,
  aes(
    x = kemiskinan,
    y = kabupaten_kota,
    fill = status
  )
) +
  geom_col(width = 0.70) +
  geom_text(
    aes(label = sprintf("%.2f%%", kemiskinan)),
    hjust = -0.15,
    size = 3.5
  ) +
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
    name = "Indeks warna"
  ) +
  scale_x_continuous(
    limits = c(0, max(kemiskinan_kabkota$kemiskinan, na.rm = TRUE) + 2),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Persentase Penduduk Miskin Kabupaten/Kota",
    subtitle = "Sulawesi Selatan, 2024",
    x = "Persentase penduduk miskin (%)",
    y = NULL,
    caption = "Sumber: Badan Pusat Statistik"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 9),
    plot.margin = margin(10, 30, 10, 10)
  )

# 6. SIMPAN GAMBAR KE FOLDER KELUARAN
print(grafik_kemiskinan)

ggsave(
  "keluaran/after_makeover.png", 
  plot = grafik_kemiskinan, 
  width = 8, 
  height = 10, 
  dpi = 300
)
