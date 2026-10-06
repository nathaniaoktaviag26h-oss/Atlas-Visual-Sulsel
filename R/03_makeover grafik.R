# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# MAKEOVER GRAFIK IPM (LEGENDA DI SAMPING + JUDUL RINGKAS)
# Indikator: Indeks Pembangunan Manusia (IPM)
# Tahun: 2024
# ============================================================

library(ggplot2)
library(dplyr)
library(stringr)

# 1. MEMANGGIL DATA BERSIH
data_bersih <- readRDS("data/data-bersih/atlas_sulsel.rds")

# 2. MENGAMBIL DATA PROVINSI DAN KABUPATEN/KOTA TAHUN 2024
data_2024 <- data_bersih %>%
  filter(tahun == 2024)

# Ambil angka provinsi untuk garis rujukan
nilai_provinsi <- data_2024 %>%
  filter(str_detect(tolower(kabupaten_kota), "sulawesi selatan")) %>%
  pull(ipm) %>%
  .[1]

# Ambil data kabupaten/kota
ipm_kabkota <- data_2024 %>%
  filter(!str_detect(tolower(kabupaten_kota), "sulawesi selatan"))

# 3. PERBAIKAN NAMA (TITLE CASE) & STATUS WARNA
ipm_kabkota <- ipm_kabkota %>%
  mutate(
    kabupaten_kota = str_to_title(kabupaten_kota),
    status = case_when(
      ipm == max(ipm, na.rm = TRUE) ~ "IPM tertinggi",
      ipm == min(ipm, na.rm = TRUE) ~ "IPM terendah",
      TRUE ~ "Kabupaten/Kota lainnya"
    )
  )

# 4. MENGURUTKAN KABUPATEN/KOTA (TERTINGGI -> TERENDAH)
ipm_kabkota <- ipm_kabkota %>%
  arrange(ipm) %>%
  mutate(
    kabupaten_kota = factor(kabupaten_kota, levels = kabupaten_kota)
  )

# 5. MEMBUAT GRAFIK MAKEOVER
grafik_ipm <- ggplot(
  ipm_kabkota,
  aes(
    x = ipm,
    y = kabupaten_kota,
    fill = status
  )
) +
  geom_col(width = 0.70) +
  geom_text(
    aes(label = sprintf("%.2f", ipm)),
    hjust = -0.3,
    size = 3.5,
    color = "gray20"
  ) +
  geom_vline(
    xintercept = nilai_provinsi,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = nilai_provinsi,
    y = 2.5,
    label = sprintf("Rata-rata Prov. Sulsel (%.2f)", nilai_provinsi),
    hjust = -0.05,
    vjust = 0,
    size = 3.3,
    color = "gray30",
    fontface = "italic"
  ) +
  scale_fill_manual(
    values = c(
      "IPM tertinggi"        = "#1E40AF", # Biru Tua
      "IPM terendah"         = "#3B82F6", # Biru Sedang
      "Kabupaten/Kota lainnya" = "#94A3B8"  # Biru-Abu Muted
    ),
    breaks = c(
      "IPM tertinggi",
      "IPM terendah",
      "Kabupaten/Kota lainnya"
    ),
    name = "Indeks Warna"
  ) +
  scale_x_continuous(
    limits = c(0, max(ipm_kabkota$ipm, na.rm = TRUE) + 12),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Indeks Pembangunan Manusia (IPM) Kabupaten/Kota",
    subtitle = "Sulawesi Selatan, 2024",
    x = "Indeks Pembangunan Manusia",
    y = NULL,
    caption = "Sumber: BPS Sulawesi Selatan (Hasil Long Form SP2020 / Sensus Penduduk 2020)\nCatatan: Garis putus-putus menunjukkan rata-rata provinsi."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 15, color = "gray10"),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 12)),
    plot.caption = element_text(hjust = 0, size = 8.5, color = "gray40", margin = margin(t = 12)),
    plot.margin = margin(15, 15, 15, 15)
  )

# 6. SIMPAN GAMBAR KE FOLDER KELUARAN
print(grafik_ipm)

ggsave(
  "keluaran/after_makeover_ipm.png", 
  plot = grafik_ipm, 
  width = 9.5, 
  height = 10, 
  dpi = 300
)
