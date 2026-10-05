# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# MAKEOVER GRAFIK KEMISKINAN (LEGENDA DI SAMPING + JUDUL RINGKAS)
# Indikator: Persentase Penduduk Miskin
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
  pull(kemiskinan) %>%
  .[1]

# Ambil data kabupaten/kota
kemiskinan_kabkota <- data_2024 %>%
  filter(!str_detect(tolower(kabupaten_kota), "sulawesi selatan"))

# 3. PERBAIKAN NAMA (TITLE CASE) & STATUS WARNA
kemiskinan_kabkota <- kemiskinan_kabkota %>%
  mutate(
    kabupaten_kota = str_to_title(kabupaten_kota),
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
    label = sprintf("Rata-rata Prov. Sulsel (%.2f%%)", nilai_provinsi),
    hjust = -0.05,
    vjust = 0,
    size = 3.3,
    color = "gray30",
    fontface = "italic"
  ) +
  scale_fill_manual(
    values = c(
      "Kemiskinan tertinggi"   = "#1E40AF", # Biru Tua
      "Kemiskinan terendah"    = "#3B82F6", # Biru Sedang
      "Kabupaten/Kota lainnya" = "#94A3B8"  # Biru-Abu Muted
    ),
    breaks = c(
      "Kemiskinan tertinggi",
      "Kemiskinan terendah",
      "Kabupaten/Kota lainnya"
    ),
    name = "Indeks Warna"
  ) +
  scale_x_continuous(
    limits = c(0, max(kemiskinan_kabkota$kemiskinan, na.rm = TRUE) + 2.5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Persentase Penduduk Miskin Kabupaten/Kota",
    subtitle = "Sulawesi Selatan, 2024",
    x = "Persentase penduduk miskin (%)",
    y = NULL,
    caption = "Sumber: BPS Sulawesi Selatan (Hasil Survei Sosial Ekonomi Nasional / Susenas 2024)\nCatatan: Angka merupakan estimasi berbasis sampel survei. Garis putus-putus menunjukkan rata-rata provinsi."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Legenda ditampilkan di sebelah kanan
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
print(grafik_kemiskinan)

ggsave(
  "keluaran/after_makeover.png", 
  plot = grafik_kemiskinan, 
  width = 9.5, 
  height = 10, 
  dpi = 300
)
