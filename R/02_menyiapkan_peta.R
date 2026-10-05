# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# 02 - MENYIAPKAN DATA BATAS WILAYAH DAN PETA
# ============================================================


# ------------------------------------------------------------
# 1A. PACKAGE
# ------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(colorspace)

# ------------------------------------------------------------
# 1B. MEMBACA DATA BERSIH
# ------------------------------------------------------------

data_bersih <- readRDS(
  "data/data-bersih/atlas_sulsel.rds"
)

# ------------------------------------------------------------
# 2. MEMBACA DATA KODE WILAYAH
# ------------------------------------------------------------

kode_wilayah <- readxl::read_excel(
  "data/data-kode-wilayah/KODE WILAYAH SULSEL.xlsx"
)

kode_wilayah <- kode_wilayah %>%
  rename(
    kode_wilayah = `Kode Kab/Kota`,
    kabupaten_kota = `Kabupaten/Kota`
  ) %>%
  mutate(
    kode_wilayah = as.character(kode_wilayah),
    kabupaten_kota = str_to_lower(
      str_squish(kabupaten_kota)
    )
  ) %>%
  select(
    kabupaten_kota,
    kode_wilayah
  )


# ------------------------------------------------------------
# 3. MEMBACA BATAS WILAYAH
# ------------------------------------------------------------

peta <- st_read(
  "data/data-batas-peta/geoBoundaries-IDN-ADM2_simplified.geojson",
  quiet = FALSE
)


# ------------------------------------------------------------
# 4. MEMILIH 24 KABUPATEN/KOTA SULAWESI SELATAN
# ------------------------------------------------------------

wilayah_sulsel <- unique(
  data_bersih$kabupaten_kota
)

wilayah_sulsel <- str_to_lower(
  str_squish(wilayah_sulsel)
)

peta_sulsel <- peta %>%
  mutate(
    nama_peta = str_to_lower(
      str_squish(shapeName)
    )
  ) %>%
  filter(
    nama_peta %in% wilayah_sulsel
  )


# ------------------------------------------------------------
# 5. MENGHUBUNGKAN KODE WILAYAH DENGAN PETA
# ------------------------------------------------------------

peta_sulsel <- peta_sulsel %>%
  left_join(
    kode_wilayah,
    by = c(
      "nama_peta" = "kabupaten_kota"
    )
  )

# ------------------------------------------------------------
# 6. CEK HASIL PETA
# ------------------------------------------------------------

cat(
  "Jumlah polygon Sulawesi Selatan:",
  nrow(peta_sulsel),
  "\n"
)

cat(
  "Jumlah kode wilayah yang NA:",
  sum(is.na(peta_sulsel$kode_wilayah)),
  "\n"
)


# ------------------------------------------------------------
# 7. DATA IPM TAHUN 2024
# ------------------------------------------------------------

data_ipm_2024 <- data_bersih %>%
  filter(
    tahun == 2024
  ) %>%
  select(
    kode_wilayah,
    kabupaten_kota,
    ipm
  )

# ------------------------------------------------------------
# 8. MENGGABUNGKAN IPM DENGAN PETA
# ------------------------------------------------------------

peta_ipm_2024 <- peta_sulsel %>%
  left_join(
    data_ipm_2024,
    by = "kode_wilayah"
  )

# ------------------------------------------------------------
# 9. CEK DATA IPM
# ------------------------------------------------------------

cat(
  "Jumlah wilayah dengan data IPM:",
  nrow(peta_ipm_2024),
  "\n"
)

cat(
  "Jumlah IPM yang NA:",
  sum(is.na(peta_ipm_2024$ipm)),
  "\n"
)

# ------------------------------------------------------------
# 10. PALET WARNA
# ------------------------------------------------------------

palet_ipm <- sequential_hcl(
  5,
  palette = "Blues 3"
)

# ------------------------------------------------------------
# 11. CHOROPLETH IPM 2024
# ------------------------------------------------------------

peta_ipm <- ggplot(
  peta_ipm_2024
) +
  geom_sf(
    aes(fill = ipm),
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_gradientn(
    colors = palet_ipm,
    name = "IPM"
  ) +
  labs(
    title = "Indeks Pembangunan Manusia Sulawesi Selatan, 2024",
    subtitle = "Nilai IPM menurut kabupaten/kota",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    legend.position = "right"
  )


# Menampilkan peta
peta_ipm

# ------------------------------------------------------------
# 12A. UJI COLORBLIND
# ------------------------------------------------------------

deutan_palet <- deutan(
  palet_ipm
)

protan_palet <- protan(
  palet_ipm
)

cat("\nPalet asli:\n")
print(palet_ipm)

cat("\nSimulasi deuteranopia:\n")
print(deutan_palet)

cat("\nSimulasi protanopia:\n")
print(protan_palet)

# ------------------------------------------------------------
# 12B. VISUALISASI UJI COLORBLIND
# ------------------------------------------------------------

peta_deutan <- ggplot(peta_ipm_2024) +
  geom_sf(
    aes(fill = ipm),
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_gradientn(
    colors = deutan_palet,
    name = "IPM"
  ) +
  labs(
    title = "Simulasi Deuteranopia",
    subtitle = "Peta IPM Sulawesi Selatan 2024"
  ) +
  theme_void()


peta_protan <- ggplot(peta_ipm_2024) +
  geom_sf(
    aes(fill = ipm),
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_gradientn(
    colors = protan_palet,
    name = "IPM"
  ) +
  labs(
    title = "Simulasi Protanopia",
    subtitle = "Peta IPM Sulawesi Selatan 2024"
  ) +
  theme_void()


# Menampilkan hasil simulasi
peta_deutan
peta_protan

# ------------------------------------------------------------
# 13. SIMPAN CHOROPLETH
# ------------------------------------------------------------

ggsave(
  filename = "keluaran/peta_ipm_2024.png",
  plot = peta_ipm,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 14. SIMPAN HASIL UJI COLORBLIND
# ------------------------------------------------------------

ggsave(
  filename = "keluaran/peta_ipm_2024_deuteranopia.png",
  plot = peta_deutan,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "keluaran/peta_ipm_2024_protanopia.png",
  plot = peta_protan,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 15. PESAN AKHIR
# ------------------------------------------------------------

cat("\n========================================\n")
cat("PEMETAAN SELESAI\n")
cat("========================================\n")
cat(
  "Jumlah polygon:",
  nrow(peta_sulsel),
  "\n"
)
cat(
  "Jumlah IPM NA:",
  sum(is.na(peta_ipm_2024$ipm)),
  "\n"
)
cat(
  "Output: keluaran/peta_ipm_2024.png\n"
)
