# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# 02 - MENYIAPKAN DATA BATAS WILAYAH
# ============================================================


# ------------------------------------------------------------
# 1. PACKAGE
# ------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)
library(colorspace)

# ------------------------------------------------------------
# 2. MEMBACA FILE GEOJSON
# ------------------------------------------------------------

peta <- st_read(
  "data/data-batas-peta/geoBoundaries-IDN-ADM2_simplified.geojson",
  quiet = FALSE
)

# ------------------------------------------------------------
# 3. MELIHAT INFORMASI DATA PETA
# ------------------------------------------------------------

print(peta)

cat("\nJumlah polygon:", nrow(peta), "\n")

cat("\nNama kolom:\n")

print(names(peta))


unique(peta$shapeISO)
head(peta$shapeName, 30)
table(peta$shapeGroup)

# ------------------------------------------------------------
# 4. CEK HASIL PETA
# ------------------------------------------------------------

cat("Jumlah polygon Sulawesi Selatan:",
    nrow(peta_sulsel), "\n")

cat("Jumlah kode wilayah yang NA:",
    sum(is.na(peta_sulsel$kode_wilayah)), "\n")

# ------------------------------------------------------------
# 5. MENGGABUNGKAN DATA IPM 2024 DENGAN PETA
# ------------------------------------------------------------

data_ipm_2024 <- data_bersih %>%
  filter(tahun == 2024) %>%
  select(
    kode_wilayah,
    kabupaten_kota,
    ipm
  )

peta_ipm_2024 <- peta_sulsel %>%
  left_join(
    data_ipm_2024,
    by = "kode_wilayah"
  )

# ------------------------------------------------------------
# 6. CHOROPLETH IPM 2024
# ------------------------------------------------------------

peta_ipm <- ggplot(peta_ipm_2024) +
  geom_sf(
    aes(fill = ipm),
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_gradientn(
    colors = sequential_hcl(
      5,
      palette = "Blues 3"
    ),
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

peta_ipm

# ------------------------------------------------------------
# 7. UJI PALET WARNA
# ------------------------------------------------------------

palet_ipm <- sequential_hcl(
  5,
  palette = "Blues 3"
)

# Simulasi deuteranopia
deutan_palet <- deutan(palet_ipm)

# Simulasi protanopia
protan_palet <- protan(palet_ipm)

palet_ipm
deutan_palet
protan_palet

# ------------------------------------------------------------
# 8. HASIL UJI COLORBLIND
# ------------------------------------------------------------

cat("\nPalet asli:\n")
print(palet_ipm)

cat("\nSimulasi deuteranopia:\n")
print(deutan_palet)

cat("\nSimulasi protanopia:\n")
print(protan_palet)

# ------------------------------------------------------------
# 9. MENYIMPAN CHOROPLETH
# ------------------------------------------------------------

ggsave(
  filename = "keluaran/peta_ipm_2024.png",
  plot = peta_ipm,
  width = 8,
  height = 6,
  dpi = 300
)