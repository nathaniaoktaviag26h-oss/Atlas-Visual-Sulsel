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
library(patchwork)

# Theme dan palet visual tim

source("R/04_theme_palet_tim.R")

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

# Validasi 
stopifnot(
  nrow(peta_sulsel) == 24,
  sum(is.na(peta_sulsel$kode_wilayah)) == 0
)

cat(
  "Validasi berhasil: 24 wilayah dan seluruh kode tersedia.\n"
)

# ------------------------------------------------------------
# 7. DATA IPM TAHUN 2022-2024
# ------------------------------------------------------------

data_ipm <- data_bersih %>%
  filter(tahun %in% 2022:2024) %>%
  select(kode_wilayah, tahun, ipm)

peta_ipm_semua <- peta_sulsel %>%
  left_join(data_ipm, by = "kode_wilayah")

stopifnot(
  nrow(peta_ipm_semua) == 72,
  sum(is.na(peta_ipm_semua$ipm)) == 0
)

# ------------------------------------------------------------
# 8. PALET DAN KATEGORI RENTANG IPM
# ------------------------------------------------------------

# Batas kategori setiap 5 poin IPM
batas_ipm_cb <- seq(
  floor(min(data_ipm$ipm, na.rm = TRUE) / 5) * 5,
  ceiling(max(data_ipm$ipm, na.rm = TRUE) / 5) * 5,
  by = 5
)

# Pastikan batas mencakup semua nilai IPM
if (length(batas_ipm_cb) < 2) {
  batas_ipm_cb <- c(
    floor(min(data_ipm$ipm, na.rm = TRUE)),
    ceiling(max(data_ipm$ipm, na.rm = TRUE)) + 1
  )
}

# Label rentang dengan format desimal Indonesia
label_ipm_cb <- paste0(
  format(
    head(batas_ipm_cb, -1),
    nsmall = 2,
    decimal.mark = ","
  ),
  "–",
  format(
    tail(batas_ipm_cb, -1),
    nsmall = 2,
    decimal.mark = ","
  )
)

# Palet warna IPM
palet_ipm_cb <- colorspace::sequential_hcl(
  5,
  palette = "Blues 3"
)

# ------------------------------------------------------------
# 9. PETA KATEGORI IPM 2022-2024
# ------------------------------------------------------------

# Buat kategori IPM dengan batas yang sama untuk semua tahun
data_ipm_semua_kategori <- peta_ipm_semua %>%
  mutate(
    kategori_ipm_cb = cut(
      ipm,
      breaks = batas_ipm_cb,
      labels = label_ipm_cb,
      include.lowest = TRUE,
      right = TRUE
    )
  )

peta_ipm <- ggplot(data_ipm_semua_kategori) +
  geom_sf(
    aes(fill = kategori_ipm_cb),
    color = "white",
    linewidth = 0.3
  ) +
  facet_wrap(~tahun, nrow = 1) +
  scale_fill_manual(
    values = palet_ipm_cb,
    drop = FALSE,
    name = "Rentang IPM"
  ) +
  labs(
    title = "Perkembangan IPM Sulawesi Selatan",
    subtitle = "Kabupaten/kota | 2022–2024",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan"
  ) +
  theme_tim() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 12,
      color = "black"
    ),
    legend.position = "right",
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    legend.text = element_text(size = 9),
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8)
  ) +
  coord_sf(datum = NA)

print(peta_ipm)

# ------------------------------------------------------------
# 10. PETA KATEGORI IPM 2024 DAN SIMULASI BUTA WARNA
# ------------------------------------------------------------

# Ambil data IPM tahun 2024 saja
data_ipm_2024 <- peta_ipm_semua %>%
  filter(tahun == 2024)

# Fungsi untuk membuat peta dengan palet tertentu
buat_peta_ipm_cb <- function(data, warna, judul) {
  
  data <- data %>%
    mutate(
      kategori_ipm_cb = cut(
        ipm,
        breaks = batas_ipm_cb,
        labels = label_ipm_cb,
        include.lowest = TRUE,
        right = TRUE
      )
    )
  ggplot(data) +
    geom_sf(
      aes(fill = kategori_ipm_cb),
      color = "white",
      linewidth = 0.3
    ) +
    scale_fill_manual(
      values = warna,
      drop = FALSE,
      name = "Rentang IPM"
    ) +
    labs(
      title = judul,
      caption = "Sumber: BPS Provinsi Sulawesi Selatan"
    ) +
    theme_tim() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.title = element_text(
        face = "bold",
        size = 11
      ),
      legend.title = element_text(
        face = "bold",
        size = 9
      ),
      legend.text = element_text(size = 8),
      legend.position = "right",
      legend.direction = "vertical",
      legend.key.size = grid::unit(4, "mm"),
      plot.margin = margin(2, 2, 2, 2)
    ) +
    coord_sf(
      datum = NA,
      expand = FALSE
    )
}

# Peta warna asli
peta_ipm_2024 <- buat_peta_ipm_cb(
  data_ipm_2024,
  palet_ipm_cb,
  "Penglihatan normal"
)

# Simulasi deuteranopia
peta_ipm_2024_deutan <- buat_peta_ipm_cb(
  data_ipm_2024,
  colorspace::deutan(palet_ipm_cb),
  "Deuteranopia (simulasi)"
)

# Simulasi protanopia
peta_ipm_2024_protan <- buat_peta_ipm_cb(
  data_ipm_2024,
  colorspace::protan(palet_ipm_cb),
  "Protanopia (simulasi)"
)

# ------------------------------------------------------------
# 11. GABUNGKAN TIGA PETA SECARA HORIZONTAL
# ------------------------------------------------------------

peta_ipm_colorblind_2024 <- (
  peta_ipm_2024 |
    peta_ipm_2024_deutan |
    peta_ipm_2024_protan
) +
  patchwork::plot_layout(
    ncol = 3,
    guides = "keep"
  ) +
  patchwork::plot_annotation(
    title = "Uji Aksesibilitas Warna — IPM Sulawesi Selatan 2024",
    subtitle = paste(
      "Perbandingan warna asli, deuteranopia,",
      "dan protanopia"
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(size = 11)
    )
  )

print(peta_ipm_colorblind_2024)

# ------------------------------------------------------------
# 12. SIMPAN PETA IPM
# ------------------------------------------------------------

# Peta utama tiga tahun
ggsave(
  "keluaran/peta_ipm_2022_2024.png",
  peta_ipm,
  width = 13,
  height = 7,
  dpi = 300,
  bg = "white"
)

# Peta uji aksesibilitas warna khusus tahun 2024
ggsave(
  "keluaran/peta_ipm_colorblind_2024.png",
  peta_ipm_colorblind_2024,
  width = 18,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

cat(
  "Peta IPM 2022-2024 dan uji aksesibilitas warna IPM 2024 selesai.\n"
)

# ------------------------------------------------------------
# 13. Data kemiskinan 2022-2024
# ------------------------------------------------------------

data_kemiskinan <- data_bersih %>%
  filter(tahun %in% 2022:2024) %>%
  select(kode_wilayah, tahun, kemiskinan)

peta_kemiskinan_semua <- peta_sulsel %>%
  left_join(data_kemiskinan, by = "kode_wilayah")

stopifnot(
  nrow(peta_kemiskinan_semua) == 72,
  sum(is.na(peta_kemiskinan_semua$kemiskinan)) == 0
)

# ------------------------------------------------------------
# 14. KATEGORI RENTANG KEMISKINAN
# ------------------------------------------------------------

rentang_kemiskinan <- range(
  data_kemiskinan$kemiskinan,
  na.rm = TRUE
)

batas_kemiskinan <- pretty(
  rentang_kemiskinan,
  n = 5
)

batas_kemiskinan <- sort(unique(batas_kemiskinan))

batas_kemiskinan[1] <- min(
  batas_kemiskinan[1],
  rentang_kemiskinan[1]
)

batas_kemiskinan[length(batas_kemiskinan)] <- max(
  batas_kemiskinan[length(batas_kemiskinan)],
  rentang_kemiskinan[2]
)

label_kemiskinan <- paste0(
  format(
    head(batas_kemiskinan, -1),
    nsmall = 2,
    decimal.mark = ","
  ),
  "–",
  format(
    tail(batas_kemiskinan, -1),
    nsmall = 2,
    decimal.mark = ","
  )
)

# Palet oranye dengan kontras jelas
palet_kemiskinan <- colorspace::sequential_hcl(
  length(label_kemiskinan),
  palette = "Oranges"
)

# ------------------------------------------------------------
# 15. PETA KATEGORI KEMISKINAN 2022-2024
# ------------------------------------------------------------

data_kemiskinan_kategori <- peta_kemiskinan_semua %>%
  mutate(
    kategori_kemiskinan = cut(
      kemiskinan,
      breaks = batas_kemiskinan,
      labels = label_kemiskinan,
      include.lowest = TRUE,
      right = TRUE
    )
  )

peta_kemiskinan <- ggplot(data_kemiskinan_kategori) +
  geom_sf(
    aes(fill = kategori_kemiskinan),
    color = "white",
    linewidth = 0.3
  ) +
  facet_wrap(~tahun, nrow = 1) +
  scale_fill_manual(
    values = palet_kemiskinan,
    drop = FALSE,
    name = "Kemiskinan (%)"
  ) +
  labs(
    title = "Perkembangan Kemiskinan Sulawesi Selatan",
    subtitle = "Persentase penduduk miskin | 2022–2024",
    caption = "Sumber: BPS Provinsi Sulawesi Selatan"
  ) +
  theme_tim() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 12,
      color = "black"
    ),
    legend.position = "right",
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    legend.text = element_text(size = 9),
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8)
  ) +
  coord_sf(datum = NA)

print(peta_kemiskinan)

# ------------------------------------------------------------
# 16. DATA KEMISKINAN TAHUN 2024
# ------------------------------------------------------------

data_kemiskinan_2024 <- peta_kemiskinan_semua %>%
  filter(tahun == 2024)

# ------------------------------------------------------------
# 17. FUNGSI PETA UJI BUTA WARNA KEMISKINAN
# ------------------------------------------------------------

buat_peta_kemiskinan_cb <- function(data, warna, judul) {
  
  data <- data %>%
    mutate(
      kategori_kemiskinan = cut(
        kemiskinan,
        breaks = batas_kemiskinan,
        labels = label_kemiskinan,
        include.lowest = TRUE,
        right = TRUE
      )
    )
  
  ggplot(data) +
    geom_sf(
      aes(fill = kategori_kemiskinan),
      color = "white",
      linewidth = 0.3
    ) +
    scale_fill_manual(
      values = warna,
      drop = FALSE,
      name = "Kemiskinan (%)"
    ) +
    labs(
      title = judul,
      caption = "Sumber: BPS Provinsi Sulawesi Selatan"
    ) +
    theme_tim() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      plot.title = element_text(
        face = "bold",
        size = 11
      ),
      legend.title = element_text(
        face = "bold",
        size = 9
      ),
      legend.text = element_text(size = 8),
      legend.position = "right",
      legend.direction = "vertical",
      legend.key.size = grid::unit(4, "mm"),
      plot.margin = margin(2, 2, 2, 2)
    ) +
    coord_sf(
      datum = NA,
      expand = FALSE
    )
}

# ------------------------------------------------------------
# 18. SIMULASI BUTA WARNA KEMISKINAN 2024
# ------------------------------------------------------------

peta_kemiskinan_2024 <- buat_peta_kemiskinan_cb(
  data_kemiskinan_2024,
  palet_kemiskinan,
  "Penglihatan normal"
)

peta_kemiskinan_deutan <- buat_peta_kemiskinan_cb(
  data_kemiskinan_2024,
  colorspace::deutan(palet_kemiskinan),
  "Deuteranopia (simulasi)"
)

peta_kemiskinan_protan <- buat_peta_kemiskinan_cb(
  data_kemiskinan_2024,
  colorspace::protan(palet_kemiskinan),
  "Protanopia (simulasi)"
)

# ------------------------------------------------------------
# 19. MENGGABUNGKAN PETA UJI AKSESIBILITAS WARNA KEMISKINAN
# ------------------------------------------------------------

peta_kemiskinan_colorblind <- (
  peta_kemiskinan_2024 +
    labs(title = "Penglihatan normal")
) |
  (
    peta_kemiskinan_deutan +
      labs(title = "Deuteranopia (simulasi)")
  ) |
  (
    peta_kemiskinan_protan +
      labs(title = "Protanopia (simulasi)")
  )

peta_kemiskinan_colorblind <-
  peta_kemiskinan_colorblind +
  patchwork::plot_layout(
    ncol = 3,
    guides = "keep"
  ) +
  patchwork::plot_annotation(
    title = "Uji Aksesibilitas Warna — Kemiskinan Sulawesi Selatan 2024",
    subtitle = paste(
      "Perbandingan warna asli, deuteranopia,",
      "dan protanopia"
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(size = 11)
    )
  )

print(peta_kemiskinan_colorblind)

# ============================================================
# 20. MENYIMPAN PETA KEMISKINAN
# ============================================================

ggsave(
  "keluaran/peta_kemiskinan_2022_2024.png",
  peta_kemiskinan,
  width = 13,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  "keluaran/peta_kemiskinan_colorblind_2024.png",
  peta_kemiskinan_colorblind,
  width = 18,
  height = 8,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# 21. PESAN AKHIR
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
  "Jumlah kemiskinan NA:",
  sum(is.na(peta_kemiskinan_2024$kemiskinan)),
  "\n"
)

cat("Output IPM: keluaran/peta_ipm_2022_2024.png\n")
cat("Output uji buta warna IPM: keluaran/peta_ipm_colorblind_2024.png\n")
cat("Output kemiskinan: keluaran/peta_kemiskinan_2022_2024.png\n")
cat("Output uji buta warna kemiskinan: keluaran/peta_kemiskinan_colorblind_2024.png\n")
