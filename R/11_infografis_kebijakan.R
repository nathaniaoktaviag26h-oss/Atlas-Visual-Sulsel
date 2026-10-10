# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# 11 - INFOGRAFIS KEBIJAKAN (1 halaman, A4 portrait)
# Untuk  : Bappeda / Dinas
# Isi    : 3 grafik ASLI proyek, 1 pesan utama, ketidakpastian ditampilkan
#          1) Sebaran IPM 2022-2024        (dari R/05_galeri.R, grafik_1)
#          2) Ketidakpastian estimasi IPM  (dari R/09_bootstrap.R, grafik_ipm)
#          3) Hubungan IPM dan kemiskinan  (dari R/05_galeri.R, grafik_3)
# Jalankan dari ROOT proyek (buka Atlas-Visual-Sulsel.Rproj dulu)
# Keluaran: keluaran/infografis.pdf
# ============================================================

library(tidyverse)
library(grid)

source("R/04_theme_palet_tim.R")   # theme_tim() dan palet warna tim

# ============================================================
# 0. UKURAN HALAMAN DAN POSISI (SEMUA DALAM INCI) - ubah di sini
#    left/top = jarak dari pojok kiri-atas halaman
# ============================================================

W <- 8.27     # lebar A4 (inci)
H <- 11.69    # tinggi A4 (inci)
M <- 0.45     # margin kiri-kanan

ukuran <- list(
  header   = list(top = 0,     h = 2.30),
  # baris A: grafik 1 (kiri) dan grafik 2 (kanan)
  judul_A  = list(top = 2.50),
  g1       = list(left = 0.45, top = 3.10, w = 3.00, h = 2.70),
  temuan   = list(left = 0.45, top = 5.90, w = 3.00, h = 1.25),
  g2       = list(left = 3.75, top = 3.10, w = 4.10, h = 4.05),
  # baris B: grafik 3 (lebar penuh)
  judul_B  = list(top = 7.35),
  g3       = list(left = 0.45, top = 7.90, w = 7.37, h = 2.25),
  rekom    = list(top = 10.28, h = 0.80),
  catatan  = list(top = 11.16)
)

# ukuran huruf (pt) - dibuat besar agar jelas dibaca
fs <- list(judul = 21, pesan = 11, panel = 11.5, sub = 9.5,
           grafik = 10, sumbu_wilayah = 9.5, catatan = 8.2)

# ============================================================
# 1. WARNA, FUNGSI BANTU, DAN DATA (kode asli dari repo)
# ============================================================

navy  <- unname(palet_indikator["IPM"])
terra <- unname(palet_dasar["Aksen"])
peach <- palet_pita
abu   <- "#5A5A5A"

koma <- function(x, d = 1)
  format(round(x, d), nsmall = d, decimal.mark = ",")

nama_baku <- function(x) {
  x |>
    str_squish() |>
    str_to_title() |>
    str_replace_all("\\bDan\\b", "dan")
}

nama_pendek <- function(x) {
  x |> nama_baku() |>
    str_replace("Kepulauan Selayar", "Kep. Selayar") |>
    str_replace("Pangkajene dan Kepulauan", "Pangkep")
}

data_bersih <- readRDS("data/data-bersih/atlas_sulsel.rds")

# --- dari R/05_galeri.R ---
data_3tahun <- data_bersih |>
  mutate(kode_wilayah = as.character(kode_wilayah)) |>
  filter(
    str_detect(kode_wilayah, "^73\\d{2}$"),
    tahun %in% 2022:2024
  ) |>
  mutate(
    wilayah = nama_baku(kabupaten_kota),
    kelompok = if_else(
      kode_wilayah %in% c("7371", "7372", "7373"),
      "Kota", "Kabupaten"
    ),
    tahun = factor(tahun, levels = 2022:2024)
  )

stopifnot(
  n_distinct(data_3tahun$kode_wilayah) == 24,
  nrow(data_3tahun) == 72
)

# --- dari R/09_bootstrap.R ---
data_atlas <- data_bersih |>
  filter(tahun %in% 2022:2024) |>
  select(kabupaten_kota, tahun, ipm, kemiskinan) |>
  filter(!is.na(kabupaten_kota))

# ============================================================
# 2. ANGKA KUNCI UNTUK TEKS (dihitung dari data)
# ============================================================

med <- data_3tahun |>
  group_by(tahun) |>
  summarise(m = median(ipm), .groups = "drop")
m22 <- med$m[med$tahun == "2022"]
m24 <- med$m[med$tahun == "2024"]

lebar_data <- data_3tahun |>
  select(kode_wilayah, tahun, ipm, kemiskinan) |>
  pivot_wider(names_from = tahun, values_from = c(ipm, kemiskinan))
n_ipm_naik  <- sum(lebar_data$ipm_2024 > lebar_data$ipm_2022)
n_mis_turun <- sum(lebar_data$kemiskinan_2024 < lebar_data$kemiskinan_2022)

rata24 <- data_3tahun |>
  filter(tahun == "2024") |>
  group_by(kelompok) |>
  summarise(m = mean(ipm), .groups = "drop")
gap24 <- rata24$m[rata24$kelompok == "Kota"] -
  rata24$m[rata24$kelompok == "Kabupaten"]

rata_wil <- data_3tahun |>
  group_by(wilayah) |>
  summarise(r = mean(ipm), .groups = "drop") |>
  arrange(r)
terendah  <- nama_pendek(head(rata_wil$wilayah, 4))
tertinggi <- tail(rata_wil$wilayah, 1)

# ============================================================
# 3. GRAFIK 1 - SEBARAN IPM (kode asli grafik_1, tanpa judul)
# ============================================================

g1 <- ggplot(
  data_3tahun,
  aes(tahun, ipm, fill = tahun)
) +
  geom_boxplot(
    width = 0.55,
    alpha = 0.30,
    colour = palet_dasar["Gelap"],
    linewidth = 0.4,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(fill = tahun),
    shape = 21,
    colour = palet_dasar["Gelap"],
    width = 0.08,
    height = 0,
    size = 2.3,
    stroke = 0.6,
    alpha = 0.9,
    seed = 2026
  ) +
  scale_fill_manual(values = palet_tahun) +
  labs(x = NULL, y = "Indeks Pembangunan Manusia") +
  theme_tim(base_size = fs$grafik) +
  theme(legend.position = "none",
        plot.margin = margin(4, 8, 4, 4))

# ============================================================
# 4. GRAFIK 2 - KETIDAKPASTIAN ESTIMASI IPM
#    (stat_sk_bootstrap() dan grafik_ipm asli dari R/09_bootstrap.R)
# ============================================================

StatSKBootstrap <- ggproto(
  "StatSKBootstrap",
  Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales,
                           B = 1999,
                           conf = 0.95,
                           seed = 123) {
    
    data <- data[
      is.finite(data$x) & is.finite(data$y),
      ,
      drop = FALSE
    ]
    
    if (nrow(data) < 2) {
      return(data.frame())
    }
    
    if (B < 1999) stop("B minimal 1999.")
    if (conf <= 0 || conf >= 1) stop("conf harus antara 0 dan 1.")
    
    set.seed(seed)
    
    nilai <- data$y
    n <- length(nilai)
    
    hasil_boot <- replicate(
      B,
      mean(sample(nilai, size = n, replace = TRUE))
    )
    
    alpha <- 1 - conf
    
    data.frame(
      x = mean(data$x),
      y = mean(nilai),
      ymin = unname(quantile(hasil_boot, alpha / 2)),
      ymax = unname(quantile(hasil_boot, 1 - alpha / 2)),
      n = n
    )
  }
)

stat_sk_bootstrap <- function(mapping = NULL,
                              data = NULL,
                              geom = "pointrange",
                              position = "identity",
                              ...,
                              B = 1999,
                              conf = 0.95,
                              seed = 123,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  
  layer(
    stat = StatSKBootstrap,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      B = B,
      conf = conf,
      seed = seed,
      na.rm = na.rm,
      ...
    )
  )
}

urutan_ipm <- data_atlas |>
  group_by(kabupaten_kota) |>
  summarise(rataan = mean(ipm), .groups = "drop") |>
  arrange(rataan) |>
  pull(kabupaten_kota)

data_grafik_ipm <- data_atlas |>
  mutate(
    kabupaten_kota = factor(
      kabupaten_kota,
      levels = urutan_ipm
    ),
    urutan = as.numeric(kabupaten_kota)
  )

g2 <- ggplot(
  data_grafik_ipm,
  aes(x = urutan, y = ipm, group = kabupaten_kota)
) +
  stat_sk_bootstrap(
    B = 1999,
    conf = 0.95,
    seed = 123,
    colour = unname(palet_indikator["IPM"]),
    linewidth = 0.8,
    fatten = 2.8
  ) +
  scale_x_continuous(
    breaks = seq_along(urutan_ipm),
    labels = nama_pendek(urutan_ipm),
    expand = expansion(add = 0.5)
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Rata-rata IPM 2022\u20132024"
  ) +
  theme_tim(base_size = fs$grafik) +
  theme(
    axis.text.y = element_text(size = fs$sumbu_wilayah),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(4, 8, 4, 2)
  )

# ============================================================
# 5. GRAFIK 3 - HUBUNGAN IPM DAN KEMISKINAN (kode asli grafik_3)
# ============================================================

g3 <- ggplot(
  data_3tahun,
  aes(ipm, kemiskinan)
) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    level = 0.95,
    colour = palet_indikator["Kemiskinan"],
    fill = palet_pita,
    alpha = 0.75,
    linewidth = 1
  ) +
  geom_point(
    aes(fill = kelompok, shape = kelompok),
    colour = palet_dasar["Gelap"],
    size = 2.3,
    stroke = 0.5,
    alpha = 0.9
  ) +
  scale_fill_manual(
    values = c(
      "Kabupaten" = "#5B8DB8",
      "Kota" = "#E69F5B"
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      "Kabupaten" = 21,
      "Kota" = 24
    ),
    name = NULL
  ) +
  facet_wrap(~tahun, nrow = 1) +
  labs(
    x = "Indeks Pembangunan Manusia",
    y = "Penduduk miskin (%)"
  ) +
  theme_tim(base_size = fs$grafik) +
  theme(
    legend.text = element_text(size = fs$grafik - 1),
    strip.text = element_text(face = "bold", size = fs$grafik),
    plot.margin = margin(2, 8, 2, 4)
  )

# ============================================================
# 6. PEMBANTU MENGGAMBAR DI HALAMAN (satuan inci dari kiri-atas)
# ============================================================

xin <- function(x) unit(x, "in")
yin <- function(top) unit(H - top, "in")

kotak <- function(left, top, w, h, fill) {
  grid.rect(x = xin(left), y = yin(top), width = xin(w), height = xin(h),
            just = c("left", "top"), gp = gpar(fill = fill, col = NA))
}

teks <- function(left, top, label, size, col = "grey10",
                 face = "plain", lineheight = 1.25) {
  grid.text(label, x = xin(left), y = yin(top), just = c("left", "top"),
            gp = gpar(fontsize = size, col = col, fontface = face,
                      lineheight = lineheight))
}

tempel <- function(p, u) {
  print(p, vp = viewport(x = xin(u$left), y = yin(u$top),
                         width = xin(u$w), height = xin(u$h),
                         just = c("left", "top")))
}

judul_panel <- function(left, top, no, judul, sub) {
  grid.circle(x = xin(left + 0.14), y = yin(top + 0.15), r = xin(0.14),
              gp = gpar(fill = terra, col = NA))
  grid.text(no, x = xin(left + 0.14), y = yin(top + 0.15),
            gp = gpar(fontsize = 11, col = "white", fontface = "bold"))
  teks(left + 0.40, top, judul, fs$panel, face = "bold")
  teks(left + 0.40, top + 0.26, sub, fs$sub, col = abu)
}

# ============================================================
# 7. SUSUN HALAMAN
# ============================================================

gambar_halaman <- function() {
  grid.newpage()
  
  # --- header ---
  kotak(0, 0, W, ukuran$header$h, navy)
  kotak(0, ukuran$header$h, W, 0.07, terra)
  teks(M, 0.36, "INFOGRAFIS KEBIJAKAN   \u2022   BAPPEDA & DINAS   \u2022   SULAWESI SELATAN",
       9.5, col = peach, face = "bold")
  teks(M, 0.70, "Pembangunan manusia Sulsel naik,", fs$judul, col = "white", face = "bold")
  teks(M, 1.10, "tetapi kesenjangan antarwilayah masih lebar", fs$judul, col = "white", face = "bold")
  teks(M, 1.58,
       paste0("Pesan utama: IPM naik di ", n_ipm_naik, " dari 24 kabupaten/kota (2022\u20132024) dan\n",
              "kemiskinan turun di ", n_mis_turun, " dari 24, tetapi jarak IPM Kota\u2013Kabupaten\n",
              "masih ", koma(gap24, 1), " poin dan wilayah ber-IPM rendah cenderung lebih miskin."),
       fs$pesan, col = "white", lineheight = 1.35)
  
  # --- baris A ---
  judul_panel(M, ukuran$judul_A$top, "1", "IPM bergeser naik",
              "Sebaran IPM 24 kab/kota, 2022\u20132024")
  tempel(g1, ukuran$g1)
  
  u <- ukuran$temuan
  kotak(u$left, u$top, u$w, u$h, "#F3F6F9")
  kotak(u$left, u$top, 0.06, u$h, terra)
  teks(u$left + 0.18, u$top + 0.10, "Temuan", 10.5, col = terra, face = "bold")
  teks(u$left + 0.18, u$top + 0.36,
       paste0("Median IPM naik dari ", koma(m22), " (2022)\n",
              "ke ", koma(m24), " (2024).\n",
              "Tertinggi: ", tertinggi, ".\n",
              "Terendah: ", nama_pendek(head(rata_wil$wilayah, 1)), "."),
       10, lineheight = 1.2)
  
  judul_panel(3.75, ukuran$judul_A$top, "2", "Ketidakpastian estimasi IPM",
              "Rata-rata 2022\u20132024 | bootstrap 95%")
  tempel(g2, ukuran$g2)
  
  # --- baris B ---
  judul_panel(M, ukuran$judul_B$top, "3",
              "IPM lebih tinggi, kemiskinan cenderung lebih rendah",
              "Garis = regresi linear; pita = selang kepercayaan 95%")
  tempel(g3, ukuran$g3)
  
  # --- rekomendasi ---
  u <- ukuran$rekom
  kotak(M, u$top, W - 2 * M, u$h, peach)
  teks(M + 0.17, u$top + 0.10, "Rekomendasi:", 10.5, col = terra, face = "bold")
  teks(M + 1.43, u$top + 0.10,
       paste0("Prioritaskan percepatan di wilayah ber-IPM terendah\n(",
              paste(terendah, collapse = ", "), "),\n",
              "dan pantau dengan interval, bukan angka tunggal."),
       10.5)
  
  # --- catatan ketidakpastian dan sumber ---
  teks(M, ukuran$catatan$top,
       paste0("Ketidakpastian: grafik 2 = interval bootstrap 95% (B = 1.999) antar tahun; hanya 3 tahun per wilayah,\n",
              "jadi interval kasar: baca sebagai rentang, bukan estimasi presisi. Pita grafik 3 = selang kepercayaan\n",
              "95% regresi linear. Korelasi bukan sebab-akibat.   Sumber: BPS Provinsi Sulawesi Selatan; olahan tim 1."),
       fs$catatan, col = abu, lineheight = 1.35)
}

# ============================================================
# 8. SIMPAN
# ============================================================

dir.create("keluaran", showWarnings = FALSE)

cairo_pdf("keluaran/infografis.pdf", width = W, height = H)
gambar_halaman()
dev.off()

message("Selesai: keluaran/infografis.pdf")
