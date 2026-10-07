# ============================================================
# ATLAS VISUAL SULAWESI SELATAN
# CP2 - theme_tim() dan palet warna
# Dasar: ggplot2::theme_minimal(), dimodifikasi (konfirmasi ke dosen)
# ============================================================
library(ggplot2)

theme_tim <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      # latar dan kisi
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      
      # judul, subjudul, sumber
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", size = base_size + 4, colour = "grey10",
                                   hjust = 0, margin = margin(b = 5)),
      plot.subtitle = element_text(size = base_size, colour = "grey35",
                                   hjust = 0, margin = margin(b = 10)),
      plot.caption  = element_text(size = base_size - 2, colour = "grey40",
                                   hjust = 0, margin = margin(t = 10)),
      # sumbu
      axis.title = element_text(size = base_size, colour = "grey20"),
      axis.text  = element_text(size = base_size - 1, colour = "grey20"),
      # legenda
      legend.position      = "top",
      legend.justification = "left",
      legend.title      = element_text(face = "bold", size = base_size - 1),
      legend.text       = element_text(size = base_size - 2),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      # facet
      strip.text       = element_text(face = "bold", size = base_size - 1,
                                      colour = "grey20", hjust = 0),
      strip.background = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
}
# ------------------------------------------------------------
# PALET WARNA (satu warna = satu makna di semua grafik)
# ------------------------------------------------------------
# ------------------------------------------------------------
# PALET WARNA
# Setiap palet memiliki warna berbeda
# ------------------------------------------------------------

palet_indikator <- c(
  "IPM"        = "#1F4E79",  # Biru
  "Kemiskinan" = "#D95F02"   # Oranye
)

palet_jenis <- c(
  "Kabupaten" = "#5B8DB8",
  "Kota"      = "#E69F5B"
)

palet_tahun <- c(
  "2022" = "#9ECAE1",
  "2023" = "#4292C6",
  "2024" = "#08519C"
)

palet_dasar <- c(
  "Netral" = "#BDBDBD",       # Abu-abu
  "Gelap"  = "#333333",       # Charcoal
  "Aksen"  = "#C45A3C"        # Terracotta
)

palet_pita <- "#F2D6C2"        # Peach
