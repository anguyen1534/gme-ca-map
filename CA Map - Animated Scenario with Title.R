# =============================================================================
# CA Residency Programs – Animated Dot Map
# =============================================================================
# install.packages(c("readxl", "tidyverse", "tidygeocoder", "ggplot2",
#                    "maps", "showtext", "sysfonts", "gganimate", "gifski", "magick"))
# =============================================================================

library(readxl)
library(tidyverse)
library(tidygeocoder)
library(ggplot2)
library(maps)
library(showtext)
library(sysfonts)
library(gganimate)
library(gifski)
library(magick)

# Load Montserrat from Google Fonts
font_add_google("Montserrat", "montserrat")
showtext_auto()
showtext_opts(dpi = 96)

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------
df <- read_excel("C:/Users/anguyen/Downloads/rstudio scripts/scenario.programs.xlsx",
                 sheet = "Scenario - Programs List")

# Column order: Discipline, Program_Name, Grant_ID, Address
colnames(df) <- c("Discipline", "Program_Name", "Grant_ID", "Address")

discipline_labels <- c(
  "FM"   = "Family Medicine",
  "IM"   = "Internal Medicine",
  "Peds" = "Pediatrics",
  "OB"   = "OB/GYN",
  "EM"   = "Emergency Med."
)

df <- df %>%
  mutate(Discipline_Label = recode(Discipline, !!!discipline_labels))

# -----------------------------------------------------------------------------
# FIX KNOWN BAD ADDRESSES BEFORE GEOCODING
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(Address = case_when(
    str_detect(Address, "San Deigo") ~
      str_replace(Address, "San Deigo", "San Diego"),
    str_detect(Address, "2051 Los Angeles, California") ~
      "2051 Marengo Street, Los Angeles, CA 90033",
    str_detect(Address, "3301 Suite 102, Atherton") ~
      "3301 El Camino Real, Suite 102, Atherton, CA 94027",
    TRUE ~ Address
  ))

# -----------------------------------------------------------------------------
# 2. GEOCODE
# -----------------------------------------------------------------------------
cat("\nGeocoding addresses... please wait.\n")

df <- df %>% mutate(row_id = row_number())

geocoded <- df %>%
  geocode(
    address      = Address,
    method       = "census",
    lat          = latitude,
    long         = longitude,
    full_results = FALSE
  )

n_ok   <- sum(!is.na(geocoded$latitude))
n_miss <- sum( is.na(geocoded$latitude))
cat(sprintf("\nGeocoded: %d succeeded, %d failed.\n", n_ok, n_miss))

if (n_miss > 0) {
  cat("Retrying failed rows with OpenStreetMap...\n")
  failed  <- geocoded %>% filter(is.na(latitude)) %>% select(-latitude, -longitude)
  retried <- failed %>%
    geocode(address = Address, method = "osm",
            lat = latitude, long = longitude, full_results = FALSE)
  geocoded <- geocoded %>%
    rows_update(retried %>% select(row_id, latitude, longitude), by = "row_id")
  cat(sprintf("After retry: %d succeeded, %d still failed.\n",
              sum(!is.na(geocoded$latitude)), sum(is.na(geocoded$latitude))))
}

# -----------------------------------------------------------------------------
# MANUAL COORDINATE OVERRIDES
# -----------------------------------------------------------------------------
manual_coords <- tribble(
  ~Program_Name,                                                                                                     ~lat,     ~lng,
  "Stanford Pediatrics Residency Program",                                                                           37.4267, -122.1735,
  "Scripps Mercy Hospital Program Internal Medicine Residency Program",                                              32.7460, -117.1611,
  "Sutter Santa Rosa Regional Hospital/University of California (San Francisco) Family Medicine Residency Program",  38.4404, -122.7141,
  "University of California (San Francisco)/Fresno Family Medicine Residency Program",                               36.7396, -119.7886,
  "University of California (San Francisco)/Fresno Internal Medicine Residency Program",                             36.7396, -119.7886,
  "University of California (San Francisco)/Fresno Pediatric Residency Program",                                     36.7396, -119.7886,
  "University of California (San Francisco)/Fresno Obstetrics and Gynecology Residency Program",                     36.7396, -119.7886
)

for (i in seq_len(nrow(manual_coords))) {
  prog  <- manual_coords$Program_Name[i]
  match <- geocoded$Program_Name == prog & is.na(geocoded$latitude)
  geocoded$latitude[match]  <- manual_coords$lat[i]
  geocoded$longitude[match] <- manual_coords$lng[i]
}

# Save geocoded table
write_csv(geocoded, "C:/Users/anguyen/Downloads/rstudio scripts/geocoded_scenario_programs.csv")

# Show failures
still_missing <- geocoded %>% filter(is.na(latitude))
if (nrow(still_missing) > 0) {
  cat("\nStill missing:\n")
  writeLines(paste(still_missing$Discipline, "|", still_missing$Program_Name, "|", still_missing$Address))
  write_csv(still_missing %>% select(Discipline, Program_Name, Address),
            "C:/Users/anguyen/Downloads/rstudio scripts/FAILED_scenario_geocodes.csv")
}

# -----------------------------------------------------------------------------
# 3. FILTER TO VALID CA COORDINATES
# -----------------------------------------------------------------------------
mapped <- geocoded %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  filter(latitude  >= 32.4, latitude  <= 42.1,
         longitude >= -124.5, longitude <= -114.0)

cat(sprintf("\n%d programs will appear on the map.\n", nrow(mapped)))

# -----------------------------------------------------------------------------
# 4. ADD ANIMATION ORDER
# -----------------------------------------------------------------------------
disc_order <- c("Family Medicine", "Internal Medicine", "Pediatrics",
                "OB/GYN", "Emergency Med.")

mapped <- mapped %>%
  mutate(Discipline_Label = factor(Discipline_Label, levels = disc_order)) %>%
  arrange(Discipline_Label) %>%
  mutate(reveal_order = row_number())

n_dots <- nrow(mapped)

# Shared theme
map_theme <- theme_minimal(base_size = 8) +
  theme(
    text              = element_text(family = "montserrat"),
    plot.title        = element_text(family = "montserrat", face = "bold",
                                     size = 14, hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle     = element_text(family = "montserrat", hjust = 0.5,
                                     colour = "grey40", size = 8, margin = margin(b = 4)),
    plot.caption      = element_text(family = "montserrat", colour = "grey60", size = 5),
    legend.position   = "bottom", legend.direction = "horizontal",
    legend.box        = "horizontal", legend.margin = margin(4, 0, 0, 0),
    legend.background = element_rect(fill = "white", colour = "grey80", linewidth = 0.3),
    legend.key.size   = unit(0.35, "cm"),
    legend.text       = element_text(family = "montserrat", size = 7),
    legend.title      = element_text(family = "montserrat", face = "bold", size = 8),
    panel.grid        = element_blank(), axis.text = element_blank()
  )

# -----------------------------------------------------------------------------
# 5. BUILD MAPS
# -----------------------------------------------------------------------------
ca_counties <- map_data("county") %>% filter(region == "california")
ca_state    <- map_data("state")  %>% filter(region == "california")

pal <- c(
  "Family Medicine"  = "#10C637",
  "Internal Medicine"= "#042B46",
  "Pediatrics"       = "#FF982C",
  "OB/GYN"           = "#18C4D6",
  "Emergency Med."   = "#9C8FC4"
)

base_layers <- list(
  geom_polygon(data = ca_counties,
               aes(x = long, y = lat, group = group),
               fill = "#f0f0eb", colour = "#555555", linewidth = 0.8),
  geom_polygon(data = ca_state,
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "#333333", linewidth = 0.9),
  scale_colour_manual(values = pal, name = NULL, drop = FALSE),
  guides(colour = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 6))),
  coord_fixed(ratio = 1.3, xlim = c(-124.5, -114.0), ylim = c(32.4, 42.1)),
  labs(title   = "Programs Across CA",
       subtitle = sprintf("%d Total Programs  |  FM · IM · Peds · OB · EM", nrow(mapped)),
       caption  = "Source: Program list provided",
       x = NULL, y = NULL),
  map_theme
)

# --- Phase 1: animated dots ---
p_dots <- ggplot() +
  base_layers +
  geom_point(data = mapped,
             aes(x = longitude, y = latitude,
                 colour = Discipline_Label, group = reveal_order),
             size = 6.5, alpha = 0.85) +
  transition_reveal(reveal_order) +
  shadow_mark(alpha = 0.85, size = 4.5)

# --- Phase 2: static final frame with text outside the map (top right via plot.tag) ---
p_final <- ggplot() +
  base_layers +
  geom_point(data = mapped,
             aes(x = longitude, y = latitude, colour = Discipline_Label),
             size = 4.5, alpha = 0.85) +
  labs(title   = "Programs Across CA",
       subtitle = sprintf("%d Total Programs  |  FM · IM · Peds · OB · EM", nrow(mapped)),
       caption  = "Source: Program list provided",
       tag      = "31 Additional\nPrograms Funded",
       x = NULL, y = NULL) +
  theme(plot.tag          = element_text(family = "montserrat", face = "bold",
                                         size = 30, colour = "#042B46",
                                         hjust = 0.5, vjust = 0.5,
                                         lineheight = 1.3),
        plot.tag.position = c(0.80, 0.70))

# -----------------------------------------------------------------------------
# 6. RENDER & STITCH
# -----------------------------------------------------------------------------
cat("\nRendering dot animation...\n")
gif1 <- animate(
  p_dots,
  nframes  = n_dots + 15,
  fps      = 8,
  width    = 1000,
  height   = 1100,
  renderer = gifski_renderer()
)

# Save static final frame as temp PNG then repeat as GIF frames using magick
cat("Rendering final frame with text...\n")
tmp_png <- tempfile(fileext = ".png")
ggsave(tmp_png, plot = p_final, width = 1000/96, height = 1100/96, dpi = 96, bg = "white")

# Read PNG and repeat it for 40 frames (5 seconds at 8fps)
frame_img  <- magick::image_read(tmp_png)
text_frames <- magick::image_resize(
  do.call(c, replicate(40, frame_img, simplify = FALSE)),
  "1000x1100"
)

cat("Stitching together...\n")
mgif1  <- magick::image_read(gif1)
final  <- c(mgif1, text_frames)

magick::image_write_gif(
  final,
  path  = "C:/Users/anguyen/Downloads/rstudio scripts/ca_residency_programs_map_scenario.gif",
  delay = 1/8
)
cat("Animated GIF saved!\n")

# --- Static PNG ---
ggsave("C:/Users/anguyen/Downloads/rstudio scripts/ca_residency_programs_map_scenario.png",
       plot = p_final, width = 10, height = 11, dpi = 300, bg = "white")
cat("Static PNG saved!\n")
