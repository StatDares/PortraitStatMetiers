
#  ------------------------------------------------------------------------
#
# Title : Bootstrap theme DARES
#    By : dreamRs
#  Date : 2020-01-21
#
#  ------------------------------------------------------------------------



library(fresh)

create_theme(
  theme = "default",
  bs_vars_color(brand_primary = "#88cac0"),
  bs_vars_global(body_bg = "#FFFFFF", text_color = "#000000"),
  bs_vars_font(family_sans_serif = "Arial, sans-serif", size_base = "14px"),
  bs_vars_navbar(
    height = "60px",
    default_bg = "#FFF",
    default_color = "#333333",
    default_link_color = "#333333",
    default_link_active_color = "#FFF",
    default_link_active_bg = "#2d378c",
    default_link_hover_color = "#2d378c"
  ),
  bs_vars_button(
    font_weight = "bold",
    border_radius_base = 0,
    default_color = "#88cac0",
    default_bg = "#FFF",
    default_border = "#88cac0",
    primary_bg = "#88cac0",
    primary_border = "#88cac0",
    primary_color = "#FFFFFF"
  ),
  bs_vars_wells(
    bg = "#FFF",
    border = "#435363"
  ),
  bs_vars_panel(default_border = "#435363"),
  bs_vars_modal(lg = "80%"),
  output_file = "www/css/theme-dares.min.css"
)
