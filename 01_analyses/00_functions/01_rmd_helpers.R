
if(!exists("rmd_helpers_executed")){
  rmd_helpers_executed = TRUE
  
  # better table formatting
  make_table <- function(df, ali = "left", aw = 0.5){
    t <- regulartable(data = df)
    t <- fontsize(t, size = 18, part = "all")
    t <- font(t, fontname = "Arial", part = "all")
    t <- autofit(t, add_w = aw)
    t <- align(t, align = ali, part = "all")
    t <- theme_zebra(t)
    t <- border(t, border = fp_border(), part = "all") 
    return(t)
  }
  
  
  # apa themed plots
  # font_import()
  # loadfonts(device = "win")
  # fonts()
  apatheme <- theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(family = "Arial", size = 14)
    )
}


percentify <- function(number){
  
  return(round(number, 4)*100)
}
