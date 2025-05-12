# jenny trying to get social caption to work
# inspo https://nrennie.rbind.io/blog/adding-social-media-icons-ggplot2/



font_add('fa-reg', 'fonts/Font Awesome 5 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 5 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 5 Free-Solid-900.otf')


github_icon <- "&#xf092"
github_username <- "jenrichmond"
linkedin_icon <- "&#xf08c"
linkedin_username <- "jenrichmondPhD"

social_caption <- glue::glue(
  "<span style='font-family:\"fa-brands\";'>{github_icon};</span>
  <span style='color: #E30B5C'>{github_username}</span>" ,
  
  "<span style='font-family:\"fa-brands\";'>{linkedin_icon};</span>
  <span style='color: #E30B5C'>{linkedin_username}</span>"
)




count / mag +
  plot_annotation(title = "The frequency and magnitude of seismic events at Mt Vesuvius",
                  subtitle = "Pressure is released either in many small events or fewer larger events",
                  caption = social_caption,
                  theme = theme_stripe_pw())
