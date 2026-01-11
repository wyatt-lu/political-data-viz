library("tidyverse")

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()

ggsave("figure/mpg_scatterplot.png")

ggplot(mpg, aes(displ, hwy)) +
  geom_point()

ggsave("figure/mpg_scatterplot_alternative.png")

ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()

ggsave("figure/mpg_color.png")

ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = "blue"))

ggsave("figure/mpg_blue.png")

ggplot(mpg, aes(displ, hwy)) + geom_point(colour = "blue")

ggsave("figure/mpg_unscaled_blue.png")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  facet_wrap(~class)

ggsave("figure/mpg_faceted.png")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth()
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

ggsave("figure/mpg_smoothed.png")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(span = 0.2)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

ggsave("figure/mpg_smoothed_wiggly.png")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(span = 1)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

ggsave("figure/mpg_smoothed_less_wiggly.png")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x))

ggsave("figure/mpg_smoothed_mgcv.png")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(method = "lm")
#> `geom_smooth()` using formula = 'y ~ x'

ggsave("figure/mpg_smoothed_linear_model.png")

ggplot(mpg, aes(drv, hwy)) + 
  geom_point()

ggsave("figure/mpg_scatterplot2.png")

ggplot(mpg, aes(drv, hwy)) + geom_jitter()

ggsave("figure/mpg_jitter.png")

ggplot(mpg, aes(drv, hwy)) + geom_boxplot()

ggsave("figure/mpg_boxplot.png")

ggplot(mpg, aes(drv, hwy)) + geom_violin()

ggsave("figure/mpg_violin.png")

ggplot(mpg, aes(hwy)) + geom_histogram()
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

ggsave("figure/mpg_histogram.png")

ggplot(mpg, aes(hwy)) + geom_freqpoly()
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

ggsave("figure/mpg_frequency.png")

ggplot(mpg, aes(hwy)) + 
  geom_freqpoly(binwidth = 2.5)

ggsave("figure/mpg_binwidth1.png")

ggplot(mpg, aes(hwy)) + 
  geom_freqpoly(binwidth = 1)

ggsave("figure/mpg_binwidth2.png")

ggplot(mpg, aes(displ, colour = drv)) + 
  geom_freqpoly(binwidth = 0.5)

ggsave("figure/mpg_binwidth_frequency.png")

ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5) + 
  facet_wrap(~drv, ncol = 1)

ggsave("figure/mpg_binwidth_histogram.png")

ggplot(mpg, aes(manufacturer)) + 
  geom_bar()

ggsave("figure/mpg_barchart.png")

drugs <- data.frame(
  drug = c("a", "b", "c"),
  effect = c(4.2, 9.7, 6.1)
)

ggplot(drugs, aes(drug, effect)) + geom_bar(stat = "identity")

ggsave("figure/drugs_default.png")

ggplot(drugs, aes(drug, effect)) + geom_point()

ggsave("figure/drugs_points.png")

ggplot(economics, aes(date, unemploy / pop)) +
  geom_line()

ggsave("figure/economics.png")

ggplot(economics, aes(date, uempmed)) +
  geom_line()

ggsave("figure/economics2.png")

ggplot(economics, aes(unemploy / pop, uempmed)) + 
  geom_path() +
  geom_point()

ggsave("figure/economics_path.png")

year <- function(x) as.POSIXlt(x)$year + 1900
ggplot(economics, aes(unemploy / pop, uempmed)) + 
  geom_path(colour = "grey50") +
  geom_point(aes(colour = year(date)))

ggsave("figure/economics_path_colored.png")

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 3)

ggsave("figure/mpg_axis.png")

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 3) + 
  xlab("city driving (mpg)") + 
  ylab("highway driving (mpg)")

ggsave("figure/mpg_axis_labels.png")

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 3) + 
  xlab(NULL) + 
  ylab(NULL)

ggsave("figure/mpg_no_labels.png")

ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25)

ggsave("figure/mpg_jitter2.png")

ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25) + 
  xlim("f", "r") + 
  ylim(20, 30)
#> Warning: Removed 138 rows containing missing values or values outside the scale range
#> (`geom_point()`).

ggsave("figure/mpg_jitter2_modified_axis.png")

ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25, na.rm = TRUE) + 
  ylim(NA, 30)

ggsave("figure/mpg_jitter2_continuous_scales.png")

ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_point()

ggsave("figure/mpg_scatterplot3.png")