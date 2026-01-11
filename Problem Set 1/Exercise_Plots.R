library("tidyverse")

ggplot(mpg, aes(cty, hwy)) + geom_point()
ggsave("0.png")

ggplot(mpg, aes(cty, hwy)) + geom_jitter()
ggsave("1.png")

ggplot(mpg, aes(model, manufacturer)) + geom_point()
ggsave("2.png")

ggplot(mpg, aes(manufacturer)) + geom_bar()
ggsave("3.png")
ggplot(mpg, aes(model)) + geom_bar()
ggsave("4.png")

ggplot(mpg, aes(cty, hwy)) + geom_point()
ggsave("5.png")

ggplot(diamonds, aes(carat, price)) + geom_point()
ggsave("6.png")

ggplot(economics, aes(date, unemploy)) + geom_line()
ggsave("7.png")

ggplot(mpg, aes(cty)) + geom_histogram()
ggsave("8.png")

ggplot(mpg, aes(displ, hwy, shape = drv, color = drv)) + geom_point()
ggsave("color.png")

ggplot(mpg, aes(displ, hwy, color = cty)) + geom_point()
ggsave("9.png")
ggplot(mpg, aes(displ, hwy, size = cty)) + geom_point()
ggsave("10.png")

ggplot(mpg, aes(drv, hwy)) + geom_violin()
ggsave("11.png")

ggplot(mpg, aes(drv, displ)) + geom_violin()
ggsave("12.png")

ggplot(mpg, aes(drv, color = class)) + geom_bar()
ggsave("13.png")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  facet_wrap(~hwy)
ggsave("14.png")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  facet_wrap(~cyl)
ggsave("15.png")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~cyl)
ggsave("16.png")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  facet_wrap(vars(cyl, drv), nrow=4, labeller = "label_both")
ggsave("17.png")

ggplot(mpg, aes(cty, hwy)) + geom_point()
ggsave("18.png")

ggplot(mpg, aes(cty, hwy)) + geom_jitter()
ggsave("19.png")

ggplot(mpg, aes(class, hwy)) + geom_boxplot() 
ggsave("20.png")

ggplot(mpg, aes(reorder(class, hwy), hwy)) + geom_boxplot()
ggsave("21.png")

ggplot(diamonds, aes(carat)) + geom_freqpoly(binwidth = 0.001)
ggsave("22.png")

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggsave("23.png")

ggplot(diamonds, aes(price)) + geom_boxplot()
ggsave("24.png")

ggplot(mpg, aes(model, manufacturer)) + geom_count() # can use count (different sizes at the intersections)
ggsave("25.png")
ggplot(mpg, aes(trans, class)) + geom_count()
ggsave("26.png")
ggplot(mpg, aes(cyl, trans)) + geom_count()
ggsave("27.png")

ggplot(mpg, aes(model, manufacturer)) + geom_tile() # can use this heatmap type thing
ggsave("28.png")
ggplot(mpg, aes(trans, class)) + geom_tile()
ggsave("29.png")
ggplot(mpg, aes(cyl, trans)) + geom_tile()
ggsave("30.png")

ggplot(mpg, aes(model)) + geom_boxplot() + facet_wrap(~manufacturer) # can use faceting
ggsave("31.png")
ggplot(mpg, aes(trans)) + geom_boxplot() + facet_wrap(~class)
ggsave("32.png")
ggplot(mpg, aes(cyl)) + geom_boxplot() + facet_wrap(~trans)
ggsave("33.png")
 