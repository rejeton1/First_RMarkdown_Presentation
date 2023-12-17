---
title: "Untitled"
author: "sw"
date: "2023-12-17"
output: ioslides_presentation
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## My first slide!
### This is sub title1
#### This is sub title2

***
- Bullet list1
- Bullet list2

1. ordered list1
2. ordered list2

*This is Italic*

**And this is Bold**

'x <- head(data)'

```{r, comment=''}
head(mtcars)
```


## Plot Slide

```{r, fig.align='center', fig.cap="My First Plot"}
plot(mtcars$wt, mtcars$mpg, xlab='Weight', ylab='Miles Per Gallon')
```

