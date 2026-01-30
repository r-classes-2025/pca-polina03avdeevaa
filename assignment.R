# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 


# 1. отберите 6 главных персонажей (по количеству реплик)
# сохраните как символьный вектор
top_speakers <- friends |> 
  filter(!is.na(speaker) & speaker != "") |> 
  group_by(speaker) |> 
  summarise(replics_count = n()) |> 
  arrange(desc(replics_count)) |>  
  head(6) |> 
  pull(speaker) |> 
  as.character()
  
# 2. отфильтруйте топ-спикеров, 
# токенизируйте их реплики, удалите из них цифры
# столбец с токенами должен называться word
# оставьте только столбцы speaker, word
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |>  
  unnest_tokens(word, text) |>  
  filter(!str_detect(word, "[0-9]")) |>  
  select(speaker, word)

# 3. отберите по 500 самых частотных слов для каждого персонажа
# посчитайте относительные частотности для слов
friends_tf <- friends_tokens |>
  count(speaker, word, sort = T) |>  
  ungroup() |>   
  left_join(
    friends_tokens |> 
      group_by(speaker) |> 
      summarise(total = n()),
    by = "speaker"
  ) |>  
  mutate(tf = n / total) |>  
  group_by(speaker) |>  
  slice_max(n, n = 500, with_ties = F) |>  
  ungroup() |> 
  select(speaker, word, tf)

# 4. преобразуйте в широкий формат; 
# столбец c именем спикера превратите в имя ряда, используя подходящую функцию 
friends_tf_wide <- friends_tf |> 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) |>  
  column_to_rownames("speaker") # Так как если не сделать это, то все ломается из-за нечислового столбца

# 5. установите зерно 123
# проведите кластеризацию k-means (k = 3) на относительных значениях частотности (nstart = 20)
# используйте scale()

set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)


# 6. примените к матрице метод главных компонент (prcomp)
# центрируйте и стандартизируйте, использовав аргументы функции
pca_fit <- prcomp(friends_tf_wide, scale. = T, center = T)

# 7. Покажите наблюдения и переменные вместе (биплот)
# в качестве геома используйте текст (=имя персонажа)
# цветом закодируйте кластер, выделенный при помощи k-means
# отберите 20 наиболее значимых переменных (по косинусу, см. документацию к функции)
# сохраните график как переменную q

q <- fviz_pca_biplot(pca_fit, 
                     geom.ind = 'text',
                     geom.var = 'arrow',
                     col.ind = as.factor(km.out$cluster),
                     select.var = list(cos2 = 20),
                     repel = T)


