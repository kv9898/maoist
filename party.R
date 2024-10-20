

toks_party <- party_corp |> corpus() |>
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,
                                            remove_url = TRUE, padding = TRUE) |>
  tokens_remove(stopwords("zh", source="misc"), padding = TRUE)

dfm_party <- toks_party %>% 
  tokens_compound(phrases, concatenator = "") |>
  tokens_compound(tstat_select, concatenator = "") |>
  tokens_ngrams(n = 1:3) |>
  dfm() |> dfm_trim(min_docfreq = 5)

# unsupervised
wca <- textmodel_ca(dfm_all, nd=2) # two dimensions
  #1D
data.frame(Year=docvars(dfm_all)$Year,
           y=wca$rowcoord[,1],
           color=docvars(dfm_all)$type) |>
  ggplot(aes(x=Year,y=y, color=color)) +
  geom_point() + 
  geom_smooth(method="loess")

data.frame(Year=docvars(dfm_all)$Year,
           y=wca$rowcoord[,2],
           color=docvars(dfm_all)$type) |>
  ggplot(aes(x=Year,y=y, color=color)) +
  geom_point() + 
  geom_smooth(method="loess")

  #2D
data.frame(x=wca$rowcoord[,1],
           y=wca$rowcoord[,2],
           color=docvars(dfm_all)$type,
           label=docnames(dfm_all)) |>
  ggplot(aes(x=x,y=y, color=color, label=label)) + 
  geom_point() + geom_text()



#hierarchical clustering
# party
pres_cluster_party <- as.dist(textstat_dist(dfm_all |> dfm_subset(subset= type=="party"), 
                                            method = "euclidean")) |>
  hclust()

# label with document names
pres_cluster_party$labels <- docvars(dfm_all |> dfm_subset(subset= type=="party"))$Year

# plot dendrogram
plot(pres_cluster_party)

#
dfm_all <- corp_all %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,
         remove_url = TRUE, padding = TRUE) |>
  tokens_remove(stopwords("zh", source="misc"), padding = TRUE) %>% 
  tokens_compound(phrases, concatenator = "") |>
  tokens_compound(tstat_select, concatenator = "") |>
  tokens_ngrams(n = 1:3) |>
  dfm()

stm_input_all <- convert(dfm_all, to = "stm")

k_search_output_all <- searchK(stm_input_all$documents, stm_input_all$vocab,
                           K = c(5, 10, 15, 20, 25, 30, 40, 50), data = stm_input_all$meta,
                           verbose = FALSE, heldout.seed = 123)
plot(k_search_output)
