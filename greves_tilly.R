greves = read.csv("data_greves.csv",sep=";")
greves[] = sapply(greves,as.numeric)
library(plotly)
library(pracma)
library(ggrepel)
library(ggplot2)
library(dplyr)
library(rallicagram)
library(ngramr)
library(scales)

from=1945
to=2004
var = "Disputes"
corpora = c("presse","livres","journal_des_debats","figaro",
            "temps","petit_journal","petit_parisien","huma","ngram viewer")
corpora_clean = c("Presse/Gallica","Livres/Gallica","Le Journal des Débats","Le Figaro",
                  "Le Temps","Le Petit Journal","Le Petit Parisien","L'Humanité","Ngram Viewer")
names(corpora_clean) = corpora
fit = vector()
total = vector()
for(corpus in corpora){
  if(corpus=="ngram viewer"){
    frequency = ngram("grève+grèves",corpus = 'fr-2019',
                      smoothing=0,year_start = from,year_end = to,count=TRUE)
    frequency = data.frame(frequency)
    frequency$year = frequency$Year
    frequency$prop_occur = frequency$Frequency
    total[corpus] = sum(frequency$Count/frequency$Frequency)
    
  }
  else{
    frequency = gallicagram_lexicon(lexicon = c("grève","grèves"),corpus = corpus,
                                resolution = "yearly",from = from,to=to)
    total[corpus] = sum(frequency$n_total)
  }
  data = left_join(greves[greves$year %in% from:to,],frequency,by="year")
  data$var = data[var]
  data$var = as.numeric(scale(log(data$var+1))) 
  z = !is.na(data$var)
  data$var[z] = detrend(data$var[z])
  data$prop_occur = as.numeric(scale(log(data$prop_occur)))
  z = !is.na(data$prop_occur)
  data$prop_occur[z] = detrend(data$prop_occur[z])
  c = cor.test(as.numeric(data$var),data$prop_occur)
  fit[corpus] = c$estimate
  print(c)
  print(corpus)
  plot = plot_ly(data=data,x=~year,y=~prop_occur,
                 type="scatter",mode="lines",
                 name = "Fréquence dans le corpus")
  plot = plot %>% add_lines(x=~year,y=~var,name="Grèves (selon Tilly, 1971)")
  print(plot %>% layout(title=corpora_clean[corpus],
                        yaxis = list(title = "Valeur normalisée"),
                        xaxis = list(title = "Année"),
                        legend = list(x=.05,y=.9)))
}
names(fit) = corpora_clean
ggplot(data.frame(corpus= factor(corpora_clean, 
                                          levels=names(sort(fit))),
                  total = total,fit = fit),aes(sign(fit)*fit^2,corpus)) + geom_bar(stat="identity") + 
  theme_minimal() + xlab(bquote(R^2)) + ylab("Corpus")

scientific_10 = function(x) {
  ifelse(
    x==0, "0",
    parse(text = sub("e[+]?", " %*% 10^", scientific_format()(x)))
  )
}
ggplot(data.frame(corpus= factor(corpora_clean, 
                                 levels=names(sort(fit))),
                  total = total,fit = fit),aes(total,corpus)) + geom_bar(stat="identity") + 
  scale_x_continuous(labels =scientific_10) + theme_minimal()


ggplot(data.frame(corpus= corpora_clean
                  ,total = total,fit = fit),aes(log10(total),fit^2)) +
  geom_point() + geom_text_repel(aes(label = corpus)) +
  scale_x_continuous(labels = function(x){return(parse(text=paste("10^", x)))}) + 
  xlab("Nombre de mots dans le corpus (log)") + ylab(bquote(R^2)) + theme_minimal() + 
  ggtitle("Qualité du modèle 'grèves (Tilly)' ~ 'occurrences de grève(s) selon le corpus") 
 
