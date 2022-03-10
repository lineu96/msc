
#-----------------------------------------------------------------

# Análise dos resultados

#-----------------------------------------------------------------

library(ggplot2)

#-----------------------------------------------------------------

time_taken_uni
time_taken_tri
time_taken_tri2

#-----------------------------------------------------------------

normal_uni_n50$df_final
poisson_uni_n50$df_final
binomial_uni_n50$df_final

normal_uni_n100$df_final
poisson_uni_n100$df_final
binomial_uni_n100$df_final

normal_uni_n250$df_final
poisson_uni_n250$df_final
binomial_uni_n250$df_final

normal_uni_n500$df_final
poisson_uni_n500$df_final
binomial_uni_n500$df_final

normal_uni_n1000$df_final
poisson_uni_n1000$df_final
binomial_uni_n1000$df_final

normal_tri_n50$df_final
poisson_tri_n50$df_final
binomial_tri_n50$df_final

normal_tri_n100$df_final
poisson_tri_n100$df_final
binomial_tri_n100$df_final

normal_tri_n250$df_final
poisson_tri_n250$df_final
binomial_tri_n250$df_final

normal_tri_n500$df_final
poisson_tri_n500$df_final
binomial_tri_n500$df_final

normal_tri_n1000$df_final
poisson_tri_n1000$df_final
binomial_tri_n1000$df_final

tri_n50$df_final
tri_n100$df_final
tri_n250$df_final
tri_n500$df_final
tri_n1000$df_final

#-----------------------------------------------------------------

results1 <- rbind(normal_uni_n50$df_final,
                  poisson_uni_n50$df_final,
                  binomial_uni_n50$df_final,
                  
                  normal_uni_n100$df_final,
                  poisson_uni_n100$df_final,
                  binomial_uni_n100$df_final,
                  
                  normal_uni_n250$df_final,
                  poisson_uni_n250$df_final,
                  binomial_uni_n250$df_final,
                  
                  normal_uni_n500$df_final,
                  poisson_uni_n500$df_final,
                  binomial_uni_n500$df_final,
                  
                  normal_uni_n1000$df_final,
                  poisson_uni_n1000$df_final,
                  binomial_uni_n1000$df_final,
                  
                  normal_tri_n50$df_final,
                  poisson_tri_n50$df_final,
                  #binomial_tri_n50$df_final,
                  
                  normal_tri_n100$df_final,
                  poisson_tri_n100$df_final,
                  #binomial_tri_n100$df_final,
                  
                  normal_tri_n250$df_final,
                  poisson_tri_n250$df_final,
                  #binomial_tri_n250$df_final,
                  
                  normal_tri_n500$df_final,
                  #poisson_tri_n500$df_final,
                  #binomial_tri_n500$df_final,
                  
                  normal_tri_n1000$df_final#,
                  #poisson_tri_n1000$df_final,
                  #binomial_tri_n1000$df_final,
                  
                  #tri_n50$df_final,
                  #tri_n100$df_final,
                  #tri_n250$df_final,
                  #tri_n500$df_final,
                  #tri_n1000$df_final
)

#-----------------------------------------------------------------

results1$sample_size <- as.factor(results1$sample_size)
names(results1)[4] <- "Tamanho amostral"
results1$distribution <- factor(results1$distribution,
                                levels = c("uni normal", "uni poisson", 
                                           "uni binomial", "tri normal", 
                                           "tri poisson", "tri binomial",
                                           "normal/poisson/binomial"))



#-----------------------------------------------------------------

ggplot2::ggplot(data = results1,
                mapping = aes(x = dist, 
                              y = rej, 
                              col = `Tamanho amostral`)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~distribution)+
  ylab("% rejeição") +
  xlab("Distância") +
  ggtitle("")+ 
  theme_bw() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(family = "Helvetica", 
                                  face = "bold", 
                                  size = (15),
                                  hjust = 0.5),
        axis.title = element_text(face = "bold",
                                  size = 10),
        text = element_text(size=10)) 

#-----------------------------------------------------------------
