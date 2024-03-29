
#-----------------------------------------------------------------

# Análise dos resultados

#-----------------------------------------------------------------

library(ggplot2)

load("~/msc/3_estudo_simulacao/betas/results.RData")

#-----------------------------------------------------------------

normal_uni_n50$df_final
poisson_uni_n50$df_final
bernoulli_uni_n50$df_final

normal_uni_n100$df_final
poisson_uni_n100$df_final
bernoulli_uni_n100$df_final

normal_uni_n250$df_final
poisson_uni_n250$df_final
bernoulli_uni_n250$df_final

normal_uni_n500$df_final
poisson_uni_n500$df_final
bernoulli_uni_n500$df_final

normal_uni_n1000$df_final
poisson_uni_n1000$df_final
bernoulli_uni_n1000$df_final

normal_tri_n50$df_final
poisson_tri_n50$df_final
bernoulli_tri_n50$df_final

normal_tri_n100$df_final
poisson_tri_n100$df_final
bernoulli_tri_n100$df_final

normal_tri_n250$df_final
poisson_tri_n250$df_final
bernoulli_tri_n250$df_final

normal_tri_n500$df_final
poisson_tri_n500$df_final
bernoulli_tri_n500$df_final

normal_tri_n1000$df_final
poisson_tri_n1000$df_final
bernoulli_tri_n1000$df_final

#tri_n50$df_final
#tri_n100$df_final
#tri_n250$df_final
#tri_n500$df_final
#tri_n1000$df_final

#-----------------------------------------------------------------

results1 <- rbind(normal_uni_n50$df_final,
                  poisson_uni_n50$df_final,
                  bernoulli_uni_n50$df_final,
                  
                  normal_uni_n100$df_final,
                  poisson_uni_n100$df_final,
                  bernoulli_uni_n100$df_final,
                  
                  normal_uni_n250$df_final,
                  poisson_uni_n250$df_final,
                  bernoulli_uni_n250$df_final,
                  
                  normal_uni_n500$df_final,
                  poisson_uni_n500$df_final,
                  bernoulli_uni_n500$df_final,
                  
                  normal_uni_n1000$df_final,
                  poisson_uni_n1000$df_final,
                  bernoulli_uni_n1000$df_final,
                  
                  normal_tri_n50$df_final,
                  poisson_tri_n50$df_final,
                  bernoulli_tri_n50$df_final,
                  
                  normal_tri_n100$df_final,
                  poisson_tri_n100$df_final,
                  bernoulli_tri_n100$df_final,
                  
                  normal_tri_n250$df_final,
                  poisson_tri_n250$df_final,
                  bernoulli_tri_n250$df_final,
                  
                  normal_tri_n500$df_final,
                  poisson_tri_n500$df_final,
                  bernoulli_tri_n500$df_final,
                  
                  normal_tri_n1000$df_final,
                  poisson_tri_n1000$df_final,
                  bernoulli_tri_n1000$df_final#,
                  
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
                                           "uni bernoulli", "tri normal", 
                                           "tri poisson", "tri bernoulli",
                                           "normal/poisson/bernoulli"))



#-----------------------------------------------------------------

ggplot(data = results1,
       mapping = aes(x = dist, 
                     y = rej, 
                     col = `Tamanho amostral`
       )) + 
  geom_point() + 
  geom_line(aes(linetype=`Tamanho amostral`)) + 
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
