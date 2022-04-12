#---------------------------------------------------------------
# TESTES DE HIPÓTESES
#---------------------------------------------------------------

source('~/msc/0_funcoes/functions.R')

#---------------------------------------------------------------

# Manovas

mc_manova_I(fit)
mc_manova_II(fit)
mc_manova_III(fit)

# Anovas

mc_anova_I(fit)
mc_anova_II(fit)
mc_anova_III(fit)

#---------------------------------------------------------------

# Avaliando parâmetros de dispersão

mc_anova_disp(fit,
              p_var = list(c(0,1), c(0,1)),
              names = list(c('tau10', 'tau11'),
                           c('tau20', 'tau11')))

mc_manova_disp(fit,
               p_var = c(0,1),
               names = c('tau11', 'tau21'))

#---------------------------------------------------------------

# Comparações múltiplas

## Por resposta
mc_multcomp(object = fit,
            effect = list(c('momento'), 
                          c('momento')), 
            data = dados_dissertacao)

mc_multcomp(object = fit,
            effect = list(c('grupo'), 
                          c('grupo'),
                          c('grupo')), 
            data = dados_dissertacao)

mc_multcomp(object = fit,
            effect = list(c('momento', 'grupo'), 
                          c('momento', 'grupo'),
                          c('momento', 'grupo')), 
            data = dados_dissertacao)

## Multivariado
mc_mult_multcomp(object = fit, 
                 effect = c('momento'), 
                 data = dados_dissertacao)

mc_mult_multcomp(object = fit, 
                 effect = c('grupo'), 
                 data = dados_dissertacao)

mc_mult_multcomp(object = fit, 
                 effect = c('momento', 'grupo'), 
                 data = dados_dissertacao)

#---------------------------------------------------------------

# Hipóteses lineares gerais

mc_linear_hypothesis(object =  fit,
                     hypothesis = c('power11 = 1',
                                    'power21 = 1'))

#---------------------------------------------------------------
