# ☕ Análise de Ciclos da Exportação de Café no Brasil  

Bem-vindo(a) ao repositório do meu **Trabalho de Conclusão de Curso (TCC II)** em Estatística na FCT/Unesp.  
O objetivo deste projeto foi **analisar os ciclos das exportações de café no Brasil** por meio de **modelos de decomposição de séries temporais e análise espectral econométrica**, com foco em compreender os padrões de **volume, receita e preço médio** dos quatro principais tipos de café exportados (Arábica, Conilon, Solúvel e Torrado).  

---

## 📊 O que foi realizado  

- **Base de dados**: Relatórios mensais do **CECAFÉ** (2015–2022).  
- **Variáveis analisadas**:  
  - Volume exportado (sacas de 60 kg)  
  - Receita (US$ 1000)  
  - Preço médio (US$/saca)  
- **Ferramentas utilizadas**:  
  - Linguagem **R**  
  - Pacotes:  
    - `ggplot2` → visualização gráfica (séries, boxplots, histogramas)  
    - `forecast`, `tseries` → testes de estacionaridade (Dickey-Fuller) e modelagem  
    - `lmtest`, `Metrics` → avaliação de modelos (p-valores, R², métricas de desempenho)  

---

## 🧩 Metodologia  

- Análise descritiva completa das séries (tendências, dispersão e sazonalidade)  
- Ajuste de **12 modelos harmônicos iniciais** (com frequência de 12 meses)  
- Verificação de resíduos com testes de **Shapiro-Wilk** (normalidade) e **Durbin-Watson** (autocorrelação)  
- Correção da autocorrelação serial por meio da **transformação de Cochrane-Orcutt**  
- Aplicação de **análise espectral** e **periodograma**, identificando frequências dominantes — especialmente **96 meses**  
- Comparação dos modelos pelo coeficiente de determinação (R²) e interpretação dos padrões temporais  

---

## ✅ Resultados principais  

- O **Arábica** se destacou como o café mais exportado em volume e receita  
- O **Solúvel** apresentou menor variabilidade relativa, mostrando estabilidade  
- O **Conilon** registrou períodos de queda acentuada entre 2016–2018, capturados por variáveis dummy nos modelos  
- O **Torrado** apresentou preços médios mais altos e maior variabilidade  
- Os modelos harmônicos, refinados pelo uso do periodograma, conseguiram **captar com eficiência tendências e sazonalidades**  
- Apesar de alguns desafios com resíduos não normais, os modelos finais mostraram-se adequados para explicar os ciclos  

---

## 🚀 Conclusão  

As técnicas de **decomposição temporal e análise harmônica** mostraram-se essenciais para compreender a dinâmica das exportações brasileiras de café.  
Os resultados reforçam a importância da Estatística aplicada à economia e ao agronegócio, oferecendo insights que podem apoiar a gestão estratégica da cadeia produtiva do café.  

---

## 🤝 Contribuição  

Sugestões, críticas construtivas e colaborações são bem-vindas!  
Este repositório é um espaço aberto para troca de ideias sobre **séries temporais, econometria e análise aplicada ao mercado de commodities**.  

---

📌 **Autoria**: Thaii Céu Santos  
🎓 **Orientador**: Prof. Dr. Manoel Ivanildo Silvestre Bezerra  
📍 Universidade Estadual Paulista (UNESP) – FCT/Presidente Prudente  
