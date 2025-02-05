# Análise de Ciclos da Exportação de Café no Brasil

Bem-vindo(a) ao repositório deste projeto de Trabalho de Conclusão de Curso (TCC), que tem como objetivo analisar os ciclos das exportações de café no Brasil através de modelos de decomposição de séries temporais e análise espectral econométrica.

---

## O que foi realizado até o momento

Nesta etapa inicial, utilizamos dados reais fornecidos pelo CECAFÉ, abrangendo os quatro tipos de café exportados (Conilon, Arábica, Torrado e Solúvel), com informações mensais sobre Volume, Receita e Preço Médio. Para desenvolver a análise, empregamos a linguagem **R** e diversos pacotes estatísticos, como:

- **ggplot2** – Para a visualização gráfica dos dados (séries temporais, histogramas e boxplots);
- **forecast** e **tseries** – Para modelagem e testes de estacionaridade (como o teste de Dickey-Fuller);
- **lmtest** e **Metrics** – Para avaliação dos modelos, através de testes e métricas de desempenho.

Foram ajustados modelos de regressão com variáveis dummy (para capturar a sazonalidade) e modelos harmônicos (usando a transformação de Fourier), que permitiram decompor as séries em seus componentes de tendência, sazonalidade e irregularidade. Os resultados parciais demonstraram que ambos os modelos conseguem captar, de forma satisfatória, os padrões e ciclos presentes nos dados de exportação do café.

---

## Próximos Passos

A próxima fase deste projeto visa aprofundar a **análise espectral**, com o intuito de identificar com maior precisão as frequências dominantes e os ciclos subjacentes nas séries temporais. Essa etapa será fundamental para aprimorar os modelos preditivos e oferecer insights mais profundos sobre o comportamento do mercado de café, contribuindo para uma melhor compreensão dos fatores históricos e econômicos que influenciam as exportações.

---

## Conclusão

Este repositório reúne os códigos e as análises realizadas até o momento, servindo como base para futuras implementações e refinamentos. Convidamos você a explorar o projeto, acompanhar as atualizações e contribuir com sugestões para o desenvolvimento contínuo desta pesquisa, que une rigor estatístico à aplicação prática em um dos setores mais importantes da economia brasileira.

---

Obrigado por seu interesse! Fique à vontade para explorar, utilizar e colaborar com este trabalho.
