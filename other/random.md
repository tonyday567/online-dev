<script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-svg.js"></script>

Random experiments

std of ma

<img style="border:2px solid grey" src="other/stdofma.svg">

average beta measure

<img style="border:2px solid grey" src="other/avbetameasure.svg">

beta error scatter

<img style="border:2px solid grey" src="other/betascatter.svg">

alpha
---
$$
\begin{align}
\alpha & = \frac{\sum y \sum x^2 - \sum x \sum xy}{n\sum x^2 - (\sum x)^2} \\
      & = \frac{n^2 \bar{y} \overline{x^2} - n^2 \bar{x} \overline{xy}}{n^2 \overline{x^2} - n^2 \bar{x}^2} \\
      & = \frac{\bar{y} \overline{x^2} - \bar{x} \overline{xy}}{\overline{x^2} - \bar{x}^2} \\
\end{align}
$$

beta
---

$$
\begin{align}
\beta & = \frac{n\sum xy - \sum x \sum y}{n\sum x^2 - (\sum x)^2} \\
      & = \frac{n^2 \overline{xy} - n^2 \bar{x} \bar{y}}{n^2 \overline{x^2} - n^2 \bar{x}^2} \\
      & = \frac{\overline{xy} - \bar{x} \bar{y}}{\overline{x^2} - \bar{x}^2} \\
\end{align}
$$

Dependencies across moments
---

$$
\begin{align}
x_{t+1} & = (alpha_t^x + beta_t^{x->x} * ma_t^x + beta_t^{s->x} * std_t^x) + s_{t+1}\\
s_{t+1} & = (alpha_t^s + beta_t^{x->s} * ma_t^x + beta_t^{s->s} * std_t^x) * N(0,1)
\end{align}
$$

