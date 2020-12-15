The trial will be designed to compare $K$ experimental treatments to a shared control arm. Response $X_{ik}$, from patient $i=1,\dots,n_k$ in arm $k=0,\dots,K$, will be assumed to be distributed as $X_{ik} \sim Po(\lambda_k)$. Then, the hypotheses to be tested will be:
$$ H_k : \tau_k = \lambda_k - \lambda_0 \le 0,\ k=1,\dots,K.$$
The *global null hypothesis*, $H_G$, will be:
$$ \lambda_0 = \cdots = \lambda_K. $$
The *global alternative hypothesis*, $H_A$, will be:
$$ \lambda_1 = \cdots = \lambda_K = \lambda_0 + \delta_1. $$
The *least favourable configuration* for experimental arm $k$, $LFC_k$, will be:
$$ \lambda_k = \lambda_0 + \delta_1,\ \lambda_1 = \cdots = \lambda_{k-1} = \lambda_{k+1} = \cdots = \lambda_K = \lambda_0 + \delta_0. $$
Here, $\delta_1$ and $\delta_0$ are *interesting* and *uninteresting* treatment
effects respectively.
