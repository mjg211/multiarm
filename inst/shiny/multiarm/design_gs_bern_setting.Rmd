The trial will be designed to compare $K$ experimental treatments to a shared
control arm. Response $X_{ik}$, from patient $i=1,\dots,n_k$ in arm
$k=0,\dots,K$, will be assumed to be distributed as
$X_{ik} \sim Bern(\pi_k)$. Then, the hypotheses to be tested will be:
$$ H_k : \tau_k = \pi_k - \pi_0 \le 0,\ k=1,\dots,K.$$
The *global null hypothesis*, $H_G$, will be:
$$ \pi_0 = \cdots = \pi_K. $$
The *global alternative hypothesis*, $H_A$, will be:
$$ \pi_1 = \cdots = \pi_K = \pi_0 + \delta_1. $$
The *least favourable configuration* for experimental arm $k$, $LFC_k$, will be:
$$ \pi_k = \pi_0 + \delta_1,\ \pi_1 = \cdots = \pi_{k-1} = \pi_{k+1} = \cdots = \pi_K = \pi_0 + \delta_0. $$
Here, $\delta_1$ and $\delta_0$ are *interesting* and *uninteresting* treatment
effects respectively.
