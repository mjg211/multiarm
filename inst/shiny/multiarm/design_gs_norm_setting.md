The trial will be designed to compare $K$ experimental treatments to a shared control arm.
Response $X_{ik}$, from patient $i=1,\dots,n_k$ in arm $k=0,\dots,K$, will be assumed to be distributed as $X_{ik} \sim N(\mu_k,\sigma_k^2)$.
Then, the hypotheses to be tested will be:
$$ H_k : \tau_k = \mu_k - \mu_0 \le 0,\ k=1,\dots,K.$$
The *global null hypothesis*, $H_G$, will be:
$$ \tau_1 = \cdots = \tau_K = 0. $$
The *global alternative hypothesis*, $H_A$, will be:
$$ \tau_1 = \cdots = \tau_K = \delta_1. $$
The *least favourable configuration* for experimental arm $k$, $LFC_k$, will be:
$$ \tau_k = \delta_1,\ \tau_1 = \cdots = \tau_{k-1} = \tau_{k+1} = \cdots = \tau_K = \delta_0. $$
Here, $\delta_1$ and $\delta_0$ are *interesting* and *uninteresting* treatment effects respectively.
