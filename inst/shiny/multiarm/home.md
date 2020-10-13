Welcome to the [R<sup><i class="fa fa-external-link" style="font-size:6px"></i></sup>](https://www.r-project.org/) [Shiny<sup><i class="fa fa-external-link" style="font-size:6px"></i></sup>](https://shiny.rstudio.com/) graphical user interface (GUI) to the [R<sup><i class="fa fa-external-link" style="font-size:6px"></i></sup>](https://www.r-project.org/) package [multiarm<sup><i class="fa fa-external-link" style="font-size:6px"></i></sup>](https://github.com/mjg211/multiarm/).

In [R<sup><i class="fa fa-external-link" style="font-size:6px"></i></sup>](https://www.r-project.org/), [multiarm<sup><i class="fa fa-external-link" style="font-size:6px"></i></sup>](https://github.com/mjg211/multiarm/) provides functionality to assist with the design of single-stage multi-arm clinical trials utilising one of several supported multiple comparison corrections, when the outcome data is assumed to be either normally or Bernoulli distributed.
Available functions allow for sample size determination (including for *A*-, *D*-, and *E*-optimal designs), trial simulation, analytical operating characteristic calculation (including the conjunctive power, disjunctive power, family-wise error-rate, and false discovery rate), and the production of several plots.

In addition, it can assist with the design of a variety of multi-stage designs for multi-arm clinical trials, when the outcome data is assumed to be normally or Bernoulli distributed.
Specifically, both group-sequential and drop-the-losers approaches are supported, with available functions allowing for sample size determination, trial simulation, analytical operating characteristic calculation, and plot production.

At present, this GUI supports execution of the commands for sample size determination and plot production, for both single- and multi-stage designs.
Additional functionality will be added over time - feedback on what new support would be most helpful would be warmly appreciated.
For an introduction to the app and some of the types of design it supports, see [Grayling and Wason (2020)<sup><i class="fa fa-external-link" style="font-size:6px"></i></sup>](http://doi.org/10.1186/s12885-020-6525-0).

See the <a onclick="openTab(&#39;design_ss_bern&#39;)" href="#">Single-stage</a>, <a onclick="openTab(&#39;design_gs_bern&#39;)" href="#">Group-sequential</a>, and <a onclick="openTab(&#39;design_dtl_bern&#39;)" href="#">Drop-the-losers</a> tabs on the sidebar for sample size determination, or the <a onclick="openTab(&#39;about&#39;)" href="#">About</a> tab for further information on the GUI.
Clicking on the [Source code<sup><i class="fa fa-external-link" style="font-size:6px"></i></sup>](https://github.com/mjg211/multiarm) tab will re-direct you to a copy of the code and materials used to create this app.
