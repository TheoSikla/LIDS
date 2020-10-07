# Available Epidemic Models

## SIR
The SIR is one of the simplest compartmental epidemic models. It consists of three groups:

* **S**: The number of **Susceptible** individuals. When a susceptible and an infectious individual come into "infectious contact", the susceptible individual contracts the disease and transitions to the infectious compartment.

* **I**: The number of **Infectious** individuals. These are individuals who have been infected and are capable of infecting susceptible individuals.

* **R**: for the number of **Removed** (and immune) or deceased individuals. These are individuals who have been infected and have either <ins>recovered</ins> from the disease and entered the removed compartment, or <ins>died</ins>. It is assumed that the number of deaths is negligible with respect to the total population. This compartment may also be called "recovered" or "resistant".

It can also be expressed by the following set of **ordinary differential equations**:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dt}}=-{\frac {\beta IS}{N}\ ,}" width="185" style="background-color:white" alt="SIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dt}}={\frac {\beta IS}{N}}-\gamma I\ ," width="205" style="background-color:white" alt="SIR"/><br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dR}{dt}}=\gamma I" width="100" style="background-color:white"  alt="SIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
</span>
</div>

<!-- 
For https://latex.codecogs.com/ :

{\displaystyle
 {\\\begin{aligned}&{\frac {dS}{dt}}=-{\frac {\beta IS}{N}}\ ,\ \\
[6pt]&{\frac {dI}{dt}}={\frac {\beta IS}{N}}-\gamma I\ ,\ \\
[6pt]&{\frac {dR}{dt}}=\gamma I\ ,\ \\\end{aligned}}
} -->

Where:  

N: The total population.  
<span>&beta;</span>: The average number of contacts per person per time. (Infection rate)  
<span>&gamma;</span>: The recovery rate.  

For more information on the SIR epidemic model check: [here](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SIR_model)

<br>

---

<br>

## SIS

Some infections, for example, those from the **common cold** and **influenza**, do not confer any <ins>long-lasting immunity</ins>. Such infections do not give immunity upon recovery from infection, and individuals become <ins>susceptible</ins> again.

This model can be expressed by the following set of **ordinary differential equations**:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dt}}=-{\frac {\beta IS}{N}%2B\gamma I\ ,}" width="220" style="background-color:white"  alt="SIS"/><br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dt}}={\frac {\beta IS}{N}-\gamma I\ }" width="185" style="background-color:white" alt="SIS"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
</span>
</div>

<!-- 
For https://latex.codecogs.com/ :

{\displaystyle 
{\begin{aligned}
{\frac {dS}{dt}}&=-{\frac {\beta SI}{N}}+\gamma I\\
[6pt]{\frac {dI}{dt}}&={\frac {\beta SI}{N}}-\gamma I
\end{aligned}}}
} -->

Where:  

N: The total population.  
<span>&beta;</span>: The average number of contacts per person per time. (Infection rate)  
<span>&gamma;</span>: The recovery rate.

For more information on the SIS epidemic model check: [here](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SIS_model)

<br>

---

<br>

## SIQ

SIQ is another very simple compartmental epidemic model which is similar to the SIQ model. The main difference is that **Infected** individuals are now going to the new **Quarantined** compartment instead of the **Recovered** one.

This model can be expressed by the following set of **ordinary differential equations**:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dt}}=\Lambda - {\frac {\beta SI}{N}} - \mu S\ ," width="240" style="background-color:white" alt="SIQ"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dt}}=\Lambda %2B {\frac {\beta SI}{N}} - (\mu %2B \delta %2B \kappa)I\ ," width="370" style="background-color:white" alt="SIQ"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dQ}{dt}}=\kappa I - (\mu %2B \delta)Q" width="250" style="background-color:white" alt="SIQ"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
</span>
</div>

<!-- 
For https://latex.codecogs.com/ :

{\displaystyle 
{\begin{aligned}
{\frac {dS}{dt}}&=\Lambda - {\frac {\beta SI}{N}} - \mu S\\
[6pt]{\frac {dI}{dt}}&=\Lambda + {\frac {\beta SI}{N}} - (\mu + \delta + \kappa)I\\
[6pt]{\frac {dQ}{dt}}&=\kappa I - (\mu + \delta)Q
\end{aligned}}}
} -->

Where:  

N: The total population.  
<span>&beta;</span>: The average number of contacts per person per time. (Infection rate)  
<span>&mu;</span>: Natural mortality rate.  
<span>&Lambda;</span>: Recruitment of the susceptible individuals (birth etc.)  
<span>&delta;</span>: Disease mortality rate.  
<span>&kappa;</span>: The rate where the infected individuals become quarantined.  

For more information on the SIQ epidemic model check: [here](https://www.researchgate.net/publication/228385000_Global_Stability_of_an_SIQ_Epidemic_Model)

<br>

---

<br>

## SIQS

In SIQS we have an extra step after the Quarantine compartment, where quarantined individuals recover from the disease, but they become susceptible again as they do not gain any type of immunity from that disease. An example of such disease can be [Covid-19](https://en.wikipedia.org/wiki/COVID-19_pandemic).

This model can be expressed by the following set of **ordinary differential equations**:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dt}}=\Lambda - {\frac {\beta SI}{N}} - \mu S %2B \gamma I %2B \zeta Q\ ," width="380" style="background-color:white"  alt="SIQS"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dt}}={\frac {\beta SI}{N}} - (\mu %2B \delta %2B \kappa %2B \gamma)I\ ," width="370" style="background-color:white" alt="SIQS"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dQ}{dt}}=\kappa I - (\mu %2B \delta1 %2B \zeta)Q" width="310" style="background-color:white" alt="SIQS"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

</span>
</div>

<!-- 
For https://latex.codecogs.com/ :

{\displaystyle 
{\begin{aligned}
{\frac {dS}{dt}}&=\Lambda - {\frac {\beta SI}{N}} - \mu S + \gamma I + \zeta Q ,\\
[6pt]{\frac {dI}{dt}}&={\frac {\beta SI}{N}} - (\mu + \delta + \kappa + \gamma)I ,\\
[6pt]{\frac {dQ}{dt}}&=\kappa I - (\mu + \delta1 + \zeta)Q
\end{aligned}}}
} -->

Where:  

N: The total population.  
<span>&beta;</span>: The average number of contacts per person per time. (Infection rate)  
<span>&gamma;</span>: The recovery rate.  
<span>&mu;</span>: Natural mortality rate.  
<span>&Lambda;</span>: Recruitment of the susceptible individuals (birth etc.)  
<span>&delta;</span>: Disease mortality rate.  
<span>&delta;1</span>: Disease mortality rate of quarantined individuals.  
<span>&kappa;</span>: The rate where the infected individuals become quarantined.  
<span>&zeta;</span>: The rate where quarantined individuals become susceptible again.  

For more information on the SIQS epidemic model check: [here](https://www.researchgate.net/publication/324680133_Dynamic_behavior_of_a_stochastic_SIQS_epidemic_model_with_Levy_jumps)

<br>

---

<br>

## SIRD

The Susceptible Infectious Recovered Deceased Model differentiates between <ins>Recovered</ins> (meaning specifically individuals having survived the disease and now immune) and <ins>Deceased</ins>.

This model can be expressed by the following set of **ordinary differential equations**:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dt}}=-{\frac {\beta IS}{N}\ ,}" width="185" style="background-color:white" alt="SIRD"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dt}}={\frac {\beta IS}{N}}-\gamma I-\mu I\  ," width="250" style="background-color:white" alt="SIRD"/><br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dR}{dt}}=\gamma I\ ," width="120" style="background-color:white"  alt="SIRD"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dD}{dt}}=\mu I" width="105" style="background-color:white" alt="SIRD"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
</span>
</div>

<!-- 
For https://latex.codecogs.com/ :

{\displaystyle 
{\begin{aligned}
&{\frac {dS}{dt}}=-{\frac {\beta IS}{N}},\\
[6pt]&{\frac {dI}{dt}}={\frac {\beta IS}{N}}-\gamma I-\mu I,\\
[6pt]&{\frac {dR}{dt}}=\gamma I,\\
[6pt]&{\frac {dD}{dt}}=\mu I,
\end{aligned}}}
} -->

Where:  

N: The total population.  
<span>&beta;</span>: The average number of contacts per person per time. (Infection rate)  
<span>&gamma;</span>: The recovery rate.  
<span>&mu;</span>: Natural mortality rate.

For more information on the SIRD epidemic model check: [here](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SIRD_model)

<br>

---

<br>

## MSIR

For many infections, including measles, babies are not born into the susceptible compartment but are <ins>immune</ins> to the disease for the first few months of life due to protection from <ins>maternal antibodies</ins> (passed across the placenta and additionally through colostrum). This is called <ins>passive immunity</ins>. This added detail can be shown by including an M class (for maternally derived immunity) at the beginning of the model.

This model can be expressed by the following set of **ordinary differential equations**:

<div>
<span align="center">
 
 <img src="https://render.githubusercontent.com/render/math?math={\frac {dM}{dT}}=\Lambda - \delta M -\mu M\ ," width="210" style="background-color:white" alt="MSIR"/>&nbsp;<br><br>
</span>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dT}}=\delta M - {\frac {\beta SI}{N}} - \mu S\ ," width="220" style="background-color:white" alt="MSIR"/><br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dT}}={\frac {\beta SI}{N}} - \gamma I - \mu I\  ," width="215" style="background-color:white" alt="MSIR"/>&nbsp;<br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dR}{dT}}= \gamma I - \mu R\ ," width="160" style="background-color:white" alt="MSIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

</span>
</div>

<!-- 
For https://latex.codecogs.com/ :

{\displaystyle 
{\begin{aligned}
{\frac {dM}{dT}}&=\Lambda -\delta M-\mu M\\
[8pt]{\frac {dS}{dT}}&=\delta M-{\frac {\beta SI}{N}}-\mu S\\
[8pt]{\frac {dI}{dT}}&={\frac {\beta SI}{N}}-\gamma I-\mu I\\
[8pt]{\frac {dR}{dT}}&=\gamma I-\mu R
\end{aligned}}}
} -->

Where:  

N: The total population.  
<span>&beta;</span>: The average number of contacts per person per time. (Infection rate)  
<span>&gamma;</span>: The recovery rate.  
<span>&mu;</span>: Natural mortality rate.  
<span>&Lambda;</span>: Recruitment of the susceptible individuals (birth etc.)  
<span>&delta;</span>: Disease mortality rate.

For more information on the MSIR epidemic model check: [here](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_MSIR_model)

<br>

---

<br>

## SEIR

For many important infections, there is a significant <ins>incubation period</ins> during which individuals have been infected but are not yet infectious themselves. During this period the individual is in compartment **E** (for exposed).

This model can be expressed by the following set of **ordinary differential equations**:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dt}}=\Lambda N - \mu S - {\frac {\beta IS}{N}}\ ," width="230" style="background-color:white" alt="SEIR"/><br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dE}{dt}}={\frac {\beta IS}{N}} - (\mu %2B a)E\ ," width="210" style="background-color:white" alt="SEIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dt}}=aE - (\gamma %2B \mu )I\  ," width="205" style="background-color:white" alt="SEIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dR}{dt}}=\gamma I - \mu R" width="160" style="background-color:white" alt="SEIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

</span>
</div>

<!-- 
For https://latex.codecogs.com/ :

{\displaystyle
{\begin{aligned}
{\frac {dS}{dt}}&=\Lambda N-\mu S-{\frac {\beta IS}{N}}\\
[8pt]{\frac {dE}{dt}}&={\frac {\beta IS}{N}}-(\mu +a)E\\
[8pt]{\frac {dI}{dt}}&=aE-(\gamma +\mu )I\\
[8pt]{\frac {dR}{dt}}&=\gamma I-\mu R.
\end{aligned}}}
} -->

Where:  

N: The total population.  
<span>&beta;</span>: The average number of contacts per person per time. (Infection rate)  
<span>&gamma;</span>: The recovery rate.  
<span>&mu;</span>: Natural mortality rate.  
<span>&Lambda;</span>: Recruitment of the susceptible individuals (birth etc.)  
<span>&alpha;</span>: Average incubation period (α¯¹).

For more information on the SEIR epidemic model check: [here](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SEIR_model)

<br>

---

<br>

## SEIS

The SEIS model is like the SEIR model (above) except that no immunity is acquired at the end.

This model can be expressed by the following set of **ordinary differential equations**:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dT}}=\Lambda -{\frac {\beta SI}{N}}-\mu S %2B\gamma I\ ," width="280" style="background-color:white" alt="SEIS"/><br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dE}{dT}}={\frac {\beta SI}{N}} - (\epsilon %2B \mu )E\ ," width="255" style="background-color:white" alt="SEIS"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dT}}=\varepsilon E - (\gamma %2B \mu )I" width="250" style="background-color:white" alt="SEIS"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

</span>
</div>

<!-- 
For https://latex.codecogs.com/ :

{\displaystyle
{\begin{aligned}
{\frac {dS}{dT}}&=\Lambda -{\frac {\beta SI}{N}}-\mu S+\gamma I\\
[6pt]{\frac {dE}{dT}}&={\frac {\beta SI}{N}}-(\epsilon +\mu )E\\
[6pt]{\frac {dI}{dT}}&=\varepsilon E-(\gamma +\mu )I
\end{aligned}}}
} -->

Where:  

N: The total population.  
<span>&beta;</span>: The average number of contacts per person per time. (Infection rate)  
<span>&gamma;</span>: The recovery rate.  
<span>&mu;</span>: Natural mortality rate.  
<span>&Lambda;</span>: Recruitment of the susceptible individuals (birth etc.)  
<span>&epsilon;</span>: The rate at which exposed individuals become infectious.

For more information on the SEIS epidemic model check: [here](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SEIS_model)

<br>

---

<br>

## MSEIR

For the case of a disease, with the factors of passive immunity, and a latency period there is the MSEIR model.

This model can be expressed by the following set of **ordinary differential equations**:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dM}{dT}}=\Lambda - \delta M - \mu M\ ," width="280" style="background-color:white" alt="MSEIR"/>&nbsp;&nbsp;&nbsp;<br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dT}}=\delta M - {\frac {\beta SI}{N}} - \mu S\ ," width="275" style="background-color:white" alt="MSEIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dE}{dT}}={\frac {\beta SI}{N}} - (\varepsilon %2B \mu )E\ ," width="270" style="background-color:white" alt="MSEIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dT}}=\varepsilon E - (\gamma %2B \mu )I\ ," width="270" style="background-color:white" alt="MSEIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dR}{dT}}=\gamma I - \mu R" width="200" style="background-color:white" alt="MSEIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

</span>
</div>

<!-- 
For https://latex.codecogs.com/ :

{\displaystyle
{\begin{aligned}
{\frac {dM}{dT}}&=\Lambda -\delta M-\mu M\\
[6pt]{\frac {dS}{dT}}&=\delta M-{\frac {\beta SI}{N}}-\mu S\\
[6pt]{\frac {dE}{dT}}&={\frac {\beta SI}{N}}-(\varepsilon +\mu )E\\
[6pt]{\frac {dI}{dT}}&=\varepsilon E-(\gamma +\mu )I\\
[6pt]{\frac {dR}{dT}}&=\gamma I-\mu R
\end{aligned}}}
} -->

Where:  

N: The total population.  
<span>&beta;</span>: The average number of contacts per person per time. (Infection rate)  
<span>&gamma;</span>: The recovery rate.  
<span>&mu;</span>: Natural mortality rate.  
<span>&Lambda;</span>: Recruitment of the susceptible individuals (birth etc.)  
<span>&delta;</span>: Disease mortality rate.  
<span>&epsilon;</span>: The rate at which exposed individuals become infectious.

For more information on the MSEIR epidemic model check: [here](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_MSEIR_model)
