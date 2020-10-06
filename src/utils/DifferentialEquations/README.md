# Available Epidemic Models

## SIR
The SIR is one of the simplest compartmental epidemic models. It consists of three groups:

* **S**: The number of **Susceptible** individuals. When a susceptible and an infectious individual come into "infectious contact", the susceptible individual contracts the disease and transitions to the infectious compartment.

* **I**: The number of **Infectious** individuals. These are individuals who have been infected and are capable of infecting susceptible individuals.

* **R**: for the number of **Removed** (and immune) or deceased individuals. These are individuals who have been infected and have either <ins>recovered</ins> from the disease and entered the removed compartment, or <ins>died</ins>. It is assumed that the number of deaths is negligible with respect to the total population. This compartment may also be called "recovered" or "resistant".

It can also be expressed by the following set of ordinary differential equations:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dt}}=-{\frac {\beta IS}{N}\ ,}" width="185"  alt="SIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dt}}={\frac {\beta IS}{N}}-\gamma I\ ," width="205" alt="SIR"/><br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dR}{dt}}=\gamma I" width="100"  alt="SIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
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

This model can be expressed by the following set of ordinary differential equations:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dt}}=-{\frac {\beta IS}{N}%2B\gamma I\ ,}" width="220" style="background-color:white"  alt="SIR"/><br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dt}}={\frac {\beta IS}{N}-\gamma I\ }" width="185" style="background-color:white"  alt="SIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

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
