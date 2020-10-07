# Available Epidemic Models

## SIR
The SIR is one of the simplest compartmental epidemic models. It consists of three groups:

* **S**: The number of **Susceptible** individuals. When a susceptible and an infectious individual come into "infectious contact", the susceptible individual contracts the disease and transitions to the infectious compartment.

* **I**: The number of **Infectious** individuals. These are individuals who have been infected and are capable of infecting susceptible individuals.

* **R**: for the number of **Removed** (and immune) or deceased individuals. These are individuals who have been infected and have either <ins>recovered</ins> from the disease and entered the removed compartment, or <ins>died</ins>. It is assumed that the number of deaths is negligible with respect to the total population. This compartment may also be called "recovered" or "resistant".

It can also be expressed by the following set of ordinary differential equations:

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

This model can be expressed by the following set of ordinary differential equations:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dt}}=-{\frac {\beta IS}{N}%2B\gamma I\ ,}" width="220" style="background-color:white"  alt="SIR"/><br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dt}}={\frac {\beta IS}{N}-\gamma I\ }" width="185" style="background-color:white" alt="SIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

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

## SIRD

The Susceptible Infectious Recovered Deceased Model differentiates between <ins>Recovered</ins> (meaning specifically individuals having survived the disease and now immune) and <ins>Deceased</ins>.

This model can be expressed by the following set of **ordinary differential equations**:

<div>
<span align="center">
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dt}}=-{\frac {\beta IS}{N}\ ,}" width="185" style="background-color:white" alt="SIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dt}}={\frac {\beta IS}{N}}-\gamma I-\mu I\  ," width="250" style="background-color:white" alt="SIR"/><br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dR}{dt}}=\gamma I\ ," width="120" style="background-color:white"  alt="SIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dD}{dt}}=\mu I" width="105" style="background-color:white" alt="SIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>
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
 
 <img src="https://render.githubusercontent.com/render/math?math={\frac {dM}{dT}}=\Lambda - \delta M -\mu M\ ," width="210" style="background-color:white" alt="SIR"/>&nbsp;<br><br>
</span>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dS}{dT}}=\delta M - {\frac {\beta SI}{N}} - \mu S\ ," width="220" style="background-color:white" alt="SIR"/><br><br>
 
<img src="https://render.githubusercontent.com/render/math?math={\frac {dI}{dT}}={\frac {\beta SI}{N}} - \gamma I - \mu I\  ," width="215" style="background-color:white" alt="SIR"/>&nbsp;<br><br>

<img src="https://render.githubusercontent.com/render/math?math={\frac {dR}{dT}}= \gamma I - \mu R\ ," width="160" style="background-color:white" alt="SIR"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>

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
