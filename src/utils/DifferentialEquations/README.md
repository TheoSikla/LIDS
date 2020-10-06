# Available Epidemic Models

## SIR
The SIR is one of the simplest compartmental epidemic models. It consists of three groups:

* **S**: The number of **Susceptible** individuals. When a susceptible and an infectious individual come into "infectious contact", the susceptible individual contracts the disease and transitions to the infectious compartment.

* **I**: The number of **Infectious** individuals. These are individuals who have been infected and are capable of infecting susceptible individuals.

* **R**: for the number of **Removed** (and immune) or deceased individuals. These are individuals who have been infected and have either <ins>recovered</ins> from the disease and entered the removed compartment, or <ins>died</ins>. It is assumed that the number of deaths is negligible with respect to the total population. This compartment may also be called "recovered" or "resistant".

It can also be expressed by the following set of ordinary differential equations:

<p align="center">
  <img src="./images/sir_de.svg" style="background-color:white" alt="SIR"/>
</p>
