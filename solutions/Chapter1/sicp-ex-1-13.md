### Proof.  
According to the definition of Fibonaci:  
<div align=center><img src ="https://latex.codecogs.com/svg.latex?\mathrm{Fib}(n)=\mathrm{Fib}(n-1)+\mathrm{Fib}(n-2)"/></div>  
This is a linear recurrence, solve it by characteristic equation:  
<div align=center><img src ="https://latex.codecogs.com/svg.latex?x^2=x+1"/></div>  
<div align=center><img src ="https://latex.codecogs.com/svg.latex?x_1=\frac{1+\sqrt{5}}{2}=\phi\qquad\qquad%20x_2=\frac{1-\sqrt{5}}{2}=\gamma"/></div>  

We know that both ![](https://latex.codecogs.com/svg.latex?\phi) and ![](https://latex.codecogs.com/svg.latex?\gamma) are <1, and Fib(n) has the special form:  
<div align=center><img src ="https://latex.codecogs.com/svg.latex?\mathrm{Fib}(n)=A\phi^n+B\gamma^n"/></div>  
It's easy to know that  
<div align=center><img src ="https://latex.codecogs.com/svg.latex?\begin{cases}\mathrm{Fib}(0)=A+B=0\\\mathrm{Fib}(1)=A\phi+B\gamma=1\end{cases}"/></div>  
So we obtain that
<div align=center><img src ="https://latex.codecogs.com/svg.latex?A=\frac{1}{\sqrt{5}}\quad%20B=-\frac{1}{\sqrt{5}}"/></div>  
<div align=center><img src ="https://latex.codecogs.com/svg.latex?\mathrm{Fib}(n)=\frac{1}{\sqrt{5}}(\phi^n-\gamma^n)"/></div>  
<div align=center><img src ="https://latex.codecogs.com/svg.latex?|\mathrm{Fib}(n)-\frac{\phi^n}{\sqrt{5}}|=\frac{|\gamma|^n}{\sqrt{5}}<0.5"/></div>  
<div align=center><img src ="https://latex.codecogs.com/svg.latex?\frac{\phi^n}{\sqrt{5}}-0.5<\mathrm{Fib}(n)<\frac{\phi^n}{\sqrt{5}}+0.5"/></div>  

Because ![](https://latex.codecogs.com/svg.latex?\forall%20n\in\mathbb{R}), there are exactly one integer in ![](https://latex.codecogs.com/svg.latex?(n-0.5,%20n+0.5))  
Therefore, Fib(n) is the integer closest to
<div align=center><img src ="https://latex.codecogs.com/svg.latex?\frac{\phi^n}{\sqrt{5}}"/></div>  
