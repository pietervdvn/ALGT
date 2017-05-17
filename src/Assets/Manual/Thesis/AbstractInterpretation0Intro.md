
Syntax Driven Abstract Interpretation
======================================

In this section, functions on parsetrees are converted into functions over sets of parsetrees. This is useful to algorithmically analyze these functions, which will help to gradualize them. This section dissects the technique to do so -abstract interpretation- and is organized as following: 

 - First, we'll work out **what abstract interpretation is**, with a simple example followed by its desired properties.
 - Then, we work out what **properties a syntax** should have. 
 - With these, we develop an **efficient representation** to capture infinite sets of parsetrees.
 - Afterwards, **operations on these setrepresentations** are stated.
 - As last, we build useful **algorithms** and checks with this algebra.

Abstract interpretation
------------------------

Per Rice's theorem, it is generally impossible to make precise statements about all programs. However, making useful statements about some programs is feasable.
 Cousot (1977) introduces a framework to do so, named **abstract interpretation**: "A program denotes computations in some domain of objects. Abstract interpretation of programs consists in using that denotation to describe computations in another domain of abstract objects, so that the results of the abstract computations give some information on the actual computation". 

%% TODO: source bibliography

The successor function, as defined below, is a prime example to illustrate this principle of using other domains: this function normally operates in the domain of _integer numbers_, as `succ` applied on `1` yields `2`; `succ -1` yields `0`. 

	succ n	= n + 1

But `succ` might also be applied on _signs_, instead of integers: we use `+`, `-` or `0` to perform the computation, where `+` represents all positive numbers, `-` represents all negative numbers and `0` represents zero. 

Using these symbols as input gives rise to a computation in the abstract domain of signs.



### The rule of signs

Per rule of signs $\code{+} + 1$ is equal to `+`, thus `succ +` yields `+`. Applying `succ` to a negative number gives less precise information, as $\code{-} + 1$ could yield both zero or a strictly negative number. In order to represent the union between the negative numbers and zero, the symbol `0-` is used.

These compositions are no more then the composition between two sets, 




\begin{figure}[h]
\center
\input{Lattice.tex}
\caption{Composition of possibilities about signs}
\label{fig:signs}
\end{figure}


#### Concretization and abstraction


The meaning of _`succ -` giving `0-`_ is intuitionally clear: _the successor of a negative number is either negative or zero_. More formally, it can be stated that, _given a negative number, `succ` will give an element from $\{n | n \leq 0\}$_. The meaning of `0-` is formalized by the **concretization** function γ, which translates from the abstract domain to the concrete domain:

$$
\begin{array}{rcl}
    \gamma(\code{-} ) & = & \{ z | z \in \mathbb{Z} \wedge z < 0 \} \\
    \gamma(\code{0-}) & = & \{ z | z \in \mathbb{Z} \wedge z \leq 0 \} \\
    \gamma(\code{0} ) & = & \{ 0 \} \\
    \gamma(\code{0+}) & = & \{ z | z \in \mathbb{Z} \wedge z \geq 0 \} \\
    \gamma(\code{+} ) & = & \{ z | z \in \mathbb{Z} \wedge z > 0 \} \\
    \gamma(\code{+-}) & = & \{z | z \in \mathbb{Z} \wedge z \neq 0\}\\
    \gamma(\top) & = & \mathbb{Z} 
\end{array}
$$

On the other hand, an element from the concrete domain is mapped onto the abstract domain with the **abstraction** function. This functions _abstracts_ a property of the concrete element:

$$
    \alpha(n) = \left\{\begin{array}{lr}
       \code{-} & \text{if } n < 0\\
       \code{0} & \text{if } n = 0\\
       \code{+} & \text{if } n > 0 
     \end{array}\right\}
$$



\begin{figure}[h!]
\center
\input{AbstractionMinus.tex}
\caption{Concretization and abstraction between integers and signs}
\end{figure}


### Ranges as abstract domain


Another possibility is to apply functions on an entire range at once (such as `[2,41]`), using abstract interpretation. In order to do so, the abstract domain used are ranges of the form `[n, m]`.

This can be calculated by taking each element the range represents, applying the function on each element and abstracting the newly obtained set, thus $$\alpha(map(\emph{f}, \gamma(\code{[n, m]})))$$

Where $map$ applies $f$ on each element of the set (like most functional programming languages), $\alpha$ is the abstraction function and $\gamma$ is the concretization function, with following definition:

$$
\begin{array}{rcl}
\alpha(n) & = & \code{[n, n]} \\
\alpha(N) & = & \code{[}min(N), max(N)\code{]} \\
\gamma(\code{[n, m]}) & = & \{ x | n \leq x \leq m \}
\end{array}
$$


For example, the outputrange for `succ [2,41]` can be calculated in the following way:
 
$$
\begin{array}{cl}
  & \alpha(map(\code{succ}, \gamma(\code{[2,41]}))) \\
= & \alpha(map(\code{succ}, \{2,3,...,40,41\})) \\        
= & \alpha(\{\code{succ} 2, \code{succ} 3, ... , \code{succ} 40, \code{succ} 41\}) \\
= & \alpha(\{3,4, ..., 41,42\}) \\
= & [3,42]
\end{array}
$$ 

However, this is computationally expensive: aside from translating the set from and to the abstract domain, the function $f$ has to be calculated $m - n$ times. By exploiting the underlying structure of ranges, calculating $f$ has only to be done _two_ times, aside from never having to leave the abstract domain.

The key insight is that addition of two ranges is equivalent to addition of the borders:

$$[n, m] + [x, x] = [n+x, m+x]$$

Thus, applying `succ` to a range can be calculated as following, giving the same result in an efficient way:

$$\begin{array}{rl}
   & \code{succ [2,5]} \\
 = & \code{[2,5]} + \alpha(1) \\
 = & \code{[2,5]} + \code{[1,1]} \\
 = & \code{[3,6]} \\
\end{array}$$



### Working with collecting semantics

At last, the abstract domain might be the _set_ of possible values, such as `{1,2,41}`. Applying `succ` to this set will yield a new set: 

$$\begin{array}{rl}
 & \code{succ \{1,2,41\}} \\
= & \code{\{1,2,41\}} + \alpha(1) \\
= & \code{\{1,2,41\}} + \code{\{1\}} \\
= & \code{\{2,3,42\}} \\
\end{array}$$


Translation from and to the abstract domain are straightforward:
$$
\begin{array}{lcl}
\alpha(n) & = & \{ n \} \\
\gamma(\{n1, n2, \ldots\} & = & \{n1, n2, ... \} \\
\end{array}
$$
 

Performing the computation in the abstract domain of sets can be more efficient than the equivalent concrete computations, as the structure of the concrete domain can be exploited to use a more efficient representation in memory (such as ranges).
Using this abstract domain effectively lifts a function over integers into a function over sets of integers. Exactly this abstract domain is used to lift the functions over parsetrees into functions over sets of parsetrees. To perform these calculations, an efficient representations of possible parsetrees will be deduced later in this section.

### Properties of α and γ

For abstract interpretation to work, the functions α and γ should obey to some properties to make this approach work, namely _monotonicity_ and _correctness_.
These properties imply a **Galois connection** between the concrete and abstract domains.

#### Monotonicity of α and γ

The first requirement is that both _abstraction_ and _concretization_ are monotone. This states that, if the set to concretize grows, the set of possible properties _might_ grow, but never shrink.

Analogously, if the set of properties grows, the set of concrete values represented by these properties might grow too.


$$
\begin{array}{c}
X \subseteq Y \Rightarrow \gamma(X) \subseteq \gamma(Y) \\
X \subseteq Y \Rightarrow \alpha(X) \subseteq \alpha(Y) \\
\end{array}
$$

When working with signs as properties, this property can be illustrated with:

$$\{1,2\} \subseteq \{0,1,2\} \Rightarrow \code{+} \subseteq \code{0+}$$

#### Soundness

When a concrete value $n$ is translated into the abstract domain, we expect that $\alpha(n)$ represents this value. An abstract object $m$ represents a concrete value $n$ iff its concretization contains this value:

$$ \begin{array}{c}
n \in \gamma(\alpha(n)) \\
\text{or equivalent} \\
X \subseteq \alpha(Y) \Rightarrow Y \subseteq \gamma(X)
\end{array}$$

Inversly, some of the concrete objects in $\gamma(m)$ should exhibit the abstract property $m$:

$$
\begin{array}{c}
p \in \alpha(\gamma(p)) \\
\text{or equivalent}\\
Y \subseteq \gamma(X) \Rightarrow X \subseteq \alpha(Y)
\end{array}
$$


This guarantees the _soundness_ of our approach. 

Without these properties tying α and γ together, abstract interpretation would be meaningless: the abstract computation would not be able to make statements about the concrete computations.
For example, working with the abstract domain of signs where α maps `0` onto `+` yields following results:

$$
    \alpha(n) = \left\{\begin{array}{lr}
       \code{-} & \text{if } n < 0\\
       \code{+} & \text{if } n = 0\\
       \code{+} & \text{if } n > 0 
     \end{array}\right\}
$$

$$
\begin{array}{l}
    \gamma(\code{+}) = \{ n | n > 0 \} \\
    \gamma(\code{-}) = \{ n | n < 0 \} \\
\end{array}
$$

What would `x - 1` give, where `x = +`? This is equivalent to 

$$
\begin{array}{rl}
 & \alpha(\gamma(\code{+}) - 1) \\
= & \alpha(\{n - 1 | n > 0\}) \\
= & \code{+} \\
\end{array}
$$ 

A blatant lie, of course; `0 - 1` is all but a positive number.


#### Galois connection

Together, α and γ form a monotone _Galois connection_, as it obeys its central property:

$$ \alpha(a) \subseteq b \Leftrightarrow a \subseteq \gamma(b) $$

\begin{figure}[h!]
\center
\input{Galois.tex}
\caption{Galois-connection, visualized}
\end{figure}
