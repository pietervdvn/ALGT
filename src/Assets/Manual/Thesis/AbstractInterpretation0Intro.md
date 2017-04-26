
Syntax Driven Abstract Interpretation
======================================

In this section, we transform a metafunction -a function on a parsetree- into a metafunction working on (possibly infinite) set of parsetrees.
This is the main contribution of the dissertation, as it is a novel technique to automatically reason about programming languages. Furthermore, these analysises will be used to build gradualization.


 - First, we'll work out **what abstract interpretation is**, with a simple example followed by its desired properties.
 - Then, we work out what **properties a syntax** should have. 
 - With these, we develop a **efficient representation** to capture infinite sets of parsetrees.
 - Afterwards, **operations on these setrepresentations are stated**.
 - As last, we build usefull **algorithms** and checks with this algebra.

What is abstract interpretation?
--------------------------------

_Abstract interpretation_ is a collection of techniques to derive properties about programs, based on sound approximation. However, per Rice's theorem, it is generally impossable to make exact statements about every possible program.

Furhtermore, it is often impractical to calculate the value we might return. Therefore, we merely calculate for a given program what property might hold. 
Essential in our approach is that these properties can be combined in a monotone way. Often, the properties we work with, will have the form of a lattice.

Examples of abstract interpretation domains are:

- Working with the sign of functions
- Working with an upper or lower bound on integer functions
- Working with a set of possible returned values, called _collecting semantics_

In contrast to _abstract interpretation_, is _concrete_ interpretation; this would be running the program. 

The last important aspect of abstract interpretation is the translation of one property domain into another, often a costly -but more precise- domain into a cheaper and less precise domain. This functions is called _abstraction_ (_α_). The inverse function is _concretization_ (_γ_). Abstraction and concretization form a Galois-connection.

First, we work out a simple example to get some feeling about the technique. Then we describe desired properties of abstraction α and concretization γ.


### Successor example


#### Concrete interpretation

Consider the extremely simple example function `succ`:

	succ	: Number -> Number
	succ x	= x + 1


#### Working with the sign of functions

For this analysis, we are only interested in what sign our example might potentially return (thus `+`,`-`, `0`). We don't want to calculate each value of course.

Converting a concrete value into a signset (abstracting to the property domain) is straightforward: 

$$
    \alpha(n) = \left\{\begin{array}{lr}
       \{\code{-}\} & \text{if } n < 0\\
       \{\code{0}\} & \text{if } n = 0\\
       \{\code{+}\} & \text{if } n > 0 
     \end{array}\right\}
$$


Calculating the abstraction of a set boils down to calculating the abstract of each element and taking them all together. We will often use these definitions intermixed.

$$
	\alpha(N) = \bigcup\{ \alpha(n) | n \in N \}
$$


Calculating the set containing all values for a certain property value, is done with following _concretization function_:

$$
\begin{array}{l}
    \gamma(\code{+}) = \{ n | n > 0 \} \\
    \gamma(\code{0}) = \{ 0 \} \\
    \gamma(\code{-}) = \{ n | n < 0 \} \\
\end{array}
$$


But how to combine two signs? For example, it might turn out that a function might return values of both `0` or `+`. To cope with this, we will work with _sets_, which compose very naturally.

$$
compose(N, M)	= N \cup M
$$

Again is taking the concretization of a set the union of concretizations of the elements:

$$
	\gamma(P) = \bigcup\{ \gamma(p) | p \in P \}
$$


\begin{figure}[h!]
\center
\input{AbstractionMinus.tex}
\caption{Concretization and abstraction relation for translation between numbers and signs}
\end{figure}

Now, we'd want to know what sign we would yield for `succ +`, `succ 0` and `succ -`. For input property `P`, this is captured by 
$$\alpha(\{\code{succ}(n) | n \in \gamma(\code{P})\})$$. In other word, we calculate the successor for each `n` in the category and then abstract the property.

For example, if we want to now what sign `succ 0` yields, we calculate:

$$
\begin{array}{rl}
 & \alpha(\{\code{succ}(n) | n \in \gamma(\code{0})\}) \\
= & \alpha(\{\code{succ}(n) | n \in \{0\}\}) \\
= & \alpha(\{\code{succ}(0)\}) \\
= & \alpha(\{1\}) \\
= & \{+\}
\end{array}
$$

If we would want to know what sign we would yield for a positive number, calculating $$\alpha(\{\text{succ}(n) | n \in \gamma(+)\})$$ is intractable. Luckily can apply the properties of `+` to calculate the property abstractly. The property we'll uses here, is that the sum of two positive numbers, is positive.

Instead of working with concrete values, we use our sign property as _symbol_, and run our program with that:

$$
\begin{array}{rl}
 & succ \code{+} \\
= & \code{n} + \alpha(1) \\
= & \code{+} + \alpha(1) \\
= & \code{+} + \code{+} \\
= & \code{+}
\end{array}
$$

Analogously, we might repeat this with `-`. Sadly, we cannot conclude anything usefull out of this analysis; as the sum of zero and a positive number is positive, whereas the sum of a negative number and a positive number might be negative, zero or positive.

Our abstract evaluation would look as following:

$$
\begin{array}{rl}
 & succ \code{-} \\
= & \code{n} + \alpha(1) \\
= & \code{-} + \alpha(1) \\
= & \code{-} + \code{+} \\
= & \{\code{-,0,+}\}
\end{array}
$$

In summary, for the sign function, we can conclude that `succ` behaves as following:

$$
\begin{array}{rll}
\code{succ}(\code{-}) & = & \{-,0,+\} \\
\code{succ}(\code{0}) & = & \{+\} \\
\code{succ}(\code{+}) & = & \{+\} \\
\end{array}
$$


#### Working with ranges

Another possibility is to keep track of the range a value might be. Our abstract property is now denoted as `[n, m]` (where $n \leq m$). To go from a concrete value to a range, we use the following abstraction function:

$$\alpha(n) = \code{[n, n]} $$

Going the other way with the concretization function, is quite predictable:

$$\gamma(\code{[n, m]}) = \{ x | n \leq x \wedge x \leq m \}$$


The last question is how to compose two ranges. A function might return values between either range `[n1, m1]` or `[n2, m2]`. 


$$
compose(\code{[n1, m1]}, \code{[n1, m2]}) = \code{[} min(\code{n1}, \code{n2}), max(\code{m1}, \code{m2}) \code{]}
$$

If we have an abstract representation (e.g. `[2,5]`) for what range `succ` would return, we can calculate this:

$$
\begin{array}{rl}
 & \code{succ [2,5]} \\
 = & \code{[2,5]} + \alpha(1) \\
 = & \code{[2,5]} + \code{[1,1]} \\
 = & \code{[3,6]}
\end{array}
$$


#### Working with collecting semantics

At last, we can keep track of _all_ possible values through the calculation. For example, if the input might be `{1,2,41}`, we might run our program _on all of these values_. At first glance, this is ridiculous. Why not run the program three times? However, this can be usefull, as this set representation might allow for efficient internal representation or to deduce other properties.

The abstraction, concretization and composition functions are trivial:

$$
\begin{array}{lcl}
\alpha(n) & = & \{ n \} \\
\gamma(\{n1, n2, \ldots\} & = & \{n1, n2, ... \} \\
compose(N, M) & = & N \cup M
\end{array}
$$

Calculating the result for the example `{1,2,41}` gives:

$$
\begin{array}{rl}
 & \code{succ \{1,2,41\}} \\
= & \code{\{1,2,41\}} + \alpha(1) \\
= & \code{\{1,2,41\}} + \code{\{1\}} \\
= & \code{\{2,3,42\}}
\end{array}
$$

Note that using the collecting semantics with a set, containing a single value, is exactly the concrete interpretation. This is guaranteed by the underlying deterministic semantics.

### Desired properties

The functions α and γ should obey to some properties to make this approach work, namely _monotonicity_ and _correctness_. It turns out that these properties form a **Galois connection** between the concrete values and the property domain of choice.

#### Monotonicity of α and γ

The first requirement is that both _abstraction_ and _concretization_ are monotone. This states that, if the set to concretize grows, the set of possible properties_might_ grow, but never shrink.

Analogously, if the set of properties grows, the set of concrete values represented by these properties might grow too.


$$
\begin{array}{c}
X \subseteq Y \Rightarrow \gamma(X) \subseteq \gamma(Y) \\
X \subseteq Y \Rightarrow \alpha(X) \subseteq \alpha(Y) \\
\end{array}
$$


When working with signs as properties, this property can be illustrated with:

$$\{1,2\} \subseteq \{0,1,2\} \Rightarrow \{\code{+}\} \subseteq \{\code{0}, \code{+}\}$$

#### Soundness

When we transform a concrete value into a property, we expect that property actually represents this value. A property represents a concrete value iff its concretization contains this value. This gives another important property: 

$$ \begin{array}{c}
n \in \gamma(\alpha(n)) \\
\text{or equivalent} \\
X \subseteq \alpha(Y) \Rightarrow Y \subseteq \gamma(X)
\end{array}$$

On the other hand, if we calculate which concrete values correspond with a certain property _p_, we expect some of these values to exhibit property _p_:

$$
\begin{array}{c}
p \in \alpha(\gamma(p)) \\
\text{or equivalent}\\
Y \subseteq \gamma(X) \Rightarrow X \subseteq \alpha(Y)
\end{array}
$$


This guarantees the _soundness_ of our approach. 

Consider we would not have this guarantee about α and γ, our approach would fail. As example, we change the functions which map numbers onto their sign, but we map `0` onto the negative range, _without changing concretization_ :

$$
    \alpha(n) = \left\{\begin{array}{lr}
       \{\code{-}\} & \text{if } n < 0\\
       \{\code{+}\} & \text{if } n = 0\\
       \{\code{+}\} & \text{if } n > 0 
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
