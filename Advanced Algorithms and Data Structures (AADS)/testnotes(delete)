Max Flow
========

Problem definition
------------------

Find maximum flow in directed graph from source $s$ to sink $t$. More
formally, we have:

Flow network graph:

:   Directed graph $G = (V, E)$, without antiparallel edges or
    self-loops and with nodes $s$ (source) and $t$ (sink).

Capacity function:

:   $c(u, v) = \begin{cases} > 0 & \text{if $(u, v)
    E$} \\ 0 & \text{otherwise}\end{cases}$.

Flows:

:   $f : V \times V \to \mathbb{R}$ which satisfies the constraints:

Capacity constraint:

:   $\forall u, v \in V \quad 0 \leq f(u, v) \leq c(u, v)$

Flow conservation:

:   $\forall u \in V - {s, t} \quad \sum_{v \in V} f(v, u) = \sum_{v \in V} f(u, v)$

Flow value:

:   Find flow of maximum *value*:
    $|f| = \sum_{v \in V} f(s, v) - \sum_{v \in V} f(v, s)$

Ford-Fulkerson method
---------------------

The Ford-Fulkerson method uses residual networks to find “augmenting
paths” — paths along which can be sent more flow.

Initialize flow $f$ to 0 augment flow $f$ along $p$ $f$

### Residual Networks

A residual network is a graph $G_f$ which represents how we can change
the flow $f$ on the network graph $G$.

Given flow network $G = (V, E)$ and flow $f$, we have the **residual
capacity** $c_f(u, v)$: $$c_f(u, v) =
\begin{cases}
    c(u, v) - f(u, v) & (u, v) \in E \\
    f(v, u)           & (v, u) \in E \\
    0                 & \text{otherwise}
\end{cases}$$

### Augmentation of flow $f$ by $f'$

$$(f \uparrow f')(u, v) =
\begin{cases}
    f(u, v) + f'(u, v) - f'(v, u) & (u, v) \in E \\
    0                             & \text{otherwise}
\end{cases}$$ Important thing is that $|f \uparrow f'| = |f| + |f'|$.

Augmenting path has flow:
$f_p(u, v) = \begin{cases} c_f(p) & (u, v) \in p \\ 0 & \text{otherwise} \end{cases}$

Additionally: $|f_p| = c_f(p) > 0$

This implies: $|f \uparrow f_p| = |f| + |f_p| > |f|$

Example of network with residual network:

![image](flowexample.png) \[flowexample\]

Cuts
----

Partitions vertices into two groups with one containing $s$, the other
containing $t$.

Net flow of cut:
$$f(S, T) = \sum_{u \in S} \sum_{v \in T} f(u , v) - \sum_{u \in S} \sum_{v \in T} f(v , u) = |f|$$
Capacity of cut:
$$c (S , T) = \sum_{u \in S} \sum_{v \in T} c(u , v) \geq |f|$$

Max-flow min-cut theorem
------------------------

The following statements are equivalent:

1.  $f$ is a maximum flow in $G$.

2.  The residual network $G_f$ contains no augmenting paths.

3.  $|f| = c(S, T)$ for some cut $(S, T)$ of $G$.

$1 \Rightarrow 2$:

:   If augmenting path exists, $|f|$ can be increased by augmentation.

$2 \Rightarrow 3$:

:   Cut $(S, T)$ where $S$ is all vertices reachable from $s$ and $T$
    are the others. Then:\
    $\forall u \in S, v \in T \quad (u, v) \in E \Rightarrow f(u, v) = c(u, v)$
    since otherwise $(u, v) \in E_f$\
    $\forall u \in S, v \in T \quad (v, u) \in E \Rightarrow f(v, u) = 0$
    since otherwise $(u, v) \in E_f$\
    $\forall u \in S, v \in T \quad (u, v) \notin E \land (v, u) \notin E \Rightarrow f(u, v) = f(v, u) = 0$
    Hence: $$\begin{aligned}
            f(S , T) &= \sum_{u \in S} \sum_{v \in T} f(u , v) - \sum_{v \in T} \sum_{u \in S} f(v , u) \\
                     &= \sum_{u \in S} \sum_{v \in T} c(u , v) - \sum_{v \in T} \sum_{u \in S} 0 \\
                     &= c(S , T) = |f| & \text{by Lemma 26.4}
        \end{aligned}$$

$3 \Rightarrow 1$:

:   $|f| \leq c(S, T)$, therefore $|f| = c(S, T)$ must be maximal.

Edmonds-Karp algorithm
----------------------

Uses shortest unit-distance (breadth-first search) to decide augmenting
path.

### Running time

Each iteration takes $O(V + E) = O(E)$ time, so we need to simply limit
the number of iterations.

Forward edge in residual network: $(u, v) \in E_f$ such that
$\delta_f(s, v) = \delta_f(s, u) + 1$.

**Lemma:** $G_0$ is residual graph at some point. $f_1, \dots, f_k$ is
some sequence of flows obtained during the next $k$ iterations. Let
$d = \delta_{f_0}(s, t)$. Then, if $\delta_{f_i}(s, t) = d$ for
$i = 1, \dots, k - 1$, then flow is only pushed along forward edges in
$G_{f_0}$ and $\delta_{f_k}(s, t) \geq d$.

**Proof by induction on k:** Base: $k = 0$, true since no iterations are
done, $G_{f_0} = G_{f_k}$.

Step: Assume $k > 0$ and it holds for $k - 1$. The algorithm will push
flow along the shortest path $p$ in $G_{f_{k - 1}}$.

For every edge $(u, v)$ in a shortest path $p$ in $G_{f_{k - 1}}$ we
must have $\delta_{f_0}(s, v) \leq \delta_{f_0}(s, u) + 1$, since flow
has only been pushed along forward edges.

But since length of $p$ is $d$, we must have
$\delta_{f_0}(s, v) = \delta_{f_0}(s, u) + 1$ for every edge
$(u, v) \in p$.

Similarly, for any edge $(u, v)$ in any shortest path $p$ from $s$ to
$t$ in $G_{f_k}$ we must have
$\delta_{f_0}(s, v) \leq \delta_{f_0}(s, u) + 1$. Since $p$ must have at
least $d$ edges, $\delta_{f_k}(s, t) \geq d$. QED.

**Theorem:** Edmonds-Karp runs in $O(VE^2)$.

**Proof:** Each iteration takes $O(V + E) = O(E)$ time. Must show number
of iterations is $O(VE)$.

Maximum-length sequence of consecutive flows $f_0, \dots, f_k$ such that
$\delta_{f_i} = d$ for $i = 1, \dots, k - 1$, where
$d = \delta_{f_0}(s, t)$. By Lemma 1 $k \leq |E|$ since each iteration
removes an edge. Must also have $\delta_{f_k}(s, t) \geq d + 1$ by
maximality of the sequence.

Hence, $\delta(s, t)$ must increase by 1 at least every $|E|$
iterations. The distance can’t be more than $|V| - 1$. Therefore, number
of iterations $O(VE)$.

Other stuff
-----------

$$\begin{aligned}
 \left( f \uparrow f ^ { \prime } \right) ( u , v ) & = f ( u , v ) + f ^ { \prime } ( u , v ) - f ^ { \prime } ( v , u ) \\ & \geq f ( u , v ) + f ^ { \prime } ( u , v ) - f ( u , v ) \\ & = f ^ { \prime } ( u , v ) \\ & \geq 0 \end{aligned}$$

$$\begin{aligned}
 \left( f \uparrow f ^ { \prime } \right) ( u , v ) & = f ( u , v ) + f ^ { \prime } ( u , v ) - f ^ { \prime } ( v , u ) \\ & \leq f ( u , v ) + f ^ { \prime } ( u , v ) \\ & \leq f ( u , v ) + c _ { f } ( u , v ) \\ & = f ( u , v ) + c ( u , v ) - f ( u , v ) \\ & = c ( u , v ) \end{aligned}$$

![image](flowcon.png)

Linear Programming
==================

Motivation
----------

Often you have optimization problems under constraints. You’d like a
general algorithm to solve such problems and find the optimal solution
that fits all of the constraints.

Linear program forms
--------------------

Standard form: $$\begin{array}{lrclll}
    \text{maximize}   & \displaystyle\sum_{j = 1}^n c_j x_j    &      &      &                                & \text{or } c^\top x \\
    \text{subject to} &                                        &      &      &                                & \\
                      & \displaystyle\sum_{j = 1}^n a_{ij} x_j & \leq & b_i  & \text{for } i = 1, 2, \dots, m & \text{or } Ax \leq b \\
                      & x_j                                    & \geq & 0    & \text{for } j = 1, 2, \dots, n & \text{or } x \geq 0
\end{array}$$ A linear program: $$\begin{array}{lrclll}
    \text{minimize}   & -2x_1    & + & 3x_2 &      & \\
    \text{subject to} &          &   &      &      & \\
                      & x_1      & + & x_2  & =    & 7 \\
                      & x_1      & - & 2x_2 & \leq & 4 \\
                      & x_1      &   &      & \geq & 0
\end{array}$$ Same linear program in standard form:
$$\begin{array}{lrclllll}
    \text{maximize}   & 2x_1     & - & 3x_2 & + & 3x_3 &      &    \\
    \text{subject to} &          &   &      &   &      &      &    \\
                      & x_1      & + & x_2  & - & x_3  & \leq & 7  \\
                      & -x_1     & - & x_2  & + & x_3  & \leq & -7 \\
                      & x_1      & - & 2x_2 & + & 2x_3 & \leq & 4  \\
                      & x_1, x_2, x_3 & &   &   &      & \geq & 0
\end{array}$$ Same linear program in slack form:
$$\begin{array}{rrrrrrrrr}
z   & = &    &   & 2x_1 & - & 3x_2 & + & 3x_3 \\
x_4 & = & 7  & - & x_1  & - & x_2  & + & x_3  \\
x_5 & = & -7 & + & x_1  & + & x_2  & - & x_3  \\
x_6 & = & 4  & - & x_1  & + & 2x_2 & - & 2x_3
\end{array}$$

Simplex
-------

Algorithm is essentially just this:

1.  Find non-basic variable $x$ with positive coefficient

2.  Check how much you can increase $x$ without violating non-negative
    constraints

3.  Pick the constraint with the lowest (limiting) amount

4.  Rewrite constraint to make $x$ a basic variable (isolate $x$ in
    the constraint)

5.  Insert the new equation for $x$ into $x$’s place
    whereever (pivoting)

6.  $z$ now has a higher value with non-basic variables set to 0

7.  Repeat until no non-basic variables with positive coefficients

Unfortunately might take exponential time on contrived input. Usually
very effective though.

Duality
-------

To prove optimality, a concept of duality is used.

Switch coefficients and targets, switch $\leq$ to $\geq$, switch $x_j$
for $y_i$, switch $n$ for $m$. Then:

$$\begin{array}{lrclll}
    \text{minimize}   & \displaystyle\sum_{i = 1}^m b_i y_i    &      &      &                                & \\
    \text{subject to} &                                        &      &      &                                & \\
                      & \displaystyle\sum_{i = 1}^m a_{ij} y_i & \geq & c_j  & \text{for } j = 1, 2, \dots, n & \\
                      & y_i                                    & \geq & 0    & \text{for } i = 1, 2, \dots, m &
\end{array}$$

Then prove: $$\begin{aligned}
    \sum_{j = 1}^n c_j \bar{x}_j &\leq \sum_{j = 1}^n {\left( \sum_{i = 1}^m a_{ij} \bar{y}_i \right)} \bar{x}_j \\
                                 &=    \sum_{i = 1}^m {\left( \sum_{j = 1}^n a_{ij} \bar{x}_j \right)} \bar{y}_i \\
                                 &\leq \sum_{i = 1}^m b_i \bar{y}_i\end{aligned}$$

Formulating programs as linear programs
---------------------------------------

### Shortest paths

$$\begin{array}{lrcll}
    \text{maximize}   & d_t      &      &               & \\
    \text{subject to} &          &      &               & \\
                      & d_v      & \leq & d_u + w(u, v) & \text{for each edge } (u, v) \in E \\
                      & d_s      & =    & 0             &
\end{array}$$

### Maximum flow

$$\begin{array}{lrcll}
    \text{maximize} & \displaystyle\sum_{v \in V} f_{sv} & -    & \displaystyle\sum_{v \in V} f_{vs} \\
    \text{subject to} \\
    & f_{uv}                             & \leq & c(u, v)                            & \text{for each } u, v \in V \\
    & \displaystyle\sum_{v \in V} f_{uv} & =    & \displaystyle\sum_{v \in V} f_{vu} & \text{for each } u \in V - {\left\{ s, t \right\}} \\
    & f_{uv}                             & \geq & 0                                  & \text{for each } u, v \in V
\end{array}$$

Randomized Algorithms
=====================

Randomized algorithms are algorithms that have some randomized component
to them. Using randomization, it’s possible for some algorithms to
exhibit very favourable running time and/or correctness in the
*expected* case. There are two main types of randomized algorithms:

Las Vegas:

:   Always returns correct result, may take a long time.

Monte Carlo:

:   Always runs fast, may not be correct.

Example Las Vegas: RandQS
-------------------------

Random quicksort works by choosing pivots randomly. We want to get an
expected running time of this algorithm.

Let $S$ be the array in sorted order. Let $S_i$ be the $i$th element of
$S$, i.e. the $i$th smallest element. We count the number of comparisons
the algorithm performs in order to give its bound. Let $X_{ij}$ be an
indicator variable that is 1 if $S_i$ and $S_j$ are compared in the
algorithm. Number of comparisons are then:
$$\sum_{i = 1}^n \sum_{j > i}^n X_{ij}$$ We want the expected value of
comparisons: $$\begin{aligned}
    {\mathbb{E}{{\left[ \sum_{i = 1}^n \sum_{j > i}^n X_{ij} \right]}}} &= \sum_{i = 1}^n \sum_{j > i}^n {\mathbb{E}{{\left[ X_{ij} \right]}}} \\
    &= \sum_{i = 1}^n \sum_{j > i}^n \frac{2}{j - i + 1}\end{aligned}$$
Last line follows because they are only compared if they are chosen
among $S_i, \dots, S_j$ sequence. $$\begin{aligned}
    \sum_{i = 1}^n \sum_{j > i}^n \frac{2}{j - i + 1} &\leq \sum_{i = 1}^n \sum_{k = 1}^{n - i + 1} \frac{2}{k} \\
    &\leq 2 \sum_{i = 1}^n \sum_{k = 1}^{n} \frac{1}{k} \\
    &= 2nH(n) \\
    &= O(n \ln n)\end{aligned}$$ Thus expected run-time (number of
comparisons) is optimal.

Example Monte Carlo: Randomized Min-Cut
---------------------------------------

Randomly choose an edge to contract. Firstly, min-cut is never reduced
by a contraction. Secondly, we may not get any optimal min-cut, since if
an edge from a min-cut is chosen, it will be eliminated.

Probability of not choosing any edge in a particular min-cut $C$ of size
$k$ at step $i$ is $\Pr{{\left[ \mathcal{E}_i \right]}}$. There are at
least $kn/2$ edges because otherwise there is some vertex with less than
$k$ edges, which is then a min-cut. At step $i$ there are at least
$k(n - i + 1)/2$ edges left. Chance of not picking one of the $k$ edges
of $C$ is then: $$\mathcal{E}_1 = 1 - \frac{k}{nk/2} = 1 - \frac{2}{n}$$
Generally for the next iterations:
$$\mathcal{E}_i = 1 - \frac{k}{k(n - i + 1)/2} = 1 - \frac{2}{n - i + 1}$$
Chance of *not* picking one of these edges at any step is then:
$$\Pr{{\left[ \bigcap_{i = 1}^{n - 2} \mathcal{E}_i \right]}} \geq \prod_{i = 1}^{n - 2} {\left( 1 - \frac{2}{n - i + 1} \right)} = \frac{2}{n {\left( n - 1 \right)}} \geq \frac{2}{n^2}$$
We repeat the algorithm $n^2/2$ times. The chance of not finding any
min-cut in any of the runs is then:
$${\left( 1 - \frac{2}{n^2} \right)}^{n^2/2} < \frac{1}{e}$$ Further
runs decrease this chance even more at the expense of more running time.

For decision problems, there are two kinds of Monte Carlo: One-sided
error and two-sided error. One-sided only has error on one case,
two-sided has error on both.

Hashing
=======

Motivation
----------

We have a large universe $U$ that we wish to map randomly to a smaller
range $[m] = {\left\{ 0, \dots, m - 1 \right\}}$. To do this perfectly
would require enormous amounts of memory, so instead, we store a bit of
random information which helps us to do it almost perfectly.

Hash functions
--------------

To do this mapping, we use hash functions:
$$h \colon U \to {\left[ m \right]}$$ Where $h$ is a random variable in
the class of all functions from $U$ to ${\left[ m \right]}$.

For a hash to be useful it should give each value a seemingly random
hash — and in doing so, preferably it should have a low *collision
chance*. That is, for two keys $x, y$, very rarely should $h(x) = h(y)$.

Universality
------------

To formalise the idea of rare collision chance, we talk about
*universal* hash functions. These are hash functions where for keys
$x, y$ chosen independently at random, we have:
$$\Pr {\left[ h{{\left( x \right)}} = h{{\left( y \right)}} \right]} \leq \frac{1}{m}$$
Sometimes we only get close to this property, in which case we may have
*c-universality*:
$$\Pr {\left[ h{{\left( x \right)}} = h{{\left( y \right)}} \right]} \leq \frac{c}{m}$$

### Strong Universality

We call a hash function *strongly universal* if, for two distinct keys
$x, y$ and for two hashes $q, r$, we have:
$$\Pr{{\left[ h(x) = q \land h(y) = r \right]}} = \frac 1{m^2}$$ Strong
universality can be generalized to *k-independence*, where strong
universality is simply 2-independence.

Also universal because:
$$\Pr [ h ( x ) = h ( y ) ] = \sum _ { q \in [ m ] } \Pr [ h ( x ) = q \wedge h ( y ) = q ] = m / m ^ { 2 } = 1 / m$$

Application: Hash table
-----------------------

We have $S \subseteq U$ that we wish to store and retrieve single key of
in expected constant time. We have $|S| = n$ and $n \leq m$. Then pick
hash function $h \colon U \to {\left[ m \right]}$ and create array $L$
containing $m$ lists. Then we can find keys in $S$ by looking in
$L{{\left[ h{{\left( x \right)}} \right]}}$. Now want to show that
$L{{\left[ h{{\left( x \right)}} \right]}}$ has expected size $1$.

Assume that $x \notin S$ for worst case. Assume $h$ is universal. $I(y)$
is 1 when $h(x) = h(y)$. Then the expected length of
$L{{\left[ h{{\left( x \right)}} \right]}}$ is:
$$\mathbb{E}{{\left[  | L{{\left[ h{{\left( x \right)}} \right]}} |  \right]}} = \mathbb{E}{{\left[  \sum_{y \in S} I(y)  \right]}} = \sum_{y \in S} \mathbb{E}{{\left[ I(y) \right]}} = \sum_{y \in S} \frac{1}{m} = \frac nm \leq 1$$

Application: Coordinated Sampling
---------------------------------

Used for sampling randomly from too big databases. Able to estimate
things about the original from the sample. For example,
$\bar{|A|} = |S_{h, t}(A)| m/t$. This is because:

$$S_{h, t}(A) = {\left\{ x \in A | h(x) < t \right\}}$$

$$\mathbb{E}{{\left[ |S_{h, t}| \right]}} = |A| \frac{t}{m}$$

We also have the following:

$$\Pr{{\left[ |X - \mu| \geq q \sqrt{\mu} \right]}} \leq \frac 1{q^2}$$

van Emde Boas Trees
===================

Motivation
----------

The lower bound of sorting makes it so priority queues must have an
$O(\lg n)$ operation. However, we can sort in linear time given
assumptions on the input, so maybe we can also have faster priority
queues given the same assumptions.

We would like a data structure capable of providing $O(\lg \lg u)$
running times for member, insertion, deletion and finding successor and
predessesor, where $u$ is the size of the universe, that is, the values
we can store.

**Binary tree:** Binary tree imposed onto an array. This allows
$O(\lg u)$.\
**Square root tree:** Tree of constant height by summary array of length
$\sqrt{u}$. Running time $O(\sqrt{u})$ (worse!). Also puts restrictions
on the size of $u = 2^{2k}$.

The van Emde Boas Tree
----------------------

A van Emde Boas tree consists of some stored information and several
clusters which are themselves van Emde Boas trees. Each tree has a
universe size $u$. $u = 2^k$ is always a power of two. Denote helper
functions: $$\begin{aligned}
    \sqrt[\uparrow]{u}   &= 2^{\lceil (\lg u) / 2 \rceil} \\
    \sqrt[\downarrow]{u} &= 2^{\lfloor (\lg u) / 2 \rfloor} \\
    \text{high}(x)       &= \lfloor x / \sqrt[\downarrow]{u} \rfloor & \text{(cluster number)} \\
    \text{low}(x)        &= x \text{ mod } \sqrt[\downarrow]{u} & \text{(position within cluster)} \\
    \text{index}(x, y)   &= x \sqrt[\downarrow]{u} + y & \text{(reconstructs number)}\end{aligned}$$
We have $x = \text{index}(\text{high}(x), \text{low}(x))$. A van Emde
Boas tree stores:

Universe size:

:   $u = 2^k$

Summary, if $u > 2$:

:   A van Emde Boas tree of size $\sqrt[\uparrow]{u}$ summarising the
    contents of the clusters.

Min and Max:

:   The minimum and maximum values of the tree. The minimum value is not
    included in the clusters.

Cluster array:

:   $\sqrt[\uparrow]{u}$ many van Emde Boas tree of size
    $\sqrt[\downarrow]{u}$.

vEB-Empty-Tree-Insert(V, x) swap(x, V.min) vEB-Tree-Insert(V.summary,
high(x)) vEB-Empty-Tree-Insert(V.cluster\[high(x)\], low(x))
vEB-Tree-Insert(V.cluster\[high(x)\], low(x)) V.max = x

Characterized by: $$\begin{aligned}
    T(u)   &\leq T(\sqrt[\uparrow]{u}) + O(1) \\
    T(2^m) &\leq T(2^{\lceil m / 2 \rceil}) + O(1) \\
           &\leq T(2^{2m / 3}) + O(1) \\
    S(m)   &\leq S(2m/3) + O(1) \\
    T(u)   &= T(2^m) = S(m) = O(\lg m) = O(\lg \lg u)\end{aligned}$$

NP-completeness
===============

To define NP-completeness, I first need to define a couple of other
things first.

Languages
---------

A **language** $L$ over an **alphabet** $\Sigma$ is a set of strings
made up of symbols from $\Sigma$.

A decision problem $Q$ defines a language $L$ over the alphabet
$\Sigma = {\left\{ 0, 1 \right\}}$, in that it contains all the strings
$x$ where $Q(x) = 1$:
$$L = {\left\{ x \in \Sigma^* : Q(x) = 1 \right\}}$$ We say that a
language $L$ is **accepted** by an algorithm $A$ if $A(x) = 1$ for all
$x \in L$. The algorithm $A$ **rejects** the language if $A(x) = 0$.

A language $L$ is **decided** by an algorithm $A$ if for all $x \in L$,
$A(x) = 1$ and for all $y \notin L$, we have $A(x) = 0$ (i.e. does not
loop forever).

P and NP
--------

Now define the complexity class $P$:
$$\text{P} = {\left\{ L \subseteq {\left\{ 0, 1 \right\}}^* : \text{there exists an algorithm $A$ that decides $L$ in polynomial time.} \right\}}$$
Now define NP as the class of languages that can be *verified* in
polynomial time. Then a language belongs to NP when it upholds:
$$L = {\left\{ x \in {\left\{ 0, 1 \right\}}^* : \exists y \text{ where $|y| = O{{\left( |x|^c \right)}}$ such that $A{{\left( x, y \right)}} = 1$} \right\}}$$

Reducibility
------------

A language $L_1$ is polynomial-time reducible to a language $L_2$,
written $L_1 \leq_\text{p} L_2$ if there exists a polynomial-time
computable function
$f : {\left\{ 0, 1 \right\}}^* \to {\left\{ 0, 1 \right\}}^*$ such that:
$$\forall x \in {\left\{ 0, 1 \right\}}^* \quad x \in L_1 \Leftrightarrow f(x) \in L_2$$

![image](reduction.png)

NP-completeness
---------------

We can now define the NP-complete class:

1.  $L \in \text{NP}$, and

2.  $\forall L' \in \text{NP} \quad L' \leq_\text{P} L$

Method for showing NP-completeness:

1.  Prove $L \in$ NP

2.  Select known NP-complete language $L'$

3.  Describe $f$ which maps $L'$ to $L$

4.  Prove that $x \in L' \Leftrightarrow f(x) \in L$

5.  Prove that $f$ is polynomial-time computable

NP-complete proofs:

$L \leq_\text{P} \text{CIRCUIT-SAT}$:

:   By simulating abstract machine as logic circuit.

$\text{CIRCUIT-SAT} \leq_\text{P} \text{SAT}$:

:   By assigning a variable to every wire, then constructing a logic
    formula with the output wire and’ed with ((the other wires) if and
    only if (the wires input)). For example, $x_1 \lor x_2 = x_3$ gives
    $x_3 \land (x_3 \Leftrightarrow (x_1 \lor x_2))$.

$\text{SAT} \leq_\text{P} \text{3-CNF-SAT}$:

:   Construct parse tree of the SAT formula, then assign variables to
    each output. Construct same kind of formula as in SAT. Then look at
    truth table for each clause and construct 3-CNF formula for each
    clause by negating the 3-DNF formula.

$\text{3-CNF-SAT} \leq_\text{P} \text{CLIQUE}$:

:   Construct graph with a node for each literal in every clause.
    Connect nodes that are (1) not in the same clause and (2) are not
    the negation of each other.

    If $\phi$ is satisfiable, then pick the literals in the clauses that
    are 1, then those form a clique of size $k$. There must be edges
    between them because they can’t be complements since they are all 1.

    If $G$ has a clique of size $k$ then that clique must have a vertex
    in every triple, since there are no edges between vertices in the
    same triple. Then assign 1 to each such vertex. We know we won’t be
    assigning 1 to both a literal and its complement since no edges are
    between them. Set variables not in the clique arbitrarily.

$\text{CLIQUE} \leq_\text{P} \text{VERTEX-COVER}$:

:   Consider a graph $G$ with a clique $V'$ of size $k$. Construct
    complement, $\bar{G}$. Then vertex cover is $V - V'$.

    Take any edge in $\bar{G}$. One of the nodes must be in $V - V'$,
    since they both can’t be in $V'$, cause then the edge would
    not exist. So that edge is covered by $V - V'$ which has size
    $|V| - k$ and same can be said for every other edge, therefore it is
    a vertex cover.

    Take any two vertices not in vertex cover $V'$ of $\bar{G}$ with
    size $|V| - k$. Then the vertices must share an edge in $G$ since
    otherwise $V'$ would not be a vertex cover. Hence $V - V'$ is a
    clique in $G$ since they must all have edges between them. Size is
    $|V| - |V'| = |V| - (|V| - k) = k$.

$\text{VERTEX-COVER} \leq_\text{P} \text{HAM-CYCLE}$:

:   Not curriculum.

$\text{HAM-CYCLE} \leq_\text{P} \text{TSP}$:

:   Consider graph $G$ with hamiltonian cycle $h$. Then construct
    complete TSP graph $G'$ where cost of an edge is 0 if it was in the
    old graph and 1 if it wasn’t in the old graph.

    Then if $G$ has a hamiltonian cycle, then $G'$ must have a TSP tour
    of cost 0, since it’s just the edges of $h$.

    If $G'$ has a tour of cost 0, then each edge on the tour must have
    cost 0, which means that all those edges are in $G$. Therefore there
    must also be a hamiltonian cycle in $G$.

$\text{3-CNF-SAT} \leq_\text{P} \text{SUBSET-SUM}$:

:   Draw huge table with variables and clauses as columns and literals
    and slack variables as rows. Then blah blah. Fuck me if they ask
    about this one.

Exact exponential algorithms and\
parameterized complexity
=================================

Motivation
----------

There are many very useful problems that we know to be NP-complete and
thus we cannot hope to solve them in polynomial time. However, sometimes
we just have to solve these difficult problems anyway and thus we’d like
algorithms that make the best of the running time constraints we have.

Exact exponential algorithms: Dynamic Travelling Salesman
---------------------------------------------------------

Given some cities $c_1, \dots, c_n$ and distances between each one, find
minimal permutation such that:
$$\sum_{i = 1}^{n - 1} {\left[ d {\left( c_{\pi (i)}, c_{\pi (i + 1)} \right)} \right]} + d {\left( c_{\pi (n)}, c_{\pi (1)} \right)}$$
**Naive Approach:** Try all permutations. There are $n!$, so $O(n!)$.

**Better idea:** Use dynamic programming.
$$S \subseteq {\left\{ c_2, \dots, c_n \right\}}$$
$$OPT {\left[ S, c_i \right]} = \text{Minimum length of tour starting in $c_1$, visiting all in $S$, ending in $c_i$}$$
Compute $OPT {\left[ S, c_i \right]}$ in increasing cardinality of $S$
(i.e:\
$OPT {\left[ \{ c_2 \}, c_2 \right]}$,
$OPT {\left[ \{ c_3 \}, c_3 \right]}$, $\dots$,
$OPT {\left[ \{ c_n \}, c_n \right]}$,
$OPT {\left[ \{ c_2, c_3 \}, c_2 \right]}$,)

Trivial if $|S| = 1$: Simply
$OPT {\left[ S, c_i \right]} = d(c_1, c_i)$.

Use the previous tours to find next tour, due to (assuming $c_j$ is
right before $c_i$):
$$OPT {\left[ S, c_i \right]} = OPT {\left[ S \setminus c_i, c_j \right]} + d {\left( c_j, c_i \right)}$$
This leads to:
$$OPT {\left[ S, c_i \right]} = \min {\left\{ OPT {\left[ S \setminus {\left\{ c_i \right\}}, c_j \right]} + d {\left( c_j, c_i \right)} : c_j \in S \setminus {\left\{ c_i \right\}} \right\}}$$
Lastly, we want:
$$\min {\left\{  OPT {\left[ {\left\{ c_2, c_3, \dots, c_n \right\}}, c_i \right]} + d {\left( c_i, c_1 \right)} : i \in {\left\{ 2, 3, \dots, n \right\}} \right\}}$$

$OPT {\left\{ c_i, c_i \right\}} = d {\left( c_1, c_i \right)}$
$OPT {\left[ S, c_i \right]} = \min {\left\{ OPT {\left[ S \setminus {\left\{ c_i \right\}}, c_k \right]} + d {\left( c_k, c_i \right)} : c_k \in S \setminus {\left\{ c_i \right\}} \right\}}$
$\min {\left\{  OPT {\left[ {\left\{ c_2, c_3, \dots, c_n \right\}}, c_i \right]} + d {\left( c_i, c_1 \right)} : i \in {\left\{ 2, 3, \dots, n \right\}} \right\}}$

Paremeterized algorithms: Vertex Cover\
aka “Bar Fight Prevention”
---------------------------------------

For certain problems, we might know some constraints on the kind of
solution we want. This is called parameterization. We give the algorithm
some extra information and in return we can design the algorithm in a
clever way such that the exponential parts of it only depend on the
parameters we want it to depend on.

### Parameterize size $k$ of vertex cover

By parameterizing the size of the vertex cover solution, we can get a
potentially much more efficient algorithm for it:

**Core idea:** Iterate through edges $(u, v)$, deciding whether $u$ or
$v$ should be in the cover or not. Call recursively two times, one where
$u$ is picked and one where $v$ is picked.

Leads to decision tree of depth $k$, therefore making $O(2^k)$ recursive
calls, each taking $O(m)$ time. $m$ is bounded by $nk/2$ because we can
easily include any vertices in the cover that has more than $k$ edges.
So the running time is $O(n k 2^k)$.

![image](bargraph.png)

![image](bartree.png)

This is an example of a *fixed-parameter algorithm* because it has the
form $f(k) \cdot n^c$, i.e. the algorithm is constantly polynomial in
$n$ but may be exponential or worse in $k$.

Approximation algorithms
========================

Motivation
----------

Sometimes we have to solve NP-complete problems, but unfortunately this
can take a very long time. In these cases, we must either wait for the
computation or we must be satisfied with an approximate solution which
can be calculated fast. This is what we use approximation algorithms
for.

We talk about approximation ratios:
$$\max{\left(  \frac{C^*}{C}, \frac{C}{C^*}  \right)} \leq \rho(n)$$

Approximate travelling salesman problem with triangle inequality
----------------------------------------------------------------

Algorithm goes like this:

1.  Select a vertex to be the root

2.  Compute minimum spanning tree from root by Prim’s algorithm

3.  Get a vertex ordered by first visit in preorder tree walk of the
    tree

4.  Return that order

Algorithm is quite obviously polynomial.

![image](tspExample.png)

The cost of a minimum spanning tree must be lower or equal to the cost
of the tree gotten by removing an edge from the optimal tour.
$$c(T) \leq c(H^*)$$ A full walk is double the cost of the tree since it
goes through every edge twice $$c(W) = 2c(T)$$ Which means:
$$c(W) = 2c(T) \leq 2c(H^*)$$ The algorithm works by simply removing the
unnecessary double visits to the nodes and thus making the walk shorter,
resulting in a tour $H$. This only works because of the triangle
inequality. Then we have: $$c(H) \leq c(W) = 2c(T) \leq 2c(H^*)$$ Hence
it is a 2-approximation algorithm.

Approximate MAX-3-CNF
---------------------

Now we talk about *expected cost*. Want to find the maximum number of
clauses we can satisfy in a 3-CNF formula with $m$ clauses.

Algorithm simply sets each variable by a coin flip. Let $Y_i$ be the
indicator variable of the $i$th clause being satisfied.

Chance of a clause not being satisfied is the same as all 3 variables
set to 0, i.e: $(1/2)^3$. Then, chance of a clauses being satisfied is:
$$\Pr {\left[ Y_i = 1 \right]} = 1 - (1/2)^3 = 7/8$$ Expected number of
satisfied clauses is then: $$\begin{aligned}
    \mathbb{E}{\left[ Y \right]} &= \mathbb{E}{\left[ \sum_{i = 1}^m Y_i \right]} \\
                          &= \sum_{i = 1}^m \mathbb{E}{\left[ Y_i \right]} \\
                          &= \sum_{i = 1}^m 7/8 \\
                          &= 7m/8\end{aligned}$$ You can have at most
$m$ satisfied clauses, hence approximation ratio is at most:
$$\frac{m}{7m/8} = 8/7$$

Other approximations
--------------------

Vertex cover:

:   Take random edge every time and add both vertices to the cover,
    removing their edges. Can at most be twice as bad since every edge
    must be covered and you take two vertices per edge.

Set-cover:

:   Greedily pick the set with most elements. Remove elements
    and continue. Gives
    $\rho{(n)} = H{{\left( \max{{\left\{ |S| : S \in \mathcal{F} \right\}}} \right)}}$.

Weighted vertex cover by linear programming:

:   Construct integer linear program where you minimize sum of weights
    times values. Every edge $(u, v)$ must have the sum of their
    vertices be at least 1. Now relax the problem (allow floating
    numbers), then take the vertices with more than 1/2 value. Then it
    is a 2-approximation algorithm.

Subset-sum:

:   Create list of sums, but trim within a percentage. Fully polynomial
    approximation scheme. This means that it runs polynomially in both
    $1/\epsilon$ and $n$.

![image](text.png)
