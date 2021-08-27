# Description

## Agents



Two types of agents are modelled in the simulations: proportional *tit-for-tat* agents (pTFT) and *Reinforcement Learning* agents (RLA).

### pTFT

 pTFT is a rule-based algorithm that chooses to play the high price with probability:
$prob(p=p_{high}) = \begin{cases} 1 ~~~~~~~\text{if}~ t=0 \\\\
\frac{j}{n-1} ~~\text{if}~ t>1 \end{cases}$
where $n$ is the number of players and $j \in \{0,1,2, ..., n-q\}$ is the number of players rival players who choose to play $p_{high}$.

### RLAs

RLA value actions based on the rewards they receive. This simulation deploys *Q-Learning* agents. They randomize their action in the first period of any supergame. Subsequently the choose an action $A_t \in \{p_{low}, p_{high}\}$ according to an $\epsilon$-greedy policy:

$$A_t = \begin{cases} arg ~ \stackrel{a}{max} Q_t(s,a)~~~~\text{with probability}~ 1-\epsilon \\\\
\text{randomize with probability}~ \epsilon \end{cases}$$

A state set $s$ comprises all actions $a$ played at $t-1$. The values of the $Q$-matrix are initialized with $0$. After prices and payoffs are collected, the reward $R_t$ is calculated as

$$R_t = \pi_t-p_{Nash}$$

 where $\pi_t$ is the payoff due to the played actions at $t$ and $p_{Nash}$ is the Nash solution in a static game. Thus, the reward is an assessment of profits *relative* to the Nash solution. After every time step, the *quality* of the played action is evaluated with this update formula:

$$Q_{t+1}(s,a)\leftarrow Q_t(s,a) + \alpha (\pi_t + \delta \stackrel{a}{max} Q_t(s',a) - Q_t(s,a))$$

##### Learning

RLAs have 100 supergames to learn. Every supergame comprises at least 20 time steps. Thereafter, the game continues with probability $0.7$. The last update in any supergame assumes that the value of the terminal state is  $0$, i.e. $Q_t(s_{terminal},a) = 0$ .

During the learning process, the parametrization is:

* $\epsilon=0.25$
* $\alpha = 0.25$ 
* $\gamma = 1$ 

##### Final supergame

After learning, there is a final supergame with learning and updates disabled, i.e. $\epsilon = \alpha = 0$ 



## Simulation Results

The script `01_execute_experiment.R` runs 6 treatments. For every treatment, there are 100 runs comprising 101 supergames (100 learning supergames, 1 final supergame).