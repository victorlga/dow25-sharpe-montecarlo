# dow25-sharpe-montecarlo

### 🛠 Build and run with profiling

#### 1. Clean and build with profiling:

```bash
cabal clean
cabal build --enable-profiling
```

#### 2. Run with runtime system (RTS) options for profiling:

* Basic time/space summary:

```bash
cabal run dow25-sharpe-montecarlo -- +RTS -s
```

* Generate **time profiling report**:

```bash
cabal run dow25-sharpe-montecarlo -- +RTS -p
```

* Generate **heap profile by cost center**:

```bash
cabal run dow25-sharpe-montecarlo -- +RTS -hc -p
```

After running with `-p`, check the generated file:

```bash
dow25-sharpe-montecarlo.prof
```

To visualize heap profiles (like `-hc`), you can use:

```bash
hp2ps dow25-sharpe-montecarlo.hp
open dow25-sharpe-montecarlo.ps
```

---

Would you like me to walk you through interpreting the `.prof` file once you run it?


