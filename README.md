# Dow25 Sharpe Ratio Portfolio Optimizer

## Table of Contents
- [Overview](#overview)
- [Project Description](#project-description)
  - [Key Components](#key-components)
  - [Optimization Strategy](#optimization-strategy)
  - [Parallel Processing Approach](#parallel-processing-approach)
  - [Performance Considerations](#performance-considerations)
- [Installation](#installation)
  - [Prerequisites](#prerequisites)
  - [Setting Up](#setting-up)
- [Usage](#usage)
  - [Running with Cabal](#running-with-cabal)
  - [Running with VS Code Dev Container](#running-with-vs-code-dev-container)
- [Configuration Options](#configuration-options)
- [Input Data](#input-data)
- [Output](#output)
- [Project Structure](#project-structure)
- [Results] (#results)
  - [Parallel Processing Performance Analysis](#parallel-processing-performance-analysis)
  - [2024 vs 2025 Sharpe Comparison]
- [AI Usage Disclosure](#ai-usage-disclosure)

## Overview

This Haskell application performs portfolio optimization using Monte Carlo simulation to find the combination of Dow Jones stocks that maximizes the Sharpe ratio. The program processes historical price data (from August to December 2024), calculates financial metrics, and leverages parallel computing to efficiently search the solution space.

## Project Description

The portfolio optimizer finds the optimal allocation of weights across a selection of Dow Jones stocks to maximize risk-adjusted returns, as measured by the Sharpe ratio. The Sharpe ratio measures excess return per unit of risk, making it an ideal metric for portfolio optimization.

### Key Components

The program follows these high-level steps:

1. **Data Loading**: Parses a CSV file containing historical stock prices of Dow Jones components
2. **Return Calculation**: Converts price data into daily returns for each stock
3. **Covariance Computation**: Creates a covariance matrix to capture price movement relationships
4. **Portfolio Generation**: Uses a divide-and-conquer strategy to:
   - Generate all possible combinations of stocks (subsets of the Dow Jones index)
   - For each combination, create multiple weight allocations using Monte Carlo simulation
   - Evaluate each portfolio's Sharpe ratio
5. **Optimization**: Tracks and returns the portfolio with the highest Sharpe ratio

### Optimization Strategy

The portfolio optimization problem involves:
- Selecting a subset of stocks from the Dow Jones index (portfolio size defined as 25)
- Determining the optimal weight allocation for each stock in the portfolio
- Maximizing the Sharpe ratio while respecting constraints (e.g., maximum weight per stock)

The program explores a large solution space using:
1. Combinatorial generation of all possible stock subsets of the specified size
2. Monte Carlo simulation to generate random weight allocations for each combination
3. Parallel evaluation of portfolios to find the global optimum

### Parallel Processing Approach

The application uses Haskell's parallel processing capabilities to accelerate computation. The key parallelization decision was to:

**Parallelize by weight sets rather than stock combinations**

Each stock combination shares the same underlying data (returns, covariance matrix). By parallelizing the weight set evaluations within each combination, we:
- Minimize data duplication across threads
- Reduce memory overhead by not replicating the same return data and covariance matrices
- Achieve better load balancing as weight set evaluations have similar computational costs

This approach distributes chunks of Monte Carlo trials across available CPU cores, with each core handling the complete evaluation of several weight sets for a given stock combination.

### Performance Considerations

The code extensively uses strict evaluation (via bang patterns and forced evaluation) to avoid thunk buildup, which is critical for numeric computations in Haskell. Key performance optimizations include:

- **Strict Evaluation**: Using `!` bang patterns to force evaluation of intermediate calculations
- **Parallelization**: Leveraging `parList` and `rdeepseq` for parallel processing
- **Memory Efficiency**: Carefully managing data structures to prevent excessive memory usage
- **Unboxed Vectors**: Using unboxed vectors for numeric calculations where appropriate
- **GHC Optimizations**: Compiling with `-O3` and `-threaded` flags

## Installation

### Prerequisites

- GHC (Glasgow Haskell Compiler) 9.12.2 or later
- Cabal 3.0 or later
- Required Haskell packages (automatically installed via Cabal):
  - bytestring
  - cassava
  - vector
  - random
  - parallel
  - split
  - deepseq

### Setting Up

Clone the repository:

```bash
git clone https://github.com/victorlga/dow25-sharpe-montecarlo
cd dow25-sharpe-montecarlo
```

## Usage

### Running with Cabal

1. Update Cabal package database:
```bash
cabal update
```

2. Build the project:
```bash
cabal build
```

3. Run the optimizer:
```bash
cabal run
```

4. (Optional) Specify the number of cores to use:
```bash
cabal run -- +RTS -N4 -RTS
```

### Running with VS Code Dev Container

The project includes a Dev Container configuration for VS Code, providing a consistent development environment:

1. Install the "Remote - Containers" extension in VS Code
2. Open the project folder in VS Code
3. Click on the green button in the bottom-left corner and select "Reopen in Container"
4. Once the container is built and running, open a terminal in VS Code
5. Run the project using Cabal commands as described above

## Configuration Options

The following constants can be modified in the source code:

- `csvFilePath`: Path to the CSV file containing historical stock prices
- `daysPerYear`: Trading days in a year, used for annualization (default: 252)
- `maxAllowedWeight`: Maximum allocation percentage for any single stock (default: 0.2 or 20%)
- `numTrials`: Number of Monte Carlo simulations per portfolio combination (default: 1000)
- `portfolioSize`: Number of stocks to include in each portfolio (default: 25)

## Input Data

The program expects a CSV file with historical stock prices in the following format:
- First column: Stock ticker symbol
- Subsequent columns: Daily closing prices from August 1, 2024, to December 31, 2024

The file should be placed in the `data/` directory with the name specified in `csvFilePath`.

## Output

The program outputs:
- A summary of the optimization process
- The best portfolio found with its Sharpe ratio
- The weight allocation for each stock in the optimal portfolio

Example output:
```
Loaded 30 stocks.
Optimizing for portfolios of size 25

Testing 142506 portfolio combinations.
Running 1000 Monte Carlo trials per combination.

Best portfolio found:
- Sharpe Ratio: 3.1846533
- Stock Count: 25
- Sum of weights: 1.0000000

Portfolio composition:
  AAPL: 8.2%
  MSFT: 12.5%
  ...
```

## Project Structure

```
dow25-sharpe-montecarlo/
│
├── app/
│   └── Main.hs           # Main application code
│   └── Simulator.hs           # Simulation application code
│   └── DataLoader.hs           # Data loading application code
│
├── data/
│   └── dow_jones_close_prices_aug_dec_2024.csv  # 2024 stock price data
│   └── dow_jones_close_prices_jan_mar_2025.csv  # 2025 stock price data
│
├── .devcontainer/        # VS Code Dev Container configuration
│   ├── Dockerfile
│   └── devcontainer.json
│
├── dow25-sharpe-montecarlo.cabal  # Package description
├── LICENSE               # License information
└── README.md            # This file
```

## Results

### Parallel Processing Performance Analysis

![Execution Time Comparison](/execution-data/execution_time_comparison_5_10_cores.png)

#### Execution Time Metrics

| Metric | 5 Cores | 10 Cores |
|--------|---------|----------|
| Average Execution Time | 33.25 minutes | 32.54 minutes |
| Speedup | 2.97x | 3.04x |
| Parallel Efficiency | 59.5% | 30.4% |
| Time Reduction | 66.4% | 67.1% |

#### Key Observations

1. **Significant Performance Improvement**
   - Sequential execution averaged 98.83 minutes
   - Parallel execution with 5 cores reduced time to 33.25 minutes
   - Parallel execution with 10 cores further reduced time to 32.54 minutes

2. **Speedup Analysis**
   - 5-core parallel processing achieved a 2.97x speedup
   - 10-core parallel processing achieved a 3.04x speedup

3. **Efficiency Considerations**
   - 5-core parallel efficiency: 59.5%
   - 10-core parallel efficiency: 30.4%

#### Performance Insights

**Parallel Efficiency Analysis**

The performance results reveal critical insights into parallel computing:
- 5 cores achieved 59.5% efficiency, providing the optimal performance
- 10 cores showed diminishing returns (30.4% efficiency)

This demonstrates that simply adding more cores does not guarantee proportional speedup. The bottleneck stems from:
- Limited parallelism in the algorithm's pipeline
- Increasing overhead of thread management
- Resource contention as core count increases

**Key Takeaway**: The optimal parallel implementation balances computational work with communication overhead. For this portfolio optimization task, 5 cores represent the sweet spot of performance and efficiency.



## AI Usage Disclosure

The codebase was reviewed and enhanced using Claude.ai to improve code organization, documentation, and performance characteristics.
