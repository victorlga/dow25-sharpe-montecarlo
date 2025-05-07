FROM haskell:9.12.2

# Install common tools
RUN apt-get update && apt-get install -y \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user for VS Code
RUN useradd -ms /bin/bash vscode
USER vscode
WORKDIR /home/vscode

# Ensure cabal directory is initialized
RUN cabal update
