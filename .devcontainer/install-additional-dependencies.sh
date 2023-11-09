# Aim: install dependencies

# Source the R script code/install.R from command line:
Rscript code/install.R

# Install Python dependencies:
pip install -r requirements.txt

# Rust, cargo and then then make the https://github.com/dabreegster/odjitter crate available from the command line:

# Install Rust:
curl https://sh.rustup.rs -sSf | sh -s -- -y

# Add cargo to the path:
export PATH="$HOME/.cargo/bin:$PATH"

# Add cargo to the path permanently:
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.bashrc

# Install odjitter:
cargo install --git https://github.com/dabreegster/odjitter odjitter

# Install targets
Rscript -e 'install.packages("targets")'