# Aim: install Rust, cargo and then then make the https://github.com/dabreegster/odjitter crate available from the command line:

# Install Rust:
curl https://sh.rustup.rs -sSf | sh

# Add cargo to the path:
export PATH="$HOME/.cargo/bin:$PATH"

# Install odjitter:
cargo install --git https://github.com/dabreegster/odjitter odjitter