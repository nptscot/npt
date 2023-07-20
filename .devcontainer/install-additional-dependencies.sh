# Aim: install Rust, cargo and then then make the https://github.com/dabreegster/odjitter crate available from the command line:

# Install Rust:
curl https://sh.rustup.rs -sSf | sh -s -- -y

# Add cargo to the path:
export PATH="$HOME/.cargo/bin:$PATH"

# Add cargo to the path permanently:
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.bashrc

# Install odjitter:
cargo install --git https://github.com/dabreegster/odjitter odjitter