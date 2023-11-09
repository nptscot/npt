# Aim: install dependencies

# Check the Linux distro we're running:
cat /etc/os-release

# Source the R script code/install.R from command line:
Rscript code/install.R

# Install Python + pip in Debian:
# Add repository:
echo "deb http://archive.ubuntu.com/ubuntu/ jammy main restricted" >> /etc/apt/sources.list
apt-get update
apt-get install -y software-properties-common
apt-get install -y python3 python3-pip

# Install Python dependencies:
pip install pandas
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