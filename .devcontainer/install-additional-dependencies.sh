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
pip install -r requirements.txt

# Rust, cargo and then then make the https://github.com/dabreegster/odjitter crate available from the command line:

# Install Rust:
curl https://sh.rustup.rs -sSf | sh -s -- -y

# Add cargo to the path:
export PATH="$HOME/.cargo/bin:$PATH"

# Add cargo to the path permanently:
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

# Install odjitter:
cargo install --git https://github.com/dabreegster/odjitter --rev 32fb58bf7f0d68afd3b76b88cf6b1272c5c66828

# Install tippecannoe
cd /tmp
git clone https://github.com/felt/tippecanoe.git
cd tippecanoe
make -j
sudo make install
tippecanoe --version