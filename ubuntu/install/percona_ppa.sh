sudo apt-key adv --keyserver keys.gnupg.net --recv-keys 1C4CBDCDCD2EFD2A

export VERSION=$(lsb_release -c | cut -f2)

sudo sh -c 'echo "" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://repo.percona.com/apt $VERSION main" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://repo.percona.com/apt $VERSION main" >> /etc/apt/sources.list'

sudo apt-get update
