sudo apt-get purge imagemagick
sudo apt-get install build-essential checkinstall && sudo apt-get build-dep imagemagick

NAME=ImageMagick-6.8.6-9
ARCHIVE=$NAME.tar.gz
wget http://mirror.checkdomain.de/imagemagick/$ARCHIVE
tar xzvf $ARCHIVE
cd $NAME
./configure --prefix /usr && make
checkinstall

