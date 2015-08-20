#!/bin/bash

sudo apt-get purge openjdk*

sudo apt-get install -y python-software-properties
sudo apt-get install -y software-properties-common

sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install -y oracle-java8-installer
sudo apt-get install -y oracle-java8-set-default
