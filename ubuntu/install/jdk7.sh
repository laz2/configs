#!/bin/bash

sudo apt-get purge openjdk*

sudo apt-get install python-software-properties
sudo apt-get install software-properties-common

sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java7-installer
sudo apt-get install oracle-java7-set-default