#!/bin/bash

MUSER="$1"
MPASS="$2"
MDB="$3"
 
MHOST="localhost"
 
[ "$4" != "" ] && MHOST="$4"
 
# Detect paths
MYSQL=$(which mysql)
AWK=$(which awk)
GREP=$(which grep)

# help
if [ ! $# -ge 3 ]
then
	echo "Usage: $0 {MySQL-User-Name} {MySQL-User-Password} {MySQL-Database-Name} [host-name]"
	echo "Recreate database from a MySQL"
	exit 1
fi

$MYSQL -u $MUSER -p$MPASS -h $MHOST -e "drop database if exists $MDB"
$MYSQL -u $MUSER -p$MPASS -h $MHOST -e "create database $MDB character set 'utf8' collate 'utf8_bin'"
$MYSQL -u $MUSER -p$MPASS -h $MHOST -e "grant all on $MDB.* to rva@localhost"
