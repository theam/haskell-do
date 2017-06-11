#!/bin/bash

printf "\n=== APT-Installing dependencies :\n"
apt-get update && apt-get install -y --no-install-recommends build-essential sudo

printf "\n=== Setting up FP Complete APT repository :\n"
# # get FP Complete public key
apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
# Ubuntu 14 APT repo for FP Complete
echo 'deb http://download.fpcomplete.com/ubuntu xenial main' | sudo tee /etc/apt/sources.list.d/fpco.list

printf "\n=== APT-Installing dependencies : \n"
apt-get update -y && apt-get install -y --no-install-recommends \
			     libgmp-dev stack &&  \
                             stack upgrade


export PATH=$(stack --stack-yaml stack.yaml path --local-install-root):$PATH


printf "\n=== Environment :\n"
printenv
