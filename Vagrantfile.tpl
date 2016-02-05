# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  config.vm.box = "precise64"
  config.vm.box_url = "http://files.vagrantup.com/precise64.box"
  config.vm.synced_folder '{{ path.working }}', "/vagrant/info_bot"
  config.vm.provision 'shell', inline: <<SCRIPT
export DEBIAN_FRONTEND=noninteractive

wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb

apt-get update > /dev/null

apt-get -y install make curl g++ mercurial git zip xsltproc esl-erlang
SCRIPT

end
