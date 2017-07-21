**Deployment using Ansible has not been updated to match changes in the
build of Potato (such as the removal of Gulp)**

**It will probably not work, we recommend instead using the Docker containers
packaged deployment**

# Setup Vagrant VM for front end Development

    $ git clone  https://github.com/cicakhq/potato.git
    $ cd potato
    $ cp potato.cfg.template potato.cfg
    
Edit and setup `potato.cfg` with your private keys.
    
With [Vagrant](http://vagrantup.com) correctly installed, the process should
be as simple as running:

    $ pip install ansible
    $ vagrant box add ddacunha/CentOS-7.2 # boxes exists for VMWare and Virtualbox
    $ cd deploy
    $ vagrant up

This will create and provision the Virtual Machine defined in `Vagrantfile`.

The list of recipes to execute is defined in `./deploy/development.yml`. To re-provision an existing vagrant instance, execute:

    $ vagrant provision

 The `/potato/` folder will be mounted with the root of the git
repository.

There is a script to forward the various ports from the vagrant instance to the
localhost interface of Mac OS X, there is a script:

    $ ./bin/vagrant-forward.sh

_(more to come)_
