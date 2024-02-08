# Salmon


## design

- DAG interleaving of trees

## default CommandLine

- Two parameters:
  - seed: a command-line starting point (goal is to communicate with humans on CLI args)
  - spec: a json-encoded (goal is to carry large objects for robots on stdin)

- Pipe the two:
   - `my-salmon config 123 | my-salmon run Up`


## manual configs

- cabal

- salmon user

```
sudo useradd -G sudo -m salmon

# enable calling
sudo mkdir /home/salmon/.ssh
sudo nc -l -p 1234 > /home/salmon/.ssh/authorized_keys

# (to-consider) add some stable dir for the receiving filepaths
mkdir /home/salmon/tmp

# (optional) enable running sudo commands
echo "salmon ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee /etc/sudoers.d/salmon-noprompt
```


- run locally as non root

chown to root then chmod u+s
