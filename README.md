# Salmon


## design

- DAG interleaving of trees

## default CommandLine

- Two parameters:
  - seed: a command-line starting point (goal is to communicate with humans on CLI args)
  - spec: a json-encoded (goal is to carry large objects for robots on stdin)

- Pipe the two:
   - `my-salmon config 123 | my-salmon run Up`

