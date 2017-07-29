import sys
import numpy as np

def generateData(fname, n):

  ocupacao = ['Engenheiro', 'Professor', 'Gerente', 'Estudante']
  conceito = ['A', 'B', 'C', 'D', 'F']

  with open(fname, 'w') as fw:
    for i in range(n):
      oc   = np.random.choice(ocupacao)
      co   = np.random.choice(conceito)
      nota = np.random.uniform(0.0, 10.0)
      fw.write(f'{oc} {co} {nota:.2f}\n')
  fw.closed

def main():

  if len(sys.argv) < 3:
    progname = sys.argv[0]
    print("Usage: {progname} output_file n")
    sys.exit(0)

  fname = sys.argv[1]
  n     = int(sys.argv[2])
  generateData(fname, n)
  

main()
