import sys
import numpy as np

def generateLinear(fname, n):

  with open(fname, 'w') as fw:
    for i in range(n):
      x1 = np.random.uniform(0.0, 1.0)
      x2 = np.random.uniform(0.0, 1.0)
      x3 = np.random.uniform(0.0, 1.0)
      y = 2*x1 + 3.5*x2 - x3
      fw.write(f'{x1:.2f} {x2:.2f} {x3:.2f} {y:.2f}\n')
  fw.closed

def generateLinearNoise(fname, n):

  with open(fname, 'w') as fw:
    for i in range(n):
      x1 = np.random.uniform(0.0, 1.0)
      x2 = np.random.uniform(0.0, 1.0)
      x3 = np.random.uniform(0.0, 1.0)
      y = 2*x1 + 3.5*x2 - x3 + np.random.normal()
      fw.write(f'{x1:.2f} {x2:.2f} {x3:.2f} {y:.2f}\n')
  fw.closed

def main():

  if len(sys.argv) < 3:
    progname = sys.argv[0]
    print("Usage: {progname} output_file1 output_file2 n")
    sys.exit(0)

  fname = sys.argv[1]
  fname2 = sys.argv[2]
  n     = int(sys.argv[3])
  generateLinear(fname, n)
  generateLinearNoise(fname2, n)
  

main()
