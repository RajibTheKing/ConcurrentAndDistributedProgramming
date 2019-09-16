import threading
import time

class SyncPhil(threading.Thread):

  def __init__(self,left,right,num):
    threading.Thread.__init__(self)
    self.left = left
    self.right = right
    self.num = num
    

  def think(self):
    print(str(self) + " is thinking")
    time.sleep(0.2)

  def eat(self):
    print(str(self) + " is eating")
    time.sleep(0.2)

  def __str__(self):
    return "Philosopher#" + str(self.num)

  def run(self):
    while True:
      self.think()
      print(str(self) + " tries to take first stick.")
      with self.left:
        print(str(self) + " got first stick.")
        time.sleep(1)
        print(str(self) + " tries to take second stick.")
        with self.right:
          print(str(self) + " got second stick.")
          self.eat()

# Number of Philosophers
n = 5

s = []
for i in range(n):
  s.append(threading.Condition())

for i in range(n-1):
  p = SyncPhil(s[i],s[(i+1)%n],i)
  p.start()

p = SyncPhil(s[0],s[n-1],n-1)
p.start()

