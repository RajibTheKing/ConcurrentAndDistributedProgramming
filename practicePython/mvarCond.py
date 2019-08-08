import threading

class MVar :

  def __init__(self) :

    self.content = None
    self.empty = True
    self.r = threading.Condition()
    self.w = threading.Condition()

  def take(self) :  

    with self.r : 
      while self.empty :
        self.r.wait()

      with self.w : 
        self.w.notify()
        help = self.content
        self.content = None
        self.empty = True
        return help

  def put(self,v) : 

    with self.w :
      while not self.empty :
        self.w.wait()

      with self.r :
        self.content = v
        self.empty = False
        self.r.notify()

def producer(mvar,n) :

  for i in range(n):
    mvar.put(1)

def consumer(mvar) :

  sum = 0
  content = None

  while content != -1 :

    content = mvar.take()
    sum += content

  print(sum+1)

mvar = MVar()

t1 = threading.Thread(target=producer, args=(mvar,1000))
t2 = threading.Thread(target=producer, args=(mvar,1000))
t3 = threading.Thread(target=producer, args=(mvar,1000))
t4 = threading.Thread(target=producer, args=(mvar,1000))
c1 = threading.Thread(target=consumer, args=(mvar,))
c2 = threading.Thread(target=consumer, args=(mvar,))
c3 = threading.Thread(target=consumer, args=(mvar,))
c4 = threading.Thread(target=consumer, args=(mvar,))

t1.start()
t2.start()
t3.start()
#t4.start()
c1.start()
c2.start()
c3.start()
c4.start()

input()

mvar.put(-1)
mvar.put(-1)
mvar.put(-1)
mvar.put(-1)

