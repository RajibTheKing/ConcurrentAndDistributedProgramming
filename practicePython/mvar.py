import threading

class MVar :

  def __init__(self) :

    self.empty = True
    self.content = None
    self.mutex = threading.Lock()
    self.read_lock = threading.Lock()
    self.write_lock = threading.Lock()

  def take(self) :

    self.mutex.acquire()
    while self.empty :
       self.mutex.release()
       self.read_lock.acquire()
       self.mutex.acquire()

    if self.write_lock.locked():
      self.write_lock.release()
    self.empty = True
    return_value = self.content
    self.mutex.release()
    return return_value

  def put(self, v) :

    self.mutex.acquire()
    while not self.empty :
        self.mutex.release()
        self.write_lock.acquire()
        self.mutex.acquire()

    if self.read_lock.locked():
      self.read_lock.release()
    self.empty = False
    self.content = v
    self.mutex.release()

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

