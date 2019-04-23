import threading
import time
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

class Chan :

  def __init__(self, sz) :

    hole = MVar()
    self.r = MVar()
    self.r.put(hole)
    self.w = MVar()
    self.w.put(hole)
    self.bufferCapacity = sz
    self.currentBuffers = 0

  def read(self) :
    
    r_end = self.r.take()
    v,next = r_end.take()
    self.r.put(next)
    self.currentBuffers -= 1
    return v

  def write(self,v) :
    if self.currentBuffers == self.bufferCapacity:
      print("Content write FAILED!!! Buffer is Full")
    else:
      hole = self.w.take()
      new_hole = MVar()
      hole.put((v,new_hole))
      self.w.put(new_hole)
      self.currentBuffers += 1

  def is_empty(self) :

    r_end = self.r.take()
    self.r.put(r_end)
    w_end = self.w.take()
    self.w.put(w_end)
    return r_end == w_end

  def add_multiple(self, listVal):
    for i in range(0, len(listVal)):
      hole = self.w.take()
      new_hole = MVar()
      hole.put((listVal[i], new_hole))
      self.w.put(new_hole)
  
  def un_get(self, val): #add new element on using reader pointer
    new_hole = MVar()
    ptr = self.r.take()
    new_hole.put((val, ptr))
    self.r.put(new_hole)




def producer(chanObj,n) :
  for i in range(n):
    chanObj.un_get(i)

def consumer(chanObj) :

  sum = 0
  content = None

  while content != -1 :
    content = chanObj.read()
    print("Got value: " + str(content))
    sum += content

  print(sum+1)

ch = Chan()

t1 = threading.Thread(target=producer, args=(ch,5))
#t2 = threading.Thread(target=producer, args=(ch,1000))
#t3 = threading.Thread(target=producer, args=(ch,1000))
#t4 = threading.Thread(target=producer, args=(ch,1000))
c1 = threading.Thread(target=consumer, args=(ch,))
#c2 = threading.Thread(target=consumer, args=(ch,))
#c3 = threading.Thread(target=consumer, args=(ch,))
#c4 = threading.Thread(target=consumer, args=(ch,))

t1.start()
#t2.start()
#t3.start()
#t4.start()
c1.start()
#c2.start()
#c3.start()
#c4.start()

input()

ch.write(-1)
#ch.write(-1)
#ch.write(-1)
#ch.write(-1)

