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

  def __init__(self) :

    hole = MVar()
    self.r = MVar()
    self.r.put(hole)
    self.w = MVar()
    self.w.put(hole)

  def read(self) :
    
    r_end = self.r.take()
    v,next = r_end.take()
    self.r.put(next)
    return v

  def write(self,v) :

    hole = self.w.take()
    new_hole = MVar()
    hole.put((v,new_hole))
    self.w.put(new_hole)

  def is_empty(self) :
    # If the chan is empty and another thread is waiting to
    # read the chan, is_empty will suspend, because the other
    # thread is holding the r_end

    r_end = self.r.take()
    self.r.put(r_end)
    w_end = self.w.take()
    self.w.put(w_end)
    return r_end == w_end

  def add_multiple(self, listVal):
     # It is not guaranteed that there are no other values in
    # in between the values of the list
    for i in range(0, len(listVal)):
      self.write(listVal[i])
      
  
  def un_get(self, val): #add new element on using reader pointer
    # If the chan is empty and another thread is waiting to
    # read the chan, un_get will suspend and cannot write the
    # new value
    new_hole = MVar()
    ptr = self.r.take()
    new_hole.put((val, ptr))
    self.r.put(new_hole)




def producer(chanObj,n) :
  for i in range(n):
    chanObj.un_get(1)

def consumer(chanObj) :

  sum = 0
  content = None

  while content != -1 :
    content = chanObj.read()
    print("Got value: " + str(content))
    sum += content

  print(sum+1)

ch = Chan()

t1 = threading.Thread(target=producer, args=(ch,1000))
t2 = threading.Thread(target=producer, args=(ch,1000))
t3 = threading.Thread(target=producer, args=(ch,1000))
t4 = threading.Thread(target=producer, args=(ch,1000))

c1 = threading.Thread(target=consumer, args=(ch,))
c2 = threading.Thread(target=consumer, args=(ch,))
c3 = threading.Thread(target=consumer, args=(ch,))
c4 = threading.Thread(target=consumer, args=(ch,))

t1.start()
t2.start()
t3.start()
t4.start()

c1.start()
c2.start()
c3.start()
c4.start()

input()

ch.write(-1)
ch.write(-1)
ch.write(-1)
ch.write(-1)

