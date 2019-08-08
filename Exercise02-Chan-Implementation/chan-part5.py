
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

class Chan :

  def __init__(self,capacity) : # new
    
    self.a = [None] * capacity
    
    self.r = MVar()
    self.w = MVar()
    self.w.put(0)
    self.count = MVar()
    self.count.put(0)

  def read(self) :
    
    read_position = self.r.take()
    value = self.a[read_position]
    old_count = self.count.take()
    self.count.put(old_count - 1)
    if old_count == len(self.a): # array was full
      self.w.put(read_position)
    if old_count > 1: # array is not empty
      self.r.put((read_position+1)%len(self.a))
    return value

  def write(self,v) :
    
    write_position = self.w.take()
    self.a[write_position] = v
    new_count = self.count.take() + 1
    self.count.put(new_count)
    if new_count == 1: # array was empty
      self.r.put(write_position)
    if new_count < len(self.a): # array is not full
      self.w.put((write_position + 1) % len(self.a))