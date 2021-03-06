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

  def __init__(self) :

    hole = MVar()
    self.r = MVar()
    self.r.put(hole)
    self.w = MVar()
    self.w.put(hole)
    self.count = threading.Semaphore(0) # new

  def read(self) :
    
    self.count.acquire() # new
    r_end = self.r.take()
    v,next = r_end.take()
    self.r.put(next)
    return v

  def write(self,v) :

    hole = self.w.take()
    new_hole = MVar()
    hole.put((v,new_hole))
    self.w.put(new_hole)
    self.count.release() # new

  def is_empty(self) :
    
    return self.count._value == 0 # new
  
  def add_multiple(self,l):
    # It is not guaranteed that there are no other values in
    # in between the values of the list
    for v in l:
      self.write(v)
      
  def un_get(self,v):
    # still not fixed
    r_end = self.r.take()
    new_r_end = MVar()
    new_r_end.put((v,r_end))
    self.r.put(new_r_end)
    self.count.release() # new