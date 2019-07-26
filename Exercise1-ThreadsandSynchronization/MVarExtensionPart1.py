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
    
  def read(self):
    self.mutex.acquire()
    while self.empty == True:
        self.mutex.release()
        self.read_lock.acquire()
        self.mutex.acquire()
    
    if self.read_lock.locked():
      self.read_lock.release()
    
    return_value = self.content
    self.mutex.release()
    return return_value