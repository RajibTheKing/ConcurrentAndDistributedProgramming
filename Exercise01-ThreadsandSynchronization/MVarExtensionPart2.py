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
        
  def read(self):
      with self.r:
          while self.empty:
              self.r.wait()
          
          help = self.content
          self.r.notify()
          return help



  def try_put(self, v):
      with self.w:
          if self.empty == True:
              with self.r:
                  self.content = v
                  self.empty = False
                  self.r.notify()
                  return True
          else:
              return False

  #A pair of (content, Boolean) would be a reasonable return type for this method.
  def try_take(self):
      with self.r:
          if self.empty == False:
              with self.w:
                  self.w.notify()
                  help = self.content
                  self.content = None
                  self.empty = True
                  return (help,True)
          else:
              return (None, False)


  def swap(self, v):
      with self.r:
          while self.empty == True:
              self.r.wait()
          self.r.notify()
          with self.w:
              oldValue = self.content
              self.content = v
              return oldValue

  def takeWithTimeout(self, timeout):
      epoch_time = int(round(time.time() * 1000))
      with self.r:
          while self.empty == True:
              curr = int(round(time.time() * 1000))
              if (curr - epoch_time) > timeout:
                  return (None, False)
              else:
                  self.r.wait(timeout)
          
          with self.w:
              help = self.content
              self.empty = True
              self.w.notify()
              return (help, True)
              
  def putWithTimeout(self, v, timeout):
      epoch_time = int(round(time.time() * 1000))
      with self.w:
          while self.empty == False:
              curr = int(round(time.time() * 1000))
              if (curr - epoch_time) > timeout:
                  return False
              else:
                  self.w.wait(timeout)
          
          with self.r:
              self.content = v
              self.empty = False
              self.r.notify()
              return True
