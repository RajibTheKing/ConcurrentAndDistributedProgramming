import threading
import time
import sys
from tkinter import *

class Counter(threading.Thread):
  
  def __init__(self,msec,name,root, contr, parent):
    threading.Thread.__init__(self)
    self.msec = msec
    self.name = name
    self.value = 0
    self.root = root
    self.controller = contr
    self.parentCounter = parent
    self.currentState = 1
    self.window = CounterGUI(self.root, self)
    self.childCounter = []
  
  def setCounterValue(self, v):
    self.value = v


  def run(self):
    while True:
      while self.currentState == 2:
        time.sleep(100 * 0.001)

      if self.currentState == 3:
        break

      time.sleep(self.msec*0.001)
      print(self.name + ": " + str(self.value))
      self.window.set_counter(self.value)
      self.value+=1
    print("Counter#"+ self.name + ": is out of the loop")
    
     
  # Actions when buttons are pressed
  def startCallBack(self):
    if self.currentState != 1:
      self.currentState = 1
      if self.parentCounter != None:
        self.parentCounter.startCallBack()

      for i in range(0, len(self.childCounter)):
        self.childCounter[i].startCallBack()
    
  def stopCallBack(self):
    if self.currentState != 2 :
      print("trying to stop: "+ self.name)
      self.currentState = 2
      if self.parentCounter != None:
        self.parentCounter.stopCallBack()

      for i in range(0, len(self.childCounter)):
        self.childCounter[i].stopCallBack()

  def closeCallBack(self):
    if self.currentState != 3:
      self.currentState = 3
      self.window.removethis()
      if self.parentCounter != None:
        self.parentCounter.closeCallBack()

      for i in range(0, len(self.childCounter)):
        self.childCounter[i].closeCallBack()

      
  def copyCallBack(self):
    temp = Counter(int(self.msec),self.name + "-Copy", self.root, self.controller, self)
    temp.setCounterValue(self.value)
    temp.start()
    self.controller.addNewCounter(temp)

  def cloneCallBack(self):
    temp = Counter(int(self.msec),self.name + "-Child"+str(len(self.childCounter)), self.root, self.controller, self)
    temp.setCounterValue(self.value)
    temp.start()
    self.childCounter.append(temp)
    
  
  # Create a new counter (for reference)
  def newCallBack(self):
    new_counter = Counter(500,"New",self.root)
    new_window = CounterGUI(self.root,new_counter)
    new_counter.add_window(new_window)
    new_counter.start()

  def getCurrentStatus(self):
    return self.currentState

class CounterGUI:
  
  # root: root window
  # counter: related counter
  def __init__(self,root,counter):
    self.root = root
    self.counter = counter
    self.window = Toplevel(self.root)
    self.window.title(self.counter.name)
    self.label = StringVar()
    l = Label(self.window,textvariable = self.label)
    l.pack(side = TOP)
    b = Button(self.window, text = "Start", command = counter.startCallBack)
    b.pack(side = LEFT)
    b = Button(self.window, text = "Stop", command = counter.stopCallBack)
    b.pack(side = LEFT)
    b = Button(self.window, text = "Close", command = counter.closeCallBack)
    b.pack(side = LEFT)
    b = Button(self.window, text = "Copy", command = counter.copyCallBack)
    b.pack(side = LEFT)
    b = Button(self.window, text = "Clone", command = counter.cloneCallBack)
    b.pack(side = LEFT)
  
  # update counter value
  def set_counter(self,value):
    self.label.set(str(value))

  def removethis(self):
        self.window.destroy()



class CounterController(threading.Thread):
  
  counterList = []
  
  def __init__(self, root, time_intervals):
    threading.Thread.__init__(self)
    self.root = root
    self.time_intervals = time_intervals
    
  
  def Initialize(self):
    self.start()
    for num,msec in enumerate(sys.argv[1:]):
      temp = Counter(int(msec),"Counter#" + str(num), self.root, self, None)
      self.counterList.append(temp)
      temp.start()

  def addNewCounter(self, counter):
    self.counterList.append(counter)

  def run(self):
    while True:
      time.sleep(1000 * 0.001)
      print("inside Controller!!!")
      flag = True
      time.sleep(100*0.001)
      for i in range(0, len(self.counterList)):
        if self.counterList[i].getCurrentStatus() != 3:
          flag = False
      if flag:
        for i in range(0, len(self.counterList)):
          self.counterList[i].join()
        print("Now trying to destroy root")
        self.root.destroy()
      

root = Tk()
root.withdraw()
contr = CounterController(root, sys.argv[1:]) 
contr.Initialize()
root.mainloop()

time.sleep(1000 * 0.001)
print("Program Closed")

# Use root.destroy() to destroy the main window (hidden)
