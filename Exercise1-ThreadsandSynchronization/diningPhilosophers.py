import threading
import time

class Stick:
    #This is a flag deciding the stick is taken or not
    isUsed = False

    #This is a stick id , to detect which stick
    stickID = 0

    conditionalLock = threading.Condition()

    def __init__(self, num):
        self.isUsed = False
        self.stickID = num


    def putBack(self):
        self.conditionalLock.acquire()
        self.isUsed = False
        self.conditionalLock.notify()
        self.conditionalLock.release()

    def getStick(self):
        self.conditionalLock.acquire()
        while self.isUsed == True:
            self.conditionalLock.wait()
        self.isUsed = True
        self.conditionalLock.release()


    def isAvailable(self):
        with self.conditionalLock:
            if self.isUsed == True:
                return False
            else:
                return True

    def getStickID(self):
        return "Stick: " + str(self.stickID)



class Philosopher:
    leftStick = None
    rightStick = None
    philosopherID = 0

    isSpecialPhilospher = None

    def __init__(self, id, v1, v2, flag):
        self.philosopherID = id
        self.leftStick = v1
        self.rightStick = v2
        self.isSpecialPhilospher = flag

    def getPhilosopherID(self):
        return "Philosopher " + str(self.philosopherID)

    def thinkMethod(self):
        print(self.getPhilosopherID() + ": is now thinking")
        time.sleep(0.5)
    
    def eatMethod(self):
        print(self.getPhilosopherID() + ":  is now eating!!!!!!!!!!!!!!!!!!")
        time.sleep(0.5)

    
    def getStick(self, stick):
        print(self.getPhilosopherID() + ": will take " + stick.getStickID())
        stick.getStick()
        print(self.getPhilosopherID() + ": has taken " + stick.getStickID())
    
    def putStick(self, stick):
        print(self.getPhilosopherID() + ":  is puting back" + stick.getStickID())
        stick.putBack()
        
    #First take left stick, then take right stick when it's available. Otherwise put left stick back
    def philosopherActivityStrategy1(self):
        while True:
            self.thinkMethod()
            self.getStick(self.leftStick)
            if self.rightStick.isAvailable():
                self.getStick(self.rightStick)
                self.eatMethod()
                self.putStick(self.leftStick)
                self.putStick(self.rightStick)
            else:
                self.putStick(self.leftStick)


    #Only one special philosopher will take right stick first and then left stick
    def philosopherActivityStrategy2(self):
        while True:
            self.thinkMethod()
            if self.isSpecialPhilospher == True:
                self.getStick(self.rightStick)
                self.getStick(self.leftStick)
                self.eatMethod()
                self.putStick(self.rightStick)
                self.putStick(self.leftStick)
            else:    
                self.getStick(self.leftStick)
                self.getStick(self.rightStick)
                self.eatMethod()
                self.putStick(self.leftStick)
                self.putStick(self.rightStick)

            




#Here I am starting to simulate Dining Philosopher Problem

numberOfPhilosophers = 5
sticks = []
philosophers = []
threads = []

for i in range(0 , numberOfPhilosophers):
    temp = Stick(i)
    sticks.append(temp)
    print(sticks[i].getStickID())

for i in range(0 , numberOfPhilosophers):
    temp = None
    if i == (numberOfPhilosophers - 1):
        temp = Philosopher(i, sticks[i], sticks[(i+1) % numberOfPhilosophers], True)
    else:
        temp = Philosopher(i, sticks[i], sticks[(i+1) % numberOfPhilosophers], False)

    philosophers.append(temp)
    print(philosophers[i].getPhilosopherID())

for i in range(0 , numberOfPhilosophers):
    temp = threading.Thread(target = philosophers[i].philosopherActivityStrategy1)
    threads.append(temp)

for i in range(0 , numberOfPhilosophers):
    threads[i].start()


    
