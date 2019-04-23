import sys
import time
import threading

class Counter:
    counter = 0
    def increament(self):
        self.counter += 1
    
    def getValue(self):
        return self.counter
    
    def setValue(self, v):
        self.counter = v

class Simulator:
    counterObj = Counter()
    
    def getCounterValue(self):
        return self.counterObj.getValue()

    def threadLoop(self):
        iterations = 1000
        for i in range(0, iterations):
            temp = self.counterObj.getValue()
            temp = temp+1
            self.counterObj.setValue(temp)

#Here we are trying to simulate the program

numberOFThread = 10
simulatorObj = Simulator()
threads = []

for i in range(0 , numberOFThread):
    temp = threading.Thread(target = simulatorObj.threadLoop)
    threads.append(temp)

for i in range(0 , numberOFThread):
    threads[i].start()

for i in range(0 , numberOFThread):
    threads[i].join()


print(simulatorObj.getCounterValue())

