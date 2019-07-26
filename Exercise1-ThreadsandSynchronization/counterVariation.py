import sys
import time
import threading

class Counter:
    counter = 0
    sleepTime = None
    counterID = None
    def __init__(self, counterID, sleepTime):
        self.counterID = counterID
        self.sleepTime = sleepTime
    
    def sleepFor(self, v):
        time.sleep(v / 1000.0)
    
    def increament(self):
        while True:
            self.counter += 1
            print("Counter ID " + str(self.counterID) + ": " + str(self.counter))
            self.sleepFor(self.sleepTime)
    

    

#Here we are trying to simulate the program

numberOFThread = len(sys.argv) - 1
print("Length: " + str(numberOFThread))

intervals = []
threads = []
Counters = []

for i in range(0, numberOFThread):
    val = sys.argv[i+1]
    intervals.append(float(val))
    
for i in range(0 , numberOFThread):
    temp = Counter(i, intervals[i])
    Counters.append(temp)

for i in range(0 , numberOFThread):
    temp = threading.Thread(target = Counters[i].increament)
    threads.append(temp)

for i in range(0 , numberOFThread):
    threads[i].start()