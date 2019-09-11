-module(records).
-record(nodeData, {name,
               type=industrial,
               hobbies,
               details=[]}).

-record(nodeData2, {category,
               age=23,
               capacity=100}).

-export([firstData/0]).



firstData() ->
    #nodeData{name="robo",type=handmade, details=[linefollower, smart, speaker]}.