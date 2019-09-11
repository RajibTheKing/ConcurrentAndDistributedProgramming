-module(gb_tree_test).
-import(gb_trees,[lookup/2,empty/0,enter/3,insert/3,keys/1,to_list/1, smallest/1, is_empty/1]).
-export([test/0, test2/0]).

%gb_trees={Size,Tree}
%Tree=  {Key, Value, Smaller, Bigger} |nil
%Smaller=Tree
%Bigger=  Tree

% https://www.cnblogs.com/me-sa/archive/2012/06/23/erlang-gb_trees.html


test() ->
    MyTree = gb_trees:empty(),
    MyTree1 = gb_trees:enter(hello, 73, MyTree),
    MyTree2 = gb_trees:insert(world, 42, MyTree1),
    MyTree3 = gb_trees:insert(aorld, 94, MyTree2),
    MyTree4 = gb_trees:insert(zorld, 94, MyTree3),
    MyTree5 = gb_trees:insert(zorlds, 45, MyTree4),
    base:printLn(base:show(gb_trees:is_empty(MyTree5))),
    base:printLn(base:show(gb_trees:get(hello, MyTree5))),
    base:printLn(base:show(gb_trees:to_list(MyTree5))),
    base:printLn(base:show(gb_trees:to_list(MyTree5))).

test2() ->
    put(nowTree, gb_trees:empty()),
    put(nowTree, gb_trees:insert(hello, 73, get(nowTree))),
    put(nowTree, gb_trees:insert(world, 70, get(nowTree))),
    put(nowTree, gb_trees:insert(rajib, 80, get(nowTree))),
    put(nowTree, gb_trees:insert(zakee, 7, get(nowTree))),
    put(nowTree, gb_trees:enter(zakee, 777, get(nowTree))),
    get(nowTree).
    

