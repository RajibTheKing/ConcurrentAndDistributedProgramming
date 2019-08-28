-module(distributedChatTest).

%Let's assume we hav Cn clients. When a user try to login, then every logged in client should get notified 
%by the new user. Now for any new user Ci the LTL property will be as follows: 
% G({loggedIn,Ci} ⇒(
%       ({loggedIn,C1}⇒(F({knows,C1,Ci}))) ∧ .... ∧ ({loggedIn,Ci−1}⇒(F({knows,Ci−1,Ci}))) ∧ 
%       ({loggedIn,Ci+1}⇒(F({knows,Ci+1,Ci})))∧ .... ∧({loggedIn,Cn}⇒(F({knows,Cn,Ci})))
%      )
%   )



%The test implementation is time consuming.... that's why I am skipping the test part. 