type Cell = (Int, Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

up:: MyState -> MyState
down:: MyState -> MyState
left:: MyState -> MyState
right:: MyState -> MyState

up (S (y,x) mines str m)= if(y-1<0) then Null else (S (y-1,x) mines "up" (S (y,x) mines str m))  

down (S (y,x) mines str m)= if(y+1>3) then Null else (S (y+1,x) mines "down" (S (y,x) mines str m)) 


left (S (y,x) mines str m)= if(x-1<0) then Null else (S (y,x-1) mines "left" (S (y,x) mines str m)) 


right (S (y,x) mines str m)= if(x+1>3) then Null else (S (y,x+1) mines "right" (S (y,x) mines str m)) 


remove c (h:t)= if c==h then t
else h:remove c t
collect:: MyState -> MyState

collect (S c mines str m)= 
                         if elem c mines == False then Null 
						 else (S c minesn "collect" (S c mines str m) ) 
					
						 where minesn=remove c mines


nextMyStates :: MyState -> [MyState]		
removenull:: [MyState]->[MyState]
removenull []=[]
removenull (h:t) =if(h==Null) then  removenull t
else h:removenull t

nextMyStates z=y
  where y=removenull (up(z):down(z):left(z):right(z):collect(z):[])

isGoal::MyState->Bool
isGoal (S (y,x) mines str m)= if mines==[] then True
                           else False
 

 
constructSolution:: MyState ->[String]
constructSolution  (S (y,x) mines str m) = if str =="" then []
else constructSolution m ++[str] 

search::[MyState]->MyState

search (h:t)=if (isGoal h) then h
                  else search (t++nextMyStates h)


solve :: Cell->[Cell]->[String]
solve a b =  constructSolution(search([S a b "" Null]))


