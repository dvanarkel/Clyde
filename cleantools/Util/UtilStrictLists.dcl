definition module UtilStrictLists

import StdString, StdClass

//	A strict list data structure

::	List t :== [!t!]

Nil :== [!!]

(:!) infixr 0
(:!) h t:==[!h:t!]
			
instance toString (List a) | toString a

Singleton		:: !(List t) -> t
IsEmptyList		:: !(List t) -> Bool
Head			:: !(List t) -> t
Concat			:: !(List .t) !u:(List .t) -> u:List .t
Reverse			:: !(List t) -> List t
Reverse2		:: !(List .a) !u:(List .a) -> v:List .a, [u <= v]
LLength			:: !(List t) -> Int
FilterR			:: !(s -> Bool) !(List s) -> List s
Filter			:: !(s -> Bool) !(List s) -> List s
Append			:: !(List t) !t -> List t
Select			:: !(List t) !Int -> t
Update2			:: !(List (List t)) !Int !Int !t -> List (List t)
AppendLists		:: !(List .a) !u:(List .a) -> v:List .a, [u <= v]
FlattenList		:: !(List (List .a)) -> List .a
P_MapR			:: !(s -> (t,Bool)) !(List s) -> (!List t,!Bool)

InsertStringInList		:: !String !(List String) -> List String
RemoveDup				:: !.(List a) -> .(List a) | Eq a
RemoveStringFromList	:: !String !(List String) -> List String
RemoveMember			:: a !u:(List a) -> v:List a | == a, [u <= v]
RemoveMembers			:: !u:(List a) !.(List a) -> v:List a | == a, [u <= v]
UnionStringList			:: !(List String) !(List String) -> List String
StringOccurs			:: !String !.(List String) -> Bool
SortStrings				:: !.(List String) -> .List String
EQStrings				:: !(List String) !(List String) -> Bool
InclString				:: !String !(List String) -> List String

StrictListToList		:: !(List .a) -> [.a]
ListToStrictList		:: ![.a] -> (List .a)
Map						:: !(s -> t) !(List s) -> List t
MapR					:: !(s -> t) !(List s) -> List t // reverses!!!
