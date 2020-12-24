-module(update_records).
-author("makst").
-export([birthday/1, max/0, showPerson/1]).

-record(person, {name,age=0,phone,addr}).

birthday(#person{age=Age} = P) -> P#person{age=Age+1}.

max() ->
  #person{
    name="Max",
    age=23,
    phone="754-854",
    addr="Kyiv" }.

showPerson(#person{age=Age,phone=Phone,name=Name,addr=Addr}) ->
  io:format("name: ~p  age: ~p  phone: ~p addr: ~p~n", [Name, Age, Phone, Addr]).
