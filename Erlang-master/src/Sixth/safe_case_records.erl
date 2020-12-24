-module(safe_case_records).
-author("makst").
-export([foobar/1]).

-record(person, {name,age=0,phone,addr}).

foobar(P) when P#person.name == "Joe" -> joe;
foobar(_P) -> no_record.
