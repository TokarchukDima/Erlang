-module(compile_run_expr).
-author("makst").
-import(list, [flatten/1]).
-export([calc/1, read/1, print/1]).

token_input() -> [
  {space, "^\\s+", none},
  {float, "^\\-{0,1}\\d+\\.\\d+", none},
  {integer, "^\\-{0,1}\\d+", none},
  {group_start, "^\\(", none},
  {unary_minus, "^\\~\\(", none}
].

token_priority() -> [
  {space, "^\\s+", none},
  {plus, "^\\+", 1},
  {minus, "^\\-", 1},
  {multiply, "^\\*", 2},
  {divide, "^\\/", 2},
  {group_end, "^\\)", 0}
].

print_format() -> [
  {num, fun({_, Value}) -> to_list(Value) end},
  {plus, fun({_, A, B}) -> flatten([print(A), "+", print(B)]) end},
  {minus, fun({_, A, B}) -> flatten([print(A), "-", print(B)]) end},
  {multiply, fun({_, A, B}) -> flatten([print(A), "*", print(B)]) end},
  {divide, fun({_, A, B}) -> flatten([print(A), "/", print(B)]) end},
  {group, fun({_, X}) -> flatten(["(", print(X), ")"]) end},
  {unary_minus, fun({_, X}) -> flatten(["~(", print(X), ")"]) end}
].

to_list(X) when is_integer(X) ->
  integer_to_list(X);
to_list(X) when is_float(X) ->
  float_to_list(X, [{decimals, 4}, compact]);
to_list(X) ->
  {error, unknown_type, X}.

print(X) ->
  case lists:keysearch(element(1, X), 1, print_format()) of
    {value, {_, Format}} -> Format(X);
    _ -> {error, invalid, X}
  end.

calc(X) when is_list(X) ->
  Read = read(X),
  flatten([print(Read), " = ", to_list(calc(Read))]);
calc(X) when is_tuple(X) ->
  case X of
    {num, I} -> I;
    {plus, A, B} -> calc(A) + calc(B);
    {minus, A, B} -> calc(A) - calc(B);
    {multiply, A, B} -> calc(A) * calc(B);
    {divide, A, B} -> calc(A) / calc(B);
    {unary_minus, A} -> -calc(A);
    {group, A} -> calc(A)
  end.

read(Input) -> read(Input, {[], []}, input).

read([], {[H | _], []}, _) -> H;
read([], {NS, [OSH | OST]}, _) -> read([], {push_ns(NS, OSH), OST}, none);
read(Input, {NS, OS}, input) ->
  case match(Input, token_input()) of
    {space, {_, Tile}} -> read(Tile, {NS, OS}, input);
    {float, {Head, Tile}} -> read(Tile, {[{num, list_to_float(Head)} | NS], OS}, priority);
    {integer, {Head, Tile}} -> read(Tile, {[{num, list_to_integer(Head)} | NS], OS}, priority);
    {group_start, {_, Tile}} -> read(Tile, {NS, [group_start | OS]}, input);
    {unary_minus, {_, Tile}} -> read(Tile, {NS, [unary_minus | OS]}, input);
    _ -> {error, input, {Input}}
  end;
read(Input, {NS, OS}, priority) ->
  case match(Input, token_priority()) of
    {space, {_, Tile}} -> read(Tile, {NS, OS}, priority);
    {group_end, {_, Tile}} ->
      read(Tile, group({NS, OS}), priority);
    {Operator, {_, Tile}} ->
      read(Tile, push_ns_os(Operator, NS, OS), input);
    _ -> {error, priority, {Input}}
  end;
read(I, {NS, OS}, M) -> {error, read, {I, NS, OS, M}}.

group({NS, [HOS | TOS]}) ->
  NNS = push_ns(NS, HOS),
  case NNS of
    [{group, _} | _] -> {NNS, TOS};
    [{unary_minus, _} | _] -> {NNS, TOS};
    _ -> group({push_ns(NS, HOS), TOS})
  end.

push_ns_os(Operator, NS, []) -> {NS, [Operator]};
push_ns_os(Operator, NS, [H | T] = OS) ->
  case compare(Operator, H) of
    big -> {NS, [Operator | OS]};
    low_or_equal -> push_ns_os(Operator, push_ns(NS, H), T)
  end.

compare(O1, O2) ->
  P1 = priority(O1, -1),
  P2 = priority(O2, P1 - 1),
  if
    P1 > P2 -> big;
    true -> low_or_equal
  end.

push_ns([V | Tile], unary_minus) ->
  [{unary_minus, V} | Tile];
push_ns([V | Tile], group_start) ->
  [{group, V} | Tile];
push_ns([V2 | [V1 | Tile]], Operator) ->
  case Operator of
    plus -> [converter({plus, V1, V2}) | Tile];
    minus -> [converter({minus, V1, V2}) | Tile];
    multiply -> [converter({multiply, V1, V2}) | Tile];
    divide -> [converter({divide, V1, V2}) | Tile];
    _ -> {error, push_ns, Operator}
  end.

converter({multiply, {num, 0}, _}) -> {num, 0};
converter({multiply, _, {num, 0}}) -> {num, 0};
converter({multiply, {num, 1}, X}) -> X;
converter({multiply, X, {num, 1}}) -> X;
converter({plus, {num, 0}, X}) -> X;
converter({plus, X, {num, 0}}) -> X;
converter({minus, {num, 0}, X}) -> X;
converter({minus, X, {num, 0}}) -> X;
converter(X) -> X.

priority(Operator, Default) ->
  case lists:keysearch(Operator, 1, token_priority()) of
    {value, {_, _, Priority}} -> Priority;
    false -> Default
  end.

match(Input, []) -> {error, nomatch, Input};
match(Input, [{Oper, RegEx, _} | Tile]) ->
  case re:run(Input, RegEx) of
    {match, [{_, Len} | _]} ->
      {Oper, take(Input, Len)};
    nomatch -> match(Input, Tile)
  end.

take(List, Count) -> take(List, Count, []).
take(List, Count, Result)
  when length(Result) =:= Count
  orelse length(List) =:= 0
  -> {Result, List};
take([H | T], Count, Result) ->
  take(T, Count, flatten([Result, [H]])).
