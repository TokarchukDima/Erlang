-module(main).
-author("makst").
-export([sum/1, sum/2]).

sum(N) -> sum1(N - 1, N).
sum(N, M) -> sum2(N, M, M).

sum1(0, Summary) -> Summary;
sum1(N, Summary) -> sum1(N - 1, Summary + N).

sum2(M, M, Sum) -> Sum;
sum2(N, M, Sum) -> sum2(N + 1, M, Sum + N);
sum2(N, M, _Sum) when N > M -> {error}.



