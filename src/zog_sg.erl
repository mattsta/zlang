-module(zog_sg).  % zog_scatter_gather

-compile(export_all).

% Need to:
%  - provide list of action with parameters
%  - collect actions
%  - after timeout, if required actions are completed, return
%  -   if not completed, wait again in 1/4 time for eight tries then fail.

scatter(Tag, Fun) when is_function(Fun) ->
  Self = self(),
  Ref = make_ref(),
  spawn(fun() -> Self ! {Tag, Ref, Fun()} end),
  Ref.

scatter(Tag, M, F, A) ->
  Self = self(),
  Ref = make_ref(),
  spawn(fun() -> Self ! {Tag, Ref, M:F(A)} end),
  Ref.

required(Fun) when is_function(Fun) ->
  scatter(required, Fun).

required(M, F, A) ->
  scatter(required, M, F, A).

optional(Fun) when is_function(Fun) ->
  scatter(optional, Fun).

optional(M, F, A) ->
  scatter(optional, M, F, A).


gather(Required, Optional) ->
  gather(Required, Optional, [], 400).

gather(Required, Optional, SoFar, Total) ->
  StartedMS = to_ms(now()),
  gather(Required, Optional, SoFar, Total, StartedMS, StartedMS).

gather([], [], SoFar, _TotalMS, StartedMS, LastMS) ->
  ElapsedMS = LastMS - StartedMS,
  {complete, all, SoFar, ElapsedMS};
gather([], _, SoFar, _TotalMS, StartedMS, LastMS) ->
  ElapsedMS = LastMS - StartedMS,
  {complete, required, SoFar, ElapsedMS};
% When the runtime (the difference between now and started) > total time then
% we inform the caller of a timeout scenario
gather(Required, Optional, SoFar, TotalMS, StartedMS, LastMS) 
  when (LastMS - StartedMS) >= TotalMS ->
  {timeout, Required, Optional, SoFar};
gather(Required, Optional, SoFar, TotalMS, StartedMS, LastMS) ->
  RemainingMS = case TotalMS - (LastMS - StartedMS) of
                  Time when Time > 0 -> Time;   % no negative timeouts
                                   _ -> 0       % 0 == timeout immediately
                end,
  receive
    {required, Ref, Result} -> gather(Required -- [Ref], Optional,
                                 [{Ref, Result} | SoFar],
                                 TotalMS, StartedMS, to_ms(now()));
    {optional, Ref, Result} -> gather(Required, Optional -- [Ref],
                                 [{Ref, Result} | SoFar],
                                 TotalMS, StartedMS, to_ms(now()))
  after
    RemainingMS -> {timeout, Required, Optional, SoFar}
  end.

-compile({inline, [{to_ms, 1}, {to_us, 1}]}).
to_ms({Mega, Sec, _MicroSec}) ->
  Mega * 1000000000000 + Sec * 1000.

to_us({Mega, Sec, MicroSec}) ->
  Mega * 1000000000000 + Sec * 1000000 + MicroSec.

