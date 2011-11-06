-module(zog_site).

-compile(export_all).

%%%----------------------------------------------------------------------
%%% compile
%%%----------------------------------------------------------------------
compile({file, Filename}) ->
  {ok, Contents} = file:read_file(Filename),
  compile(Contents);
compile(Module) when is_list(Module) orelse is_binary(Module) ->
  FixedModule = fixup_syntax(iolist_to_binary([Module, "\n"])),
  {ok, Tokens, _} = zlang_lexer:string(FixedModule),
  {ok, ParseTree} = zlang_parser:parse(Tokens),
  ParseTree.

source_to_binary(Module, Name) when is_list(Name) ->
  Compiled = compile(Module),
  Evald = mod(Compiled, Name),
  lfe_module_binary(iolist_to_binary(Evald)).

fixup_syntax(Module) ->
  % Fix end/done/fin newline requirements
  ExpandedSemis = re:replace(Module, ";", "\n", [{return, list}, global]),
  Fixed = fixup_matches(ExpandedSemis),
  io:format("fixed: ~s~n", [Fixed]),
  Fixed.

% IMPROVEMENTS:
% the in-match flag needs to be recursive.  Make it a count of the current
% "in-match" depth.  If 0 == not in match, if > 0, in match.
% ALSO -- retain a count of how many newlines were inserted so we can adjust
% the parser error lines correctly by subtracting the number of inserted lines.
fixup_matches(Module) ->
  fixup_matches(re:split(Module, "([\n])", [{return, list}]), false, []).

-define(DONE_REGEX, "\s+(end|done|fin)(\s)?$").
fixup_matches([Line|Lines], false = _InMatchStmt, Acc) ->
  case re:run(Line, "\s+match\s+") of
    {match, _} -> case re:run(Line, ?DONE_REGEX) of % catch one-line match
                    {match, _} -> fixup_matches(Lines, false, [Line | Acc]);
                             _ -> fixup_matches(Lines, true, [Line | Acc])
                  end;
             _ -> fixup_matches(Lines, false, [Line | Acc])
  end;
fixup_matches([Line|Lines], true = _InMatchStmt, Acc) ->
  case re:run(Line, ?DONE_REGEX) of
    % NOTE: To add a newline BEFORE something, insert "\n" AFTER the line since
    % we are building the list backwards.  It will get reverse/1'd at the end.
    {match, _} -> case re:run(Line, "->") of  % If a one-line match, mv end
                    {match, _} -> FixedLine = re:replace(Line, "end", ""),
                                  fixup_matches(Lines, false,
                                    ["end", "\n", "\n", FixedLine, "\n" | Acc]);
                             _ -> fixup_matches(Lines, false,
                                    [Line, "\n" | Acc])
                   end;
             _ -> case re:run(Line, "->") of
                    {match, _} -> fixup_matches(Lines, true,
                                    [Line, "\n" | Acc]);
                             _ -> fixup_matches(Lines, true, [Line | Acc])
                  end
  end;
fixup_matches([], _, Acc) -> unicode:characters_to_list(lists:reverse(Acc)).

%%%----------------------------------------------------------------------
%%% evaluate primary forms
%%%----------------------------------------------------------------------
mod({module, Stmts}, ModuleName) ->
  Mod =[args(["defmodule", ModuleName, args(["export", "all"])]), "\n\n",
        site_macros(),
        inline_stmts(Stmts)],
  BMod = iolist_to_binary(Mod),
  replace_until_done(BMod, "\\)\s+\\)", "))").

site_macros() ->
  PathToEbin = filename:dirname(code:where_is_file("zog_site.beam")),
  MacroSource = filename:join([PathToEbin, "..", "priv", "site_macros.lfe"]),
  {ok, MacroBin} = file:read_file(MacroSource),
  MacroBin.

inline_stmts(Stmts) ->
  ManagedStmts = lists:foldl(fun combine_functions/2, [], Stmts),
  [stmt(S) || S <- ManagedStmts].

% HTTP functions are defined individually in zlang, but for LFE, all
% function args/bodies must be under the same function name.  Here we combine
% all http functions with the same name into the same tuple for creation.
combine_functions({function,
                   {http, Method, [PrimaryRoute|RestPath], Body}}, Accum) ->
  case lists:keyfind(PrimaryRoute, 2, Accum) of
    {Hdr, PrimaryRoute, SubProps} ->
      NewRoute = {Hdr, PrimaryRoute, [{Method, RestPath, Body} | SubProps]},
      lists:keyreplace(PrimaryRoute, 2, Accum, NewRoute);
    _ -> [{managed_http_function,
           PrimaryRoute, [{Method, RestPath, Body}]} | Accum]
  end;
combine_functions({function,
                   {local_fun, Name, Args, Body}}, Accum) ->
  case lists:keyfind(Name, 2, Accum) of
    {Hdr, Name, Bodies} ->
      % We're building the args/body backwards.  We should really reverse it.
      NewFun = {Hdr, Name, [{Args, Body} | Bodies]},
      lists:keyreplace(Name, 2, Accum, NewFun);
    _ -> [{managed_local_fun,
           Name, [{Args, Body}]} | Accum]
  end;
combine_functions(Other, Accum) ->
  [Other | Accum].

% NB: we reverse the managed properties/bodies because we build them backwards
stmt({managed_http_function, PrimaryRoute, SubProps}) ->
  ExpandedSubProps =
  [args([ato(Method), strargs(RestPath), "\n",
         http_body(Body, Method, RestPath)]) ||
   {Method, RestPath, Body} <- lists:reverse(SubProps)],
  topcall(["create-http-function", PrimaryRoute, lst(ExpandedSubProps)]);
stmt({function, {local_fun, Name, Body}}) ->
  topcall(["local-function-noargs", Name, prgn(local_body(Name, Body))]);
stmt({managed_local_fun, Name, ArgsBodies}) ->
  ExpandedArgsBodies =
  [args([args(Args), "\n", local_body(Name, Body)]) ||
    {Args, Body} <- lists:reverse(ArgsBodies)],
  topcall(["local-function", Name, lst(ExpandedArgsBodies)]);
stmt({binding, Binding}) ->
  {LocalName, RemoteModule, RemoteFun, RemoteArgs} = resolve_binding(Binding),
  % use deproper for all because we don't care about string versus named
  % attributes at the top level
  topcall(["external-binding",
    deproper(LocalName),
    deproper(RemoteModule),
    deproper(RemoteFun),
    args([deproper(R) || R <- RemoteArgs])]).

resolve_binding({external_named_import, ExternalName, Importing}) ->
  {LocalName, ExternalArgs, ExternalMod} = resolve_import(Importing),
  {LocalName, ExternalMod, ExternalName, ExternalArgs};
resolve_binding({external_import, Importing}) ->
  {LocalName, ExternalArgs, ExternalMod} = resolve_import(Importing),
  {LocalName, ExternalMod, LocalName, ExternalArgs}.

resolve_import({import, FunName, Args, FromModule}) ->
  % We could do a lookup on permissions here for the active user/namespace.
  % We could *also* import the entire function here instead of leaving it as
  % a live reference to external code.
  {FunName, Args, FromModule}.

http_body([], _Method, _Path) -> [];
http_body([{vars, PullVarsFrom, Vars}|RestBody], Method, Path) ->
  VarsAppliersRemoved = remove_full_applier(Vars),
  LocalVars = [vars_local(V) || V <- VarsAppliersRemoved],
  WebVars = [vars_web(V) || V <- VarsAppliersRemoved],
  [indent(l),
   expr(["with-http-vars", PullVarsFrom, Method,
         args(LocalVars), alst(WebVars), fapplier(Vars, "tvar"),
         lst(http_body(RestBody, Method, Path))])];
http_body([{equality, _, _} = Equality|RestBody], Method, Path) ->
  equality(Equality, RestBody, fun(E) -> http_body(E, Method, Path) end);
http_body([H|T], Method, Path) ->
  [body(H) | http_body(T, Method, Path)];
http_body(OneStmt, _, _) when is_tuple(OneStmt) ->
  body(OneStmt).

% If we have a list of one item, capture the list itself
name_or_names(Names) ->
  case Names of
    [N] -> proper(N);
      _ -> args([proper(N) || N <- Names])
  end.

vars_local({alias, _WebName, LocalName, default, _Default}) -> LocalName;
vars_local({default, WebName, _Default}) -> WebName; % web name is only name
vars_local({alias, _WebName, LocalName}) -> LocalName;
vars_local(V) -> V.

vars_web({alias, WebName, _LocalName, default, Default}) ->
  tup([str(WebName), bin(Default)]);
vars_web({default, WebName, Default}) -> tup([str(WebName), bin(Default)]);
vars_web({alias, WebName, _LocalName}) -> str(WebName);
vars_web(V) -> str(V).

local_body(_, []) -> [];
local_body(FunName, [{equality, _, _} = Equality|RestBody]) ->
%  io:format("Processing local equality of: ~p~n", [Equality]),
  equality(Equality, RestBody, fun(E) -> local_body(FunName, E) end);
local_body(FunName, [{redo, Args}|RestBody]) ->
  [body({redo, FunName, Args}) | local_body(FunName, RestBody)];
local_body(FunName, [{async, nowait, Stmts}|RestBody]) ->
  [expr(["list", async_local_body(FunName, Stmts)]) |
    local_body(FunName, RestBody)];
local_body(FunName, [{async_wait, PriorAsync, Timeout}|RestBody]) ->
  [async_wait_local_body(PriorAsync, Timeout) | local_body(FunName, RestBody)];
local_body(FunName, [H|T]) ->
  [body(H) | local_body(FunName, T)];
local_body(FunName, OneStmt) when is_tuple(OneStmt) ->
  local_body(FunName, [OneStmt]).

async_local_body(_, []) -> [];
async_local_body(FunName, [H|T]) ->
  [expr(["async", lst(local_body(FunName, H))]) |
     async_local_body(FunName, T)].

async_wait_local_body(PriorAsync, Timeout) ->
  expr(["get-async-return-values", proper(PriorAsync), a2l(Timeout)]).

%%%----------------------------------------------------------------------
%%% equality = recurisve let
%%%----------------------------------------------------------------------
generate_from_source(SourceValue, NextFun) when is_tuple(SourceValue) ->
  NextFun(SourceValue);
generate_from_source(SourceValue, NextFun) when is_list(SourceValue) ->
  alst([NextFun(E) || E <- SourceValue]).

equality({equality, _, SourceValue}, [], NextFun) ->
  % No RestBody, so just generate (no bind).
  generate_from_source(SourceValue, NextFun);

equality({equality, LocalNames, SourceValue}, RestBody, NextFun) ->
  UseLocalNames = name_or_names(LocalNames),
%  io:format("Equality with source value of: ~p~n", [SourceValue]),
  ResolvedSource = generate_from_source(SourceValue, NextFun),
%  io:format("Resolved source is: ~p [[(~s]] (for ~p)~n",
%    [ResolvedSource, ResolvedSource, LocalNames]),
  expr(["local-bind", proper(UseLocalNames), ResolvedSource,
        lst(NextFun(RestBody))]).

%%%----------------------------------------------------------------------
%%% math doing
%%%----------------------------------------------------------------------
math_body([{math, Op, InnerRemaining} | More], Acc) ->
  math_body(More, [args(["math", Op, math_body(InnerRemaining, [])]) | Acc]);
math_body([StringNumber | More], Acc) ->
  math_body(More, [proper(StringNumber) | Acc]);
math_body([], Acc) -> args(lists:reverse(Acc)).


%%%----------------------------------------------------------------------
%%% body statements
%%%----------------------------------------------------------------------
body({list, Parts}) ->
  args(["list"] ++ [body(P) || P <- Parts]);

body({over, Var, LocalVar, Stmt}) ->
  lc(proper(LocalVar), proper(Var), body(Stmt));

body(N) when is_number(N) ->
  proper(N);

body({unique, big}) ->
  args(["unique-big"]);

body({unique, small}) ->
  args(["unique-small"]);

body({whisper, good, WhisperList}) ->
  args(["whisper-logger-good", proper_proper_args(WhisperList)]);
body({whisper, bad, WhisperList}) ->
  args(["whisper-logger-bad", proper_proper_args(WhisperList)]);
body({whisper, WhisperList}) ->
  args(["whisper-logger", proper_proper_args(WhisperList)]);

body({cxn, {_, Property}}) ->
  args(["cxn-property", bin(Property)]);

body({user, {_, Property}}) ->
  args(["user-property", bin(Property)]);

body({numbers,_} = N) ->
  proper(N);

body({str, _} = S) ->
  proper(S);

body({var, _} = V) ->
  proper(V);

body({pair, Key, Value}) ->
  tup([proper(Key), proper(Value)]);

body({append, From, Pairs}) ->
  expr(["append-tups", From, alst([body(P) || P <- Pairs])]);

body({math, Op, OnWhat}) ->
  expr(["math", Op, math_body(OnWhat, [])]);

body({output, "plain", [Arg]}) ->
  expr(["output", proper(Arg)]);
body({output, Type, Args}) ->
  expr(["output", Type, args(lists:concat(Args))]);

body({external_call, {var, Module}, {var, Function}}) ->
  expr(["safe-external-call", Module, Function]);

body({external_call, {var, Module}, {var, Function}, Args}) ->
  expr(["safe-external-call", Module, Function, arglist(lists:concat(Args))]);

body({call, {var, Function}}) ->
  expr(["safe-call", Function]);

body({call, {var, Function}, Args}) ->
  expr(["safe-call", Function, arglist(lists:concat(Args))]);

body({redo, FunName, Args}) ->
  expr(["safe-redo", FunName, proper_proper(Args)]);

body({inline_fun, {against, {Args, LineNo}}, {stmts, Stmts}}) ->
  % (create-letrec (name is AGAINST+line NO) (args..) (body...))
  % Stmts are: [{args, Args}, {body, Body}]
  StringArgs = [deproper(A) || A <- Args],
  LocalMatcherName = lists:flatten(StringArgs) ++ "::" ++ LineNo,
  expr(["local-matcher", LocalMatcherName,
    proper_proper(Args),  % PROBABLY FAILS WITH MULTIARGS?
    lst([inline_body(LocalMatcherName, S) || S <- Stmts])]);

%% Data Statements (Write, Read, Get, Update)
body({write, {type, Type}, {contents, Pairs}, {index, Indexes}}) ->
  expr(["writer", proper(Type), alst(depair(Pairs)), alst(Indexes)]);

body({find, all, Type, Pairs}) ->
  expr(["finder-all", proper(Type), alst(depair(Pairs))]);

body({find, all, Type, Pairs, using, IdxSpec}) ->
  expr(["finder-all-idx", proper(Type), alst(depair(Pairs)),
    alst(IdxSpec)]);

body({find, one, Type, Pairs}) ->
  expr(["finder-one", proper(Type), alst(depair(Pairs))]);

body({get, var, Keys, TargetVar}) ->
  expr(["getter", proper_proper(Keys), proper(TargetVar)]);

body({get, proplist, _Keys, _Pairs}) -> [];

body({update, {obj, BoundVar}, Mutations}) ->
  ProperMutations = [mutation(M) || M <- Mutations],
  expr(["updater-obj", proper(BoundVar), alst(ProperMutations)]);

body({update, {find, Finder}, Mutations}) ->
  ProperMutations = [mutation(M) || M <- Mutations],
  expr(["updater-find", body(Finder), alst(ProperMutations)]).



%%%----------------------------------------------------------------------
%%% inline fun/matcher/letrec expansion
%%%----------------------------------------------------------------------
inline_body(FunName, {stmt, {args, Args}, {body, Body}}) ->
  case close_reduction(Args) of
          Args -> expr([lst(proper_proper(Args)), local_body(FunName, Body)]);
    ClosedArgs ->
      [expr([lst(proper_proper(ClosedArgs)), args([])]), % no body on no list
       expr([lst(proper_proper(Args)), local_body(FunName, Body)])]
   end.

close_reduction([]) -> [];
close_reduction([{first_rest, _, _} | T]) ->
  [match_empty | close_reduction(T)];
close_reduction([H|T]) ->
  [H | close_reduction(T)].

%%%----------------------------------------------------------------------
%%% make proper lists (or not) out of proper things
%%%----------------------------------------------------------------------
proper_proper(Args) when is_list(Args) ->
  alst([proper(A) || A <- Args]).

proper_proper_args(Args) when is_list(Args) ->
  lst([proper(A) || A <- Args]).

%%%----------------------------------------------------------------------
%%% num safety
%%%----------------------------------------------------------------------
num_safe(X) when is_binary(X) ->
  num_safe(binary_to_list(X));
num_safe(X) when is_list(X) andalso not is_binary(hd(X)) ->
  try
    list_to_integer(X)
  catch
    _:_ -> list_to_float(X)
  end;
num_safe(X) when is_tuple(X) -> X;
num_safe(X) when is_number(X) -> X;
num_safe(ok) -> [].  % hack to get around fib recursion limit throwing nomatch

%%%----------------------------------------------------------------------
%%% mutation expanding
%%%----------------------------------------------------------------------
mutation({set, Pair})    -> tup({set, depair(Pair)});
mutation({add, Pair})    -> tup({add, depair(Pair)});
mutation({remove, Pair}) -> tup({remove, depair(Pair)});
mutation({clear, Str})   -> tup({clear, proper(Str)});
mutation({delete, Str})  -> tup({delete, proper(Str)}).

%%%----------------------------------------------------------------------
%%% applier expanding
%%%----------------------------------------------------------------------
arglist(ArgList) ->
  expand_appliers(ArgList).

expand_appliers(Args) ->
  InlineExpanded = expand_inline_appliers(Args),
  expand_full_applier(InlineExpanded).

expand_inline_appliers(Stuff) when is_list(Stuff) ->
  expand_inline_appliers(Stuff, []).

expand_inline_appliers([], Accum) -> lists:reverse(Accum);
expand_inline_appliers([{applier, Applier}|T], Accum) ->
  Resolved =
  case Applier of
    {math, _, _} = Math -> body(Math);
    {"meta", Module} -> args(["run-with-mod", Module]);
    {using, vars, remove, Members, Then} ->
      case Then of
        combine_name_values -> args(["remove-then-zip", args(Members)])
      end
  end,
  expand_inline_appliers(T, [Resolved | Accum]);
expand_inline_appliers([H|T], Accum) ->
  expand_inline_appliers(T, [H | Accum]).

remove_full_applier(Vars) ->
  lists:keydelete(applier_full, 1, Vars).

expand_full_applier(InlineAlreadyExpanded) ->
  case lists:keytake(applier_full, 1, InlineAlreadyExpanded) of
    {value, {applier_full, LCApplier}, FullRemoved} ->
      lc("tvar", args(FullRemoved), lc_applier(LCApplier));
    false -> args(InlineAlreadyExpanded)
  end.

fapplier(Args) -> fapplier(Args, []).
fapplier(Args, Default) ->
  case lists:keyfind(applier_full, 1, Args) of
    {applier_full, LCApplier} -> lc_applier(LCApplier);
                            _ -> Default
  end.

% Function of the applier for this lc
lc_applier({convert, remove, What}) ->
  fnc("erlang", "iolist_to_binary", [
    fnc("re", "replace", ["tvar", str(What), str([]), alst([ato("global")])])
  ]).


%%%----------------------------------------------------------------------
%%% output helpers
%%%----------------------------------------------------------------------
function_name_with_path([Base | T]) ->
  {Base, args([str(X) || X <- T])}.

args(Args) ->
  lst(space(Args)).

tups(Tups) when is_list(Tups) ->
  [tup(T) || T <- Tups].

strargs(Args) ->
  args([str(X) || X <- Args]).

topcall(Args) ->
  [args(Args), "\n"].

expr(Args) ->
  ["\n", "\t", args(Args), "\n"].

lc(Target, From, DoWhat) ->
  args(["lc", lst(args(["<-", Target, From])), DoWhat]).

a2l({var, Var}) when is_list(Var) -> Var;
a2l({list, Parts}) when is_list(Parts) ->
  args(["list"] ++ [body(P) || P <- Parts]);
a2l(X) when is_atom(X) -> atom_to_list(X);
a2l(X) when is_list(X) andalso is_list(hd(X)) -> X;
a2l(X) when is_float(X) -> mochinum:digits(X);
a2l(X) when is_integer(X) -> integer_to_list(X);
a2l(X) when is_binary(X) -> binary_to_list(X);
a2l(X) when is_list(X) -> X.

a2b(X) -> list_to_binary(a2l(X)).

proper({var, Var}) when is_list(Var) -> Var;
proper({str, Str}) when is_list(Str) -> bin(Str);
proper(Number) when is_number(Number) -> bin(Number);
proper({numbers, Numbers}) when is_list(Numbers) -> [bin(N) || N <- Numbers];
proper({first_rest, {submatched, First}, Rest}) ->
  args([proper_proper(First), ".", proper(Rest)]);
proper({first_rest, First, Rest}) -> args([proper(First), ".", proper(Rest)]);
proper(match_empty) -> args([]);
proper(Other) -> Other.

deproper({_, Any}) -> Any.

str({local_bind, Name}) -> Name;  % inject a top-level var here. nostr.
str(E) -> ["'\"", a2l(E), "\""].

flatstr(E) -> ["\"", a2l(E), "\""].

bin(B) when is_binary(B) -> B;
bin(B) when is_list(B) orelse is_number(B) -> ["#b", lst(flatstr(B)), " "];
bin({str, Str}) when is_list(Str) -> bin(Str).

ato(A) when is_list(A) -> ["\'", A];
ato(A) when is_atom(A) -> ["\'", atom_to_list(A)].

tup({pair, Key, Value}) ->
  tup([Key, Value]);
tup(T) when is_tuple(T) ->
  args(["tuple" | tuple_to_list(T)]);
tup(T) when is_list(T) ->
  args(["tuple" | T]).

depair(Pair) when is_tuple(Pair) ->
  body(Pair);
depair(Pairs) when is_list(Pairs) ->
  [body(P) || P <- Pairs].

lst(E) -> ["(", E, ") "].

alst(E) when is_list(E) -> args(["list" | E]).
prgn(E) when is_list(E) -> args(["progn" | E]).

atolst(A) -> ["'", args(A)].

fnc(Mod, Fnc, Args) ->
  ["(: ", Mod, " ", Fnc, " ", string:join(Args, " "), ")"].

indent(l) -> " ";
indent(v) -> [indent(l), "       "].

space(Things) ->
  string:join([a2l(proper(X)) || X <- Things], " ").

replace_until_done(Input, What, Fix) ->
  case re:replace(Input, What, Fix, [global]) of
    Input -> Input;
    Other -> replace_until_done(Other, What, Fix)
  end.

%%%----------------------------------------------------------------------
%%% write output
%%%----------------------------------------------------------------------
write_temp(Dir, TmpBaseName, Contents) ->
  {A,B,C} = erlang:now(),
  RandomNum = A bxor B bxor C,
  RandomName = TmpBaseName ++ integer_to_list(RandomNum) ++ ".lfe",
  CreatedName = filename:join([Dir, RandomName]),
  case file:write_file(CreatedName, Contents, [exclusive]) of
                 ok -> CreatedName;
    {error, eexist} -> write_temp(Dir, TmpBaseName, Contents) % again!
  end.

run_with_temp(Contents, Fun) ->
  TmpFilePath = write_temp("/tmp", "zog_site_tmp", Contents),
  Result = Fun(TmpFilePath),
  io:format("Path is: ~p~n", [TmpFilePath]),
  case Result of
    {error, _, _} -> error;
            error -> error;
                _ -> file:delete(TmpFilePath)
  end,
  Result.

lfe_module_debug(Contents) ->
  lfe_module(Contents, [to_exp, report]).

lfe_module_binary(Contents) ->
  lfe_module(Contents, [binary, report]).

lfe_module(Contents, Args) ->
  Fun = fun(Filename) ->
          lfe_comp:file(Filename, Args)
        end,
  run_with_temp(Contents, Fun).

load_lfe(Name, Contents) ->
  LfeBinary = lfe_module_binary(Contents),
  load_lfe_binary(Name, LfeBinary).

load_lfe_binary(Name, LfeBinary) ->
  code:purge(Name),
  code:delete(Name),
  code:load_binary(Name, "lfe_binary_module", LfeBinary).
