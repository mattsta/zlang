-module(zog_site).

-compile(export_all).

%%%----------------------------------------------------------------------
%%% compile
%%%----------------------------------------------------------------------
compile({file, Filename}) ->
  {ok, Contents} = file:read_file(Filename),
  compile(Contents);
compile(Module) when is_binary(Module) ->
  compile(unicode:characters_to_list(Module));
compile(Module) when is_list(Module) ->
  {ok, Tokens, _} = zlang_lexer:string(Module),
  {ok, ParseTree} = zlang_parser:parse(Tokens),
  ParseTree.

source_to_binary(Module, Name) ->
  Compiled = compile(Module),
  Evald = mod(Compiled, Name),
  lfe_module_binary(iolist_to_binary(Evald)).

%%%----------------------------------------------------------------------
%%% evaluate primary forms
%%%----------------------------------------------------------------------
mod(Mod, ModName) ->
  mod(Mod, ModName, "default", "default").

mod({module, Stmts}, ModuleName, ClientNamespace, SiteNamespace) ->
  Mod =[args(["defmodule", ModuleName, args(["export", "all"])]), "\n\n",
        namespaces([{"client", ClientNamespace},
                    {"site", SiteNamespace}]),
        site_macros(),
        inline_stmts(Stmts)].
%  BMod = iolist_to_binary(Mod),
%  replace_until_done(BMod, "\\)\s+\\)", "))").

site_macros() ->
  PathToEbin = filename:dirname(code:where_is_file("zog_site.beam")),
  MacroSource = filename:join([PathToEbin, "..", "priv", "site_macros.lfe"]),
  {ok, MacroBin} = file:read_file(MacroSource),
  MacroBin.

namespaces(InboundNamespaces) ->
  Namespaces = [args([args([ato(Namespace)]), str(Value)]) ||
                {Namespace, Value} <- InboundNamespaces],
  topcall(["defsyntax", "namespace" | Namespaces]).

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
combine_functions(Other, Accum) ->
  [Other | Accum].


stmt({managed_http_function, PrimaryRoute, SubProps}) ->
  ExpandedSubProps =
  [args([ato(Method), strargs(RestPath), "\n",
         http_body(Body, Method, RestPath)]) ||
   {Method, RestPath, Body} <- SubProps],
  topcall(["create-http-function", PrimaryRoute, lst(ExpandedSubProps)]);
stmt({function, {local_fun, Name, Body}}) ->
  topcall(["local-function", Name, local_body(Body)]);
stmt({function, {local_fun, Name, Args, Body}}) ->
  topcall(["local-function", Name, args(Args), local_body(Body)]).


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
  [body(H) | http_body(T, Method, Path)].

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

local_body([]) -> [];
local_body([{equality, _, _} = Equality|RestBody]) ->
  equality(Equality, RestBody, fun(E) -> local_body(E) end);
local_body([H|T]) ->
  [body(H) | local_body(T)].


equality({equality, LocalNames, SourceValue}, RestBody, NextFun) ->
  UseLocalNames = name_or_names(LocalNames),
  ResolvedSource = case SourceValue of
                     S when is_tuple(S) -> body(S);
                      S when is_list(S) -> [body(E) || E <- S]
                   end,
  expr(["local-bind", proper(UseLocalNames), ResolvedSource,
        lst(NextFun(RestBody))]).

math_body([{math, Op, InnerRemaining} | More], Acc) ->
  math_body(More, [args(["math", Op, math_body(InnerRemaining, [])]) | Acc]);
math_body([StringNumber | More], Acc) ->
  math_body(More, [StringNumber | Acc]);
math_body([], Acc) -> args(lists:reverse(Acc)).


body({cxn, {_, Property}}) ->
  args(["cxn-property", bin(Property)]);

body({user, {_, Property}}) ->
  args(["user-property", bin(Property)]);

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
  expr(["safe-call", Function, arglist(lists:concat(Args))]).

%%%----------------------------------------------------------------------
%%% num safety
%%%----------------------------------------------------------------------
num_safe(X) when is_list(X) ->
  try
    list_to_integer(X)
  catch
    _:_ -> list_to_float(X)
  end.

%%%----------------------------------------------------------------------
%%% applier expanding
%%%----------------------------------------------------------------------
arglist(ArgList) ->
  expand_appliers(ArgList).

expand_appliers(Args) ->
  InlineExpanded = expand_inline_appliers(Args),
  expand_full_applier(InlineExpanded).

expand_inline_appliers(Stuff) ->
  expand_inline_appliers(Stuff, []).
expand_inline_appliers([], Accum) -> lists:reverse(Accum);
expand_inline_appliers([{applier, Applier}|T], Accum) ->
  Resolved =
  case Applier of
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
    fnc("re", "replace", ["tvar", str(What), str([]), atolst(["global"])])
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
  args(["lc", args(["<-", Target, From]), DoWhat]).

a2l({var, Var}) when is_list(Var) -> Var;
a2l(X) when is_atom(X) -> atom_to_list(X);
a2l(X) when is_list(X) andalso is_list(hd(X)) -> X;
a2l(X) when is_float(X) -> mochinum:digits(X);
a2l(X) when is_integer(X) -> integer_to_list(X);
a2l(X) when is_list(X) -> X.

proper({var, Var}) when is_list(Var) -> Var;
proper({str, Str}) when is_list(Str) -> bin(Str);
proper(Other) -> Other.

str({local_bind, Name}) -> Name;  % inject a top-level var here. nostr.
str(E) -> ["'\"", a2l(E), "\""].

flatstr(E) -> ["\"", a2l(E), "\""].

bin(B) when is_list(B) -> ["#b", lst(flatstr(B)), " "];
bin({str, Str}) when is_list(Str) -> bin(Str).

ato(A) when is_list(A) -> ["\'", A];
ato(A) when is_atom(A) -> ["\'", atom_to_list(A)].

tup({pair, Key, Value}) ->
  tup([Key, Value]);
tup(T) when is_tuple(T) ->
  args(["tuple" | tuple_to_list(T)]);
tup(T) when is_list(T) ->
  args(["tuple" | T]).

lst(E) -> ["(", E, ") "].

alst(E) when is_list(E) -> args(["list" | E]).

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
  code:purge(Name),
  code:delete(Name),
  code:load_binary(Name, "lfe_binary_module", LfeBinary).
