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
%%%----------------------------------------------------------------------
%%% evaluate primary forms
%%%----------------------------------------------------------------------
mod(Mod) ->
  mod(Mod, "default", "default").

mod({module, Stmts}, ClientNamespace, SiteNamespace) ->
  Mod =["(defmodule poopin (export all))\n\n",
        namespaces([{"client", ClientNamespace},
                    {"site", SiteNamespace}]),
        site_macros(),
        [stmt(S) || S <- Stmts]],
  BMod = iolist_to_binary(Mod),
  replace_until_done(BMod, "\\)\s+\\)", "))").

site_macros() ->
  PathToEbin = filename:dirname(code:where_is_file("zog_site.beam")),
  MacroSource = filename:join([PathToEbin, "..", "priv", "site_macros.lfe"]),
  {ok, MacroBin} = file:read_file(MacroSource),
  MacroBin.

namespaces(InboundNamespaces) ->
  Namespaces = [args([args([ato(Namespace)]), str(Value)]) ||
                {Namespace, Value} <- InboundNamespaces],
  expr(["defsyntax", "namespace" | Namespaces]).

stmt({function, {http, Method, Path, Body}}) ->
  expr(["http-function", ato(Method), args(Path), "\n",
          http_body(Body, Method, Path)]);
stmt({function, {http, Path, Body}}) ->
  stmt({function, {http, 'GET', Path, Body}}).


http_body([], _Method, _Path) -> [];
http_body([{vars, Vars}|RestBody], Method, Path) ->
  VarsAppliersRemoved = remove_full_applier(Vars),
  [indent(l),
   expr(["with-http-vars", Method,
         args(VarsAppliersRemoved), fapplier(Vars, "tvar"),
         lst(http_body(RestBody, Method, Path))])];
http_body([H|T], Method, Path) ->
  [body(H) | http_body(T, Method, Path)].


body({external_call, Module, Function}) ->
  expr(["safe-external-call", Module, Function]);

body({external_call, Module, Function, Args}) ->
  expr(["safe-external-call", Module, Function, arglist(Args)]);

body({call, Function}) ->
  expr(["safe-call", Function]);

body({call, Function, Args}) ->
  expr(["safe-call", Function, arglist(Args)]).

arglist(ArgList) ->
  expand_appliers(ArgList).

%%%----------------------------------------------------------------------
%%% applier expanding
%%%----------------------------------------------------------------------
expand_appliers(Args) ->
  InlineExpanded = expand_inline_appliers(Args),
  expand_full_applier(InlineExpanded).

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
  fnc("re", "remove", ["tvar", str(What), atolst(["global"])]).

expand_inline_appliers(Stuff) ->
  expand_inline_appliers(Stuff, []).
expand_inline_appliers([], Accum) -> lists:reverse(Accum);
expand_inline_appliers([{applier, Applier}|T], Accum) ->
  Resolved =
  case Applier of
    {using, vars, remove, Members, Then} ->
      ResolvedThen = case Then of
                       combine_name_values -> ato("zip-name-values")
                     end,
      args(["remove-from-vars-then", args(Members), ResolvedThen])
  end,
  expand_inline_appliers(T, [Resolved | Accum]);
expand_inline_appliers([H|T], Accum) ->
  expand_inline_appliers(T, [H | Accum]).
   



%%%----------------------------------------------------------------------
%%% output helpers
%%%----------------------------------------------------------------------
function_name_with_path([Base | T]) ->
  {Base, args([str(X) || X <- T])}.

args(Args) ->
  lst(space(Args)).

expr(Args) ->
  [args(Args), "\n"].

lc(Target, From, DoWhat) ->
  args(["lc", args(["<-", Target, From]), DoWhat]).

a2l(X) when is_atom(X) -> atom_to_list(X);
a2l(X) when is_list(X) andalso is_list(hd(X)) -> X;
a2l(X) when is_list(X) -> X.

str(E) when is_list(E) -> ["'\"", E, "\""].

ato(A) when is_list(A) -> ["\'", A].

lst(E) -> ["(", E, ") "].

atolst(A) -> ["'", args(A)].

fnc(Mod, Fnc, Args) ->
  ["(: ", Mod, " ", Fnc, " ", string:join(Args, " "), ")"].

indent(l) -> " ";
indent(v) -> [indent(l), "       "].

space(Things) ->
  string:join(Things, " ").

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
