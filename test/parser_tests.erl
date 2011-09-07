-module(parser_tests).
-include_lib("eunit/include/eunit.hrl").

-define(E(A, B), ?assertEqual(A, B)).
-define(_E(A, B), ?_assertEqual(A, B)).


method_test() ->
  Program =
"GET /editor -> vars site, name (defaults web, front_page)
               deliver-editor-page site, name",
  {ok, Lexed, _} = zlang_lexer:string(Program),
  ProperParseTree =
{module,
        [{function,
            {http,"GET",
                ["editor"],
                [{vars,
                     ["site","name",
                     {applier_full,{"defaults",["web","front_page"]}}]},
                 {call,"deliver-editor-page",["site","name"]}]}}]},
  ?E({ok, ProperParseTree}, zlang_parser:parse(Lexed)).

nomethod_test() ->
  Program =
"/editor -> vars site, name (defaults web, front_page)
               deliver-editor-page site, name",
  {ok, Lexed, _} = zlang_lexer:string(Program),
  ProperParseTree =
{module,
        [{function,
            {http,
                ["editor"],
                [{vars,
                     ["site","name",
                     {applier_full,{"defaults",["web","front_page"]}}]},
                 {call,"deliver-editor-page",["site","name"]}]}}]},
  ?E({ok, ProperParseTree}, zlang_parser:parse(Lexed)).

larger_method_test() ->
  Program =
"POST /editor -> vars mhtml, html, markdown, site, name, type,
                     js, coffee, css, scss (convert by remove \\r)
                (zog_template) write_template site, name,
                  (for vars remove site, name then combine names with values)
                (zog_template) refresh_cache
                deliver-editor-page using site, name",
  {ok, Lexed, _} = zlang_lexer:string(Program),
  ProperParseTree =
{module,
    [{function,
         {http,"POST",
             ["editor"],
             [{vars,
                  ["mhtml","html","markdown","site","name","type","js",
                   "coffee","css","scss",
                   {applier_full,{convert,remove,"\\r"}}]},
              {external_call,"zog_template","write_template",
                  ["site","name",
                   {applier,
                       {using,vars,remove,
                           ["site","name"],
                           combine_name_values}}]},
              {external_call,"zog_template","refresh_cache"},
              {call,"deliver-editor-page",["site","name"]}]}}]},
  ?E({ok, ProperParseTree}, zlang_parser:parse(Lexed)).
