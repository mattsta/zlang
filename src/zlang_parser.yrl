Nonterminals
Module
Statements Statement
Vars InlineApplier ApplierType InlineFun InlineFunHead InlineFuns InlineFunStmt
InlineRunnables SingleInlineStmt
Function
FunctionBody
FunctionStatements FunctionStatement FunctionRunnable
FunctionName HttpFunctionName
HttpFunctionBody
HttpFunctionStatements HttpFunctionStatement HttpRunnable
InternalCall ExternalCall ArgName ArgNames ArgNamesArgs
Names Name NameStr
SpacedNames
ForUse ForUseArgs ForUseArgsArgs
Equality Delivery
Comma Redo
MathApplier MathTerms
NLEater Number Numbers
Pair Pairs ExistingPlusMore
Str AnyList
Logging
Find Write Get WriteIndex
Update UpdateAction UpdateActions
Unique
StoreDelim Storage TemporalUnit Term
GlobalActor AnyName AnyArg
AnyArgN AnyListN AnyNameN AnySubMatcher
.


Terminals '(' ')' ',' '->' '/' 'NL' ':' '*' ';'
http_method
slash using
vars
math cxn user whisper
use form cookie cookies and fields are come from is
convert conversion_op
pair combine for names values
a within in events last the window day hour minute
foruse vars_src equals output output_type
float as default ustr
all find one get write
add remove clear delete update
index quot done match redo
unique big small
atom var integer string set union intersect comparator uterm.

%%%----------------------------------------------------------------------
%%% Starting
%%%----------------------------------------------------------------------
Rootsymbol Module.

%Nonassoc 100 FunctionStatements.

Module -> NLEater Statements : {module, '$2'}.
Module -> Statements : {module, '$1'}.

% Blindly slurp up all consecutive newlines
NLEater -> 'NL' : [].
NLEater -> 'NL' NLEater : [].

%%%----------------------------------------------------------------------
%%% Top-Level statements
%%%----------------------------------------------------------------------
Statements -> Statement : ['$1'].
Statements -> Statement Statements : ['$1'] ++ '$2'.

Statement -> Function : {function, '$1'}.

%%%----------------------------------------------------------------------
%%% Function Heads
%%%----------------------------------------------------------------------
Function -> http_method HttpFunctionName '->' NLEater HttpFunctionBody :
    {http, unwrap('$1'), '$2', '$5'}.
Function -> HttpFunctionName '->' NLEater HttpFunctionBody :
    {http, '$1', '$4'}.
Function -> FunctionName '->' NLEater FunctionBody :
    {local_fun, '$1', '$4'}.
Function -> FunctionName ForUseArgs '->' NLEater FunctionBody :
    {local_fun, '$1', '$2', '$5'}.

HttpFunctionName -> '/' Name : ['$2'].
HttpFunctionName -> '/' ':' Name ':' : [{local_bind, '$3'}].
HttpFunctionName -> '/' '*' : [match_tail].
HttpFunctionName -> '/' Name HttpFunctionName : ['$2'] ++ '$3'.

FunctionName -> Name : '$1'.

HttpFunctionBody -> HttpFunctionStatements NLEater : '$1'.

FunctionBody -> FunctionStatements NLEater : '$1'.

%%%----------------------------------------------------------------------
%%% Function Components
%%%----------------------------------------------------------------------
HttpFunctionStatements -> HttpFunctionStatement : ['$1'].
HttpFunctionStatements -> HttpFunctionStatement HttpFunctionStatements :
    ['$1'] ++ '$2'.
FunctionStatements -> FunctionStatement : ['$1'].
FunctionStatements -> FunctionStatement FunctionStatements : ['$1'] ++ '$2'.

HttpFunctionStatement -> HttpRunnable 'NL' : '$1'.
HttpFunctionStatement -> FunctionRunnable 'NL' : '$1'.
HttpRunnable -> Vars : '$1'.

FunctionStatement -> FunctionRunnable 'NL' : '$1'.

FunctionRunnable -> Delivery           : '$1'.
FunctionRunnable -> InlineApplier      : '$1'.
FunctionRunnable -> AnyName            : '$1'.
FunctionRunnable -> Find               : '$1'.
FunctionRunnable -> Write              : '$1'.
FunctionRunnable -> Get                : '$1'.
FunctionRunnable -> Update             : '$1'.
FunctionRunnable -> Equality           : '$1'.
FunctionRunnable -> ExternalCall       : '$1'.
FunctionRunnable -> InternalCall       : '$1'.
FunctionRunnable -> InlineFun          : '$1'.
FunctionRunnable -> Redo               : '$1'.
FunctionRunnable -> Logging            : '$1'.
FunctionRunnable -> Unique             : '$1'.

%%%----------------------------------------------------------------------
%%% Uniqueness
%%%----------------------------------------------------------------------
Unique -> unique big       : {unique, big}.
Unique -> unique small     : {unique, small}.

%%%----------------------------------------------------------------------
%%% Logging
%%%----------------------------------------------------------------------
Logging -> whisper AnyListN : {whisper, '$2'}.

%%%----------------------------------------------------------------------
%%% Setting / Equality
%%%----------------------------------------------------------------------
Equality -> Names equals ExternalCall  : {equality, '$1', '$3'}.
Equality -> Names equals InternalCall  : {equality, '$1', '$3'}.
Equality -> Names equals InlineApplier : {equality, '$1', '$3'}.
Equality -> Names equals Pairs         : {equality, '$1', '$3'}.
Equality -> Names equals GlobalActor   : {equality, '$1', '$3'}.
Equality -> Names equals ExistingPlusMore : {equality, '$1', '$3'}.
Equality -> Names equals Get              : {equality, '$1', '$3'}.
Equality -> Names equals Find             : {equality, '$1', '$3'}.
Equality -> Names equals AnyList          : {equality, '$1', delist('$3')}.
Equality -> Names equals InlineFun        : {equality, '$1', '$3'}.
Equality -> Names equals Numbers          : {equality, '$1', {numbers, '$3'}}.
Equality -> Names equals Unique           : {equality, '$1', '$3'}.

Pair -> '(' pair AnyNameN AnyNameN ')' : {pair, '$3', '$4'}.
Pair -> '(' AnyNameN AnyNameN ')'      : {pair, '$2', '$3'}.
Pair -> quot '(' NameStr NameStr ')'         : {pair, '$3', '$4'}.
Pairs -> Pair : ['$1'].
Pairs -> Pair Comma Pairs : ['$1'] ++ '$3'.

ExistingPlusMore -> Name foruse pair Pairs : {append, '$1', '$4'}.
ExistingPlusMore -> Name foruse Pairs : {append, '$1', '$3'}.

Vars -> use vars_src vars ArgNames : {vars, unwrap('$2'), '$4'}.
Vars -> use vars_src ArgNames : {vars, unwrap('$2'), '$3'}.

GlobalActor -> '(' cxn AnyName ')' : {cxn, '$3'}.
GlobalActor -> '(' user AnyName ')' : {cxn, '$3'}.

%%%----------------------------------------------------------------------
%%% Inline Funs
%%%----------------------------------------------------------------------
InlineFun -> InlineFunHead SingleInlineStmt done :
    {inline_fun, {against, '$1'}, {stmts, '$2'}}.
InlineFun -> InlineFunHead InlineFuns done :
    {inline_fun, {against, '$1'}, {stmts, '$2'}}.

InlineFunHead -> match AnyList         : {'$2', lineno('$1')}.
InlineFunHead -> match AnyList NLEater : {'$2', lineno('$1')}.

InlineFuns -> InlineFunStmt            : ['$1'].
InlineFuns -> InlineFunStmt InlineFuns : ['$1'] ++ '$2'.

InlineFunStmt -> ForUseArgs '->' NLEater InlineRunnables NLEater :
    {stmt, {args, '$1'}, {body, '$4'}}.
InlineFunStmt -> ForUseArgs '->' InlineRunnables NLEater :
    {stmt, {args, '$1'}, {body, '$3'}}.

InlineRunnables -> FunctionRunnable 'NL' : ['$1'].
InlineRunnables -> FunctionRunnable 'NL' InlineRunnables : ['$1'] ++ '$3'.


SingleInlineStmt -> ForUseArgs '->' FunctionRunnable :
    [{stmt, {args, '$1'}, {body, ['$3']}}].

Redo -> redo AnyList        : {redo, '$2'}.
Redo -> redo foruse AnyList : {redo, '$3'}.

%%%----------------------------------------------------------------------
%%% Reading
%%%----------------------------------------------------------------------
Find -> find all AnyName foruse Pairs : {find, all, '$3', '$5'}.
Find -> find all AnyName foruse Pairs foruse AnyList :
    {find, all, '$3', '$5', using, '$7'}.
Find -> find all AnyName foruse Pairs WriteIndex :
    {find, all, '$3', '$5', using, '$6'}.
%Find -> find one AnyName foruse Pairs : {find, one, '$3', '$5'}.

%%%----------------------------------------------------------------------
%%% Getting fields of proplists
%%%----------------------------------------------------------------------
Get -> get AnyList from Name  : {get, var, '$2', '$4'}.
Get -> get AnyList from Pairs : {get, proplist, '$2', '$4'}.

%%%----------------------------------------------------------------------
%%% Writing
%%%----------------------------------------------------------------------
WriteIndex -> index AnyList : '$2'.
WriteIndex -> index foruse AnyList : '$3'.
WriteIndex -> find foruse AnyList : '$3'.
WriteIndex -> intersect index foruse AnyList : '$4'.
WriteIndex -> intersect find foruse AnyList : '$4'.

Write -> write AnyName Pairs WriteIndex :
    {write, {type, '$2'}, {contents, '$3'}, {index, '$4'}}.

%%%----------------------------------------------------------------------
%%% Updating
%%%----------------------------------------------------------------------
UpdateAction -> set Pair       : {set, '$2'}.
UpdateAction -> add Pair       : {add, '$2'}.
UpdateAction -> remove Pair    : {remove, '$2'}.
UpdateAction -> clear Str      : {clear, '$2'}.
UpdateAction -> delete Str     : {delete, '$2'}.

UpdateActions -> UpdateAction : ['$1'].
UpdateActions -> UpdateAction Comma UpdateActions : ['$1'] ++ '$3'.

Update -> update '(' Find ')' UpdateActions  : {update, {find, '$3'}, '$5'}.
Update -> update Name UpdateActions          : {update, {obj, '$2'}, '$3'}.

%%%----------------------------------------------------------------------
%%% Appliers
%%%----------------------------------------------------------------------
InlineApplier -> '(' ApplierType ')'       : '$2'.
InlineApplier -> MathApplier               : '$1'.

% basic inline: (defaults abc, def, hij, ...)
ApplierType -> Name Names : {'$1', '$2'}.
% converting things
ApplierType -> convert foruse conversion_op Name :
    {convert, unwrap('$3'), '$4'}.
% comprehension applier thing
ApplierType -> foruse vars conversion_op Names intersect pair names ',' values :
    {using, vars, unwrap('$3'), '$4', combine_name_values}.
ApplierType -> foruse Name conversion_op Names intersect pair names ',' values :
    {using, '$2', unwrap('$3'), '$4', combine_name_values}.

%%%----------------------------------------------------------------------
%%% Maths!
%%%----------------------------------------------------------------------
% MathApplier is a bit odd because 'math' *includes* the first '('
% *because* '/' is already a top level token for URL naming.
MathApplier -> math MathTerms ')' : {math, unwrap('$1'), '$2'}.

MathTerms -> Name : ['$1'].          % we can do math on varialbes
MathTerms -> Number : ['$1'].        % and numbers
MathTerms -> MathApplier : ['$1'].   % and other maths
MathTerms -> Name MathTerms : ['$1'] ++ '$2'.   % and variables with other terms
MathTerms -> Number MathTerms : ['$1'] ++ '$2'. % and numbers with other terms
MathTerms -> MathApplier MathTerms : ['$1'] ++ '$2'.  % and math on math action

%%%----------------------------------------------------------------------
%%% Calls
%%%----------------------------------------------------------------------
ExternalCall -> Name from '(' uterm Name ')' : {external_call, '$5', '$1'}.
ExternalCall -> Name ForUse from '(' uterm Name ')' :
    {external_call, '$6', '$1', '$2'}.

InternalCall -> Name ForUse : {call, '$1', '$2'}.

ForUseArgs -> ForUseArgsArgs : lists:flatten('$1').

ForUseArgsArgs -> AnyArgN                             : ['$1'].
ForUseArgsArgs -> AnyArgN Comma ForUseArgsArgs        : ['$1'] ++ '$3'.
ForUseArgsArgs -> foruse AnyArgN                      : ['$2'].
ForUseArgsArgs -> foruse AnyArgN Comma ForUseArgsArgs : ['$2'] ++ '$4'.

ForUse -> foruse ArgNames : ['$2'].
ForUse -> foruse ArgNames ForUse : ['$2'] ++ '$3'.
ForUse -> foruse 'NL' ArgNames : ['$3'].

%%%----------------------------------------------------------------------
%%% Args, Names, ArgNames
%%%----------------------------------------------------------------------
ArgName -> Name : '$1'.
ArgName -> Str  : '$1'.
ArgName -> Name as Name : {alias, '$1', '$3'}.
ArgName -> Name default AnyName : {default, '$1', '$3'}.
ArgName -> Name as Name default Name : {alias, '$1', '$3', default, '$5'}.
ArgName -> Name default AnyName as Name : {alias, '$1', '$5', default, '$3'}.
ArgName -> InlineApplier : {applier, '$1'}.

ArgNames -> ArgNamesArgs : lists:flatten('$1').

ArgNamesArgs -> ArgName                    : ['$1'].
ArgNamesArgs -> ArgName Comma ArgNamesArgs : ['$1'] ++ '$3'.
ArgNamesArgs -> ArgName Comma ArgNamesArgs InlineApplier :
    ['$1'] ++ ['$3', {applier_full, '$4'}].

Names -> Name : ['$1'].
Names -> Name Comma Names : ['$1'] ++ '$3'.

Name -> uterm : {var, unwrap('$1')}.
Str -> ustr   : {str, unwrap('$1')}.

NameStr -> uterm : {str, unwrap('$1')}.
NameStr -> ustr  : {str, unwrap('$1')}.

AnyName -> Name   : '$1'.
AnyName -> Str    : '$1'.

AnyList -> AnyArg               : ['$1'].
AnyList -> AnyArg Comma AnyList : ['$1'] ++ '$3'.

AnyArg -> AnyName             : '$1'.
AnyArg -> AnyName ':' AnyName : {first_rest, '$1', '$3'}.

Comma -> ','      : nil.
Comma -> ',' 'NL' : nil.

% AnySubMatcher is necessary if we are processing nested lists of lists.
% Example: With a target of [[a, b, c], [d, e, f]], we can't just use:
%   match target with first, second, third : rest -- rest will be bound to
% (third .rest) and NOT (first, second, third) : rest.  That's what ASM allows.
%  So now we: match target with (first, second, third) : rest -> redo rest
AnySubMatcher -> '(' AnyListN ')'  : '$2'.
AnyNameN -> Name   : '$1'.
AnyNameN -> Str    : '$1'.
AnyNameN -> Number : '$1'.
AnyArgN -> AnyNameN             : '$1'.
AnyArgN -> AnySubMatcher             : '$1'.
AnyArgN -> AnyNameN ':' AnyNameN : {first_rest, '$1', '$3'}.
AnyArgN -> AnySubMatcher ':' AnyNameN : {first_rest, {submatched, '$1'}, '$3'}.
AnyListN -> AnyArgN               : ['$1'].
AnyListN -> AnyArgN Comma AnyListN : ['$1'] ++ '$3'.

%%%----------------------------------------------------------------------
%%% number/term
%%%----------------------------------------------------------------------
Number -> integer            : unwrap('$1').
Number -> float              : unwrap('$1').

Numbers -> Number               : ['$1'].
Numbers -> Number Comma Numbers : ['$1'] ++ '$3'.

Term -> var '(' Term ')'   : {func, unwrap('$1'), '$3'}.
Term -> Number             : '$1'.
Term -> var                : unwrap('$1').
Term -> string             : unwrap('$1').
Term -> uterm              : unwrap('$1').

%%%----------------------------------------------------------------------
%%% Meta-specific built-ins (template delivery, XHR, comet, pubsub, ...)
%%%----------------------------------------------------------------------
Delivery -> output Name : {output, "plain", ['$2']}.
Delivery -> output output_type ForUse : {output, unwrap('$2'), '$3'}.

%%%----------------------------------------------------------------------
%%% remnants from reverse-pubsub
%%%----------------------------------------------------------------------
StoreDelim -> within a : nil.
StoreDelim -> within : nil.
StoreDelim -> in a : nil.
StoreDelim -> in : nil.

% in the last 5 events
% in last 5 events
% 5 minute window
Storage -> last integer events          : {store_events, '$3'}.
Storage -> the last integer events      : {store_events, '$3'}.
Storage -> last TemporalUnit            : {store_window, '$2'}.
Storage -> the last TemporalUnit        : {store_window, '$3'}.
Storage -> integer TemporalUnit window  : {store_window, {unwrap('$1'), '$2'}}.

TemporalUnit -> minute : minute.
TemporalUnit -> hour   : hour.
TemporalUnit -> day    : day.

%predicates -> predicate union predicate : {union, '$1', '$3'}.
%predicates -> predicates union predicate : {union, '$1', '$3'}.
%
%predicates -> predicate intersect predicate :
%              {intersect, '$1', '$3'}.
%
%predicate -> var set list :
%            {predicate, {var, unwrap('$1')}, memberof, '$3'}.
%
%predicate -> var comparator element :
%            {predicate, {var, unwrap('$1')}, unwrap('$2'), '$3'}.
%
%list -> '(' ')' : nil.
%list -> '(' elements ')' : {list,'$2'}.
%
%elements -> element : ['$1'].
%elements -> element ',' elements : ['$1'] ++ '$3'.
%element -> atom : '$1'.
%element -> var : unwrap('$1').
%element -> integer : unwrap('$1').
%element -> string : unwrap('$1').

Erlang code.

unwrap({_,_,V}) -> V;
unwrap({V,_}) -> V.

lineno({_,L}) -> integer_to_list(L).

delist([A]) -> A;
delist(Other) -> Other.
