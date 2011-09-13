Nonterminals
Module
Statements Statement
Vars InlineApplier ApplierType
Function
FunctionBody
FunctionStatements FunctionStatement
FunctionName HttpFunctionName
HttpFunctionBody
HttpFunctionStatements HttpFunctionStatement
InternalCall ExternalCall ArgName ArgNames
Names Name
SpacedNames
ForUse ForUseArgs
EqualityFirst Equality Delivery
Comma
MathApplier MathTerms
NLEater Number
Pair Pairs ExistingPlusMore
Str
StoreDelim Storage TemporalUnit Term
AnyName
.


Terminals '(' ')' ',' '->' '/' 'NL' ':' '*'
http_method
slash using
vars
math
use form cookie cookies and fields are come from is
convert by conversion_op
pair combine for names then values
a within in events last the window day hour minute
foruse vars_src equals output output_type
float as default ustr
atom var integer string set union intersection comparator uterm.

%%%----------------------------------------------------------------------
%%% Starting
%%%----------------------------------------------------------------------
Rootsymbol Module.

Module -> NLEater Statements : {module, '$2'}.
Module -> Statements : {module, '$1'}.
Module -> Statements NLEater : {module, '$1'}.

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
Function -> http_method HttpFunctionName '->' 'NL' HttpFunctionBody :
    {http, unwrap('$1'), '$2', '$5'}.
Function -> HttpFunctionName '->' 'NL' HttpFunctionBody :
    {http, '$1', '$4'}.
Function -> FunctionName '->' 'NL' FunctionBody :
    {local_fun, '$1', '$4'}.
Function -> FunctionName ForUseArgs '->' 'NL' FunctionBody :
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

HttpFunctionStatement -> Vars : '$1'.
HttpFunctionStatement -> Vars 'NL' : '$1'.
HttpFunctionStatement -> FunctionStatement : '$1'.
%FunctionStatement -> Delivery : '$1'.
FunctionStatement -> Delivery 'NL' : '$1'.
FunctionStatement -> Equality : '$1'.
FunctionStatement -> Equality 'NL' : '$1'.
%FunctionStatement -> ExternalCall : '$1'.
FunctionStatement -> ExternalCall 'NL' : '$1'.
%FunctionStatement -> InternalCall : '$1'.
FunctionStatement -> InternalCall 'NL' : '$1'.

%%%----------------------------------------------------------------------
%%% Setting / Equality
%%%----------------------------------------------------------------------
Equality -> Names equals ExternalCall  : {equality, '$1', '$3'}.
Equality -> Names equals InternalCall  : {equality, '$1', '$3'}.
Equality -> Names equals InlineApplier : {equality, '$1', '$3'}.
Equality -> Names equals Pairs         : {equality, '$1', '$3'}.
Equality -> Names equals ExistingPlusMore : {equality, '$1', '$3'}.

Pair -> '(' pair AnyName AnyName ')' : {pair, '$3', '$4'}.
Pair -> '(' AnyName AnyName ')' : {pair, '$2', '$3'}.
Pairs -> Pair : ['$1'].
Pairs -> Pair Comma Pairs : ['$1'] ++ '$3'.

ExistingPlusMore -> Name foruse pair Pairs : {append, '$1', '$4'}.
ExistingPlusMore -> Name foruse Pairs : {append, '$1', '$3'}.

Vars -> use vars_src vars ArgNames : {vars, unwrap('$2'), '$4'}.
Vars -> use vars_src ArgNames : {vars, unwrap('$2'), '$3'}.

%%%----------------------------------------------------------------------
%%% Appliers
%%%----------------------------------------------------------------------
InlineApplier -> '(' ApplierType ')' : '$2'.
InlineApplier -> '(' ApplierType ')' 'NL' : '$2'.
InlineApplier -> MathApplier : '$1'.
InlineApplier -> MathApplier 'NL' : '$1'.

% basic inline: (defaults abc, def, hij, ...)
ApplierType -> Name Names : {'$1', '$2'}.
% converting things
ApplierType -> convert by conversion_op Name :
    {convert, unwrap('$3'), '$4'}.
% comprehension applier thing
ApplierType -> foruse vars conversion_op Names then pair names ',' values :
    {using, vars, unwrap('$3'), '$4', combine_name_values}.
ApplierType -> foruse Name conversion_op Names then pair names ',' values :
    {using, '$2', unwrap('$3'), '$4', combine_name_values}.

%%%----------------------------------------------------------------------
%%% Maths!
%%%----------------------------------------------------------------------
% MathApplier is a bit odd because 'math' *includes* the first '('
% *because* '/' is already a top level token for URL naming.
MathApplier -> math MathTerms ')' : {math, unwrap('$1'), '$2'}.

MathTerms -> Name : ['$1'].
MathTerms -> Number : ['$1'].
MathTerms -> MathApplier : ['$1'].
MathTerms -> Name MathTerms : ['$1'] ++ '$2'.
MathTerms -> Number MathTerms : ['$1'] ++ '$2'.
MathTerms -> MathApplier MathTerms : ['$1'] ++ '$2'.

%%%----------------------------------------------------------------------
%%% Calls
%%%----------------------------------------------------------------------
ExternalCall -> Name from '(' uterm Name ')' : {external_call, '$5', '$1'}.
ExternalCall -> Name ForUse from '(' uterm Name ')' :
    {external_call, '$6', '$1', '$2'}.
ExternalCall -> Name ForUse from 'NL' '(' uterm Name ')' :
    {external_call, '$7', '$1', '$2'}.

InternalCall -> Name ForUse : {call, '$1', '$2'}.

ForUseArgs -> foruse Names : '$2'.

ForUse -> foruse ArgNames : ['$2'].
ForUse -> foruse ArgNames ForUse : ['$2'] ++ '$3'.
ForUse -> foruse 'NL' ArgNames : ['$3'].

%%%----------------------------------------------------------------------
%%% Args, Names, ArgNames
%%%----------------------------------------------------------------------
ArgName -> Name : '$1'.
ArgName -> Name as Name : {alias, '$1', '$3'}.
ArgName -> Name default AnyName : {default, '$1', '$3'}.
ArgName -> Name as Name default Name : {alias, '$1', '$3', default, '$5'}.
ArgName -> Name default AnyName as Name : {alias, '$1', '$5', default, '$3'}.
ArgName -> InlineApplier : {applier, '$1'}.

ArgNames -> ArgName : ['$1'].
ArgNames -> ArgName Comma ArgNames : lists:flatten(['$1'] ++ '$3').
ArgNames -> ArgName Comma ArgNames InlineApplier :
    lists:flatten(['$1'] ++ ['$3', {applier_full, '$4'}]).

Names -> Name : ['$1'].
Names -> Name Comma Names : ['$1'] ++ '$3'.

Name -> uterm : {var, unwrap('$1')}.
Str -> ustr   : {str, unwrap('$1')}.

AnyName -> Name : '$1'.
AnyName -> Str  : '$1'.

Comma -> ',' : nil.
Comma -> ',' 'NL' : nil.

SpacedNames -> Name : ['$1'].
SpacedNames -> Name SpacedNames : ['$1'] ++ '$2'.

%%%----------------------------------------------------------------------
%%% number/term
%%%----------------------------------------------------------------------
Number -> integer            : unwrap('$1').
Number -> float              : unwrap('$1').

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
%predicates -> predicate intersection predicate :
%              {intersection, '$1', '$3'}.
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
