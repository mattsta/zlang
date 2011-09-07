Nonterminals
Module
Statements Statement
Vars InlineApplier ApplierType
Functions Function
FunctionBody
FunctionStatements FunctionStatement
FunctionName HttpFunctionName
InternalCall ExternalCall ArgNames
Names Name
SpacedNames
Args Arg
Comma
StoreDelim Storage TemporalUnit Term
.


Terminals '(' ')' ',' '->' '/' 'NL'
http_method
slash using
vars
math
convert by conversion_op
combine for names then values with combine
a within in events last the window day hour minute
atom var integer string set union intersection comparator uterm.

Rootsymbol Module.

Module -> Statements : {module, '$1'}.

Statements -> Statement : ['$1'].
Statements -> Statement Statements : ['$1'] ++ '$2'.

Statement -> Function : {function, '$1'}.

Function -> http_method HttpFunctionName '->' FunctionBody :
    {http, unwrap('$1'), '$2', '$4'}.
Function -> HttpFunctionName '->' FunctionBody :
    {http, '$1', '$3'}.
Function -> FunctionName Args '->' FunctionBody :
    {normal, '$1', '$2', '$4'}.

HttpFunctionName -> '/' Name : ['$2'].
HttpFunctionName -> '/' Name HttpFunctionName : ['$2'] ++ '$3'.

FunctionName -> Name : '$1'.

FunctionBody -> FunctionStatements : '$1'.

FunctionStatements -> FunctionStatement : ['$1'].
FunctionStatements -> FunctionStatement FunctionStatements : ['$1'] ++ '$2'.

FunctionStatement -> Vars : '$1'.
FunctionStatement -> Vars 'NL' : '$1'.
FunctionStatement -> ExternalCall : '$1'.
FunctionStatement -> ExternalCall 'NL' : '$1'.
FunctionStatement -> InternalCall : '$1'.
FunctionStatement -> InternalCall 'NL' : '$1'.


Vars -> vars ArgNames : {vars, '$2'}.

InlineApplier -> '(' ApplierType ')' : '$2'.
InlineApplier -> '(' math SpacedNames ')' : {math, unwrap('$2'), '$3'}.

% basic inline: (defaults abc, def, hij, ...) 
ApplierType -> Name Names : {'$1', '$2'}.
% converting things
ApplierType -> convert by conversion_op Name :
    {convert, unwrap('$3'), '$4'}.
% comprehension applier thing
ApplierType -> for vars conversion_op Names then combine names with values :
    {using, vars, unwrap('$3'), '$4', combine_name_values}.
ApplierType -> for Name conversion_op Names then combine names with values :
    {using, '$2', unwrap('$3'), '$4', combine_name_values}.

ExternalCall -> '(' Name ')' Name : {external_call, '$2', '$4'}.
ExternalCall -> '(' Name ')' Name ArgNames : {external_call, '$2', '$4', '$5'}.
ExternalCall -> '(' Name ')' Name using ArgNames :
    {external_call, '$2', '$4', '$6'}.

InternalCall -> Name Names : {call, '$1', '$2'}.
InternalCall -> Name using Names : {call, '$1', '$3'}.

ArgNames -> Name : ['$1'].
ArgNames -> Name InlineApplier : ['$1', {applier_full, '$2'}].
ArgNames -> Name Comma ArgNames : ['$1'] ++ '$3'.
ArgNames -> Name Comma InlineApplier : ['$1', {applier, '$3'}].

Args -> using Names : ['$2'].
Args -> with Names : ['$2'].

Names -> Name : ['$1'].
Names -> Name Comma Names : ['$1'] ++ '$3'.
%Names -> Name ',' 'NL' Names : ['$1'] ++ '$4'.

Name -> uterm : unwrap('$1').

Comma -> ',' : nil.
Comma -> ',' 'NL' : nil.

SpacedNames -> Name : ['$1'].
SpacedNames -> Name SpacedNames : ['$1'] ++ '$2'.

Term -> var '(' Term ')'   : {func, unwrap('$1'), '$3'}.
Term -> integer            : unwrap('$1').
Term -> var                : unwrap('$1').
Term -> string             : unwrap('$1').
Term -> uterm              : unwrap('$1').

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
