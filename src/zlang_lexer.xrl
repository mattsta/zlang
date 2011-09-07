Definitions.

D        = [0-9]
METHOD   = (OPTIONS|GET|HEAD|POST|PUT|DELETE|TRACE|CONNECT)
L        = [A-Za-z]
SL       = [A-Za-z0-9-_\\]
WS       = ([\s\t\f]|;.*)  % whitespace is stuff or ; to end of line.
NL       = (\n|\n\s+)
C        = (<|<=|=|=>|>)
MATH     = (-|\+|\*|/)


Rules.

% HTTP methods
{METHOD}    : {token, {http_method, TokenLine, TokenChars}}.

% keywords
variable    : {token,{vars,TokenLine}}.
var         : {token,{vars,TokenLine}}.
variables   : {token,{vars,TokenLine}}.
vars        : {token,{vars,TokenLine}}.
by          : {token,{by,TokenLine}}.
convert     : {token,{convert,TokenLine}}.
combine     : {token,{combine,TokenLine}}.
pair        : {token,{combine,TokenLine}}.
for         : {token,{for,TokenLine}}.
using       : {token,{using,TokenLine}}.
having      : {token,{using,TokenLine}}.
with        : {token,{with,TokenLine}}.
then        : {token,{then,TokenLine}}.
names       : {token,{names,TokenLine}}.
values      : {token,{values,TokenLine}}.

% types of things to deal with
form      : {token,{form,TokenLine}}.
cookie    : {token,{cookie,TokenLine}}.
cookies   : {token,{cookie,TokenLine}}.

% function parts
is         : {token,{is,TokenLine}}.
are        : {token,{are,TokenLine}}.
come       : {token,{come,TokenLine}}.
from       : {token,{from,TokenLine}}.

% conversion ops
removing    : {token,{conversion_op,TokenLine,remove}}.
remove      : {token,{conversion_op,TokenLine,remove}}.

% temporal units
millisecond  : {token,{millisecond,TokenLine}}.
minute       : {token,{minute,TokenLine}}.
hour         : {token,{hour,TokenLine}}.
day          : {token,{day,TokenLine}}.

% post types
json     : {token,{json,TokenLine,list_to_atom(TokenChars)}}.
term     : {token,{term,TokenLine,list_to_atom(TokenChars)}}.

or     : {token,{union,TokenLine,list_to_atom(TokenChars)}}.
and    : {token,{intersection,TokenLine,list_to_atom(TokenChars)}}.

/      : {token, {'/', TokenLine}}.
{C}    : {token,{comparator,TokenLine,list_to_atom(TokenChars)}}.

'{SL}+' : S = strip(TokenChars,TokenLen),
          {token,{uterm,TokenLine,S}}.
"{SL}+" : S = strip(TokenChars,TokenLen),
          {token,{uterm,TokenLine,S}}.
{SL}+   : {token,{uterm,TokenLine,TokenChars}}.

{D}+   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
[(),]  : {token,{list_to_atom(TokenChars),TokenLine}}.
->     : {token,{'->', TokenLine}}.
{NL}+  : {token,{'NL', TokenLine}}.
{MATH} : {token,{math,TokenLine,list_to_atom(TokenChars)}}. 
{WS}+  : skip_token.
\r     : skip_token.


Erlang code.

l2a(List) -> list_to_atom(List).

strip(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).
