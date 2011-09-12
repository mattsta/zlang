Definitions.

D        = [0-9]
FLOAT    = [0-9\.]
METHOD   = (OPTIONS|GET|HEAD|POST|PUT|DELETE|TRACE|CONNECT)
L        = [A-Za-z]
SL       = [A-Za-z0-9-_\\]
% The newline in WS captures newlines in comments.  comment char is ; to EOL
WS       = ([\s\t\f]|;(.*\n?))
NL       = (\n|\n\s+)
C        = (<|<=|=|=>|>)
START_MATH  = \([\-\+\*\/]


Rules.

% HTTP methods
{METHOD}    : {token, {http_method, TokenLine, TokenChars}}.

% keywords
use         : {token,{use,TokenLine}}.
variable    : {token,{vars,TokenLine}}.
var         : {token,{vars,TokenLine}}.
variables   : {token,{vars,TokenLine}}.
vars        : {token,{vars,TokenLine}}.
by          : {token,{by,TokenLine}}.
convert     : {token,{convert,TokenLine}}.
combine     : {token,{combine,TokenLine}}.
pair        : {token,{combine,TokenLine}}.
then        : {token,{then,TokenLine}}.
names       : {token,{names,TokenLine}}.
values      : {token,{values,TokenLine}}.

% types of things to deal with
form      : {token,{vars_src,TokenLine,form}}.
server    : {token,{vars_src,TokenLine,list_to_atom(TokenChars)}}.
cookie    : {token,{vars_src,TokenLine,cookie}}.
cookies   : {token,{vars_src,TokenLine,cookie}}.

% function parts
is         : {token,{equals,TokenLine}}.
are        : {token,{equals,TokenLine}}.
=          : {token,{equals,TokenLine}}.
come       : {token,{come,TokenLine}}.

% foruse
for         : {token,{foruse,TokenLine}}.
using       : {token,{foruse,TokenLine}}.
having      : {token,{foruse,TokenLine}}.
with        : {token,{foruse,TokenLine}}.

% external module resolving
from        : {token,{from,TokenLine}}.

% conversion ops
removing    : {token,{conversion_op,TokenLine,remove}}.
remove      : {token,{conversion_op,TokenLine,remove}}.

% variable defaults
default      : {token,{default,TokenLine}}.

% cxn output
output       : {token,{output,TokenLine}}.
deliver      : {token,{output,TokenLine}}.
send         : {token,{output,TokenLine}}.
template     : {token,{output_type,TokenLine,list_to_atom(TokenChars)}}.
json         : {token,{output_type,TokenLine,list_to_atom(TokenChars)}}.
plain        : {token,{output_type,TokenLine,list_to_atom(TokenChars)}}.


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

as     : {token,{as,TokenLine}}.
:      : {token,{':',TokenLine}}.
\*      : {token,{'*',TokenLine}}.
{START_MATH} : {token,{math,TokenLine,tl(TokenChars)}}. 
/      : {token, {'/', TokenLine}}.
{C}    : {token,{comparator,TokenLine,list_to_atom(TokenChars)}}.

{D}+   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{FLOAT}+ : {token,{float,TokenLine,list_to_float(TokenChars)}}.

'{SL}+' : S = strip(TokenChars,TokenLen),
          {token,{uterm,TokenLine,S}}.
"{SL}+" : S = strip(TokenChars,TokenLen),
          {token,{uterm,TokenLine,S}}.
{SL}+   : {token,{uterm,TokenLine,TokenChars}}.

[(),]  : {token,{list_to_atom(TokenChars),TokenLine}}.
->     : {token,{'->', TokenLine}}.
{NL}   : {token,{'NL', TokenLine}}.
{WS}+  : skip_token.
\r     : skip_token.


Erlang code.

l2a(List) -> list_to_atom(List).

strip(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).
