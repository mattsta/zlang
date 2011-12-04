Definitions.

D        = [0-9]
FLOAT    = [0-9\.]
METHOD   = (OPTIONS|GET|HEAD|POST|PUT|DELETE|TRACE|CONNECT)
L        = [A-Za-z]
SL       = [A-Za-z0-9-\?\$\&\@\+_\\]
% The newline in WS captures newlines in comments.  comment char is ; to EOL
WS       = [\s\t\f]
COMMENT  = #.*\n?
NL       = (\n|\n\s+)
C        = (<|<=|=|=>|>)
START_MATH  = \([\-\+\*\/\%]
%ATOM      = ([^"\s+,\(\)])  % this is still too liberal.  captures \n, etc
ATOM      = ([A-Za-z0-9_-])
DOUBLE_QUOTED = "(\\\^.|\\.|[^\"])*"


Rules.

% HTTP methods
import      : {token,{import,TokenLine}}.

% HTTP methods
{METHOD}    : {token, {http_method, TokenLine, TokenChars}}.

% simple comprehensions
over      : {token,{over,TokenLine}}.

% async
async      : {token,{async,TokenLine}}.
wait       : {token,{wait,TokenLine}}.

% run a no-arg function
do          : {token,{do,TokenLine}}.
run         : {token,{do,TokenLine}}.

% unique things
unique      : {token,{unique,TokenLine}}.

% locking things
lock        : {token,{lock,TokenLine}}.

% sizes
big         : {token,{big,TokenLine}}.
long        : {token,{big,TokenLine}}.
small       : {token,{small,TokenLine}}.
short       : {token,{small,TokenLine}}.

% logging
whisper     : {token,{whisper,TokenLine}}.
say         : {token,{whisper,TokenLine}}.
good         : {token,{good,TokenLine}}.
bueno        : {token,{good,TokenLine}}.
bad          : {token,{bad,TokenLine}}.
fail         : {token,{bad,TokenLine}}.
mal          : {token,{bad,TokenLine}}.
malo         : {token,{bad,TokenLine}}.

% keywords
variable    : {token,{vars,TokenLine}}.
var         : {token,{vars,TokenLine}}.
variables   : {token,{vars,TokenLine}}.
vars        : {token,{vars,TokenLine}}.
convert     : {token,{convert,TokenLine}}.
names       : {token,{names,TokenLine}}.
values      : {token,{values,TokenLine}}.

% pattern matching/inline fun expansion
match        : {token,{match,TokenLine}}.
redo         : {token,{redo,TokenLine}}.


% end pattern matching/inline fun expansion
done        : {token,{done,TokenLine}}.
end         : {token,{done,TokenLine}}.
fin         : {token,{done,TokenLine}}.
unlock      : {token,{done,TokenLine}}.
unmatch     : {token,{done,TokenLine}}.

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

% data doers
find          : {token,{find,TokenLine}}.
all           : {token,{all,TokenLine}}.
one           : {token,{one,TokenLine}}.
get           : {token,{get,TokenLine}}.
write         : {token,{write,TokenLine}}.
update        : {token,{update,TokenLine}}.
index         : {token,{index,TokenLine}}.

% data modifiers
set            : {token,{set,TokenLine}}.
add            : {token,{add,TokenLine}}.
clear          : {token,{clear,TokenLine}}.
delete         : {token,{delete,TokenLine}}.

% foruse
for         : {token,{foruse,TokenLine}}.
using       : {token,{foruse,TokenLine}}.
having      : {token,{foruse,TokenLine}}.
with        : {token,{foruse,TokenLine}}.
by          : {token,{foruse,TokenLine}}.

% external module resolving and property getting
from        : {token,{from,TokenLine}}.
in          : {token,{from,TokenLine}}.

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

% our built-in datatypes
pair         : {token,{pair,TokenLine}}.

% temporal units
millisecond  : {token,{millisecond,TokenLine}}.
minute       : {token,{minute,TokenLine}}.
hour         : {token,{hour,TokenLine}}.
day          : {token,{day,TokenLine}}.

% magic available globals
cxn      : {token,{cxn,TokenLine}}.
user     : {token,{user,TokenLine}}.
usr      : {token,{user,TokenLine}}.

% docustrings

% Doesn't work as expected?
""{DOUBLE_QUOTED}"" : DocStr = lists:sublist(TokenChars, 3, TokenLen - 4),
                      S = build_string(DocStr, length(DocStr)),
                      {token,{docustr,TokenLine,S}}.

% post types
json     : {token,{json,TokenLine,list_to_atom(TokenChars)}}.
term     : {token,{term,TokenLine,list_to_atom(TokenChars)}}.

or     : {token,{union,TokenLine,list_to_atom(TokenChars)}}.

and    : {token,{intersect,TokenLine,list_to_atom(TokenChars)}}.
then   : {token,{intersect,TokenLine,list_to_atom(TokenChars)}}.

as     : {token,{as,TokenLine}}.
:      : {token,{':',TokenLine}}.
\*      : {token,{'*',TokenLine}}.
{START_MATH} : {token,{math,TokenLine,tl(TokenChars)}}.
/      : {token, {'/', TokenLine}}.
{C}    : {token,{comparator,TokenLine,list_to_atom(TokenChars)}}.

{D}+   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
\-{D}+   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
\+{D}+   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{FLOAT}+ : {token,{float,TokenLine,list_to_float(TokenChars)}}.

'{ATOM}+ : S = build_atom(TokenChars,TokenLen),
          {token,{ustr,TokenLine,S}}.

{DOUBLE_QUOTED} : S = build_string(TokenChars,TokenLen),
                  {token,{ustr,TokenLine,S}}.

% Let's make '"Some string" =:= "Some string" and NOT "\"Some string\""
'{DOUBLE_QUOTED} : S = build_string(tl(TokenChars),TokenLen - 1),
                  {token,{ustr,TokenLine,S}}.

={SL}+  : {token,{uterm,TokenLine,tl(TokenChars)}}.
{SL}+   : {token,{uterm,TokenLine,TokenChars}}.

[(),;]  : {token,{list_to_atom(TokenChars),TokenLine}}.
->     : {token,{'->', TokenLine}}.
\\\n   : skip_token.  % don't report lines ending in slash (continue next line)
{NL}   : {token,{'NL', TokenLine}}.
\[     : {token,{'[', TokenLine}}.
\]     : {token,{']', TokenLine}}.
\'     : {token,{quot, TokenLine}}. %' -- fix syntax highlighting again

% skippable
{COMMENT} : skip_token.
{WS}+     : skip_token.
\r        : skip_token.


Erlang code.

build_atom(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 1).

build_string(TokenChars,TokenLen) ->
    unescape_string(lists:sublist(TokenChars, 2, TokenLen - 2)).

unescape_string(String) -> unescape_string(String, []).
unescape_string([], Output) ->
  % Note: don't flatten this list here.  Things will break.
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Output) ->
  Char = case Escaped of
    $\\ -> $\\;
    $/  -> $/;
    % Since we are converting all strings to binaries, and LFE binaries are
    % defined as #b("string"), we must leave quotes *escaped* in the binary
    % shorthand or else string "string\"quote\"er" ends up
    % as #b("string"quote"er") which breaks.
    $\" -> [$\\, $\"];
    $\' -> $\';
    $b  -> $\b;
    $d  -> $\d;
    $e  -> $\e;
    $f  -> $\f;
    $n  -> $\n;
    $r  -> $\r;
    $s  -> $\s;
    $t  -> $\t;
    $v  -> $\v;
    _   -> throw({error, {"unrecognized escape sequence: ", [$\\, Escaped]}})
  end,
  unescape_string(Rest, [Char|Output]);
unescape_string([Char|Rest], Output) ->
  unescape_string(Rest, [Char|Output]).
