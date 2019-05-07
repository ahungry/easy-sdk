% http://csci431.artifice.cc/notes/prolog-parsing.html
% https://www.metalevel.at/prolog/dcg
% http://www.pathwayslms.com/swipltuts/dcg/

:- use_module(library(pio)).
:- set_prolog_flag(double_quotes, chars).

any --> [].
any --> [_], any.
any([]) --> [].
any([H|T]) --> [H], any(T).
ws --> [W], { char_type(W, space) }, ws.
ws --> [].
mws --> [W], { char_type(W, space) }, mws.
mws --> [W], { char_type(W, space) }.
identifier([H|T]) --> [H], { code_type(H, alpha) ; H = '-' }, identifier(T).
identifier([]) --> [].
newline --> "\n", newline.
newline --> [].

%method("ROOT").
method("GET").
method("POST").
method(M) --> identifier(M), { method(M) }.

domain(Host) --> "ROOT", any(Host).

% FIXME: Currently the matches must end in terminating clause of an EOL or route breaks.
route(Method, Url) --> method(Method), mws, identifier(Url), ws, newline.
routes(Res) --> routes([], Res).
routes(Acc, Res) --> ws,
                     route(Method, Url),
                     ws,
                     {
                       D = fn{method: Method, url: Url},
                       append(Acc, [D], Acc2)
                     },
                     routes(Acc2, Res).
routes(R, R) --> [].

dcg(Host, Urls) --> domain(Host), routes(Urls).

main(S1, S2) :-
  phrase(dcg(Host, Url), "ROOT http://httpbin.org\n\nGET /ip"),
  string_chars(S1, Host),
  string_chars(S2, Url).
  %% open('petstore.ezsdk', read, In),
  %% phrase(dcg(Host, Url), In),
  %% format('~w~n', [Host, Url]),
  %% close(In).
