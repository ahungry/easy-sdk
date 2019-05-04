:- use_module(library(pio)).
:- set_prolog_flag(double_quotes, chars).

any --> [].
any --> [_], any.
any([]) --> [].
any([H|T]) --> [H], any(T).
ws --> [W], { char_type(W, space) }, ws.
ws --> [].
identifier([H|T]) --> [H], { code_type(H, alpha) ; H = '-' }, identifier(T).
identifier([]) --> [].

kw("ROOT").
kw("GET").
kw("POST").

domain(Host) --> "ROOT", any(Host).
route(Url) --> "GET", any(Url).

dcg(Host, Url) --> domain(Host), route(Url).

main(S1, S2) :-
  phrase(dcg(Host, Url), "ROOT http://httpbin.org\n\nGET /ip"),
  string_chars(S1, Host),
  string_chars(S2, Url).
  %% open('petstore.ezsdk', read, In),
  %% phrase(dcg(Host, Url), In),
  %% format('~w~n', [Host, Url]),
  %% close(In).
