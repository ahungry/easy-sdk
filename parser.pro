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

method("ROOT").
method("GET").
method("POST").

domain(Host) --> "ROOT", any(Host).

% TODO Fix this part
route(Method, Url) --> [Method], { method(Method) }, any(Url), ws.
routes(Res) --> routes([], Res).
routes(Acc, Res) --> route(Method, Url),
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
