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
not_identifier([H|T]) --> [H], { \+ (code_type(H, alpha) ; H = '-') }, not_identifier(T).
not_identifier([]) --> [].
newline --> "\n", newline.
newline --> [].

%method("ROOT").
method("GET").
method("POST").
method(M) --> identifier(M), { method(M) }.

domain(Host) --> "ROOT", mws, any(Host).

route(Method, Url) --> method(Method), mws, any(Url).
routes(Res) --> routes([], Res).
routes(Acc, Res) --> route(Method, Url),
                     { D = fn{method:Method, url:Url},
                       append(Acc, [D], Res)}.
routes(Acc, Res) --> route(Method, Url),
                     newline,
                     {
                       D = fn{method: Method, url: Url},
                       append(Acc, [D], Acc2)
                     },
                     routes(Acc2, Res).
routes(R, R) --> [].

dcg(Host, Urls) --> domain(Host), newline, routes(Urls).

% https://stackoverflow.com/questions/13503953/dcg-and-left-recursion
comma --> ",".
csv_line(Res) --> csv_line([], Res).
csv_line(Acc, Res) --> identifier(A), { append(Acc, [A], Res) }.
csv_line(Acc, Res) --> identifier(A), { append(Acc, [A], Acc2) }, comma, csv_line(Acc2, Res).
csv_line(Res, Res) --> [].
my_csv(Res) --> my_csv([], Res).
my_csv(Acc, Res) --> csv_line(A), { append(Acc, [A], Res) }.
my_csv(Acc, Res) --> csv_line(A), { append(Acc, [A], Acc2) }, "\n", my_csv(Acc2, Res).
my_csv(Res, Res) --> [].

alphas_only(Res) --> alphas_only([], Res).
alphas_only(Acc, Res) --> identifier(Alphas), {append(Acc, [Alphas], Res)}.
alphas_only(Acc, Res) --> identifier(Alphas), {append(Acc, [Alphas], Acc2)}, not_identifier(_), alphas_only(Acc2, Res).
alphas_only(R, R) --> [].

to_js(Url) :-
  string_chars(SMethod, Url.method),
  string_chars(SUrl, Url.url),
  format('sdk.~w.~w', [SMethod, SUrl]).

main(S1, Urls) :-
  phrase(dcg(Host, Urls), "ROOT http://httpbin.org\n\nGET /ip"),
  string_chars(S1, Host),
  [H|_] = Urls,
  to_js(H).
  %string_chars(H, Host).
  %string_chars(S2, Url).
  %% open('petstore.ezsdk', read, In),
  %% phrase(dcg(Host, Url), In),
  %% format('~w~n', [Host, Url]),
  %% close(In).
