%% Copyright (c) 2010, Dev:Extend
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%  * Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%  * Neither the name of Dev:Extend nor the names of its contributors may be
%%    used to endorse or promote products derived from this software without
%%    specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% @type uri() = #ex_uri{}.
%% @type ref() = #ex_uri_ref{} | uri().

-module(ex_uri).
-include("ex_uri.hrl").

-export([decode/1,
         decode_ref/1,
         decode_abs/1,
         decode_pct/1,
         encode/1,
         encode_pct/2,
         resolve/2,
         merge/2,
         remove_dot_segments/1,
         hide_userinfo/1
        ]).

%% @spec decode(string()) -> {ok, uri(), string()}
%% @doc Decode an URI.
decode(String) ->
  case ex_uri_parser:decode('URI', String) of
      {ok, _URI, _Rest} = Ret -> Ret;
      fail -> {error, invalid_uri}
  end.

%% @spec decode_ref(string()) -> {ok, ref(), string()}
%% @doc Decode an URI reference.
decode_ref(String) ->
    case ex_uri_parser:decode('URI-reference', String) of
        {ok, _URI, _Rest} = Ret -> Ret;
        fail -> {error, invalid_uri_ref}
    end.

%% @spec decode_abs(string()) -> {ok, uri(), string()}
%% @doc Decode an absolute URI.
decode_abs(String) ->
    case ex_uri_parser:decode('absolute-URI', String) of
        {ok, _URI, _Rest} = Ret -> Ret;
        fail -> {error, invalid_abs_uri}
    end.

%% @spec decode_pct(string()) -> string()
%% @doc Decode a percent-encoded string.
decode_pct(String) ->
  decode_pct(String, []).

%% @spec encode(Value :: uri() | ref()) -> string()
%% @doc Encode an URI or an URI reference.
encode(#ex_uri{scheme = Scheme, authority = Authority, path = Path,
               q = Query, fragment = Fragment}) ->
  Acc = [$:, Scheme],
  encode(Authority, Path, Query, Fragment, Acc);
encode(#ex_uri_ref{authority = Authority, path = Path, q = Query,
                   fragment = Fragment}) ->
  encode(Authority, Path, Query, Fragment, []).

%% @spec encode_pct(string(), [char()]) -> string()
%% @doc Percent-encode a given set of chars in a string.
encode_pct(String, Chars) ->
  encode_pct(String, Chars, []).

%% @hidden
decode_pct([C | String], Acc) when C =/= $% ->
  decode_pct(String, [C | Acc]);
decode_pct([$%, D1, D2 | String], Acc) ->
  decode_pct(String, [erlang:list_to_integer([D1, D2], 16) | Acc]);
decode_pct([], Acc) ->
  lists:reverse(Acc).

%% @spec encode(#ex_uri_authority{} | undefined,
%%              string(),
%%              string() | undefined,
%%              string() | undefined,
%%              string()) -> string()
encode(Authority, Path, Query, Fragment, Acc) ->
  Acc1 = case Authority of
           undefined -> Acc;
           A -> encode_authority(A, [$/, $/ | Acc]) end,
  Acc2 = case Path of
             undefined -> Acc1;
             _ -> [Path | Acc1]
         end,
  Acc3 = case Query of
           undefined -> Acc2;
           Q -> [Q, $? | Acc2] end,
  Acc4 = case Fragment of
           undefined -> Acc3;
           F -> [F, $# | Acc3] end,
  lists:flatten(lists:reverse(Acc4)).

%% @spec encode_authority(Authority :: #ex_uri_authority{},
%%                        string()) -> string()
encode_authority(#ex_uri_authority{userinfo = UserInfo,
                                   host = Host,
                                   port = Port}, Acc) ->
  Acc1 = case UserInfo of
           undefined -> Acc;
           UI -> [$@, UI | Acc] end,
  Acc2 = [Host | Acc1],
  case Port of
    undefined -> Acc2;
    P -> [integer_to_list(P), $: | Acc2] end.

%% @spec resolve(Ref :: ref(), Base :: uri()) -> uri()
%% @doc Resolve an URI reference with a given base URI.
resolve(#ex_uri_ref{authority = RA, path = RP, q = RQ, fragment = RF},
        #ex_uri{scheme = BS, authority = BA, path = BP, q = BQ}) ->
  [TA, TP, TQ] = case RA of
                   undefined ->
                     Acc = case RP of
                             "" ->
                               TQ1 = case RQ of
                                       undefined -> BQ;
                                       RQ -> RQ end,
                               [BP, TQ1];
                             RP ->
                               BP1 = case BP of
                                       "" when BA =/= undefined -> "/";
                                       BP -> BP end,
                               TP1 = case RP of
                                       [$/ | _] -> RP;
                                       RP -> merge(RP, BP1) end,
                               TP2 = remove_dot_segments(TP1),
                               TP3 = case TP2 of
                                       "//" ++ _ when BA =:= undefined ->
                                         "/." ++ TP2;
                                       TP2 -> TP2 end,
                               [TP3, RQ] end,
                     [BA | Acc];
                   RA ->
                     [RA, remove_dot_segments(RP), RQ] end,
  #ex_uri{scheme = BS, authority = TA, path = TP, q = TQ, fragment = RF};
resolve(Ref = #ex_uri{path = RP}, #ex_uri{}) ->
  Ref#ex_uri{path = remove_dot_segments(RP)}.

%% @spec merge(string(), string()) -> string()
%% @doc Merge a relative path with a given base path.
merge(Rel, Base) ->
  merge(Rel, Base, "", "").

%% @spec remove_dot_segments(string()) -> string()
%% @doc Remove superfluous dot segments from a given path.
remove_dot_segments(Path) ->
  remove_dot_segments1(Path).

%% @hidden
encode_pct([C | String], Chars, Acc) ->
  case lists:member(C, Chars) of
    true ->
      [D1, D2] = erlang:integer_to_list(C, 16),
      encode_pct(String, Chars, [D2, D1, $% | Acc]);
    false -> encode_pct(String, Chars, [C | Acc]) end.

%% @hidden
merge(Rel, [$/ | Base], Last, Acc) ->
  merge(Rel, Base, [], [$/ | Last] ++ Acc);
merge(Rel, [C | Base], Last, Acc) ->
  merge(Rel, Base, [C | Last], Acc);
merge(Rel, [], _Last, Acc) ->
  lists:reverse(Acc, Rel).

%% @hidden
remove_dot_segments1("/" ++ Path) ->
  remove_dot_segments1(Path, "/", "");
remove_dot_segments1(Path) ->
  remove_dot_segments1(Path, ".", "").

%% @hidden
remove_dot_segments1("", Prefix, Acc) ->
  remove_dot_segments2(lists:reverse(Acc), Prefix);
remove_dot_segments1(Path, Prefix, Acc) ->
  Pred = fun (C) -> C =/= $/ end,
  {S, R} = case lists:splitwith(Pred, Path) of
             {Segment, "/" ++ Rest} -> {Segment ++ "/", Rest};
             Result -> Result end,
  if
    S =:= "."; S =:= "./" ->
      remove_dot_segments1(R, Prefix, Acc);
    S =:= ".."; S =:= "../" ->
      Acc2 = case lists:dropwhile(Pred, Acc) of
               "/" ++ Acc1 -> Acc1;
               Acc1 -> Acc1 end,
      remove_dot_segments1(R, Prefix, lists:dropwhile(Pred, Acc2));
    true ->
      remove_dot_segments1(R, Prefix, lists:reverse(S, Acc)) end.

remove_dot_segments2(Path = "//" ++ _, ".") ->
  "." ++ Path;
remove_dot_segments2("/" ++ Path, ".") ->
  Path;
remove_dot_segments2(Path, ".") ->
  Path;
remove_dot_segments2(Path, "/") ->
  "/" ++ Path.

hide_userinfo(Uri = #ex_uri{authority=
                                Authority = #ex_uri_authority{userinfo=Info}})
  when Info =/= undefined ->
    Uri#ex_uri{authority = Authority#ex_uri_authority{userinfo="xxx:yyy@"}};
hide_userinfo(Uri = #ex_uri{}) -> Uri.

