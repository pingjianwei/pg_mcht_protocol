%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2017 9:39
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_validate_format).
-author("simon").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([validate_format_one_field/2]).

%%------------------------------------------------------
%% for mcht req
validate_format_one_field(<<"merchId">>, Value) when is_binary(Value) ->
  ok = validate_string(integer, Value);
validate_format_one_field(<<"tranTime">>, Value) when is_binary(Value) ->
  6 = byte_size(Value),
  ok = validate_string(integer, Value);
%%validate_format_one(mcht, <<"origTranDate">>, <<>>) ->
%%  %% can be empty?
%%  ok;
validate_format_one_field(<<"origTranDate">>, Value) when is_binary(Value) ->
  ok = validate_string(date_yyyymmdd, Value);
validate_format_one_field(<<"tranDate">>, Value) when is_binary(Value) ->
  ok = validate_string(date_yyyymmdd, Value);
validate_format_one_field(<<"queryId">>, Value) when is_binary(Value) ->
  ok;
validate_format_one_field(<<"trustBackUrl">>, Value) when is_binary(Value) ->
  ok = validate_string(url, Value);
validate_format_one_field(<<"trustFrontUrl">>, Value) when is_binary(Value) ->
  <<"http", _/binary>> = Value,
  ok;
validate_format_one_field(<<"tranAmt">>, Value) when is_binary(Value) ->
  ok = validate_string(txn_amt, Value);
validate_format_one_field(<<"bankCardNo">>, <<>>) ->
  %% can be empty
  ok;
validate_format_one_field(<<"bankCardNo">>, Value) when is_binary(Value) ->
  ok = validate_string(bank_card_no, Value);
validate_format_one_field(<<"orderDesc">>, Value) when is_binary(Value) ->
  %% orderDesc could not be omit or empty string
  ok = validate_string(not_empty, Value);
validate_format_one_field(<<"signature">>, Value) ->
  %% orderDesc could not be omit or empty string
  ok = validate_string(not_empty, Value);
validate_format_one_field(<<"certifType">>, Value) when is_binary(Value) ->
  ok = validate_string({length, 1, 2}, Value),
  ok = validate_string({number, 1, 20}, Value);
validate_format_one_field(<<"certifId">>, Value) when is_binary(Value) ->
  ok = validate_string({length, 15, 18}, Value);
validate_format_one_field(<<"certifName">>, Value) when is_binary(Value) ->
  ok = validate_string(not_empty, Value);
validate_format_one_field(<<"phoneNo">>, Value) when is_binary(Value) ->
  ok = validate_string(mobile, Value);
validate_format_one_field(<<"bankId">>, _) ->
  ok;
validate_format_one_field(_, _) ->
  ok.

%%------------------------------------------------------------
validate_string(integer, String) when is_list(String) ->
  validate_string(integer, list_to_binary(String));
validate_string(integer, String) when is_binary(String) ->
  try binary_to_integer(String) of
    _ ->
      ok
  catch
    _:_ ->
      fail
  end;
validate_string(url, Value) when is_binary(Value) ->
  <<"http", _/binary>> = Value,
  ok;
validate_string(not_empty, <<>>) ->
  throw({badmatch, empty_string_not_allowed});
validate_string(not_empty, <<"\"\"">>) ->
  throw({badmatch, empty_string_not_allowed});
validate_string(not_empty, Value) when is_binary(Value) ->
  ok;
validate_string(bank_card_no, Value) when is_binary(Value) ->
  ok = validate_string(integer, Value),
  Len = byte_size(Value),
  {LenMin, LenMax} = pg_mcht_protocol:limit(bank_card_no_len),
  true = (LenMin =< Len) and (LenMax >= Len),
  ok;
validate_string(txn_amt, Value) when is_binary(Value) ->
  ok = validate_string(integer, Value),
  ok;
validate_string({length, Min, Max}, Value)
  when is_binary(Value), is_integer(Min), is_integer(Max), (Min =< Max) ->
  true = ((Min =< byte_size(Value)) and (byte_size(Value) =< Max)),
  ok;
validate_string({number, Min, Max}, Value)
  when is_binary(Value), is_integer(Min), is_integer(Max), (Min =< Max) ->
  ok = validate_string(integer, Value),
  true = ((Min =< binary_to_integer(Value)) and (binary_to_integer(Value) =< Max)),
  ok;
validate_string(mobile, Value) when is_binary(Value) ->
  ok = validate_string({length, 11, 11}, Value),
  <<"1", _/binary>> = Value,
  ok = validate_string({number, 10000000000, 19999999999}, Value),
  ok;
validate_string(date_yyyymmdd, Value) when is_binary(Value) ->
%%  8 = byte_size(Value),
  xfutils:assert(yyyymmdd, Value),
%%  ok = validate_string(integer, Value),
%%  <<Y:4/bytes, M:2/bytes, D:2/bytes>> = Value,
%%  Year = binary_to_integer(Y),
%%  Month = binary_to_integer(M),
%%  Day = binary_to_integer(D),
%%  true = (Year > 2000) and (Year < 2030),
%%  true = (Month > 0) and (Month < 13),
%%  true = (Day > 0) and (Day < 32),
  ok.

validate_format_one_test() ->
  ?assertEqual(ok, validate_format_one_field(<<"tranTime">>, <<"121212">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"tranTime">>, <<"u21212">>)),

  ?assertEqual(ok, validate_format_one_field(<<"tranDate">>, <<"20161010">>)),
%%  ?assertError({badmatch, _}, validate_format_one_field(mcht, <<"tranDate">>, <<"201610yy">>)),
  ?assertError(badarg, validate_format_one_field(<<"tranDate">>, <<"201610yy">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"tranDate">>, <<"19991919">>)),

  ?assertEqual(ok, validate_format_one_field(<<"trustBackUrl">>, <<"http://www.a.b">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"trustBackUrl">>, <<"/www.a.b">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"trustBackUrl">>, <<"www.a.b">>)),

  ?assertEqual(ok, validate_format_one_field(<<"trustFrontUrl">>, <<"http://www.a.b">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"trustFrontUrl">>, <<"/www.a.b">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"trustFrontUrl">>, <<"www.a.b">>)),

  ?assertEqual(ok, validate_format_one_field(<<"tranAmt">>, <<"100">>)),
  ?assertEqual(ok, validate_format_one_field(<<"tranAmt">>, <<"50">>)),
  ?assertEqual(ok, validate_format_one_field(<<"tranAmt">>, <<"0">>)),
%%  ?assertError({badmatch, _}, validate_format_one_field(<<"tranAmt">>, <<"49">>)),
%%  ?assertError({badmatch, _}, validate_format_one_field(<<"tranAmt">>, <<"0">>)),
%%  ?assertError({badmatch, _}, validate_format_one_field(<<"tranAmt">>, <<"-30">>)),

  ?assertThrow({badmatch, _}, validate_format_one_field(<<"orderDesc">>, <<>>)),
  ?assertThrow({badmatch, _}, validate_format_one_field(<<"orderDesc">>, <<"\"\"">>)),
  ?assertEqual(ok, validate_format_one_field(<<"orderDesc">>, <<"xxx">>)),

  ?assertEqual(ok, validate_format_one_field(<<"certifType">>, <<"01">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"certifType">>, <<"011">>)),

  ok.


