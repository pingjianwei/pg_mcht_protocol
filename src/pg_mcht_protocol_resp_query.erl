%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2017 17:04
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_resp_query).
-author("jiarj").
-include_lib("eunit/include/eunit.hrl").
-include_lib("mixer/include/mixer.hrl").
-behaviour(pg_model).
-behaviour(pg_protocol).
-behaviour(pg_mcht_protocol).

-compile(export_all).
%% API
%% callbacks of mcht_protocol
-mixin([{pg_mcht_protocol, [
  pr_formatter/1
  , in_2_out_map/0
]}]).
%% API
%% callbacks of pg_protocol
-export([
  sign_fields/0
  , options/0
]).
%% callbacks of pg_protocol
%%-------------------------------------------------------------------
-define(TXN, ?MODULE).

-record(?TXN, {
  mcht_id = 9999
  , mcht_txn_date = <<>>
  , mcht_txn_time = <<>>
  , mcht_txn_seq = <<"9999">>
  , query_id = <<>>
  , orig_mcht_txn_date = <<>>
  , orig_mcht_txn_seq = <<>>
  , mcht_txn_amt = 0
  , settle_date = <<>>
  , orig_resp_code = <<>>
  , orig_resp_msg = <<>>
  , quota = 0
  , resp_code
  , resp_msg
  , signature

}).

-type ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).
-export_records([?TXN]).


sign_fields() ->
  [
    mcht_id
    , mcht_txn_date
    , mcht_txn_seq
    , mcht_txn_time
    , query_id
    , mcht_txn_amt
    , orig_mcht_txn_date
    , orig_mcht_txn_seq
    , settle_date
    , orig_resp_code
    , orig_resp_msg
    , quota
    , resp_code
    , resp_msg

  ].

options() ->
  #{
    direction => resp
  }.

validate() ->
  true.