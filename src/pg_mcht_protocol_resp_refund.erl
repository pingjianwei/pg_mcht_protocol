%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2017 17:05
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_resp_refund).
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
  , mcht_txn_amt = 0
  , orig_mcht_txn_date = <<>>
  , orig_mcht_txn_seq = <<>>
  , orig_query_id = <<>>
  , signature = <<"9">>
  , txn_status = waiting
  , mcht_back_url = <<>>
  , resp_code = <<"05">>
  , resp_msg = <<"accepted">>
  , query_id = <<>>
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
    , orig_mcht_txn_date
    , orig_mcht_txn_seq
    , orig_query_id
    , mcht_txn_amt
    , mcht_back_url
    , query_id
    , resp_code
    , resp_msg
  ].


options() ->
  #{
    direction => resp
  }.

validate() ->
  true.
