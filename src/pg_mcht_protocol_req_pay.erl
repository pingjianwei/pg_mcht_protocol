%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2017 13:56
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_req_pay).
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
  , save/2
]).
%%-------------------------------------------------------------------
-define(TXN, ?MODULE).

-record(?TXN, {
  mcht_id = 9999
  , mcht_txn_date = <<>>
  , mcht_txn_time = <<>>
  , mcht_txn_seq = <<"9999">>
  , mcht_txn_amt = 0
  , mcht_order_desc = <<>>
  , mcht_front_url
  , mcht_back_url
  , signature = <<"9">>
  , bank_card_no = <<>>
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
    , mcht_txn_amt
    , mcht_order_desc
    , mcht_back_url
    , mcht_front_url
    , bank_card_no

  ].


options() ->
  #{
    direction => req
  }.

validate() ->
  true.

%%---------------------------------
save(M, Protocol) when is_atom(M), is_record(Protocol, ?TXN) ->
  VL = [
    {txn_type, pay}
    , {txn_status, waiting}
    , {mcht_index_key, pg_mcht_protocol:get(M, Protocol, mcht_index_key)}
  ] ++ pg_model:to(M, Protocol, proplists),

  Repo = pg_model:new(repo_mcht_txn_log_pt, VL),
  pg_repo:save(Repo).
