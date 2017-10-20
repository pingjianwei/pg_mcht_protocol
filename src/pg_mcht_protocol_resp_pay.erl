%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2017 17:04
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_resp_pay).
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
  , mcht_txn_seq = <<"9999">>
  , query_id
  , settle_date
  , quota = 0
  , resp_code
  , resp_msg
  , mcht_front_url
  , mcht_back_url
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
    , query_id
    , settle_date
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

%%---------------------------------
save(M, Protocol) when is_atom(M), is_record(Protocol, ?TXN) ->
  VL = pg_model:get_proplist(M, Protocol, [query_id, settle_date, resp_code, resp_msg])
    ++ [{txn_status, xftuils:up_resp_code_2_txn_status(pg_model:get(M, Protocol, resp_code))}],

  PK = pg_mcht_protocol:get(M, Protocol, mcht_index_key),
  pg_repo:update_pk(repo_mcht_txn_log_pt, PK, VL).
