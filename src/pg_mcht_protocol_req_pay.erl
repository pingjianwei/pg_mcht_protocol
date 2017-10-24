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

-export([
  validate/0
]).
%%-------------------------------------------------------------------
-define(P, ?MODULE).

-record(?P, {
  mcht_id = 9999 :: pg_mcht_protocol:mcht_id()
  , mcht_txn_date = <<>> :: pg_mcht_protocol:txn_date()
  , mcht_txn_time = <<>> :: pg_mcht_protocol:txn_time()
  , mcht_txn_seq = <<"9999">> :: pg_mcht_protocol:txn_seq()
  , mcht_txn_amt = 0 :: pg_mcht_protocol:txn_amt()
  , mcht_order_desc = <<>> :: pg_mcht_protocol:order_desc()
  , mcht_front_url = <<>> :: pg_mcht_protocol:url()
  , mcht_back_url
  , signature = <<"9">> :: pg_mcht_protocol:signature()
  , bank_card_no = <<>> :: pg_mcht_protocol:bank_card_no()
}).

-type ?P() :: #?P{}.
-export_type([?P/0]).
-export_records([?P]).



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
