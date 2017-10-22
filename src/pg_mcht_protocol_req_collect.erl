%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2017 3:57 PM
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_req_collect).
-compile({parse_trans, exprecs}).
-author("simon").
-include_lib("mixer/include/mixer.hrl").
-behaviour(pg_model).
-behavior(pg_convert).
-behavior(pg_protocol).
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
  , to_list/1
]).
%%-------------------------------------------------------------------
-define(P, ?MODULE).

-record(?P, {
  mcht_id = 9999 :: pg_mcht_protocol:mcht_id()
  , txn_date = <<>> :: pg_mcht_protocol:txn_date()
  , txn_time = <<>> :: pg_mcht_protocol:txn_time()
  , txn_seq = <<"9999">> :: pg_mcht_protocol:txn_seq()
  , txn_amt = 0 :: pg_mcht_protocol:txn_amt()
  , order_desc = <<>> :: pg_mcht_protocol:order_desc()
  , back_url :: pg_mcht_protocol:url()
  , signature = <<"9">> :: pg_mcht_protocol:signature()
  , bank_card_no = <<>> :: pg_mcht_protocol:bank_card_no()
  , id_type = <<"01">> :: pg_mcht_protocol:id_type()
  , id_no = <<>> :: pg_mcht_protocol:id_type()
  , id_name = <<>> :: pg_mcht_protocol:id_name()
  , mobile = <<>> :: pg_mcht_protocol:mobile()
  , txn_type = collect
  , txn_status = waiting :: pg_mcht_protocol:txn_status()
}).

-type ?P() :: #?P{}.
-export_type([?P/0]).
-export_records([?P]).

%%-------------------------------------------------------------------
sign_fields() ->
  [
    mcht_id
    , txn_date
    , txn_seq
    , txn_time
    , txn_amt
    , order_desc
    , back_url
    , bank_card_no
    , id_type
    , id_no
    , id_name
    , mobile

  ].

options() ->
  #{
    direction => req
  }.

to_list(Protocol) when is_tuple(Protocol) ->
  VL = [
    {txn_type, collect}
    , {txn_status, waiting}
    , {mcht_index_key, pg_mcht_protocol:get(?MODULE, Protocol, mcht_index_key)}
  ] ++ pg_model:to(?MODULE, Protocol, proplists),
  VL.
