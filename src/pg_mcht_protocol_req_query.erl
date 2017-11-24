%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2017 13:57
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_req_query).
-compile({parse_trans, exprecs}).
-author("jiarj").
-include_lib("eunit/include/eunit.hrl").
-include_lib("mixer/include/mixer.hrl").
-behaviour(pg_convert).
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
  , convert_config/0
]).
%% callbacks of pg_protocol
%%-------------------------------------------------------------------
-define(P, ?MODULE).

-record(?P, {
  mcht_id = 9999 :: pg_mcht_protocol:mcht_id()
  , txn_date = <<>> :: pg_mcht_protocol:txn_date()
  , txn_time = <<>> :: pg_mcht_protocol:txn_time()
  , txn_seq = <<"9999">> :: pg_mcht_protocol:txn_seq()
  , signature :: pg_mcht_protocol:signature()

}).

-type ?P() :: #?P{}.
-export_type([?P/0]).
-export_records([?P]).


sign_fields() ->
  [
    mcht_id
    , txn_date
    , txn_seq
    , txn_time

  ].

options() ->
  #{
    channel_type => mcht,
    txn_type => query,
    direction => req
  }.

convert_config() ->
  [

  ].
%%---------------------------------
