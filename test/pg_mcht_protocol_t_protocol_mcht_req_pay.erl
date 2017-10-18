%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2017 3:57 PM
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_t_protocol_mcht_req_pay).
-author("simon").
-include_lib("eunit/include/eunit.hrl").
-include_lib("mixer/include/mixer.hrl").
-behaviour(pg_model).
-behaviour(pg_protocol).
-behaviour(pg_mcht_protocol).

-compile(export_all).
%% API
%% callbacks of pg_model
%%-export([
%%  pr_formatter/1
%%]).
-mixin([{pg_mcht_protocol, [
  pr_formatter/1
  , in_2_out_map/0
]}]).

%% callbacks of pg_protocol
%%-export([
%%  in_2_out_map/0
%%]).
%% callbacks of pg_mcht_protocol
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
  , mcht_order_desc = <<>>
  , mcht_front_url
  , mcht_back_url
  , signature = <<"9">>
  , bank_card_no = <<>>
}).

-type ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).
-export_records([?TXN]).

%%-------------------------------------------------------------------
%%pr_formatter(Field) ->
%%  pg_mcht_protocol:pr_formatter(Field).

%%in_2_out_map() ->
%%  pg_mcht_protocol:in_2_out_map().

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

convert_config() ->
  [

  ].

%%---------------------------------
options() ->
  #{
    direction => req
  }.

%%===============================================
%% UT
%%==============================================

