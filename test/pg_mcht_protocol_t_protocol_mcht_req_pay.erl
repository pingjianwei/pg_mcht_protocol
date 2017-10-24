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
-behavior(pg_convert).
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
  , to_list/1
]).

-export([
  convert_config/0
]).

%% callbacks of pg_protocol
%%-------------------------------------------------------------------
-define(TXN, ?MODULE).

-record(?TXN, {
  mcht_id = 9999
  , txn_date = <<>>
  , txn_time = <<>>
  , txn_seq = <<"9999">>
  , txn_amt = 0
  , order_desc = <<>>
  , front_url
  , back_url
  , signature = <<"9">>
  , bank_card_no = <<>>
}).

-type ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).
-export_records([?TXN]).

%%-------------------------------------------------------------------
%%pr_formatter(Field) ->
%%  pg_protocol:pr_formatter(Field).

%%in_2_out_map() ->
%%  pg_protocol:in_2_out_map().

sign_fields() ->
  [
    mcht_id
    , txn_date
    , txn_seq
    , txn_time
    , txn_amt
    , order_desc
    , back_url
    , front_url
    , bank_card_no

  ].

%%---------------------------------
options() ->
  #{
    direction => req
    , type=>pay
    , save_2_repo => true
  }.

%%---------------------------------
to_list(Protocol) when is_tuple(Protocol) ->
  VL = [
    {txn_type, pay}
    , {txn_status, waiting}
    , {mcht_index_key, pg_mcht_protocol:get(?MODULE, Protocol, mcht_index_key)}
  ] ++ pg_model:to(?MODULE, Protocol, proplists),
  VL.


convert_config() ->
  [
    {save_req,
      [

        {to, fun pg_mcht_protocol:repo_mcht_module/0},
        {from,
          [
            {?MODULE,
              [
                {txn_type, {static, pay}}
                , {txn_status, {static, waiting}}
                , {mcht_index_key, pg_mcht_protocol, mcht_index_key}
              ]
            },
            {?MODULE, all}
          ]}
      ]
    }
  ].
%%===============================================
%% UT
%%==============================================

