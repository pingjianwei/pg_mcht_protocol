%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2017 3:57 PM
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_resp_collect).
-compile({parse_trans, exprecs}).
-author("simon").
-include_lib("mixer/include/mixer.hrl").
-behavior(pg_convert).
-behavior(pg_protocol).
-behaviour(pg_mcht_protocol).

%% API
%% callbacks of mcht_protocol
-mixin([{pg_mcht_protocol, [
  pr_formatter/1
  , in_2_out_map/0
]}]).
-export([
  validate/0
]).

%% callbacks of pg_convert
-export([
  convert_config/0
]).
%% API
%% callbacks of pg_protocol
-export([
  sign_fields/0
  , options/0
]).

%%-------------------------------------------------------------------
-define(P, ?MODULE).

-record(?P, {
  mcht_id = 9999 :: pg_mcht_protocol:mcht_id()
  , txn_date = <<>> :: pg_mcht_protocol:txn_date()
  , txn_seq = <<"9999">> :: pg_mcht_protocol:txn_seq()
  , txn_amt = 0 :: pg_mcht_protocol:txn_amt()
  , query_id = <<>> :: pg_mcht_protocol:query_id()
  , quota = 0 :: pg_mcht_protocol:quota()
  , resp_code = <<>> :: pg_mcht_protocol:resp_code()
  , resp_msg = <<>> :: pg_mcht_protocol:resp_msg()
  , signature = <<"9">> :: pg_mcht_protocol:signature()

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
    , txn_amt
    , query_id
    , resp_code
    , resp_msg

  ].

options() ->
  #{
    direction => resp
  }.


convert_config() ->
  [
    {update_resp,
      [

        {to, {fun pg_mcht_protocol:repo_module/1, [mcht_txn_log]}},
        {from,
          [
            {?MODULE,
              [
                {txn_status, {fun xfutils:up_resp_code_2_txn_status/1, [resp_code]}}
                , {mcht_index_key, pg_mcht_protocol, mcht_index_key}
              ]
            },
            {?MODULE, all}
          ]}
      ]
    },
    {fail_resp,
      [
        {to, ?MODULE},
        {from,
          [
            {pg_mcht_protocol_req_collect, all}
          ]
        }

      ]
    }
  ].


validate() ->
  ok.