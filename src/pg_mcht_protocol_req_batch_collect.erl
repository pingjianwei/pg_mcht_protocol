%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2017 3:57 PM
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_req_batch_collect).
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
%%  , to_list/1
]).

%%-------------------------------------------------------------------
-define(P, ?MODULE).

-record(?P, {
  mcht_id = 9999 :: pg_mcht_protocol:mcht_id()
  , txn_date = <<>> :: pg_mcht_protocol:txn_date()
  , txn_time = <<>> :: pg_mcht_protocol:txn_time()
  , txn_seq = <<"9999">> :: pg_mcht_protocol:txn_seq()
  , txn_amt = 0 :: pg_mcht_protocol:txn_amt()
  , txn_count = 0 :: pg_mcht_protocol:txn_count()
  , batch_no = 0 :: pg_mcht_protocol:batch_no()
  , file_content = <<>> :: pg_mcht_protocol:file_content()
  , order_desc = <<>> :: pg_mcht_protocol:order_desc()
  , back_url :: pg_mcht_protocol:url()
  , signature = <<"9">> :: pg_mcht_protocol:signature()
  , txn_type = batch_collect
  , txn_status = waiting :: pg_mcht_protocol:txn_status()
  , req_reserved = <<>> :: pg_mcht_protocol:req_reserved()
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
    , txn_count
    , batch_no
    , file_content
    , req_reserved
  ].

options() ->
  #{
    channel_type => mcht,
    direction => req
  }.


convert_config() ->
  [
    {save_req,
      [

        {to, {fun pg_mcht_protocol:repo_module/1, [mcht_txn_log]}},
        {from,
          [
            {?MODULE,
              [
                {txn_type, {static, batch_collect}}
                , {txn_status, {static, waiting}}
                , {mcht_index_key, pg_mcht_protocol, mcht_index_key}
              ]
            },
            {?MODULE, all}
          ]}
      ]
    }

  ].


validate() ->
  ok.