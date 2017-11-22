%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2017 17:04
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_resp_query).
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
-define(TXN, ?MODULE).

-record(?TXN, {
  mcht_id = 9999
  , txn_date = <<>>
  , txn_time = <<>>
  , txn_seq = <<"9999">>
  , query_id = <<>>
  , txn_amt = 0
  , settle_date = <<>>
  , orig_resp_code = <<>>
  , orig_resp_msg = <<>>
  , quota = 0
  , resp_code
  , resp_msg
  , signature

}).

-type ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).
-export_records([?TXN]).


sign_fields() ->
  [
    mcht_id
    , txn_date
    , txn_seq
    , txn_time
    , query_id
    , txn_amt
    , settle_date
    , orig_resp_code
    , orig_resp_msg
    , quota
    , resp_code
    , resp_msg

  ].

options() ->
  #{
    channel_type => mcht,
    direction => resp
  }.

convert_config() ->
  [
    {normal_resp,
      [
        {to, ?MODULE},
        {from,
          [
            {
              {resp_code, <<"00">>}
              , {resp_msg, <<"success">>}
              , {orig_resp_code, resp_code}
              , {orig_resp_msg, resp_msg}
            },
            {{pg_mcht_protocol, repo_module, [mcht_txn_log]}, all}
          ]
        }

      ]
    },
    {fail_resp,
      [
        {to, ?MODULE},
        {from,
          [
            {pg_mcht_protocol_req_query, all}
          ]}
      ]
    }

  ].
