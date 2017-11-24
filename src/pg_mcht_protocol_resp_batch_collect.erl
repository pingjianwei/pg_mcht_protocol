%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2017 3:57 PM
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_resp_batch_collect).
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
  , txn_seq = <<"9999">> :: pg_mcht_protocol:txn_seq()
  , txn_amt = 0 :: pg_mcht_protocol:txn_amt()
  , query_id :: pg_mcht_protocol:query_id()
  , resp_code :: pg_mcht_protocol:resp_code()
  , resp_msg :: pg_mcht_protocol:resp_msg()
  , limit :: pg_mcht_protocol:limit()
  , signature = <<"9">> :: pg_mcht_protocol:signature()
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
    channel_type => mcht,
    txn_type => batch_collect,
    direction => resp
  }.


convert_config() ->
  [
    {default,       %% convert from up_txn_log
      [

        {to, proplists},
        {from,
          [
            {{pg_mcht_protocol, repo_module, [up_txn_log]},     %% dynamic up_txn_log repo name
              [
                {txn_status, txn_status}
                , {resp_code, up_respCode}
                , {resp_msg, up_respMsg}
                , {query_id, up_orderId}
                , {mcht_index_key, mcht_index_key}
              ]
            }
          ]
        }
      ]
    },
    {normal_resp,         %% copy all from mcht_txn_log
      [
        {to, ?MODULE},
        {from,
          [
            {{pg_mcht_protocol, repo_module, [mcht_txn_log]}, all   %% copy from mcht_txn_log repo name
            }
          ]
        }
      ]
    },
    {fail_resp,
      [
        {to, ?MODULE},
        {from,
          [
            {pg_mcht_protocol_req_collect, all}
          ]}
      ]}

  ].


