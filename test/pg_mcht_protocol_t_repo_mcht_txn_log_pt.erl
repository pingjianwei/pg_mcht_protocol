%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2016 8:17 PM
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_t_repo_mcht_txn_log_pt).
-compile({parse_trans, exprecs}).
-behavior(pg_repo).
-author("simon").

%%-define(BH, behaviour_repo).
%% API
%% callbacks
-export([
  table_config/0
]).

-compile(export_all).
%%-------------------------------------------------------------
-define(TBL, mcht_txn_log).


-type txn_type() :: pay |refund|gws_up_query.
-type status() :: success |waiting |fail.
-type txn_amt() :: non_neg_integer().

-export_type([txn_type/0, status/0, txn_amt/0]).

-record(?TBL, {
  mcht_index_key
  , txn_type :: txn_type()
  , mcht_id
  , mcht_txn_date
  , mcht_txn_time
  , mcht_txn_seq
  , mcht_txn_amt :: txn_amt()
  , mcht_order_desc
  , gateway_id
  , bank_id
  , prod_id
  , prod_bank_acct_id
  , prod_bank_acct_corp_name
  , prod_bank_name
  , mcht_back_url
  , mcht_front_url
  , prod_memo

  , query_id
  , settle_date
  , quota
  , resp_code
  , resp_msg

  , orig_mcht_txn_date
  , orig_mcht_txn_seq
  , orig_query_id

  , txn_status = waiting :: status()
  , bank_card_no


}).
-type ?TBL() :: #?TBL{}.
-export_type([?TBL/0]).

-export_records([?TBL]).
%%-------------------------------------------------------------
%% call backs
table_config() ->
  #{
    table_indexes => [mcht_txn_date]
    , data_init => []
    , pk_is_sequence => false
    , pk_key_name => mcht_index_key
    , pk_type => tuple

    , unique_index_name => mcht_index_key
    , query_option =>
  #{
    mcht_id => integer_equal
  }

  }.

