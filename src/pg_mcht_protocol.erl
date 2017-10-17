-module(pg_mcht_protocol).
-behavior(pg_model).
-behavior(pg_protocol).

%% callbacks
-callback sign_fields() -> list().
-callback options() -> map().
-callback validate() -> boolean().

%% API exports
%% callbacks of pg_protocol
-export([
  in_2_out_map/0
]).

%% callbacks of pg_model
-export([
  pr_formatter/1
]).


%% own apis
-export([
  get/3
%%  , pr/1

]).
%%====================================================================
%% API functions
%%====================================================================
pr_formatter(Field)
  when (Field =:= mcht_order_desc)
  or (Field =:= signature)
  or (Field =:= id_name)
  ->
  string;
pr_formatter(_) ->
  default.

%%------------------------------------------------------

in_2_out_map() ->
  #{
    mcht_id => <<"merchId">>
    , mcht_txn_date => <<"tranDate">>
    , mcht_txn_seq => <<"tranId">>
    , mcht_txn_time => <<"tranTime">>
    , mcht_txn_amt => {<<"tranAmt">>, integer}
    , mcht_order_desc=> <<"orderDesc">>
    , signature=> <<"signature">>
    , mcht_front_url=> <<"trustFrontUrl">>
    , mcht_back_url=> <<"trustBackUrl">>
    , bank_card_no=> <<"bankCardNo">>
    , orig_mcht_txn_date => <<"origTranDate">>
    , orig_mcht_txn_seq => <<"origTranId">>
    , orig_query_id => <<"origQueryId">>
    , orig_resp_code => <<"origRespCode">>
    , orig_resp_msg => <<"origRespMsg">>
    , query_id => <<"queryId">>
    , settle_date => <<"settleDate">>
    , resp_code => <<"respCode">>
    , resp_msg => <<"respMsg">>
    , quota => {<<"quota">>, integer}
    , id_type => <<"certifType">>
    , id_no => <<"certifId">>
    , id_name => <<"certifName">>
    , mobile => <<"phoneNo">>

  }.
%%------------------------------------------------------
get(M, Model, mcht_index_key) when is_atom(M), is_tuple(Model) ->
  {
    pg_model:get(M, Model, mcht_id)
    , pg_model:get(M, Model, mcht_txn_date)
    , pg_model:get(M, Model, mcht_txn_seq)
  };
get(M, Model, Field) when is_atom(Field), is_atom(M), is_tuple(Model) ->
  pg_model:get(M, Model, Field).
%%------------------------------------------------------


%%====================================================================
%% Internal functions
%%====================================================================
