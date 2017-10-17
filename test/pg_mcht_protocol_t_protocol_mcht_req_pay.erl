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
-behaviour(pg_model).
-behaviour(pg_protocol).
-behaviour(pg_mcht_protocol).

-compile(export_all).
%% API
%% callbacks of pg_model
-export([
  pr_formatter/1
]).
%% callbacks of pg_protocol
-export([
  in_2_out_map/0
]).
%% callbacks of pg_mcht_protocol
-export([
  sign_fields/0
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
pr_formatter(Field) ->
  pg_mcht_protocol:pr_formatter(Field).

in_2_out_map() ->
  pg_mcht_protocol:in_2_out_map().

sign_fields() ->
  [
    mcht_id
    , mcht_txn_date
    , mcht_txn_seq
    , mcht_txn_time
    , mcht_txn_amt
    , bank_id
    , mcht_order_desc
    , gateway_id
    , mcht_back_url
    , mcht_front_url
    , prod_id
    , prod_memo
    , prod_bank_acct_id
    , prod_bank_acct_corp_name
    , prod_bank_name
    , bank_card_no

  ].

convert_config() ->
  [

  ].


%%===============================================
%% UT
%%==============================================
qs() ->

  PostVals = [
    {<<"tranAmt">>, <<"100">>}
    , {<<"orderDesc">>, <<"{pI=test,aI=03429500040006212,aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行}"/utf8>>}
    , {<<"merchId">>, <<"00001">>}
    , {<<"tranId">>, <<"20170124140404395762577">>}
    , {<<"bankCardNo">>, <<>>}
    , {<<"prodId">>, <<>>}
    , {<<"tranDate">>, <<"20170124">>}
    , {<<"tranTime">>, <<"140404">>}
    , {<<"accountId">>, <<>>}
    , {<<"accountName">>, <<>>}
    , {<<"accountBank">>, <<>>}
    , {<<"signature">>, <<"D5647B10AC1B573569645DB67DBBF987A0CBF45B965D4C89009EFCA04191197C45633B0328DA84B3C61CC7E5FC09449F7875A283296339C13D0BB615799F6CA58EAC1135CC925447185BAE2E6EDFC7F363313D3382E07EA5E65D3124FFD14CC679E9C65B93D33EDACB763A8B5ABCA25F4EDE628AD98E9EE0F98833F6C0BD1A40388067BBD2E8F3BEC650344A6F90E92A7687160F18DBD9554DA6347D3E0A5FBF546B6C979AE306EA1DD41525BE44C858CBC5E954985825C862199E90DA29749612B89052B1888308B27101D00BE43AC7FD84FF55B144DE02FFC1B3DB9FCCD7E9005942D0A4BBAEA5FAD97B2923669C366C11CD4866197CD9D424801D7F68325D">>}
    , {<<"gateWayId">>, <<"1003">>}
    , {<<"bankId">>, <<>>}
    , {<<"trustFrontUrl">>, <<"http://localhost:8888/pg/simu_mcht_front_succ">>}
    , {<<"trustBackUrl">>, <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>}
  ],

  PostVals.

pk() ->
  {<<"00001">>, <<"20170124">>, <<"20170124140404395762577">>}.

protocol() ->
  M = ?MODULE,
  pg_protocol:out_2_in(M, qs()).


get_test() ->

  M = ?MODULE,
  MchtProtocol = protocol(),

  ?assertEqual(<<"00001">>, pg_mcht_protocol:get(M, MchtProtocol, mcht_id)),
  ?assertEqual(pk(), pg_mcht_protocol:get(M, MchtProtocol, mcht_index_key)),
  ok.

pr_test() ->
  M = ?MODULE,
  P = protocol(),

  String = pg_model:pr(M, P),

  ExpString = <<"mcht_id=<<\"00001\">>,mcht_txn_date=<<\"20170124\">>,mcht_txn_time=<<\"140404\">>,"
  "mcht_txn_seq=<<\"20170124140404395762577\">>,mcht_txn_amt=100,mcht_order_desc={pI=test,aI=03429500040006212,"
  "aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行},mcht_front_url=<<\"http://localhost:8888/pg/simu_mcht_front_succ\">>,"
  "mcht_back_url=<<\"http://localhost:8888/pg/simu_mcht_back_succ_info\">>,"
  "signature=D5647B10AC1B573569645DB67DBBF987A0CBF45B965D4C89009EFCA04191197C45633B0328DA84B3C61CC7E5FC09449F7875A283296339C13D0BB615799F6CA58EAC1135CC925447185BAE2E6EDFC7F363313D3382E07EA5E65D3124FFD14CC679E9C65B93D33EDACB763A8B5ABCA25F4EDE628AD98E9EE0F98833F6C0BD1A40388067BBD2E8F3BEC650344A6F90E92A7687160F18DBD9554DA6347D3E0A5FBF546B6C979AE306EA1DD41525BE44C858CBC5E954985825C862199E90DA29749612B89052B1888308B27101D00BE43AC7FD84FF55B144DE02FFC1B3DB9FCCD7E9005942D0A4BBAEA5FAD97B2923669C366C11CD4866197CD9D424801D7F68325D"
  ",bank_card_no=<<>>,"/utf8>>,

  ?assertEqual(ExpString, unicode:characters_to_binary(String)),


  ok.

