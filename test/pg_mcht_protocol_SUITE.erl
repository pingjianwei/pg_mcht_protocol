%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 十月 2017 16:50
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_SUITE).
-author("simon").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

-define(M_Protocol, pg_mcht_protocol_t_protocol_mcht_req_pay).
-define(M_Repo, pg_mcht_protocol_t_repo_mcht_txn_log_pt).

-compile(export_all).

setup() ->
  lager:start(),
  pg_mcht_enc:start(),

  pg_test_utils:setup(mnesia),

  pg_repo:drop(?M_Repo),
  pg_repo:init(?M_Repo),

  ok.


my_test_() ->
  {
    setup
    , fun setup/0
    ,
    {
      inorder,
      [
        fun verify_test_1/0
        , fun sign_test_1/0
      ]
    }

  }.

%%-----------------------------------------------------------
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
    , {<<"signature">>, <<"A49FA652C9C6320BC1E6EA06D1B6BA244FB541346D4EF36699BCAF14DE53B668A63B502AB5FF4BB304BD703B9B8E517157C503C53BFAD397AEEAB11627CBF160C6ED40063566E7BB45F8BF3C5F8CD4E327CD538094F216574EEE79EF9C23567078088B3834F45CD29D13332181D48109FA8EDC4EFD3911B10CD55F6AF01D5C72BD57D3433A935DB2F90F5601DE3B332784CDB986BD3358225C30DC50A3236656625FCAAF1F2BA4399A348CAE9A3E29FCB9B51D4B74E6AB7C483F8BD556D43831DE3335C3126681124407FF334F36B2B9EF96B92921319DACF46A27254436D0A3E41D60261FC9544E0D93D09C59D8C55D18A914B907AA2AE695F96ABFE11433DE">>}
    , {<<"bankId">>, <<>>}
    , {<<"trustFrontUrl">>, <<"http://localhost:8888/pg/simu_mcht_front_succ">>}
    , {<<"trustBackUrl">>, <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>}
  ],

  PostVals.

pk() ->
  {<<"00001">>, <<"20170124">>, <<"20170124140404395762577">>}.

protocol() ->
  M = ?M_Protocol,
  pg_protocol:out_2_in(M, qs()).


get_test() ->

  M = ?M_Protocol,
  MchtProtocol = protocol(),

  ?assertEqual(<<"00001">>, pg_mcht_protocol:get(M, MchtProtocol, mcht_id)),
  ?assertEqual(pk(), pg_mcht_protocol:get(M, MchtProtocol, mcht_index_key)),
  ok.

pr_test() ->
  M = ?M_Protocol,
  P = protocol(),

  String = pg_model:pr(M, P),

  ExpString = <<"mcht_id=<<\"00001\">>,mcht_txn_date=<<\"20170124\">>,mcht_txn_time=<<\"140404\">>,"
  "mcht_txn_seq=<<\"20170124140404395762577\">>,mcht_txn_amt=100,mcht_order_desc={pI=test,aI=03429500040006212,"
  "aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行},mcht_front_url=<<\"http://localhost:8888/pg/simu_mcht_front_succ\">>,"
  "mcht_back_url=<<\"http://localhost:8888/pg/simu_mcht_back_succ_info\">>,"
  "signature=<<\"A49FA652C9C6320BC1E6EA06D1B6BA244FB541346D4EF36699BCAF14DE53B668A63B502AB5FF4BB304BD703B9B8E517157C503C53BFAD397AEEAB11627CBF160C6ED40063566E7BB45F8BF3C5F8CD4E327CD538094F216574EEE79EF9C23567078088B3834F45CD29D13332181D48109FA8EDC4EFD3911B10CD55F6AF01D5C72BD57D3433A935DB2F90F5601DE3B332784CDB986BD3358225C30DC50A3236656625FCAAF1F2BA4399A348CAE9A3E29FCB9B51D4B74E6AB7C483F8BD556D43831DE3335C3126681124407FF334F36B2B9EF96B92921319DACF46A27254436D0A3E41D60261FC9544E0D93D09C59D8C55D18A914B907AA2AE695F96ABFE11433DE\">>"
  ",bank_card_no=<<>>,"/utf8>>,

%%  lager:start(),
%%  lager:error("ExpString = ~ts~n", [ExpString]),

  PrString = unicode:characters_to_binary(String),
%%  lager:error("PrString = ~ts~n", [PrString]),

  ?assertEqual(ExpString, PrString),


  ok.

sign_string_test() ->
  M = ?M_Protocol,
  P = protocol(),

  SignString = pg_mcht_protocol:sign_string(M, P),
  ?assertEqual(
    <<"000012017012420170124140404395762577140404100{pI=test,aI=03429500040006212,"
    "aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行}"
    "http://localhost:8888/pg/simu_mcht_back_succ_info"
    "http://localhost:8888/pg/simu_mcht_front_succ"
    /utf8>>
    , SignString),
  ok.
%%-----------------------------------------------------------
verify_test_1() ->
  M = ?M_Protocol,
  P = protocol(),


  ?assertEqual(ok, pg_mcht_protocol:verify(M, P)),
  ok.

sign_test_1() ->
  M = ?M_Protocol,
  P = protocol(),

  {_, Sig} = pg_mcht_protocol:sign(M, P),

  SigExpected = <<"A49FA652C9C6320BC1E6EA06D1B6BA244FB541346D4EF36699BCAF14DE53B668A63B502AB5FF4BB304BD703B9B8E517157C503C53BFAD397AEEAB11627CBF160C6ED40063566E7BB45F8BF3C5F8CD4E327CD538094F216574EEE79EF9C23567078088B3834F45CD29D13332181D48109FA8EDC4EFD3911B10CD55F6AF01D5C72BD57D3433A935DB2F90F5601DE3B332784CDB986BD3358225C30DC50A3236656625FCAAF1F2BA4399A348CAE9A3E29FCB9B51D4B74E6AB7C483F8BD556D43831DE3335C3126681124407FF334F36B2B9EF96B92921319DACF46A27254436D0A3E41D60261FC9544E0D93D09C59D8C55D18A914B907AA2AE695F96ABFE11433DE">>,

  ?assertEqual(SigExpected, Sig),

  ok.

%%-----------------------------------------------------------
save_test_1() ->
  M = ?M_Protocol,
  P = protocol(),

  pg_mcht_protocol:save(?M_Protocol, P),

  [Repo] = pg_repo:read(?M_Repo, pk()),

  ?assertEqual([pay, waiting, 100],
    pg_model:get(?M_Repo, Repo, [txn_type, txn_status, mcht_txn_amt])),
  ok.

