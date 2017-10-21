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
-define(APP, pg_mcht_protocol).

-compile(export_all).

setup() ->
  lager:start(),
  application:start(pg_mcht_enc),

  pg_test_utils:setup(mnesia),

  pg_repo:drop(?M_Repo),
  pg_repo:init(?M_Repo),


  application:set_env(?APP, mcht_repo_name, pg_mcht_protocol_t_repo_mcht_txn_log_pt),
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
        , fun save_test_1/0
        , fun collect_sign_test_1/0
        , fun collect_save_test_1/0
      ]
    }

  }.

%%-----------------------------------------------------------
qs(pay) ->

  [
    {<<"tranAmt">>, <<"100">>}
    , {<<"orderDesc">>, <<"{pI=test,aI=03429500040006212,aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行}"/utf8>>}
    , {<<"merchId">>, <<"00001">>}
    , {<<"tranId">>, <<"20170124140404395762577">>}
    , {<<"bankCardNo">>, <<>>}
    , {<<"tranDate">>, <<"20170124">>}
    , {<<"tranTime">>, <<"140404">>}
    , {<<"signature">>, <<"A49FA652C9C6320BC1E6EA06D1B6BA244FB541346D4EF36699BCAF14DE53B668A63B502AB5FF4BB304BD703B9B8E517157C503C53BFAD397AEEAB11627CBF160C6ED40063566E7BB45F8BF3C5F8CD4E327CD538094F216574EEE79EF9C23567078088B3834F45CD29D13332181D48109FA8EDC4EFD3911B10CD55F6AF01D5C72BD57D3433A935DB2F90F5601DE3B332784CDB986BD3358225C30DC50A3236656625FCAAF1F2BA4399A348CAE9A3E29FCB9B51D4B74E6AB7C483F8BD556D43831DE3335C3126681124407FF334F36B2B9EF96B92921319DACF46A27254436D0A3E41D60261FC9544E0D93D09C59D8C55D18A914B907AA2AE695F96ABFE11433DE">>}
    , {<<"bankId">>, <<>>}
    , {<<"trustFrontUrl">>, <<"http://localhost:8888/pg/simu_mcht_front_succ">>}
    , {<<"trustBackUrl">>, <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>}
  ];
qs(collect) ->
  [
    {<<"tranAmt">>, <<"50">>}
    , {<<"orderDesc">>, <<"测试交易"/utf8>>}
    , {<<"merchId">>, <<"00001">>}
    , {<<"tranId">>, <<"20171021095817473460847">>}
    , {<<"bankCardNo">>, <<"9555500216246958">>}
    , {<<"tranDate">>, <<"20171021">>}
    , {<<"tranTime">>, <<"095817">>}
    , {<<"signature">>, <<"16808B681094E884DC4EDF3882D59AFA4063D1D58867EAC6E52852F1018E2363A93F5790E2E737411716270A9A04B394294A1F91599C9603DA0EC96EE82B796CF483C94BC4D88C85EB7CE3B0EC9C142D7F512C95B428AF16F870C7458A07A270EE7773BAA44414462D7FAEBC430E59FCAB1AEAC587520D15933EDEC262741A9FE8D7F12DFEB8C87F568F3B9E074103E7731D8713275BA004B18C33F54C4ABB9815B63AF3A2585B4268354E52B19D094D33653771D77949E873A683AD9E9282EC75E8D1DF22F845FCCD9B50F2971072A82026A0D270E78B63C55ED065DE025F472E04B9F24D8F31AE0BE9133E42F029CF18C7128F13770B3F7BEC9DCBC329527B">>}
    , {<<"certifType">>, <<"01">>}
    , {<<"certifId">>, <<"320404197205161013">>}
    , {<<"certifName">>, <<"徐峰"/utf8>>}
    , {<<"phoneNo">>, <<"13916043073">>}
    , {<<"trustBackUrl">>, <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>}
  ].

pk(pay) ->
  {<<"00001">>, <<"20170124">>, <<"20170124140404395762577">>};
pk(collect) ->
  {<<"00001">>, <<"20171021">>, <<"20171021095817473460847">>}.

protocol(pay) ->
  M = ?M_Protocol,
  P = pg_protocol:out_2_in(M, qs(pay)),
  P;
protocol(collect) ->
  pg_protocol:out_2_in(pg_mcht_protocol_req_collect, qs(collect)).

get_test() ->

  M = ?M_Protocol,
  MchtProtocol = protocol(pay),

  ?assertEqual(<<"00001">>, pg_mcht_protocol:get(M, MchtProtocol, mcht_id)),
  ?assertEqual(pk(pay), pg_mcht_protocol:get(M, MchtProtocol, mcht_index_key)),
  ok.

pr_test() ->
  M = ?M_Protocol,
  P = protocol(pay),

  String = pg_model:pr(M, P),

  ExpString = <<"mcht_id=<<\"00001\">>,txn_date=<<\"20170124\">>,txn_time=<<\"140404\">>,"
  "txn_seq=<<\"20170124140404395762577\">>,txn_amt=100,order_desc={pI=test,aI=03429500040006212,"
  "aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行},front_url=<<\"http://localhost:8888/pg/simu_mcht_front_succ\">>,"
  "back_url=<<\"http://localhost:8888/pg/simu_mcht_back_succ_info\">>,"
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
  P = protocol(pay),

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
  P = protocol(pay),


  ?assertEqual(ok, pg_mcht_protocol:verify(M, P)),
  ok.

sign_test_1() ->
  M = ?M_Protocol,
  P = protocol(pay),

  {_, Sig} = pg_mcht_protocol:sign(M, P),

  SigExpected = <<"A49FA652C9C6320BC1E6EA06D1B6BA244FB541346D4EF36699BCAF14DE53B668A63B502AB5FF4BB304BD703B9B8E517157C503C53BFAD397AEEAB11627CBF160C6ED40063566E7BB45F8BF3C5F8CD4E327CD538094F216574EEE79EF9C23567078088B3834F45CD29D13332181D48109FA8EDC4EFD3911B10CD55F6AF01D5C72BD57D3433A935DB2F90F5601DE3B332784CDB986BD3358225C30DC50A3236656625FCAAF1F2BA4399A348CAE9A3E29FCB9B51D4B74E6AB7C483F8BD556D43831DE3335C3126681124407FF334F36B2B9EF96B92921319DACF46A27254436D0A3E41D60261FC9544E0D93D09C59D8C55D18A914B907AA2AE695F96ABFE11433DE">>,

  ?assertEqual(SigExpected, Sig),

  ok.

%%-----------------------------------------------------------
save_test_1() ->
  P = protocol(pay),

  pg_mcht_protocol:save(?M_Protocol, P),

  [Repo] = pg_repo:read(?M_Repo, pk(pay)),

  ?assertEqual([pay, waiting, 100],
    pg_model:get(?M_Repo, Repo, [txn_type, txn_status, txn_amt])),
  ok.

%%-----------------------------------------------------------
%% collect test
collect_sign_string_test() ->
  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),
  ?assertEqual({ok, <<>>, <<>>}, pg_mcht_protocol:validate_format(qs(collect))),
  ?assertEqual(<<"00001201710212017102109581747346084709581750测试交易http://localhost:8888/pg/simu_mcht_back_succ_info955550021624695801320404197205161013徐峰13916043073"/utf8>>,
    pg_mcht_protocol:sign_string(M, P)),
  ?assertEqual(pk(collect), pg_mcht_protocol:get(M, P, mcht_index_key)),
  ok.

collect_sign_test_1() ->
  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),
  {_, Sig} = pg_mcht_protocol:sign(M, P),
  ?assertEqual(<<"16808B681094E884DC4EDF3882D59AFA4063D1D58867EAC6E52852F1018E2363A93F5790E2E737411716270A9A04B394294A1F91599C9603DA0EC96EE82B796CF483C94BC4D88C85EB7CE3B0EC9C142D7F512C95B428AF16F870C7458A07A270EE7773BAA44414462D7FAEBC430E59FCAB1AEAC587520D15933EDEC262741A9FE8D7F12DFEB8C87F568F3B9E074103E7731D8713275BA004B18C33F54C4ABB9815B63AF3A2585B4268354E52B19D094D33653771D77949E873A683AD9E9282EC75E8D1DF22F845FCCD9B50F2971072A82026A0D270E78B63C55ED065DE025F472E04B9F24D8F31AE0BE9133E42F029CF18C7128F13770B3F7BEC9DCBC329527B">>,
    Sig),
  ok.

collect_save_test_1() ->
  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),
  MRepo = pg_mcht_protocol:repo_mcht_module(),
  lager:error("M=~p,P=~p", [M, P]),
  pg_mcht_protocol:save(M, P),

  [Repo] = pg_repo:read(MRepo, pk(collect)),
  ?assertEqual([collect, waiting, 50, <<"320404197205161013">>, <<"徐峰"/utf8>>],
    pg_model:get(MRepo, Repo, [txn_type, txn_status, txn_amt, id_no, id_name])),
  ok.
