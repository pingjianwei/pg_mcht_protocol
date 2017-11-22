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
-define(APP, pg_mcht_protocol).

-compile(export_all).

cleanup(_Pid) ->
  db_init(),
  ok.

setup() ->
  pg_test_utils:lager_init(),
  application:start(pg_mcht_enc),
  application:start(up_config),
  env_init(),

  pg_test_utils:setup(mnesia),

  db_init(),

  ok.


db_init() ->
  RepoContents = [
    {pg_mcht_protocol:repo_module(mchants),
      [
        [
          {id, 1}
          , {payment_method, [gw_collect]}
          , {sign_method, rsa_hex}
        ],
        [
          {id, 2}
          , {payment_method, [gw_wap]}
          , {sign_method, rsa_base64}
        ],
        [
          {id, 3}
          , {payment_method, [gw_netbank]}
        ],
        [
          {id, 4}
          , {payment_method, [gw_netbank_only]}
        ]

      ]
    },
    {pg_mcht_protocol:repo_module(mcht_txn_log),
      [

      ]
    }
  ],

  pg_test_utils:db_init(RepoContents),
  ok.

%%---------------------------------------------------------------
db_init_test_1() ->
  M = pg_mcht_protocol:repo_module(mchants),
  [Repo] = pg_repo:read(M, 1),
  ?assertEqual(1, pg_model:get(M, Repo, id)),
  ok.
%%---------------------------------------------------------------
env_init() ->
  Cfgs = [
    {pg_convert,
      [
        {debug, true}
      ]
    },
    {?APP,
      [
        {debug, true}
        , {mcht_repo_name, pg_mcht_protocol_t_repo_mchants_pt}
        , {mcht_txn_log_repo_name, pg_mcht_protocol_t_repo_mcht_txn_log_pt}
        , {up_txn_log_repo_name, pg_mcht_protocol_t_repo_up_txn_log_pt}
        , {limit_txn_amt_min, 50}
        , {limit_bank_card_no_len, {16, 21}}

      ]
    }
  ],

  pg_test_utils:env_init(Cfgs),
  ok.

my_test_() ->
  {
    setup
    , fun setup/0
    , fun cleanup/1
    ,
    {
      inorder,
      [
        fun db_init_test_1/0

        , fun verify_test_1/0
        , fun sign_test_1/0
        , fun save_test_1/0
        , fun collect_sign_test_1/0
        , fun collect_save_test_1/0
        , fun collect_validate_test_1/0
        , fun collect_verify_test_1/0

        , fun validate_biz_test_1/0
        , fun validate_biz_all_test_1/0

        , fun req_collect_resp_fail_convert_test_1/0
        , fun resp_collect_convert_test_1/0
        , fun info_collect_convert_test_1/0

        , fun batch_collect_test_1/0
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
    , {<<"bankCardNo">>, <<"6216261000000000018">>}
    , {<<"tranDate">>, <<"20171021">>}
    , {<<"tranTime">>, <<"095817">>}
    , {<<"signature">>, <<"7D2B74AF2BCC3B1C4C1B6FF2328E3C27881FB0497FB0413D4E53801047E1F83CD19CE97B4D9A0C4C7D9BD17B3D9AF4F652536EAA6076E1A1B5D1E7C53A6E3CF1572C8647407BFEF7CD5BBE8ECF210EA495A4335E43A012E4CAF17B6E9FD7813D2E6D44D52B84D823FF8EBD156E10B446E673994DFA1060F1C1D5371DB618439E2FD666BC1E99A49BCC1642A44592292A8942373967E48A51D27C2C5DD8276F679CD30025C3E8ED9F22B004494DFBA2DB0EEA311A5596B6D4B4067CD534A5CFCF61CE1086C6871CE33AF8525E1F2A7B0F8FF33A7D6CF431FB0A309A6441DBF414C7A4F7DF3D1FC2734C40913D566D900B32DA85D01D0583FF0AA69EC326C2E01A">>}
    , {<<"certifType">>, <<"01">>}
    , {<<"certifId">>, <<"341126197709218366">>}
    , {<<"certifName">>, <<"全渠道"/utf8>>}
    , {<<"phoneNo">>, <<"13552535506">>}
    , {<<"trustBackUrl">>, <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>}
  ];
qs(batch_collect) ->
  [
    {<<"tranAmt">>, <<"130000">>}
    , {<<"orderDesc">>, <<"测试交易"/utf8>>}
    , {<<"merchId">>, <<"00001">>}
    , {<<"tranId">>, <<"20171021095817473460847">>}
    , {<<"tranDate">>, <<"20171021">>}
    , {<<"tranTime">>, <<"095817">>}
    , {<<"signature">>, <<"7D2B74AF2BCC3B1C4C1B6FF2328E3C27881FB0497FB0413D4E53801047E1F83CD19CE97B4D9A0C4C7D9BD17B3D9AF4F652536EAA6076E1A1B5D1E7C53A6E3CF1572C8647407BFEF7CD5BBE8ECF210EA495A4335E43A012E4CAF17B6E9FD7813D2E6D44D52B84D823FF8EBD156E10B446E673994DFA1060F1C1D5371DB618439E2FD666BC1E99A49BCC1642A44592292A8942373967E48A51D27C2C5DD8276F679CD30025C3E8ED9F22B004494DFBA2DB0EEA311A5596B6D4B4067CD534A5CFCF61CE1086C6871CE33AF8525E1F2A7B0F8FF33A7D6CF431FB0A309A6441DBF414C7A4F7DF3D1FC2734C40913D566D900B32DA85D01D0583FF0AA69EC326C2E01A">>}
    , {<<"trustBackUrl">>, <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>}
    , {<<"tranCount">>, <<"3">>}
    , {<<"fileContent">>, <<"aaa">>}
    , {<<"batchNo">>, <<"0009">>}
    , {<<"reqReserved">>, <<"qqq">>}
  ];
qs(query_collect) ->
  [
    {<<"merchId">>, <<"00001">>}
    , {<<"tranId">>, <<"20171021095817473460847">>}
    , {<<"tranDate">>, <<"20171021">>}
    , {<<"tranTime">>, <<"095817">>}
  ].

pk(pay) ->
  {<<"00001">>, <<"20170124">>, <<"20170124140404395762577">>};
pk(collect) ->
  {<<"00001">>, <<"20171021">>, <<"20171021095817473460847">>};
pk(query_collect) ->
  {<<"00001">>, <<"20171021">>, <<"20171021095817473460847">>};
pk(batch_collect) ->
  {<<"00001">>, <<"20171021">>, <<"20171021095817473460847">>}.

protocol(pay) ->
  M = ?M_Protocol,
  P = pg_protocol:out_2_in(M, qs(pay)),
  P;
protocol(collect) ->
  pg_protocol:out_2_in(pg_mcht_protocol_req_collect, qs(collect));
protocol(query_collect) ->
  pg_protocol:out_2_in(pg_mcht_protocol_req_query, qs(query_collect));
protocol(batch_collect) ->
  pg_protocol:out_2_in(pg_mcht_protocol_req_batch_collect, qs(batch_collect)).

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
  ",bank_card_no=<<>>,bank_id=<<>>,"/utf8>>,

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

  {_, Sig64} = pg_mcht_protocol:sign(M, pg_model:set(M, P, mcht_id, <<"00002">>)),
  SigExpected64 = <<"iL5jvvq8Cah70OUa+TVLhKVkndcuWS2wJt3JuJnVvltQbEXoQF7w908u0rKbkjRF9Z2ZVbIbckZF0zfTejb8WbuqAR2XYNXj9j5t1WwsDEcZMy3KuBcfoyCngrN69fuPThBDe2Fe55AKVh1iWzW02aEg72Y2CbMzWQitqFFL1wUwCx6OFWlaRTZ95hA5AGmkXSuDFU5T3y5A+i+VNSiv5zB46PeMnd+B/63y8UAnkW21ezM+mmbxhxeeI3XBuEkflCgNhUwWgfj+ovEAY0hDI+a9Gy3ZCWJj0xPUJst6ylrHjy9DR2UgcgA1/bVvtxt1tmJtMtRr03V5obCV9m21ig==">>,
  ?assertEqual(SigExpected64, Sig64),
  ok.

%%-----------------------------------------------------------
save_test_1() ->
  P = protocol(pay),
  MRepo = pg_mcht_protocol:repo_module(mcht_txn_log),

  Repo = pg_convert:convert(?M_Protocol, [P, P], save_req),
  ?assertEqual(pay, pg_model:get(MRepo, Repo, txn_type)),
  ?assertEqual(<<"00001">>, pg_model:get(MRepo, Repo, mcht_id)),
  ?assertEqual(pk(pay), pg_model:get(MRepo, Repo, mcht_index_key)),

  pg_mcht_protocol:save(?M_Protocol, P),

  [Repo] = pg_repo:read(MRepo, pk(pay)),

  ?assertEqual([pay, waiting, 100],
    pg_model:get(MRepo, Repo, [txn_type, txn_status, txn_amt])),
  ok.

%%-----------------------------------------------------------
%% collect test
collect_sign_string_test() ->
  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),
  ?assertEqual(<<"00001201710212017102109581747346084709581750测试交易http://localhost:8888/pg/simu_mcht_back_succ_info621626100000000001801341126197709218366全渠道13552535506"/utf8>>,
    pg_mcht_protocol:sign_string(M, P)),
  ?assertEqual(pk(collect), pg_mcht_protocol:get(M, P, mcht_index_key)),
  ok.

collect_sign_test_1() ->
  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),
  {_, Sig} = pg_mcht_protocol:sign(M, P),
  ?assertEqual(<<"7D2B74AF2BCC3B1C4C1B6FF2328E3C27881FB0497FB0413D4E53801047E1F83CD19CE97B4D9A0C4C7D9BD17B3D9AF4F652536EAA6076E1A1B5D1E7C53A6E3CF1572C8647407BFEF7CD5BBE8ECF210EA495A4335E43A012E4CAF17B6E9FD7813D2E6D44D52B84D823FF8EBD156E10B446E673994DFA1060F1C1D5371DB618439E2FD666BC1E99A49BCC1642A44592292A8942373967E48A51D27C2C5DD8276F679CD30025C3E8ED9F22B004494DFBA2DB0EEA311A5596B6D4B4067CD534A5CFCF61CE1086C6871CE33AF8525E1F2A7B0F8FF33A7D6CF431FB0A309A6441DBF414C7A4F7DF3D1FC2734C40913D566D900B32DA85D01D0583FF0AA69EC326C2E01A">>,
    Sig),

  {_, Sig64} = pg_mcht_protocol:sign(M, pg_model:set(M, P, mcht_id, <<"00002">>)),
  ?assertEqual(<<"2dk5VdDMFoVBnfxj0rSP+Xf9ygt4RdRU/+l2llQaHMmVpkocOOAuwuhc5D7S7hKf58eL6boY0aAmGLtlZuLBbFUlj1hfJFD1rS1m0puLYsv5qXMiwq9DdQ9F87CXhsqmT167kNsWQr6FWSD1k5CebsIsVMp/0ZaxR69W7b8Nx/jV1CpgndZI5qcVxPx3WgtddfbxnS8FYcUjU29lumyVEThucfoTsluuz4iHxERUxpVkQq+Qr7g4772KmVNLc1J4n9zx4QiwevlznUBfq7dpDu2958EqbWtibO+/rTVALcZdVj/yFwNgiEgqfDWCQlB7Vy+bCn/H+yLZYqmDpEf0og==">>,
    Sig64),
  ok.

collect_verify_test_1() ->
  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),
  ?assertEqual(ok, pg_mcht_protocol:verify(M, P)),

  P64 = pg_model:set(M, P,
    [{mcht_id, <<"00002">>},
      {signature, <<"2dk5VdDMFoVBnfxj0rSP+Xf9ygt4RdRU/+l2llQaHMmVpkocOOAuwuhc5D7S7hKf58eL6boY0aAmGLtlZuLBbFUlj1hfJFD1rS1m0puLYsv5qXMiwq9DdQ9F87CXhsqmT167kNsWQr6FWSD1k5CebsIsVMp/0ZaxR69W7b8Nx/jV1CpgndZI5qcVxPx3WgtddfbxnS8FYcUjU29lumyVEThucfoTsluuz4iHxERUxpVkQq+Qr7g4772KmVNLc1J4n9zx4QiwevlznUBfq7dpDu2958EqbWtibO+/rTVALcZdVj/yFwNgiEgqfDWCQlB7Vy+bCn/H+yLZYqmDpEf0og==">>}]
  ),
  ?assertEqual(ok, pg_mcht_protocol:verify(M, P64)),
  ok.

collect_save_test_1() ->
  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),
  MRepo = pg_mcht_protocol:repo_module(mcht_txn_log),
  lager:error("M=~p,P=~p", [M, P]),
  {ok, RepoSave} = pg_mcht_protocol:save(M, P),

  [Repo] = pg_repo:read(MRepo, pk(collect)),
  ?assertEqual([collect, waiting, 50, <<"341126197709218366">>, <<"全渠道"/utf8>>],
    pg_model:get(MRepo, Repo, [txn_type, txn_status, txn_amt, id_no, id_name])),

  %% biz_test
  ?assertThrow({validate_fail, _, _}, pg_mcht_protocol_validate_biz:validate_biz_rule(M, P, tran_id)),
  ?assertEqual(ok, pg_mcht_protocol_validate_biz:validate_biz_rule(M, P, txn_amt)),

  pg_repo:update(pg_mcht_protocol:repo_module(mchants), [{id, 1}, {quota, [{txn, 100000}, {daily, -1}, {month, -1}]}]),
  ?assertEqual(ok, pg_mcht_protocol_validate_biz:validate_biz_rule(M, P, txn_amt)),

  ?assertThrow({validate_fail, <<"33">>, _},
    pg_mcht_protocol_validate_biz:validate_biz_rule(M, pg_model:set(M, P, txn_amt, 200000), txn_amt)),


  P1 = pg_model:set(M, P, mcht_id, <<"111">>),
  PK1 = pg_mcht_protocol:get(M, P1, mcht_index_key),
%%  {ok, [Repo1]} = pg_repo:fetch(MRepo, PK1),
%%  ?debugFmt("Repo1=~p", [Repo1]),

  ?assertEqual(ok, pg_mcht_protocol_validate_biz:validate_biz_rule(M, P1, tran_id)),

  %% query validate test
  MQuery = pg_mcht_protocol_req_query,
  PQuery = protocol(query_collect),
  lager:debug("PQuery = ~ts", [pg_model:pr(MQuery, PQuery)]),
  ?debugFmt("PQuery = ~ts", [pg_model:pr(MQuery, PQuery)]),
  ?assertEqual(ok, pg_mcht_protocol_validate_biz:validate_biz_rule(MQuery, PQuery, orig_txn)),

  ?assertThrow({validate_fail, <<"35">>, _}, pg_mcht_protocol_validate_biz:validate_biz_rule(MQuery,
    pg_model:set(MQuery, PQuery, txn_date, <<"20170101">>), orig_txn)),

  PRespQuery = pg_convert:convert(pg_mcht_protocol_resp_query, Repo, normal_resp),
  ?assertEqual([pk(collect)], pg_mcht_protocol:get(pg_mcht_protocol_resp_query, PRespQuery, [mcht_index_key])),


  ok.

collect_validate_test_1() ->
  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),
  ?assertEqual(ok, pg_mcht_protocol:validate_format(qs(collect))),
  ok.

%%-----------------------------------------------------------
%% validate_biz test
validate_biz_test_1() ->
  db_init(),

  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),


  %%------------------------------------
  %% mcht_id
  ?assertEqual(ok, pg_mcht_protocol_validate_biz:validate_biz_rule(M, P, mcht_id)),
  ?assertThrow({validate_fail, _, _},
    pg_mcht_protocol_validate_biz:validate_biz_rule(M,
      pg_model:set(M, P, mcht_id, <<"100">>),
      mcht_id)),

  %%------------------------------------
  %% tran_id
  PK = pk(collect),
  MRepo = pg_mcht_protocol:repo_module(mcht_txn_log),
  {ok, Repo} = pg_repo:fetch(MRepo, PK),
  ?assertEqual([], Repo),
  ?assertEqual(ok, pg_mcht_protocol_validate_biz:validate_biz_rule(M, P, tran_id)),
  %%------------------------------------
  %% sig
  ?assertEqual(ok, pg_mcht_protocol_validate_biz:validate_biz_rule(M, P, sig)),
  ?assertThrow({validate_fail, _, _},
    pg_mcht_protocol_validate_biz:validate_biz_rule(M,
      pg_model:set(M, P, signature, <<"AAAA">>),
      sig)),

  %%------------------------------------
  %% payment_method
  ?assertEqual(ok, pg_mcht_protocol_validate_biz:validate_biz_rule(M, P, payment_method)),

  %%------------------------------------
  %% gw_netbank/gw_wap
  MPay = ?M_Protocol,
  PPay = protocol(pay),
%%  ?debugFmt("PPay = ~p", [PPay]),
  ?assertThrow({validate_fail, _, _}, pg_mcht_protocol_validate_biz:validate_biz_rule(MPay, PPay, payment_method)),
  ?assertEqual(ok,
    pg_mcht_protocol_validate_biz:validate_biz_rule(MPay,
      pg_model:set(MPay, PPay, [{mcht_id, <<"00002">>}]),
      payment_method)),
  ?assertEqual(ok,
    pg_mcht_protocol_validate_biz:validate_biz_rule(MPay,
      pg_model:set(MPay, PPay, [{mcht_id, <<"00003">>}]),
      payment_method)),
  ?assertThrow({validate_fail, _, _},
    pg_mcht_protocol_validate_biz:validate_biz_rule(MPay,
      pg_model:set(MPay, PPay, [{mcht_id, <<"00004">>}]),
      payment_method)),

  %% ------------------------------------------
  %% txn_amt
  ?assertEqual(ok, pg_mcht_protocol_validate_biz:validate_biz_rule(MPay, PPay, txn_amt)),
  ?assertThrow({validate_fail, _, _},
    pg_mcht_protocol_validate_biz:validate_biz_rule(MPay,
      pg_model:set(MPay, PPay, [{txn_amt, 1}]),
      txn_amt)),

  ok.

validate_biz_all_test_1() ->
  db_init(),

  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),
  ?assertEqual(ok, pg_mcht_protocol:validate_biz(M, P)),
  ok.
%%-----------------------------------------------------------
req_collect_resp_fail_convert_test_1() ->
  M = pg_mcht_protocol_req_collect,
  P = protocol(collect),
  MResp = pg_mcht_protocol_resp_collect,

  Resp = pg_convert:convert(MResp, P, fail_resp),
  ExpResp = pg_model:new(MResp, pg_model:to(M, P, proplists)),

  ?assertEqual(ExpResp, Resp),

  ok.
%%-----------------------------------------------------------
resp_collect_convert_test_1() ->
  MRepoUp = pg_mcht_protocol:repo_module(up_txn_log),

  RepoUp = pg_model:new(MRepoUp, [
    {mcht_index_key, {a, b, c}}
    , {up_respCode, <<"88">>}
    , {up_respMsg, <<"Die for hard">>}
    , {txn_status, fail}
    , {up_orderId, <<"20170101">>}
  ]),

  VL = pg_convert:convert(pg_mcht_protocol_resp_collect, RepoUp),
  ?assertEqual([
    {mcht_index_key, {a, b, c}}
    , {resp_code, <<"88">>}
    , {resp_msg, <<"Die for hard">>}
    , {txn_status, fail}
    , {query_id, <<"20170101">>}
  ], [{Key, proplists:get_value(Key, VL)} || Key <- [mcht_index_key, resp_code, resp_msg, txn_status, query_id]]),

  ok.

%% -------------------------------------------------------------------
info_collect_convert_test_1() ->
  MRepoUp = pg_mcht_protocol:repo_module(up_txn_log),

  RepoUp = pg_model:new(MRepoUp, [
    {mcht_index_key, {a, b, c}}
    , {up_respCode, <<"88">>}
    , {up_respMsg, <<"Die for hard">>}
    , {txn_status, fail}
    , {up_orderId, <<"20170101">>}
    , {up_settleDate, <<"20101010">>}
  ]),

  VL = pg_convert:convert(pg_mcht_protocol_info_collect, RepoUp),
  ?assertEqual([
    {mcht_index_key, {a, b, c}}
    , {resp_code, <<"88">>}
    , {resp_msg, <<"Die for hard">>}
    , {txn_status, fail}
    , {query_id, <<"20170101">>}
    , {settle_date, <<"20101010">>}
  ], [{Key, proplists:get_value(Key, VL)}
    || Key <- [mcht_index_key, resp_code, resp_msg, txn_status, query_id, settle_date]]),
  ok.
%%-----------------------------------------------------------
batch_collect_test_1() ->
  M = pg_mcht_protocol_req_batch_collect,
  P = protocol(batch_collect),

  {ok, RepoNew} = pg_mcht_protocol:save(M, P),
  ?assertEqual([pk(batch_collect), 130000, 3, 9, <<"aaa">>],
    pg_model:get(pg_mcht_protocol:repo_module(mcht_txn_log),
      RepoNew,
      [mcht_index_key, txn_amt, txn_count, batch_no, file_content])),

  {SignString, Sig} = pg_mcht_protocol:sign(M, P),
  lager:debug("SignString = ~ts,Sig=~ts", [SignString, Sig]),

  ok.
