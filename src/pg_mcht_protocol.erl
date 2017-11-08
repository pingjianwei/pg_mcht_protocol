-module(pg_mcht_protocol).
-include_lib("eunit/include/eunit.hrl").
-include("include/type_mcht_protocol.hrl").
%%-behavior(pg_model).
%%-behavior(pg_protocol).

%% callbacks
-callback sign_fields() -> [atom()].
-callback options() -> map().
-callback validate() -> boolean().
%%-callback to_list(Protocol :: pg_model:pg_model()) -> proplists:proplist().

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
  , verify/2
  , sign_string/2
  , sign/2
  , validate_format/1
  , validate_biz/2
  , save/2
  , repo_module/1
  , limit/1
  , out_fields/1
  , out_2_in/2
  , in_2_out/3
]).

-type validate_result() :: ok | fail.

-define(APP, pg_mcht_protocol).
%%====================================================================
%% API functions
%%====================================================================
pr_formatter(Field)
  when
  (Field =:= order_desc)
%%  or (Field =:= signature)
    or (Field =:= id_name)
  ->
  string;
pr_formatter(_) ->
  default.

%%------------------------------------------------------

in_2_out_map() ->
  #{
    mcht_id => <<"merchId">>
    , txn_date => <<"tranDate">>
    , txn_seq => <<"tranId">>
    , txn_time => <<"tranTime">>
    , txn_amt => {<<"tranAmt">>, integer}
    , order_desc=> <<"orderDesc">>
    , signature=> <<"signature">>
    , front_url=> <<"trustFrontUrl">>
    , back_url=> <<"trustBackUrl">>
    , bank_card_no=> <<"bankCardNo">>
    , orig_txn_date => <<"origTranDate">>
    , orig_txn_seq => <<"origTranId">>
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
    , bank_id => <<"bankId">>

  }.
%%------------------------------------------------------
-spec get(M :: atom(), Model :: pg_model:pg_model(), Field :: atom())
      -> Value :: any().

get(M, Model, mcht_index_key) when is_atom(M), is_tuple(Model) ->
  {
    pg_model:get(M, Model, mcht_id)
    , pg_model:get(M, Model, txn_date)
    , pg_model:get(M, Model, txn_seq)
  };
get(M, Model, Field) when is_atom(Field), is_atom(M), is_tuple(Model) ->
  pg_model:get(M, Model, Field).
%%------------------------------------------------------
-spec verify(M, Protocol) -> PassOrNot when
  M :: atom(),
  Protocol :: pg_model:pg_model(),
  PassOrNot :: ok | fail.

verify(M, P) when is_atom(M), is_tuple(P) ->
  SignFields = M:sign_fields(),
  do_verify_msg(M, P, SignFields).


%%------------------------------------------------------
-spec sign_string(M, P) -> SignString when
  M :: atom(),
  P :: pg_model:pg_model(),
  SignString :: binary()| iolist().

sign_string(M, P) when is_atom(M), is_tuple(P) ->
  SignFields = M:sign_fields(),
  VL = [sign_value(pg_model:get(M, P, Field)) || Field <- SignFields],
  list_to_binary(VL).

sign_value(undefined) ->
  <<>>;
sign_value(Value) when is_integer(Value) ->
  integer_to_binary(Value);
sign_value(Value) when is_binary(Value) ->
  Value.


%%------------------------------------------------------------
-spec sign(M, P) -> Sig when
  M :: atom(),
  P :: pg_model:pg_model(),
  Sig :: binary() | iolist().

sign(M, P) when is_atom(M), is_tuple(P) ->
  SignString = sign_string(M, P),
  MchtId = pg_model:get(M, P, mcht_id),

  Direction = direction(M),

  MMchants = pg_mcht_protocol:repo_module(mchants),

  SignMethod = pg_repo:fetch_by(MMchants, binary_to_integer(MchtId), sign_method),

  Sig = case SignMethod of
          rsa_hex ->
            pg_mcht_enc:sign_hex(MchtId, Direction, SignString);
          rsa_base64 ->
            pg_mcht_enc:sign(MchtId, Direction, SignString)
        end,
  lager:debug("SignString = ~ts,Sig = ~ts", [SignString, Sig]),
  {SignString, Sig}.

%%------------------------------------------------------
-spec save(M, Protocol) -> Result when
  M :: atom(),
  Protocol :: pg_model:pg_model(),
  Result :: ok |fail.

save(M, Protocol) when is_atom(M), is_tuple(Protocol) ->
%%  VL = M:to_list(Protocol),
%%  MRepo = repo_mcht_module(),
%%  Repo = pg_model:new(MRepo, VL),
  Repo = pg_convert:convert(M, [Protocol, Protocol], save_req),
  pg_repo:save(Repo),
  {ok, Repo}.
%%------------------------------------------------------
-spec validate_format(VL) -> Result when
  VL :: proplists:proplist(),
  Result :: {validate_result, validate_result(), resp_code(), resp_msg()}.

%%validate_format(Params) when is_list(Params) ->
%%  F = fun({Key, Value}, {ok, _, _} = _AccIn) when is_binary(Key) ->
%%    try
%%      pg_mcht_protocol_validate_format:validate_format_one_field(Key, Value),
%%      {ok, <<>>, <<>>}
%%    catch
%%      _:_ ->
%%        ErrorMsg = <<Key/binary, "=[", Value/binary, "]格式错误"/utf8>>,
%%        lager:error("Post vals error = ~ts", [ErrorMsg]),
%%        {fail, <<"99">>, ErrorMsg}
%%    end;
%%
%%    (_, {fail, _, _} = AccIn) ->
%%      %% previous post kv already validate fail, just pass it throuth
%%      AccIn
%%      end,
%%
%%  {OkOrFail, RespCd, RespMsg} = lists:foldl(F, {ok, <<>>, <<>>}, Params),
%%  {OkOrFail, RespCd, RespMsg}.
validate_format(VL) when is_list(VL) ->
  F =
    fun({Key, Value}) ->
      try
        pg_mcht_protocol_validate_format:validate_format_one_field(Key, Value)
      catch
        _:X ->
          lager:error("Error =~p,stacktrace=~s",
            [X, lager:pr_stacktrace(erlang:get_stacktrace())]),

          ErrorMsg = <<Key/binary, "=[", Value/binary, "]格式错误"/utf8>>,
          lager:error("Post vals error = ~ts", [ErrorMsg]),
          throw({validate_format_fail, <<"99">>, ErrorMsg})
      end
    end,

  lists:foreach(F, VL),
  ok.

validate_format_test() ->
  PostVals = pg_mcht_protocol_SUITE:qs(pay),
  ?assertEqual(ok, validate_format(PostVals)),

  ?assertEqual(ok, validate_format(PostVals ++ [{<<"tranAmt">>, <<"1">>}])),
  ?assertThrow({validate_format_fail, <<"99">>, _},
    validate_format(proplists:delete(<<"merchId">>, PostVals) ++ [{<<"merchId">>, <<"d">>}])),
  ok.

%%------------------------------------------------------
repo_module(mchants) ->
  {ok, Module} = application:get_env(?APP, mcht_repo_name),
  mchants = pg_model:name(Module),
  Module;
repo_module(mcht_txn_log) ->
  {ok, Module} = application:get_env(?APP, mcht_txn_log_repo_name),
  mcht_txn_log = pg_model:name(Module),
  Module;
repo_module(up_txn_log) ->
  {ok, Module} = application:get_env(?APP, up_txn_log_repo_name),
  up_txn_log = pg_model:name(Module),
  Module.

%%------------------------------------------------------

%%------------------------------------------------------
validate_biz(M, P) when is_atom(M), is_tuple(P) ->
  BizValidateItems = [mcht_id, tran_id, txn_amt, quota, payment_method, sig],
  [pg_mcht_protocol_validate_biz:validate_biz_rule(M, P, Item)
    || Item <- BizValidateItems],
  ok.
%%------------------------------------------------------
limit(txn_amt) ->
  {ok, TxnAmtMin} = application:get_env(?APP, limit_txn_amt_min),
  TxnAmtMin;
limit(bank_card_no_len) ->
  {ok, LenLimit} = application:get_env(?APP, limit_bank_card_no_len),
  LenLimit.
%%------------------------------------------------------
out_fields(M) when is_atom(M) ->
  [signature | M:sign_fields()].
%%------------------------------------------------------
out_2_in(M, PV) when is_atom(M), is_list(PV) ->
  pg_protocol:out_2_in(M, PV).


%%------------------------------------------------------
in_2_out(M, Protocol, proplists) when is_atom(M), is_tuple(Protocol) ->
  pg_model:to(M, Protocol, {proplists, out_fields(M), in_2_out_map()});
in_2_out(M, Protocol, post) when is_atom(M), is_tuple(Protocol) ->
  pg_model:to(M, Protocol, {poststring, out_fields(M), in_2_out_map()}).

%%====================================================================
%% Internal functions
%%====================================================================
do_verify_msg(M, P, SignFields) when is_atom(M), is_tuple(P), is_list(SignFields) ->
  SignString = sign_string(M, P),
  Signature = pg_model:get(M, P, signature),

  Direction = direction(M),
  MchtId = pg_model:get(M, P, mcht_id),
  SignMethod = pg_repo:fetch_by(pg_mcht_protocol:repo_module(mchants), binary_to_integer(MchtId), sign_method),


  VerifyResult = case SignMethod of
                   rsa_hex ->
                     pg_mcht_enc:verify_hex(MchtId, Direction, SignString, Signature);
                   rsa_base64 ->
                     pg_mcht_enc:verify(MchtId, Direction, SignString, Signature)
                 end,
  lager:debug("VerifyResult = ~p,SignMethod = ~p,SignString = ~p,Direction = ~p,Sig = ~p",
    [VerifyResult, SignMethod, SignString, Direction, Signature]),

  case VerifyResult of
    true -> ok;
    false ->
      % verify fail
      lager:error("sig verify failed. SignString = ~ts,"
      "Sig=~ts,txnDate = ~ts,txnSeq=~ts",
        [SignString, Signature, pg_model:get(M, P, txn_date),
          pg_model:get(M, P, txn_seq)]),
      fail
  end.

direction(M) when is_atom(M) ->
  Options = M:options(),
  Direction = do_direction(Options),
  Direction.



do_direction(#{direction := Direction})
  when (Direction =:= req) or (Direction =:= resp) ->
  Direction.

direction_test() ->
  ?assertEqual(req, do_direction(#{direction=>req, a=>b})),
  ?assertEqual(req, do_direction(#{direction=>req})),
  ?assertEqual(resp, do_direction(#{direction=>resp, a=>b})),
  ?assertEqual(resp, do_direction(#{direction=>resp})),


  ?assertEqual(req, direction(pg_mcht_protocol_t_protocol_mcht_req_pay)),
  ok.
