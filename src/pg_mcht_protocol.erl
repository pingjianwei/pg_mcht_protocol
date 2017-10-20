-module(pg_mcht_protocol).
-include_lib("eunit/include/eunit.hrl").
-behavior(pg_model).
-behavior(pg_protocol).

%% callbacks
-callback sign_fields() -> [atom()].
-callback options() -> map().
-callback validate() -> boolean().
-callback save(M :: atom(), Protocol :: pg_model:pg_model()) -> ok|fail.

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

]).

-type validate_result() :: ok | fail.
-type resp_cd() :: binary().
-type resp_msg() :: binary().

-define(TxnAmtMin, 50).
-define(BankCardNoLenMin, 15).
-define(BankCardNoLenMax, 21).
%%====================================================================
%% API functions
%%====================================================================
pr_formatter(Field)
  when
  (Field =:= mcht_order_desc)
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
    , bank_id => <<"bankId">>

  }.
%%------------------------------------------------------
-spec get(M :: atom(), Model :: pg_model:pg_model(), Field :: atom())
      -> Value :: any().

get(M, Model, mcht_index_key) when is_atom(M), is_tuple(Model) ->
  {
    pg_model:get(M, Model, mcht_id)
    , pg_model:get(M, Model, mcht_txn_date)
    , pg_model:get(M, Model, mcht_txn_seq)
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
  MchtId = binary_to_integer(pg_model:get(M, P, mcht_id)),

  Direction = direction(M),

  Sig = pg_mcht_enc:sign_hex(MchtId, Direction, SignString),
  lager:debug("SignString = ~ts,Sig = ~ts", [SignString, Sig]),
  {SignString, Sig}.

%%------------------------------------------------------
-spec validate_format(VL) -> Result when
  VL :: proplists:proplist(),
  Result :: {validate_result, validate_result(), resp_cd(), resp_msg()}.

validate_format(Params) when is_list(Params) ->
  F = fun({Key, Value}, {ok, _, _} = _AccIn) when is_binary(Key) ->
    try
      validate_format_one_field(Key, Value),
      {ok, <<>>, <<>>}
    catch
      _:_ ->
        ErrorMsg = <<Key/binary, "=[", Value/binary, "]格式错误"/utf8>>,
        lager:error("Post vals error = ~ts", [ErrorMsg]),
        {fail, <<"99">>, ErrorMsg}
    end;

    (_, {fail, _, _} = AccIn) ->
      %% previous post kv already validate fail, just pass it throuth
      AccIn
      end,

  {OkOrFail, RespCd, RespMsg} = lists:foldl(F, {ok, <<>>, <<>>}, Params),
  {OkOrFail, RespCd, RespMsg}.

validate_format_test() ->
  PostVals = pg_mcht_protocol_SUITE:qs(),
  ?assertEqual({ok, <<>>, <<>>}, validate_format(PostVals)),
  ok.

%% for mcht req
validate_format_one_field(<<"merchId">>, Value) when is_binary(Value) ->
  ok = validate_string(integer, Value);
validate_format_one_field(<<"tranTime">>, Value) when is_binary(Value) ->
  6 = byte_size(Value),
  ok = validate_string(integer, Value);
%%validate_format_one(mcht, <<"origTranDate">>, <<>>) ->
%%  %% can be empty?
%%  ok;
validate_format_one_field(<<"origTranDate">>, Value) when is_binary(Value) ->
  ok = validate_string(date_yyyymmdd, Value);
validate_format_one_field(<<"tranDate">>, Value) when is_binary(Value) ->
  ok = validate_string(date_yyyymmdd, Value);
validate_format_one_field(<<"queryId">>, Value) when is_binary(Value) ->
  ok;
validate_format_one_field(<<"trustBackUrl">>, Value) when is_binary(Value) ->
  <<"http", _/binary>> = Value,
  ok;
validate_format_one_field(<<"trustFrontUrl">>, Value) when is_binary(Value) ->
  <<"http", _/binary>> = Value,
  ok;
validate_format_one_field(<<"tranAmt">>, Value) when is_binary(Value) ->
  ok = validate_string(txn_amt, Value);
validate_format_one_field(<<"bankCardNo">>, <<>>) ->
  %% can be empty
  ok;
validate_format_one_field(<<"bankCardNo">>, Value) when is_binary(Value) ->
  ok = validate_string(bank_card_no, Value);
validate_format_one_field(<<"orderDesc">>, <<>>) ->
  %% orderDesc could not be omit or empty string
  ok = bad;
validate_format_one_field(<<"orderDesc">>, <<"\"\"">>) ->
  ok = bad;
validate_format_one_field(<<"orderDesc">>, _) ->
  ok;
validate_format_one_field(<<"signature">>, <<>>) ->
  %% orderDesc could not be omit or empty string
  ok = bad;
validate_format_one_field(<<"signature">>, <<"\"\"">>) ->
  ok = bad;
validate_format_one_field(<<"signature">>, _) ->
  ok;
validate_format_one_field(_, _) ->
  ok.

validate_string(integer, String) when is_list(String) ->
  validate_string(integer, list_to_binary(String));
validate_string(integer, String) when is_binary(String) ->
  try
    binary_to_integer(String),
    ok
  catch
    _:_ ->
      fail
  end;
validate_string(bank_card_no, Value) when is_binary(Value) ->
  ok = validate_string(integer, Value),
  Len = byte_size(Value),
  true = (?BankCardNoLenMin =< Len) and (?BankCardNoLenMax >= Len),
  ok;
validate_string(txn_amt, Value) when is_binary(Value) ->
  ok = validate_string(integer, Value),
  ok;
validate_string(date_yyyymmdd, Value) when is_binary(Value) ->
%%  8 = byte_size(Value),
  xfutils:assert(yyyymmdd, Value),
%%  ok = validate_string(integer, Value),
%%  <<Y:4/bytes, M:2/bytes, D:2/bytes>> = Value,
%%  Year = binary_to_integer(Y),
%%  Month = binary_to_integer(M),
%%  Day = binary_to_integer(D),
%%  true = (Year > 2000) and (Year < 2030),
%%  true = (Month > 0) and (Month < 13),
%%  true = (Day > 0) and (Day < 32),
  ok.

validate_format_one_test() ->
  ?assertEqual(ok, validate_format_one_field(<<"tranTime">>, <<"121212">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"tranTime">>, <<"u21212">>)),

  ?assertEqual(ok, validate_format_one_field(<<"tranDate">>, <<"20161010">>)),
%%  ?assertError({badmatch, _}, validate_format_one_field(mcht, <<"tranDate">>, <<"201610yy">>)),
  ?assertError(badarg, validate_format_one_field(<<"tranDate">>, <<"201610yy">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"tranDate">>, <<"19991919">>)),

  ?assertEqual(ok, validate_format_one_field(<<"trustBackUrl">>, <<"http://www.a.b">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"trustBackUrl">>, <<"/www.a.b">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"trustBackUrl">>, <<"www.a.b">>)),

  ?assertEqual(ok, validate_format_one_field(<<"trustFrontUrl">>, <<"http://www.a.b">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"trustFrontUrl">>, <<"/www.a.b">>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"trustFrontUrl">>, <<"www.a.b">>)),

  ?assertEqual(ok, validate_format_one_field(<<"tranAmt">>, <<"100">>)),
  ?assertEqual(ok, validate_format_one_field(<<"tranAmt">>, <<"50">>)),
%%  ?assertError({badmatch, _}, validate_format_one_field(mcht, <<"tranAmt">>, <<"49">>)),
%%  ?assertError({badmatch, _}, validate_format_one_field(mcht, <<"tranAmt">>, <<"0">>)),
%%  ?assertError({badmatch, _}, validate_format_one_field(mcht, <<"tranAmt">>, <<"-30">>)),

  ?assertError({badmatch, _}, validate_format_one_field(<<"orderDesc">>, <<>>)),
  ?assertError({badmatch, _}, validate_format_one_field(<<"orderDesc">>, <<"">>)),
  ?assertEqual(ok, validate_format_one_field(<<"orderDesc">>, <<"xxx">>)),

  ok.


%%------------------------------------------------------
-spec save(M, Protocol) -> Result when
  M :: atom(),
  Protocol :: pg_model:pg_model(),
  Result :: ok |fail.

save(M, Protocol) when is_atom(M), is_tuple(Protocol) ->
  Options = M:options(),
  TxnStatus = maps:get(txn_type, Options),

  case maps:get(direction, Options) of
    req ->
      waiting;
    resp ->
      get
  end,

  ok.
%%====================================================================
%% Internal functions
%%====================================================================
do_verify_msg(M, P, SignFields) when is_atom(M), is_tuple(P), is_list(SignFields) ->
  SignString = sign_string(M, P),
  Signature = pg_model:get(M, P, signature),

  Direction = direction(M),

  case pg_mcht_enc:verify_hex(
    binary_to_integer(pg_model:get(M, P, mcht_id))
    , Direction, SignString, Signature) of
    true -> ok;
    false ->
      % verify fail
      lager:error("sig verify failed. SignString = ~ts,"
      "Sig=~ts,txnDate = ~ts,txnSeq=~ts",
        [SignString, Signature, pg_model:get(M, P, mcht_txn_date),
          pg_model:get(M, P, mcht_txn_seq)]),
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
