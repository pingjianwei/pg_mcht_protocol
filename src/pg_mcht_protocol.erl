-module(pg_mcht_protocol).
-include_lib("eunit/include/eunit.hrl").
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
  , verify/2
  , sign_string/2
  , sign/2

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
