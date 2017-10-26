%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十月 2017 9:17
%%%-------------------------------------------------------------------
-module(pg_mcht_protocol_validate_biz).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([
  validate_biz_rule/3
]).


validate_biz_rule(M, Model, mcht_id) ->
  try

    MchtId = pg_model:get(M, Model, mcht_id),
    {ok, [_Mchant]} = pg_repo:fetch(pg_mcht_protocol:repo_module(mchants), MchtId),
    ok
  catch
    _:{badmatch, _} ->
      %% not found from repo
      MchtId1 = pg_model:get(M, Model, mcht_id),
      lager:error("mcht_id [~p] not exist!", [MchtId1]),
      throw({validate_fail, <<"31">>, <<"商户号不存在"/utf8>>})
  end;
validate_biz_rule(MP, Model, tran_id) ->
  try
    PK = pg_mcht_protocol:get(MP, Model, mcht_index_key),
    MRepo = pg_mcht_protocol:repo_module(mcht_txn_log),
    {ok, []} = pg_repo:fetch(MRepo, PK),
    ok

  catch
    _:{badmatch, X} ->
%%      ?debugFmt("X=~p", [X]),
%%      PK1 = pg_mcht_protocol:get(MP, Model, mcht_index_key),
%%      ?debugFmt("Dup PK = ~p", [PK1]),
%%      MRepo1 = pg_mcht_protocol:repo_module(mcht_txn_log),
%%      {ok, [Repo]} = pg_repo:fetch(MRepo1, PK1),
%%      ?debugFmt("OrigTxnLog = ~p", [Repo]),
      xfutils:cond_lager(pg_mcht_protocol, debug, error, "X = ~p", [X]),
      throw({validate_fail, <<"12">>, <<"商户交易流水号重复"/utf8>>})
  end;
validate_biz_rule(M, Model, sig) ->
  try
    ok = pg_mcht_protocol:verify(M, Model)
  catch
    _:X ->
      lager:error("verify mcht sig error . Reason = ~p", [X]),
      throw({validate_fail, <<"11">>, <<"签名验证失败"/utf8>>})
  end;
validate_biz_rule(M, Model, quota) ->
  ok;
validate_biz_rule(M, Model, payment_method) ->
  do_validate_txn_type(M, Model);
validate_biz_rule(M, Model, txn_amt) ->
  try
    TxnAmtMin = pg_mcht_protocol:limit(txn_amt),
    true = (TxnAmtMin =< pg_model:get(M, Model, txn_amt)),
    ok
  catch
    _:X ->
      xfutils:cond_lager(pg_mcht_protocol, debug, error, "X = ~p", [X]),
      throw({validate_fail, <<"12">>, <<"交易金额太小"/utf8>>})
  end.


%%====================================================
%% internal functions
%%=================================================
get_mcht_payment_method(M, Model) ->
  MchtId = pg_model:get(M, Model, mcht_id),
  MRepoMchants = pg_mcht_protocol:repo_module(mchants),
  [PaymentMethod] = pg_repo:fetch_by(MRepoMchants, MchtId, payment_method),
  PaymentMethod.

do_validate_txn_type(pg_mcht_protocol_req_collect = M, Model) ->
  try
    gw_collect = get_mcht_payment_method(M, Model),
    ok
  catch
    _:{badmatch, _} ->
      throw({validate_fail, <<"32">>, <<"该商户未配置代收业务"/utf8>>})
  end;
do_validate_txn_type(M, Model)
  when (M =:= pg_mcht_protocol_req_pay) orelse
  (M =:= pg_mcht_protocol_t_protocol_mcht_req_pay) ->
  PaymentMethod = get_mcht_payment_method(M, Model),

  [BankId, BankCardNo] = pg_model:get(M, Model, [bank_id, bank_card_no]),
  try
    true = ((PaymentMethod =:= gw_netbank) or (PaymentMethod =:= gw_wap)),
    ok = up_config:check_payment_method(PaymentMethod, BankId, BankCardNo),
    ok
  catch
    _:{badmatch, error_bank_id_not_allowed} ->
      lager:error("Verify payment method error, payment_method = ~p,bank_id = ~p", [PaymentMethod, BankId]),
      throw({validate_fail, <<"32">>, <<"网银无卡通道不允许指定银行直连代码"/utf8>>});
    _:{badmatch, error_bank_id_error} ->
      lager:error("Verify payment method error, payment_method = ~p,bank_id = ~p", [PaymentMethod, BankId]),
      throw({validate_fail, <<"32">>, <<"网银直连银行代码错"/utf8>>});
    _:{badmatch, error_card_no_not_allowd} ->
      lager:error("Verify payment method error, payment_method = ~p,bank_id = ~p,BankCardNo = ~p",
        [PaymentMethod, BankId, BankCardNo]),
      throw({validate_fail, <<"32">>, <<"网银直连银行不允许指定银行卡号"/utf8>>});
    _:X ->
      lager:error("Verify payment method error, Reason = ~p", [X]),
      throw({validate_fail, <<"32">>, <<"支付方式检查错误">>})
  end.

