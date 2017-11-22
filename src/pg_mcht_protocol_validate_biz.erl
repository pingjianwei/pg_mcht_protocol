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


validate_biz_rule(M, Model, orig_txn) when is_atom(M), is_tuple(Model) ->
  OrigMchtIndexKey = pg_mcht_protocol:get(M, Model, mcht_index_key),
  MRepoMcht = pg_mcht_protocol:repo_module(mcht_txn_log),
  {ok, OrigMchtTxn} = pg_repo:fetch(MRepoMcht, OrigMchtIndexKey),
  case OrigMchtTxn of
    [] ->
      %% not found
      lager:error("Mcht original txn not found! MchtIndex = ~p", [OrigMchtIndexKey]),
      throw({validate_fail, <<"35">>, <<"交易流水号查无原商户交易"/utf8>>});
    _ ->
      ok
  end;
validate_biz_rule(M, Model, mcht_id) ->
  try

    MchtId = pg_model:get(M, Model, mcht_id),
    {ok, [_Mchant]} = pg_repo:fetch(pg_mcht_protocol:repo_module(mchants), MchtId),
    ok
  catch
    _:{badmatch, _} ->
      %% not found from repo
      lager:error("validate fail,mcht_id no exist, Model = ~p", [Model]),
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
      lager:error("validate fail,tran_id duplicated,Model = ~p", [Model]),
      throw({validate_fail, <<"12">>, <<"商户交易流水号重复"/utf8>>})
  end;
validate_biz_rule(M, Model, sig) ->
  try
    ok = pg_mcht_protocol:verify(M, Model)
  catch
    _:X ->
      lager:error("verify mcht sig error . Reason = ~p,Model = ~p", [X, Model]),
      throw({validate_fail, <<"11">>, <<"签名验证失败"/utf8>>})
  end;
validate_biz_rule(_M, _Model, quota) ->
  ok;
validate_biz_rule(M, Model, payment_method) ->
  do_validate_txn_type(M, Model);
validate_biz_rule(M, Model, txn_amt) ->
  try
    TxnAmtMin = pg_mcht_protocol:limit(txn_amt),
    TxnAmt = pg_model:get(M, Model, txn_amt),
    true = (TxnAmtMin =< TxnAmt),
    MchtId = pg_model:get(M, Model, mcht_id),
    [{txn, TxnAmtMax} | _] = pg_repo:fetch_by(pg_mcht_protocol:repo_module(mchants), MchtId, quota),
    true = (TxnAmtMax =:= -1) orelse (TxnAmt =< TxnAmtMax),
    ok
  catch
    _:X ->
      lager:error("validate fail, txn amt too small/large,X = ~p,Model=~p", [X, Model]),
      throw({validate_fail, <<"33">>, <<"交易金额超限"/utf8>>})
  end.


%%====================================================
%% internal functions
%%=================================================
get_mcht_payment_method(M, Model) ->
  MchtId = pg_model:get(M, Model, mcht_id),
  MRepoMchants = pg_mcht_protocol:repo_module(mchants),
  [PaymentMethod] = pg_repo:fetch_by(MRepoMchants, MchtId, payment_method),
  PaymentMethod.

do_validate_txn_type(M, Model)
  when (M =:= pg_mcht_protocol_req_collect)
  orelse (M =:= pg_mcht_protocol_req_batch_collect) ->
  try
    case get_mcht_payment_method(M, Model) of
      gw_collect ->
        ok;
      gw_collect1 ->
        ok;
      _ ->
        throw({badmatch, true})
    end
  catch
    _:{badmatch, _} ->
      lager:error("validate payment method fail, not gw_collect, Model = ~p", [Model]),
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
      lager:error("Verify payment method error, Reason = ~p,Model = ~p", [X, Model]),
      throw({validate_fail, <<"32">>, <<"支付方式检查错误">>})
  end.

