%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十月 2017 9:17
%%%-------------------------------------------------------------------
-module(ph_mcht_protocol_validate_biz).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([
  validate_biz_rule/3
]).

repo() ->
  pg_mcht_protocol:repo_mcht_module().

validate_biz_rule(M, Model, mcht_id) ->
  try

    MchtId = pg_model:get(M, Model, mcht_id),
    {ok, [Mchant]} = pg_repo:fetch(repo(), MchtId),
    ok
  catch
    _:{badmatch} ->
      %% not found from repo
      throw({validate_fail, <<"31">>, <<"商户号不存在"/utf8>>})
  end;
validate_biz_rule(M, Model, tran_id) ->
  ok;
validate_biz_rule(M, Model, sig) ->
  ok;
validate_biz_rule(M, Model, quota) ->
  ok;
validate_biz_rule(M, Model, bank_id) ->
  ok;
validate_biz_rule(M, Model, payment_method) ->
  ok;
validate_biz_rule(M, Model, txn_amt) ->
  ok.



