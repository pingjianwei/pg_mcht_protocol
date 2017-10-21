%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2016 16:17
%%%-------------------------------------------------------------------
-author("simonxu").
-include("./type_binaries.hrl").

-type request_qs() :: proplists:proplist().

-type date() :: {Year :: 1900..2100, Month :: 1..12, Day :: 1..31}.
-type time() :: {Hour :: 0..23, Min :: 0..59, Second :: 0..59}.

-type mcht_id() :: non_neg_integer().
-type txn_date() :: byte8().
-type txn_time() :: byte6().
-type txn_seq() :: byte23_up().
-type txn_amt() :: non_neg_integer().
-type order_desc() :: bytes().
-type signature() :: bytes().
-type resp_code() :: byte2().
-type resp_msg() :: bytes().
-type url() :: bytes().
-type settle_date() :: byte8().
-type txn_status() :: success | waiting |fail.
-type bank_card_no() :: byte16_21().

-type id_type() :: byte2().
-type id_no() :: byte15() | byte18().
-type id_name() :: bytes().
-type mobile() :: byte11().


-type query_id() :: byte23_up().

-export_type([
  mcht_id/0
  , txn_date/0
  , txn_time/0
  , txn_seq/0
  , txn_amt/0
  , order_desc/0
  , signature/0
  , resp_code/0
  , resp_msg/0
  , url/0
  , settle_date/0
  , txn_status/0
  , bank_card_no/0
  , query_id/0
  , id_type/0
  , id_no/0
  , id_name/0
  , mobile/0
]).