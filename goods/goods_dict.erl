%% ----------------------------------------------------------------------------
%% A goods dict module
%% Key-Value in the dict is: db_id-#goods{}
%% This module just serve goods_api
%% ----------------------------------------------------------------------------

-module(goods_dict).
-include("goods_def.hrl").
-compile(export_all).


%% @doc init a goods dict
init() -> dict:new().

%% @doc add a #goods{} into dict
add(Dict, #goods{db_id=DbId}=Goods) -> 
    dict:store(DbId, Goods, Dict).

%% @doc delete a #goods{} from dict
del(Dict, #goods{db_id=DbId}) ->
    dict:erase(DbId, Dict).

%% @doc update #goods{}
%% @param KVList :: {key, Value}
update(Dict, Goods, []) -> add(Dict, Goods);
update(Dict, Goods, [{Key, Value} | KVList]) ->
    NewGoods = case Key of 
        num -> Goods#goods{num=Value};
        _ -> Goods
    end,
    update(Dict, NewGoods, KVList).

%% @doc find #goods{} from dict
%% @output error | [] | #goods{} | [#goods{}]
find(Type, Dict, DbId) when Type=:=db_id ->
    case dict:find(DbId, Dict) of
        {ok, Value} -> Value;
        error -> error
    end;
find(Type, Dict, TypeId) when Type=:=type_id ->
    FilteredDict = 
    dict:filter(
      fun(_K,#goods{type_id=DTypeId}) -> DTypeId=:=TypeId end, 
      Dict),
    FilteredList = dict:to_list(FilteredDict),
    [Goods ||{_DbId, Goods} <- FilteredList].
