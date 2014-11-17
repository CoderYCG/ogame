%% ----------------------------------------------------------------------------
%% @doc goods api
%% ----------------------------------------------------------------------------

-module(goods_api).
-include("goods_def.hrl").
-compile(export_all).

%% @doc init a goods Dict
init() -> goods_api:init().

%% @doc add goods list
%% @param input: [{goods, TypeId, Num}, ...]
add(Dict, []) -> Dict;
add(Dict, [{goods, TypeId, Num} | List]) ->
    FindList = goods_dict:find(type_id, Dict, TypeId),
    Dict1 = add_into(Dict, FindList, {goods, TypeId, Num}),
    add(Dict1, List).

add_into(Dict, _GoodsList, {goods, _TypeId, 0}) -> Dict;
add_into(Dict, [], {goods, TypeId, Num}) -> 
    Goods = make_goods(#goods{}, [{type_id,TypeId}, {num, Num}]),
    Goods1 = add_to_db(Goods),
    goods_dict:add(Dict, Goods1);
add_into(Dict, [#goods{num=GNum}=Goods | GoodsList], {goods, TypeId, Num}) ->
    if GNum + Num =< ?GRID_CAPACITY ->
           goods_dict:update(Dict, Goods, [{num, GNum+Num}]);
       GNum =:= ?GRID_CAPACITY ->
           add_into(Dict, GoodsList, {goods, TypeId, Num});
       true ->
           Dict1 = goods_dict:update(Dict, Goods, [{num, ?GRID_CAPACITY}]),
           add_into(Dict1, GoodsList, {goods, TypeId, Num-(?GRID_CAPACITY-GNum)})
    end.

%% @doc delete goods list
%% @param input: [{goods, TypeId, Num}, ...]
del(Dict, []) -> Dict;
del(Dict, [{goods, TypeId, Num} | List]) ->
    FindList = goods_dict:find(type_id, Dict, TypeId),
    Dict1 = del_from(Dict, FindList, {goods, TypeId, Num}),
    del(Dict1, List).

del_from(Dict, _GoodsList, {goods, _TypeId, 0}) -> Dict;
del_from(Dict, [], {goods, _TypeId, _Num}) -> Dict;
del_from(Dict, [#goods{num=GNum}=Goods | GoodsList], {goods,TypeId, Num}) ->
    if GNum > Num ->
           goods_dict:update(Dict, Goods, [{num, GNum-Num}]);
       GNum =:= Num ->
           goods_dict:del(Dict, Goods);
       true ->
           Dict1 = goods_dict:del(Dict, Goods),
           del_from(Dict1, GoodsList, {goods, TypeId, Num-GNum})
    end.

%% @doc make a #goods{} by key value list
make_goods(Goods, [{Key, Value} | KVList]) -> 
    NewGoods = case Key of
        type_id -> Goods#goods{type_id=Value};
        num -> Goods#goods{num=Value};
        _ -> Goods
    end,
    make_goods(NewGoods, KVList).

%% @doc add goods to db
add_to_db(Goods) ->
    %% db operation ...
    DbId = util:rand(1, 100000),
    Goods#goods{db_id=DbId}.
