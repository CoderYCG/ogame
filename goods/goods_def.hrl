%% ----------------------------------------------------------------------------
%% @doc goods module's define
%% ----------------------------------------------------------------------------


%% @doc goods record
-record(goods, {db_id=0, type_id=1, num=0}).

%% @doc max num of goods in a grid
-define(GRID_CAPACITY, 99).
